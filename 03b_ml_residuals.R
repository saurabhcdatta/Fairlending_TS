# =============================================================================
# 03b_ml_residuals.R -- MACHINE-LEARNING race-blind residuals (DML-style),
#                       drop-in companion to 03_models.R.
#
# Run order: 02_prepare.R -> 03_models.R -> THIS FILE -> 04a (or 04) -> 05/06.
# Writes residuals in the exact schema 03 produces, so every downstream
# stage runs unchanged. Keep 03's econometric residuals as the model of
# record; use this track for robustness -- cells flagged by BOTH tracks are
# the strongest field cases.
#
# WHAT MAKES THESE RESIDUALS VALID FOR SCREENING (each is deliberate):
#
#   GROUPED CROSS-FITTING BY LEI. Folds split by INSTITUTION, never by row.
#   If a CU's own loans helped train the model that scores them, the model
#   learns that CU's practices as "normal" and shrinks exactly the residuals
#   the screen needs. Every prediction here comes from a model that never
#   saw any loan from that credit union.
#
#   RACE-BLIND, CURATED FEATURES ONLY. The same legitimate underwriting
#   factors as the econometric track (continuous, not binned -- that is the
#   ML gain), with geography kept COARSE (state x metro). Rich geography or
#   income micro-features would let a flexible learner reconstruct race by
#   proxy and mask discrimination by construction. More model, not more
#   features.
#
#   MONOTONE CONSTRAINTS (xgboost engine). Denial risk falls with credit
#   score and rises with DTI/CLTV by construction -- correctness plus
#   examiner explainability.
#
#   CALIBRATION DIAGNOSTICS. Residual MEANS only mean something if the
#   predicted probabilities are calibrated; a decile calibration table is
#   printed and saved for the record.
#
#   PER-LOAN SHAP EXPLANATIONS (xgboost engine) for adverse-residual loans:
#   "the model expected approval because score=761, CLTV=80" -- the exact
#   sentence a field case package needs.
#
# ENGINES: xgboost preferred (install.packages("xgboost") -- Windows binary
# on the workstation); ranger fallback (no monotonicity/SHAP; importance
# still exported). The FILTERS block pins everything, seed included.
# =============================================================================

library(data.table)
library(Matrix)

source("settings.R")

# ------------------------------- FILTERS (edit here) --------------------------
engine        <- "auto"   # "auto" | "xgboost" | "ranger"
k_folds       <- 5L       # LEI-grouped cross-fitting folds
seed          <- 20250101
# xgboost
xgb_nrounds   <- 600L; xgb_eta <- 0.05; xgb_depth <- 6L
xgb_early     <- 40L      # early-stopping rounds (10% of train as validation)
# ranger
rf_trees      <- 500L
# outputs
make_default  <- TRUE     # copy ML residuals over residuals_2025.rds
                          # (econ residuals backed up to residuals_econ_2025.rds)
shap_cuts     <- c(denial = 0.5, withdrawal = 0.5, pricing = 0.25)
shap_max_loans <- 5000L   # cap SHAP export size
# ------------------------------------------------------------------------------

set.seed(seed)
if (engine == "auto") {
  engine <- if (requireNamespace("xgboost", quietly = TRUE)) {
    "xgboost"
  } else if (requireNamespace("ranger", quietly = TRUE)) {
    "ranger"
  } else {
    stop("Install xgboost (preferred) or ranger:\n",
         "  install.packages(\"xgboost\")", call. = FALSE)
  }
}
cat("ML engine:", engine,
    if (engine == "ranger") "(no monotone constraints / SHAP on this engine)",
    "\n")

dat <- readRDS(out("analysis_2025.rds"))

# ---- features: the curated Popick set, continuous, coarse geography ----------
num_feats  <- c("credit_score", "dti", "ltv_combined", "log_amount",
                "log_income", "log_origs")
bin_feats  <- c("aus", "broker", "other_lien", "early_bankrupt",
                "offers_other_types", "jumbo", "metro")
cat_feats  <- c("loan_cat", "property_state", "year_month")
dat[, `:=`(log_amount = log(pmax(loan_amount, 1)),
           log_income = log(pmax(income, 1)),
           log_origs  = log1p(total_origs))]

# median-impute numerics + missing indicators (engine-agnostic NA handling)
for (v in num_feats) {
  mi <- paste0(v, "_mis")
  dat[, (mi) := as.integer(is.na(get(v)))]
  med <- dat[, median(get(v), na.rm = TRUE)]
  dat[is.na(get(v)), (v) := med]
}
mis_feats <- paste0(num_feats, "_mis")

.design <- function(d, credit) {
  nf <- if (credit) num_feats else setdiff(num_feats,
                                           c("credit_score", "dti",
                                             "ltv_combined"))
  mf <- if (credit) mis_feats else setdiff(mis_feats,
          paste0(c("credit_score", "dti", "ltv_combined"), "_mis"))
  fml <- as.formula(paste("~ 0 +", paste(c(nf, mf, bin_feats, cat_feats),
                                         collapse = " + ")))
  sparse.model.matrix(fml, data = d[, c(nf, mf, bin_feats, cat_feats),
                                    with = FALSE])
}

# monotone constraints by column name (xgboost only)
.mono <- function(cols, credit, outcome) {
  m <- integer(length(cols))
  if (credit && outcome %in% c("binary", "rate")) {
    m[cols == "credit_score"] <- -1L
    m[cols == "dti"]          <-  1L
    m[cols == "ltv_combined"] <-  1L
  }
  m
}

.auc <- function(y, p) {                       # rank (Wilcoxon) AUC
  r <- rank(p)                                 # numeric throughout: n1*n0
  n1 <- as.numeric(sum(y == 1)); n0 <- as.numeric(sum(y == 0))
  if (n1 == 0 || n0 == 0) return(NA_real_)     # overflows 32-bit integers
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

.fit_predict_fold <- function(X_tr, y_tr, X_te, binary, mono) {
  if (engine == "xgboost") {
    v  <- sample(nrow(X_tr), max(1000L, floor(0.1 * nrow(X_tr))))
    dtr <- xgboost::xgb.DMatrix(X_tr[-v, , drop = FALSE], label = y_tr[-v])
    dva <- xgboost::xgb.DMatrix(X_tr[ v, , drop = FALSE], label = y_tr[ v])
    prm <- list(objective = if (binary) "binary:logistic" else "reg:squarederror",
                eta = xgb_eta, max_depth = xgb_depth,
                subsample = 0.8, colsample_bytree = 0.8,
                monotone_constraints = paste0("(", paste(mono, collapse = ","),
                                              ")"),
                nthread = 0)
    args <- list(params = prm, data = dtr, nrounds = xgb_nrounds,
                 early_stopping_rounds = xgb_early, verbose = 0)
    args[[if (utils::packageVersion("xgboost") >= "2.1.0") "evals"
          else "watchlist"]] <- list(va = dva)
    m <- do.call(xgboost::xgb.train, args)
    list(model = m,
         pred  = predict(m, xgboost::xgb.DMatrix(X_te)))
  } else {
    m <- ranger::ranger(x = X_tr, y = if (binary) factor(y_tr) else y_tr,
                        num.trees = rf_trees, probability = binary,
                        respect.unordered.factors = "order",
                        num.threads = 0, seed = seed, verbose = FALSE,
                        importance = "impurity")
    p <- predict(m, data = X_te)$predictions
    list(model = m, pred = if (binary) p[, "1"] else p)
  }
}

# ---- the cross-fitted residual engine -----------------------------------------
crossfit <- function(d, yvar, credit, binary, label, outcome_kind) {
  X <- .design(d, credit)
  y <- d[[yvar]]
  mono <- .mono(colnames(X), credit, outcome_kind)
  leis  <- unique(d$lei)
  fmap  <- data.table(lei = leis,
                      fold = sample(rep_len(seq_len(k_folds), length(leis))))
  d[, fold := fmap[.SD, on = "lei", x.fold]]
  pred <- rep(NA_real_, nrow(d)); imp <- NULL; models <- list()
  for (k in seq_len(k_folds)) {
    tr <- which(d$fold != k); te <- which(d$fold == k)
    if (!length(te)) next
    fk <- .fit_predict_fold(X[tr, , drop = FALSE], y[tr],
                            X[te, , drop = FALSE], binary, mono)
    pred[te] <- fk$pred
    models[[k]] <- fk$model
    ii <- if (engine == "xgboost")
      xgboost::xgb.importance(model = fk$model)[, .(feature = Feature,
                                                    gain = Gain)]
    else data.table(feature = names(fk$model$variable.importance),
                    gain = fk$model$variable.importance /
                           sum(fk$model$variable.importance))
    imp <- rbind(imp, ii)
  }
  d[, resid := y - pred]
  d[, .p := pred]                    # as a real column -- inside by-groups a
                                     # bare env vector silently gives the
                                     # OVERALL mean for every decile
  # diagnostics
  if (binary) {
    cat(sprintf("  [%s] OOF AUC = %.3f | mean(p)=%.3f vs mean(y)=%.3f\n",
                label, .auc(y, pred), mean(pred, na.rm = TRUE), mean(y)))
    d[, .dec := ceiling(10 * frank(.p, ties.method = "first") / .N)]
    calib <- d[, .(mean_pred = mean(.p), mean_obs = mean(get(yvar)),
                   n = .N), by = .(decile = .dec)][order(decile)]
    d[, .dec := NULL]
    print(calib[, .(decile, mean_pred = round(mean_pred, 3),
                    mean_obs = round(mean_obs, 3), n)])
    .CALIB[[label]] <<- calib[, `:=`(model = label)]
  } else {
    cat(sprintf("  [%s] OOF R2 = %.3f\n", label,
                1 - var(d$resid, na.rm = TRUE) / var(y, na.rm = TRUE)))
  }
  d[, .p := NULL]
  .IMP[[label]] <<- imp[, .(gain = mean(gain)), by = feature][
                      order(-gain)][, model := label]
  list(d = d, X = X, models = models, fold = d$fold)
}

.CALIB <- list(); .IMP <- list()

# ---- denial ---------------------------------------------------------------------
cat("== ML denial (grouped 5-fold, race-blind) ==\n")
den_d <- dat[in_denial_universe == TRUE & !is.na(group)]
den <- crossfit(den_d, "denied", credit = TRUE, binary = TRUE,
                "denial", "binary")

# ---- withdrawal (no-credit feature set) -------------------------------------------
cat("== ML withdrawal ==\n")
wdr_d <- dat[in_withdrawal_universe == TRUE & !is.na(group)]
wdr <- crossfit(wdr_d, "withdrawn", credit = FALSE, binary = TRUE,
                "withdrawal", "binary")
wdr_cf <- wdr                             # kept for SHAP below

# ---- pricing: four components -----------------------------------------------------
rate_dv <- if (dat[, any(!is.na(ir_spread_pmms))]) {
  "ir_spread_pmms"
} else "interest_rate"
comps <- c(interest_rate = rate_dv, disc_points = "disc_pts_pct",
           lender_credits = "lend_cred_pct", loan_costs = "loan_cost_pct")
pri_res <- list(); pri_models <- NULL
for (nm in names(comps)) {
  cat(sprintf("== ML pricing: %s ==\n", nm))
  pd <- dat[originated == 1 & !is.na(get(comps[[nm]])) & !is.na(group)]
  pr <- crossfit(pd, comps[[nm]], credit = TRUE, binary = FALSE,
                 nm, if (nm == "interest_rate") "rate" else "other")
  pri_res[[nm]] <- pr$d[, .(uli, lei, cu_number, group, loan_cat,
                            component = nm, resid)]
  if (nm == "interest_rate") pri_models <- pr    # kept for SHAP below
}
W <- dcast(rbindlist(pri_res), uli + lei + cu_number + group + loan_cat ~
             component, value.var = "resid")
for (v in names(comps)) if (!v %in% names(W)) W[, (v) := NA_real_]
W[, resid_price := interest_rate]
W[, resid_oth   := disc_points - lender_credits + loan_costs]

# ---- save in 03's exact schema ------------------------------------------------------
ml <- list(
  denial     = den$d[, .(uli, lei, cu_number, group, loan_cat,
                         resid_denial = resid)],
  withdrawal = wdr$d[, .(uli, lei, cu_number, group, loan_cat,
                         resid_withdrawn = resid)],
  pricing    = W[!is.na(resid_price),
                 .(uli, lei, cu_number, group, loan_cat, resid_price,
                   resid_oth)])
saveRDS(ml, out("residuals_ml_2025.rds"), compress = FALSE)
fwrite(rbindlist(.IMP), out("ml_importance_2025.csv"))
if (length(.CALIB)) fwrite(rbindlist(.CALIB), out("ml_calibration_2025.csv"))

# residual agreement with the econometric track, if present
econ_f <- out("residuals_2025.rds")
if (file.exists(econ_f)) {
  econ <- readRDS(econ_f)
  a <- merge(ml$denial, econ$denial, by = "uli",
             suffixes = c("_ml", "_econ"))
  cat(sprintf("Track agreement: cor(ML, econ) denial residuals = %.3f on %s loans\n",
              a[, cor(resid_denial_ml, resid_denial_econ)],
              format(nrow(a), big.mark = ",")))
}
if (make_default) {
  if (file.exists(econ_f) && !file.exists(out("residuals_econ_2025.rds")))
    file.copy(econ_f, out("residuals_econ_2025.rds"))
  file.copy(out("residuals_ml_2025.rds"), econ_f, overwrite = TRUE)
  cat("residuals_2025.rds now = ML track (econ backed up to",
      "residuals_econ_2025.rds); rerun 04a + 06 to rescreen.\n")
}

# ---- SHAP for adverse-residual loans (xgboost only): the field explanation ----
if (engine == "xgboost") {
  shap_rows <- list()
  grab <- function(cf, tab, resid_col, screen) {
    idx <- which(tab[[resid_col]] >= shap_cuts[[screen]] &
                 tab$group != "white")
    if (!length(idx)) return(NULL)
    idx <- head(idx[order(-tab[[resid_col]][idx])], shap_max_loans)
    ct <- predict(cf$models[[1]],
                  xgboost::xgb.DMatrix(cf$X[idx, , drop = FALSE]),
                  predcontrib = TRUE)
    cn <- colnames(ct)
    rbindlist(lapply(seq_along(idx), function(i) {
      o <- order(-abs(ct[i, cn != "BIAS"]))[1:5]
      data.table(uli = tab$uli[idx[i]], screen = screen,
                 feature = cn[cn != "BIAS"][o],
                 contribution = round(ct[i, cn != "BIAS"][o], 4))
    }))
  }
  shap_rows[["denial"]]     <- grab(den, den$d, "resid", "denial")
  shap_rows[["withdrawal"]] <- grab(wdr_cf, wdr_cf$d, "resid", "withdrawal")
  shap_rows[["pricing"]] <- if (!is.null(pri_models))
    grab(pri_models, pri_models$d, "resid", "pricing")
  shap <- rbindlist(shap_rows, fill = TRUE)
  if (nrow(shap)) {
    fwrite(shap, out("shap_outliers_2025.csv"))
    cat(sprintf("SHAP explanations for %s adverse loans -> shap_outliers_2025.csv\n",
                format(uniqueN(shap$uli), big.mark = ",")))
  }
} else cat("(SHAP export skipped: available on the xgboost engine)\n")

cat("Done. ML residuals ->", out("residuals_ml_2025.rds"), "\n")
