# =============================================================================
# dta_header.R  --  read column names/types straight from a .dta binary header.
# No data parser involved, so it CANNOT hit the strL memory failure: the
# names/types live in plain bytes a few KB into the file. Formats 117-119.
# Used by read_hmda_2025.R (inspection) and 01_extract.R (to exclude strLs).
# =============================================================================

dta_header <- function(path, max_bytes = 8e6) {
  con <- file(path, "rb"); on.exit(close(con))
  raw <- readBin(con, "raw", n = max_bytes)
  rfind <- function(s) {                      # find ASCII tag in raw bytes
    pat <- charToRaw(s)
    idx <- which(raw == pat[1])
    for (k in seq_along(pat)[-1]) idx <- idx[raw[idx + k - 1L] == pat[k]]
    if (!length(idx)) stop("tag not found: ", s)
    idx[1]
  }
  rel  <- as.integer(rawToChar(raw[rfind("<release>") + 9:11]))
  if (!rel %in% 117:119) stop("Unsupported .dta format: ", rel)
  bo   <- rawToChar(raw[rfind("<byteorder>") + 11:13])
  endian <- if (bo == "LSF") "little" else "big"
  kpos <- rfind("<K>") + 3L
  K <- if (rel == 119) readBin(raw[kpos + 0:3], "integer", size = 4, endian = endian)
       else            readBin(raw[kpos + 0:1], "integer", size = 2,
                               signed = FALSE, endian = endian)
  npos <- rfind("<N>") + 3L
  N <- if (rel == 117) readBin(raw[npos + 0:3], "integer", size = 4, endian = endian)
       else {                                   # 8-byte int: combine two words
    w1 <- readBin(raw[npos + 0:3], "integer", size = 4, endian = endian)
    w2 <- readBin(raw[npos + 4:7], "integer", size = 4, endian = endian)
    if (endian == "little") (w1 %% 2^32) + w2 * 2^32 else (w2 %% 2^32) + w1 * 2^32
  }
  tpos  <- rfind("<variable_types>") + 16L
  types <- readBin(raw[tpos + seq_len(2 * K) - 1L], "integer", n = K,
                   size = 2, signed = FALSE, endian = endian)
  w     <- if (rel == 117) 33L else 129L
  vpos  <- rfind("<varnames>") + 10L
  nm <- vapply(seq_len(K), function(i) {
    b <- raw[vpos + (i - 1L) * w + seq_len(w) - 1L]
    z <- which(b == as.raw(0))[1]
    rawToChar(b[seq_len(if (is.na(z)) w else z - 1L)])
  }, character(1))
  type_label <- ifelse(types >= 1 & types <= 2045, paste0("str", types),
                ifelse(types == 32768, "strL",
                c("65526" = "double", "65527" = "float", "65528" = "long",
                  "65529" = "int", "65530" = "byte")[as.character(types)]))
  out <- data.frame(name = nm, type = type_label, is_strL = types == 32768,
                    stringsAsFactors = FALSE)
  attr(out, "n_rows") <- N; attr(out, "format") <- rel
  out
}
