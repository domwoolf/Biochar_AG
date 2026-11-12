# scripts/results_cache.R
# Fingerprints of the model and an on-disk cache for the slow analysis steps (spatial SHAP, Monte Carlo).
#
# The cache lives outside the repository, in BIOCHAR_CACHE_DIR (default below). Each cached object is
# stored under a key that hashes the model fingerprint (package code, input data, parameters.csv without
# its text columns, processed GIS layers) together with the code and settings of the stage that produced
# it, so an object is reused only if nothing that could change it has changed. Stale entries are never
# read; they can be deleted by hand.
#
# The fingerprint functions mirror those of Article/pipeline.qmd, which uses them to decide whether to rerun
# a whole step; keep the two in step. Paths are relative to the repository root (the working directory of
# every pipeline step).

CACHE_DIR_DEFAULT <- "/run/media/dominic/hdd18/BiomassTEA/results_cache"

cache_dir <- function(sub = NULL) {
  d <- Sys.getenv("BIOCHAR_CACHE_DIR", CACHE_DIR_DEFAULT)
  if (!dir.exists(dirname(d))) stop("Cache drive not available: ", dirname(d), " (set BIOCHAR_CACHE_DIR)")
  d <- if (is.null(sub)) d else file.path(d, sub)
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  d
}

files_in <- function(paths, root = ".") {
  paths <- file.path(root, paths)
  f <- unlist(lapply(paths, function(p) if (dir.exists(p)) list.files(p, recursive = TRUE, full.names = TRUE) else p))
  sort(f[file.exists(f)])
}
fp_hash <- function(..., root = ".") unname(tools::md5sum(files_in(c(...), root))) # content hash
fp_stat <- function(..., root = ".") { # size + mtime, for large untracked data
  i <- file.info(files_in(c(...), root))
  paste(basename(rownames(i)), i$size, as.numeric(i$mtime))
}
# parameters.csv is hashed without its text columns (description, units, notes, category), so that editing
# wording does not invalidate the model results
fp_params <- function(root = ".") {
  x <- read.csv(file.path(root, "BiocharAG/inst/extdata/parameters.csv"), colClasses = "character")
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f))
  write.csv(x[, setdiff(names(x), c("description", "units", "notes", "category"))], f, row.names = FALSE)
  unname(tools::md5sum(f))
}
fp_model <- function(root = ".") {
  extdata <- setdiff(list.files(file.path(root, "BiocharAG/inst/extdata"), recursive = TRUE), "parameters.csv")
  list(fp_hash("BiocharAG/R", file.path("BiocharAG/inst/extdata", extdata), "BiocharAG/DESCRIPTION",
               "scripts/manuscript_figures.R", root = root),
       fp_params(root), fp_stat("GIS/processed", root = root))
}

# Short hash of any R values (functions are hashed by their source text, so editing a function's code
# changes the key, but editing other code in the same script does not)
cache_key <- function(...) {
  x <- lapply(list(...), function(v) if (is.function(v)) deparse(v) else v)
  f <- tempfile()
  on.exit(unlink(f))
  saveRDS(x, f, version = 3, compress = FALSE)
  substr(unname(tools::md5sum(f)), 1, 16)
}

# Return the cached value of `expr` stored under name and key, computing and saving it if absent
# (compressed RDS; written to a temporary file and renamed, so an interrupted write leaves no entry)
cached <- function(name, key, expr, sub = NULL) {
  f <- file.path(cache_dir(sub), sprintf("%s-%s.rds", name, key))
  if (file.exists(f)) {
    message("  cache hit: ", basename(f))
    return(readRDS(f))
  }
  val <- expr
  tmp <- paste0(f, ".tmp")
  saveRDS(val, tmp, compress = "xz")
  file.rename(tmp, f)
  message("  cached: ", basename(f), sprintf(" (%.1f MB)", file.size(f) / 1e6))
  val
}
