# 1. Identify if this is the MAIN language server process (ignores hidden background workers)
is_main_lsp <- any(grepl("languageserver", commandArgs(trailingOnly = FALSE), ignore.case = TRUE))

if (is_main_lsp) {
  if (file.exists("DESCRIPTION")) {
    suppressMessages(try(devtools::load_all(".", quiet = TRUE), silent = TRUE))
  } else if (file.exists("BiocharAG/DESCRIPTION")) {
    suppressMessages(try(devtools::load_all("BiocharAG", quiet = TRUE), silent = TRUE))
  }
}

# 2. Override styler behavior to prevent line wrapping on save (Applies to all LSP processes)
if (interactive() || Sys.getenv("VSCR_LSP_PORT") != "") {
  options(
    languageserver.formatting_style = function(options) {
      styler::tidyverse_style(
        indent_by = 2,
        scope = "indention"
      )
    }
  )
}
