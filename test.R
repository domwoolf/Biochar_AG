md_units <- function(x, latex = TRUE) {
  x[is.na(x)] <- ""
  if (latex) {
    x <- gsub("([$%&_#])", "\\\\\\1", x)
    x <- gsub("\\^([^^]+)\\^", "\\\\textsuperscript{\\1}", x)
    gsub("~([^~]+)~", "\\\\textsubscript{\\1}", x)
  } else {
    x <- gsub("\\^([^^]+)\\^", "<sup>\\1</sup>", x)
    gsub("~([^~]+)~", "<sub>\\1</sub>", x)
  }
}
print(md_units("CO~2~ Transport & Storage"))
