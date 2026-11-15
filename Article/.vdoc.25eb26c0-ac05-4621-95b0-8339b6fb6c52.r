# Helpers shared by the SI tables
# Units in parameters.csv use Pandoc markdown (Mg^-1^, CO~2~); tables need LaTeX (or HTML) markup
is_docx <- knitr::pandoc_to("docx")

# Word tables (flextable): Pandoc markup in units and labels becomes Unicode super/subscripts; optional
# heading rows per group; fixed column widths (inches, summing to the 6.5-inch text width)
uni_units <- function(x) {
  sup <- c("0" = "\u2070", "1" = "\u00b9", "2" = "\u00b2", "3" = "\u00b3", "4" = "\u2074", "5" = "\u2075",
    "6" = "\u2076", "7" = "\u2077", "8" = "\u2078", "9" = "\u2079", "-" = "\u207b", "+" = "\u207a")
  sub <- c("0" = "\u2080", "1" = "\u2081", "2" = "\u2082", "3" = "\u2083", "4" = "\u2084", "5" = "\u2085",
    "6" = "\u2086", "7" = "\u2087", "8" = "\u2088", "9" = "\u2089", "e" = "\u2091", "h" = "\u2095",
    "t" = "\u209c", "a" = "\u2090", "o" = "\u2092")
  conv <- function(s, map, delim) {
    pat <- paste0("\\", delim, "([^", delim, "]+)\\", delim)
    m <- gregexpr(pat, s, perl = TRUE)
    regmatches(s, m) <- lapply(regmatches(s, m), function(v) vapply(v, function(t) {
      ch <- strsplit(substr(t, 2, nchar(t) - 1), "")[[1]]
      if (all(ch %in% names(map))) paste(map[ch], collapse = "") else substr(t, 2, nchar(t) - 1)
    }, character(1)))
    s
  }
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- conv(x, sup, "^")
  conv(gsub("\\\\\\$", "$", x), sub, "~")
}
word_table <- function(df, widths, group = NULL, mono = NULL, font_size = 8) {
  df[] <- lapply(df, uni_units)
  if (!is.null(group)) {
    gd <- flextable::as_grouped_data(df, groups = group)
    ft <- flextable::as_flextable(gd, hide_grouplabel = TRUE)
    grp_rows <- which(!is.na(gd[[group]]))
    ft <- flextable::bold(ft, i = grp_rows, part = "body")
    ft <- flextable::bg(ft, i = grp_rows, bg = "#EDEDED", part = "body")
  } else {
    ft <- flextable::flextable(df)
  }
  ft <- flextable::font(ft, fontname = "Times New Roman", part = "all")
  ft <- flextable::fontsize(ft, size = font_size, part = "all")
  if (!is.null(mono)) { # Consolas looks larger than Times at the same size
    ft <- flextable::font(ft, j = mono, fontname = "Consolas", part = "body")
    ft <- flextable::fontsize(ft, j = mono, size = font_size - 1, part = "body")
  }
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::valign(ft, valign = "top", part = "body")
  ft <- flextable::padding(ft, padding.top = 1, padding.bottom = 1, part = "all")
  ft <- flextable::set_table_properties(ft, layout = "fixed")
  flextable::width(ft, width = widths)
}

md_units <- function(x, latex = knitr::is_latex_output()) {
  x[is.na(x)] <- ""
  if (is_docx) return(x) # Pandoc markup renders directly in Word (pipe tables)
  if (latex) {
    x <- gsub("([$%&_#])", "\\\\\\1", x)
    x <- gsub("\\^([^^]+)\\^", "\\\\textsuperscript{\\1}", x)
    gsub("~([^~]+)~", "\\\\textsubscript{\\1}", x)
  } else {
    x <- gsub("\\^([^^]+)\\^", "<sup>\\1</sup>", x)
    gsub("~([^~]+)~", "<sub>\\1</sub>", x)
  }
}
