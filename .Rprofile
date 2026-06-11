options(
  lintr.linters = lintr::linters_with_defaults(
    line_length_linter = lintr::line_length_linter(120),
    indentation_linter = lintr::indentation_linter(4),
    object_usage_linter = NULL
  )
)

setHook(packageEvent("lintr", "onLoad"), function(...) {
  options(
    lintr.linters = lintr::linters_with_defaults(
      line_length_linter = lintr::line_length_linter(120),
      indentation_linter = lintr::indentation_linter(4),
      object_usage_linter = NULL
    )
  )
})
