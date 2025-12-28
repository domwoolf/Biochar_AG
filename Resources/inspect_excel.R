
library(readxl)

file_path <- "op_space_2.41.xlsm"

sheets <- excel_sheets(file_path)
print("Sheets found:")
print(sheets)

for (sheet in sheets) {
  cat("\n--- Sheet:", sheet, "---\n")
  tryChange <- tryCatch({
    data <- read_excel(file_path, sheet = sheet, n_max = 5)
    print(data)
  }, error = function(e) {
    print(paste("Error reading sheet:", sheet, "-", e$message))
  })
}
