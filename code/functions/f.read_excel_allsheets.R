f.read_excel_allsheets <- function(filename, tibble = FALSE, skip = 0) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, skip = skip))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}
