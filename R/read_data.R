# Define a function to read data from a file
read_data <- function(filename) {
  if (tolower(tools::file_ext(filename)) == "csv") {
    data <- readr::read_csv(filename)
  } else if (tolower(tools::file_ext(filename)) %in% c("xls", "xlsx")) {
    data <- readxl::read_excel(filename)
  } else if (tolower(tools::file_ext(filename)) == "txt") {
    data <- utils::read.table(filename, header = TRUE)
  } else if (tolower(tools::file_ext(filename)) %in% c("rdata", "rds")) {
    data <- readRDS(filename)
  } else {
    stop("Unsupported file format.")
  }
  return(data)
}
