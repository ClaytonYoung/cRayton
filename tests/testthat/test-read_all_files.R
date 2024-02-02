library(testthat)

# Create temporary files for the tests
write.csv(data.frame(a = 1:3, b = 4:6), "temp1.csv")
write.csv(data.frame(a = 1:3, b = 4:6, pidn = c('x', 'y', 'z')), "temp2.csv")

test_that("Basic CSV test", {
  result <- read_all_files(c("temp1.csv", "temp2.csv"))

  # Check if function returns a list using the updated function
  expect_type(result, "list")

  # Check if PIDN column exists for temp2.csv
  expect_true("PIDN" %in% colnames(result$temp2))
})

test_that("Invalid filename", {
  # Expecting an error since the file doesn't exist
  expect_error(read_all_files("non_existent.csv"))
})

test_that("Testing prefix", {
  result <- read_all_files("temp1.csv", prefix = "myprefix")
  # Check if the list name starts with the prefix
  expect_true(grepl("^myprefix_", names(result)[[1]]))
})

test_that("Invalid file type", {
  # Write a temporary txt file
  write("This is a txt file", "temp.txt")
  # Expecting an error since .txt isn't a valid type
  expect_error(read_all_files("temp.txt"), "The following files are neither in .csv nor .xlsx/.xls format: temp.txt")
})

# Remove temporary files
file.remove("temp1.csv", "temp2.csv", "temp.txt")

