# Load the testthat package
library(testthat)

# Test for coalesce_join function
test_that("coalesce_join works correctly", {

  # Test 1: Full join without NA
  df1 <- data.frame(a = 1:3, b = 4:6)
  df2 <- data.frame(a = 3:5, c = 7:9)
  res1 <- coalesce_join(df1, df2, by = "a")
  exp1 <- data.frame(a = c(1, 2, 3, 4, 5), b = c(4, 5, 6, NA, NA), c = c(NA, NA, 7, 8, 9))
  expect_equal(res1, exp1)

  # Test 2: Full join with NAs and coalescing
  df3 <- data.frame(a = 1:3, b = c(NA, NA, 3))
  df4 <- data.frame(a = 2:4, b = 2:4)
  res2 <- coalesce_join(df3, df4, by = "a")
  exp2 <- data.frame(a = c(1, 2, 3, 4), b = c(NA, 2, 3, 4))
  expect_equal(res2, exp2)

  # Test 3: Using a different join function (left_join)
  df5 <- data.frame(a = 1:3, b = 4:6)
  df6 <- data.frame(a = 3:5, c = 7:9)
  res3 <- coalesce_join(df5, df6, by = "a")
  exp3 <- data.frame(a = 1:5, b = c(4:6, NA, NA), c = c(NA, NA, 7:9))
  expect_equal(res3, exp3)

  # Test 4: Coalesce multi-columns
  df7 <- data.frame(a = 1:3, b = c(NA, NA, 3), c = c(NA, 5, NA))
  df8 <- data.frame(a = 2:4, b = 2:4, c = 7:9)
  res4 <- coalesce_join(df7, df8, by = "a")
  exp4 <- data.frame(a = c(1, 2, 3, 4), b = c(NA, 2, 3, 4), c = c(NA, 5, 8, 9))
  expect_equal(res4, exp4)

})
