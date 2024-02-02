# Test: combiner function without duplicated data frames
test_that("combiner handles list without duplicates", {
  df1 <- data.frame(a = 1:3)
  df2 <- data.frame(b = 4:6)
  input <- list(one = df1, two = df2)

  result <- combiner(input)

  expect_equal(length(result), 2)
  expect_equal(result$one, df1)
  expect_equal(result$two, df2)
})

# Test: combiner function with duplicated named data frames with same rows
test_that("combiner handles duplicated data frames with same rows", {
  df1 <- data.frame(a = 1:3)
  df2 <- data.frame(b = 4:6)
  input <- list(one = df1, two = df2, one = df1)

  result <- combiner(input)

  expect_equal(length(result), 2)
  expect_equal(result$one, df1)
  expect_equal(result$two, df2)
})

# Test: combiner function with duplicated named data frames with different rows
test_that("combiner handles duplicated data frames with different rows", {
  df1 <- data.frame(a = 1:3)
  df2 <- data.frame(b = 4:6)
  df1_dup <- data.frame(a = 4:6)
  input <- list(one = df1, two = df2, one = df1_dup)

  result <- combiner(input)

  expect_equal(length(result), 2)
  expect_equal(nrow(result$one), 6)
  expect_equal(result$two, df2)
})
