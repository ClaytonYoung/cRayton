library(testthat)
library(dplyr)

# Define a test
test_that("score_berlin works as expected", {

  # Create a minimal data frame with known values
  test_data <- data.frame(
    do_you_snore = c("Yes", "No", "Yes"),
    your_snoring_is = c("Louder than talking", NA, "normal"),
    how_often_do_you_snore = c("3-4 times a week", NA, "never"),
    has_your_snoring_ever_bothered_other_people = c("Yes", "No", "Yes"),
    has_anyone_noticed_that_you_quit_breathing_during_your_sleep = c("Almost every day", "No", "No"),
    how_often_do_you_feel_tired_or_fatigued_after_your_sleep = c("Almost every day", "3-4 times a week", "Never"),
    during_your_waking_time_do_you_feel_tired_fatigued_or_not_up_to_par = c("Almost every day", "No", "No"),
    have_you_ever_nodded_off_or_fallen_asleep_while_driving_a_vehicle = c("Yes", "No", "No"),
    do_you_have_high_blood_pressure = c("Yes", "No", "Yes"),
    bmi = c(32, 22, 35)
  )

  # Run the function
  result <- score_berlin(test_data)

  # Check if the output has the required columns
  expect_true(all(c("cat1", "cat2", "cat3", "cat1_pos", "cat2_pos", "cat3_pos", "total", "Berlin_apneaRisk") %in% names(result)))

  # Check if the output values are as expected
  expect_equal(result$cat1, c(6, 0, 2))
  expect_equal(result$cat2, c(3, 1, 0))
  expect_equal(result$cat3, c(2, 0, 2))
  expect_equal(result$total, c(3, 0, 2))
  expect_equal(result$Berlin_apneaRisk, c("High Risk", "Low Risk", "High Risk"))
})
