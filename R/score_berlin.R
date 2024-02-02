#' Berlin Sleep Scoring Function
#'
#' This function scores sleep quality based on the Berlin questionnaire.
#'
#' @param data A data frame containing the Berlin questionnaire fields.
#'
#' @return A data frame with additional columns for scoring and risk level.
#' @export
#' @examples
#' # Prepare some example data and score it
#' example_data <- data.frame(
#'   do_you_snore = c("yes", "no", "yes"),
#'   your_snoring_is = c("Louder than talking", NA, "normal"),
#'   how_often_do_you_snore = c("3-4 times a week", NA, "never"),
#'   bmi = c(32, 25, 28)
#' )
#' scored_data <- score_berlin(example_data)

score_berlin <- function(data) {
  library(stringr)
  library(dplyr)

  data <- data %>%
    mutate(
      cat1 = as.double(
        (!is.na(do_you_snore) & tolower(do_you_snore) == 'yes') +
          (!is.na(your_snoring_is) & str_detect(tolower(your_snoring_is), 'louder than talking|very loud- can be heard in adjacent rooms')) +
          (!is.na(how_often_do_you_snore) & str_detect(tolower(how_often_do_you_snore), 'almost every day|3-4 times a week')) +
          (!is.na(has_your_snoring_ever_bothered_other_people) & tolower(has_your_snoring_ever_bothered_other_people) == 'yes') +
          2 * (!is.na(has_anyone_noticed_that_you_quit_breathing_during_your_sleep) & str_detect(tolower(has_anyone_noticed_that_you_quit_breathing_during_your_sleep), 'almost every day|3-4 times a week'))
      ),
      cat2 = as.double(
        (!is.na(how_often_do_you_feel_tired_or_fatigued_after_your_sleep) & str_detect(tolower(how_often_do_you_feel_tired_or_fatigued_after_your_sleep), 'almost every day|3-4 times a week')) +
          (!is.na(during_your_waking_time_do_you_feel_tired_fatigued_or_not_up_to_par) & str_detect(tolower(during_your_waking_time_do_you_feel_tired_fatigued_or_not_up_to_par), 'almost every day|3-4 times a week')) +
          (!is.na(have_you_ever_nodded_off_or_fallen_asleep_while_driving_a_vehicle) & tolower(have_you_ever_nodded_off_or_fallen_asleep_while_driving_a_vehicle) == 'yes')
      ),
      cat3 = as.double(
        (!is.na(do_you_have_high_blood_pressure) & tolower(do_you_have_high_blood_pressure) == 'yes') +
          (!is.na(bmi) & bmi > 30)
      ),
      cat1_pos = as.double(cat1 >= 2),
      cat2_pos = as.double(cat2 >= 2),
      cat3_pos = as.double(cat3 >= 1),
      total = cat1_pos + cat2_pos + cat3_pos,
      Berlin_apneaRisk = if_else(total >= 2, "High Risk", "Low Risk")
    )
  return(data)
}
