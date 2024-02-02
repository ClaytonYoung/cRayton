expected_output <- tibble::tibble(
  PIDN                        = c(1, 2, 3),
  DCDate                      = as.Date(c("2023-01-10", "2023-02-01", "2023-03-01")),
  date_diff.some_tibble       = lubridate::as.difftime(c(NA, 1, 1), units = 'days'),
  DCDate.some_tibble          = as.Date(c(NA, "2023-02-02", "2023-03-02")),
  Value.some_tibble           = c(NA, 1.2, 1.3),
  Name.some_tibble            = c(NA, "Bob", "Charlie"),
  Age.some_tibble             = c(NA, 30, 35),
  date_diff.another_tibble    = lubridate::as.difftime(c(NA, 1, 1), units = 'days'),
  DCDate.another_tibble       = as.Date(c(NA, "2023-02-02", "2023-03-02")),
  Value.another_tibble        = c(NA, 2.2, 2.3),
  Occupation.another_tibble   = c(NA, "Doctor", "Nurse"),
  Salary.another_tibble       = c(NA, 90000, 50000),
  date_diff.final_tibble      = lubridate::as.difftime(c(NA, NA, 2), units = 'days'),
  DCDate.final_tibble         = as.Date(c(NA, NA, "2023-03-03")),
  Value.final_tibble          = c(NA, NA, 4.2),
  Favorite_Food.final_tibble  = c(NA, NA, "Sushi"),
  Favorite_Color.final_tibble = c(NA, NA, "Blue")
)

test_that("Function returns a tibble", {
  # Create some mock data
  pidns_and_timepoints <- tibble::tibble(PIDN = 1:3, DCDate = as.Date(c("2023-01-10", "2023-02-01", "2023-03-01")))

  list_of_dfs <- list(
    some_tibble = tibble::tibble(PIDN = 1:3,
                                 DCDate = as.Date(c("2023-01-02", "2023-02-02", "2023-03-02")),
                                 Value = c(1.1, 1.2, 1.3),
                                 Name = c("Alice", "Bob", "Charlie"),
                                 Age = c(25, 30, 35)),

    another_tibble = tibble::tibble(PIDN = 1:6,
                                    DCDate = as.Date(c("2023-01-02", "2023-02-02", "2023-03-02", "2023-03-05", "2023-03-10", "2023-03-15")),
                                    Value = c(2.1, 2.2, 2.3, 2.4, 2.5, 2.6),
                                    Occupation = c("Engineer", "Doctor", "Nurse", "Lawyer", "Artist", "Scientist"),
                                    Salary = c(70000, 90000, 50000, 95000, 60000, 98000)),

    yet_another_tibble = tibble::tibble(PIDN = 4:6,
                                        DCDate = as.Date(c("2023-04-02", "2023-05-02", "2023-06-02")),
                                        Value = c(3.1, 3.2, 3.3),
                                        Country = c("USA", "Canada", "UK"),
                                        Language = c("English", "English", "English")),

    final_tibble = tibble::tibble(PIDN = 2:4,
                                  DCDate = as.Date(c("2023-03-01", "2023-03-03", "2023-03-05")),
                                  Value = c(4.1, 4.2, 4.3),
                                  Favorite_Food = c("Pizza", "Sushi", "Pasta"),
                                  Favorite_Color = c("Red", "Blue", "Green")),

    random_tibble = tibble::tibble(PIDN = 5:7,
                                   DCDate = as.Date(c("2023-04-03", "2023-04-06", "2023-04-09")),
                                   Value = c(5.1, 5.2, 5.3),
                                   Hobby = c("Reading", "Painting", "Gaming"),
                                   Pet = c("Dog", "Cat", "Fish"))
  )



  result <- join_list_of_dfs_to_timepoints(pidns_and_timepoints, list_of_dfs, 5)

  expect_true(tibble::is_tibble(result))
  expect_equal(result, expected_output)
})
