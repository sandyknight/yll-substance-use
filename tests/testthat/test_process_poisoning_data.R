library(testthat)
library(dplyr)

test_that("process_poisoning_data works with default parameters", {
  # Create a mock dataset
  test_data <- tibble(
    treatment_status = c("no match with NDTMS", "some other status", "some other status", "no match with NDTMS"),
    drug_misuse_combined = c(1, 0, 1, 0),
    drug_group = c("Total Deaths", "Total Deaths", "Total Deaths", "Total Deaths"),
    dod_year = c(2022, 2022, 2023, 2023),   # Occurrence years
    reg_year = c(2022, 2022, 2023, 2023),   # Registration years
    sex = c("M", "F", "M", "F"),
    dat = c("Area1", "Area1", "Area2", "Area2"),
    dat_nm = c("Area Name1", "Area Name1", "Area Name2", "Area Name2"),
    ageinyrs = c(30, 40, 50, 60)
  )

  # Run the function with default parameters (by = NULL, by_sex = FALSE, date_of = "occurrence")
  result <- process_poisoning_data(
    data = test_data,
    date_of = "occurrence",
    years = c(2022, 2023),
    by = NULL,
    by_sex = FALSE
  )

  # Since by = NULL, we expect no grouping columns other than date_of_var (dod_year here)
  expect_true(all(c("dod_year", "additional_poisoning_deaths", "count") %in% names(result)))
  expect_equal(ncol(result), 3)  # Only these three columns should be present

  # Check that all years are within our selected years
  expect_true(all(result$dod_year %in% c(2022, 2023)))

  # Check that counts are correct
  # For default settings, all rows from test_data that meet filtering criteria should be counted
  # Criteria: drug_group == "Total Deaths" and (drug_misuse_combined == 1 OR treatment_status != "no match with NDTMS")
  # Row-wise:
  # 1: drug_misuse_combined = 1, included
  # 2: drug_misuse_combined = 0 but treatment_status = "some other status", included
  # 3: drug_misuse_combined = 1, included
  # 4: drug_misuse_combined = 0 and treatment_status = "no match with NDTMS", excluded
  # Total included: 3 deaths
  expect_equal(sum(result$count), 3)
})

test_that("process_poisoning_data groups by age when by = 'age'", {
  test_data <- tibble(
    treatment_status = c("no match with NDTMS", "some other status"),
    drug_misuse_combined = c(1, 0),
    drug_group = c("Total Deaths", "Total Deaths"),
    dod_year = c(2022, 2022),
    reg_year = c(2022, 2022),
    sex = c("M", "F"),
    dat = c("Area1", "Area1"),
    dat_nm = c("Area Name1", "Area Name1"),
    ageinyrs = c(30, 40)
  )

  # Group by age
  result <- process_poisoning_data(
    data = test_data,
    date_of = "occurrence",
    years = 2022,
    by = "age",
    by_sex = FALSE
  )

  # Check that 'ageinyrs' is present in the result as a grouping variable
  expect_true("ageinyrs" %in% names(result))
  # Check the structure is as expected
  expect_true(all(c("ageinyrs", "dod_year", "additional_poisoning_deaths", "count") %in% names(result)))
})

test_that("process_poisoning_data groups by sex when by_sex = TRUE", {
  test_data <- tibble(
    treatment_status = c("no match with NDTMS", "some other status"),
    drug_misuse_combined = c(1, 1),
    drug_group = c("Total Deaths", "Total Deaths"),
    dod_year = c(2022, 2023),
    reg_year = c(2022, 2023),
    sex = c("M", "F"),
    dat = c("Area1", "Area2"),
    dat_nm = c("Area Name1", "Area Name2"),
    ageinyrs = c(30, 50)
  )

  # Include sex grouping
  result <- process_poisoning_data(
    data = test_data,
    date_of = "occurrence",
    years = c(2022, 2023),
    by = "age",
    by_sex = TRUE
  )

  # Check that sex is present and that it has been transformed to "male"/"female"
  expect_true("sex" %in% names(result))
  expect_setequal(unique(result$sex), c("male", "female"))

  # Check that ageinyrs and sex are grouping variables
  expect_true("ageinyrs" %in% names(result))
  # Additional columns as before
  expect_true(all(c("dod_year", "additional_poisoning_deaths", "count") %in% names(result)))
})
