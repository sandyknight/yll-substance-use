#' Combine natioinal data
#'
#' Merges poisoning data and treatment death data, ensuring consistent grouping by death category.
#'
#' @param poisoning_data Processed poisoning data.
#' @param treatment_deaths_data Processed treatment death data.
#' @return A tibble combining both datasets.
combine_national_data <- function(poisoning_data, treatment_deaths_data) {
  bind_rows(
    rename(poisoning_data, "death_category" = additional_poisoning_deaths) |>
      rename_with(.cols = 1, .fn = ~ str_remove(.x, "dod_|reg_")),
    rename(treatment_deaths_data, "death_category" = treatment_status, "year" = period)
  )
}
