#' Process deaths in treatment data
#'
#' Reads and processes data about deaths in treatment. Filters, groups, and summarizes the data
#' based on various parameters.
#'
#' @param data A data frame produced by `get_non_poisoning_deaths`
#' @param years Vector of years to include in the analysis.
#' @param by Grouping variable: "area", "age", or NULL.
#' @param by_treatment_status Whether to group by treatment status.
#' @param by_death_cause Whether to group by cause of death.
#' @param by_sex Whether to group by sex
#' @param exclude_poisoning Whether to exclude drug poisoning deaths.
#' @param exclude_alcohol_specific_deaths Whether to exclude alcohol-specific deaths.
#' @return A tibble with grouped and summarized treatment data.
process_deaths_in_treatment <- function(data,
                                        years = c(2022, 2023),
                                        by = NULL,
                                        by_treatment_status = FALSE,
                                        by_death_cause = FALSE,
                                        by_sex = FALSE,
                                        exclude_poisoning = TRUE,
                                        exclude_alcohol_specific_deaths = TRUE
) {
  # Start with grouping variable
  grouping_vars <- "period"

  # Handle additional grouping by 'by' parameter
  if (!is.null(by)) {
    by_vars <- switch(by,
                      "area" = c("area_code", "area_name"),
                      "age" = "age",
                      stop("Invalid 'by' value. Choose 'area' or 'age', or leave as NULL.")
    )
    grouping_vars <- c(grouping_vars, by_vars)
  }

  # Add grouping for treatment status or death cause if requested
  if (isTRUE(by_treatment_status)) {
    grouping_vars <- c(grouping_vars, "treatment_status")
  }

  if (isTRUE(by_death_cause)) {
    grouping_vars <- c(grouping_vars, "death_cause")
  }

  if (isTRUE(by_sex)) {
    grouping_vars <- c(grouping_vars, "sex")
  }


  # Read and process data
  data <- data %>%
    mutate(    # Deal with Excel date madness
      period = as.Date(period, origin = "1899-12-30"),
      period = lubridate::year(period)
    )

  # Apply filters
  data <- data %>%
    filter(period %in% years)

  if (isTRUE(exclude_poisoning)) {
    data <- data %>%
      filter(death_cause != "Drug poisoning")
  }

  if (isTRUE(exclude_alcohol_specific_deaths)) {
    data <- data %>%
      filter(death_cause != "Alcohol-specific death")
  }

  # Group and summarize
  result <- data %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(count = sum(count), .groups = "drop")

  # If grouped by sex column, standardise sex coding

  if (isTRUE(by_sex)) {
    result <-
      result |>
      mutate(sex = tolower(sex))
  }


  return(result)
}
