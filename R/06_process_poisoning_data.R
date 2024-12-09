#' Process drug poisoning data
#'
#' Reads, cleans, and processes drug poisoning data from a file. Filters and groups the data
#' based on user-defined parameters, such as years, date type, and grouping.
#'
#' @param file_path Path to the parquet file containing drug poisoning data.
#' @param date_of Either "occurrence" (year of death) or "registration" (year of registration).
#' @param years Vector of years to include in the analysis.
#' @param by Grouping variable: "area", "age", or NULL (national-level data).
#' @param by_sex Whether to group by sex
#' @return A tibble with grouped and summarized poisoning data.
process_poisoning_data <- function(data, date_of = "occurrence", years = c(2022, 2023), by = NULL, by_sex = FALSE) {
  if (is.null(by)) {
    message(paste("Drug poisoning: national level data, all ages", years, sep = ", "))
  }
  # Decide which date variable to use
  date_of_var <- switch(
    date_of,
    "occurrence" = "dod_year",
    "registration" = "reg_year",
    stop("Only 'occurrence' or 'registration' are valid options!")
  )

  # Handle grouping variable(s)
  if (!is.null(by)) {
    by_var <- switch(
      by,
      "area" = c("dat", "dat_nm"),
      "age" = "ageinyrs",
      stop("Invalid 'by' value. Use 'area', 'age', or leave as NULL.")
    )
  } else {
    by_var <- NULL
  }

  if (isTRUE(by_sex)){
    by_vars <- c(by_var, "sex")
  } else {
    by_vars <- by_var
  }

  # Read and process data
  result <-
    data %>%
    janitor::clean_names() %>%
    mutate(
      ndtms_match = if_else(
        treatment_status == "no match with NDTMS",
        "No record of contact with treatment system",
        "Record of contact with treatment system"
      ),
      ons_misuse = if_else(
        drug_misuse_combined == 1,
        "Recorded as drug misuse by ONS",
        "Not recorded as drug misuse by ONS"
      )
    ) %>%
    filter(
      # 'Total Deaths' only, not by substance. NB the substance groups are not mutually exclusive
      drug_group == "Total Deaths",
      # Year of occurrence OR registration as selected in the parameters
      .data[[date_of_var]] %in% years
    ) %>%
    filter(
      # Retain rows that were recoreded as drug misuse OR not recorded as drug misuse but have a record of contact with the treatment system
      drug_misuse_combined == 1 | (drug_misuse_combined == 0 & treatment_status != "no match with NDTMS")
    ) %>%
    mutate(
      additional_poisoning_deaths = if_else(
        # Where drug_misuse_combined equals 1
        drug_misuse_combined == 1,
        "Initial poisoning deaths",
        "Additional poisoning deaths"
      )
    ) %>%
    group_by(
      !!!syms(by_vars),
      !!!syms(date_of_var),
      additional_poisoning_deaths
    ) %>%
    summarise(
      count = n(),
      .groups = "drop"
    )

  if (isTRUE(by_sex)) {
    result <-
      result |>
      mutate(sex = case_match(sex,"M" ~ "male", "F" ~ "female"))
  }

  return(result)

}
