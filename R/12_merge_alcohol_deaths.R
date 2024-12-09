merge_alcohol_deaths <- function() {
  # Get ONS alcohol-specific death data
  ons_alc_deaths <- get_ons_alcohol_specific_death_data()

  # Get alcohol only deaths that occurred while in treatment, excluding alcohol-specific deaths
  alc_deaths_tx <-
    get_alcohol_specific_deaths_from_tx(get_non_poisoning_deaths()) |>
    # Focus on the specified time range and exclude rows categorised as "Alcohol-specific death"
    filter(period_range == "April 2021 to March 2022", death_cause != "Alcohol-specific death") |>
    # Group results by sex and age before summarising the count
    group_by(sex, age) |>
    summarise(count = sum(count, na.rm = TRUE), .groups = "drop")

  # Create a data frame that maps raw age values to defined age groups
  age_df <- parse_age_groups(pull(ons_alc_deaths, age_group))

  # Assign each death record from the treatment data to its corresponding age group
  # and re-summarise by sex and this new age group variable
  alc_deaths_tx <-
    alc_deaths_tx |>
    mutate(age_group = assign_age_group(pull(alc_deaths_tx, age), age_df = age_df)) |>
    group_by(sex, age_group) |>
    summarise(count = sum(count), .groups = "drop")

  # Combine the ONS alcohol-specific death data with the in-treatment alcohol-related death data
  # and then summarise counts by sex and age group
  alcohol_deaths <-
    bind_rows(alc_deaths_tx, ons_alc_deaths) |>
    group_by(sex, age_group) |>
    summarise(count = sum(count), .groups = "drop")

  # Convert 'sex' values to lower case, remove trailing 's', and label the substance as 'Alcohol'
  alcohol_deaths <-
    alcohol_deaths |>
    mutate(sex = tolower(sex)) |>
    mutate(sex = str_remove(sex, "s")) |>
    mutate(substance = "Alcohol")

  # Return the final aggregated dataset
  return(alcohol_deaths)
}
