merge_drug_deaths <- function() {
  # Process and aggregate poisoning deaths by age and sex for the specified year
  poisoning_deaths_by_age <-
    process_poisoning_data(
      data = get_drug_poisoning_deaths(), # Fetch raw drug poisoning death data
      date_of = "occurrence",             # Use date of occurrence (not registration date)
      years = 2022,                       # Only include data for the year 2022
      by = "age",                         # Aggregate results by age
      by_sex = TRUE                       # Also stratify by sex
    ) |> 
    rename("age" = ageinyrs) |>           # Rename age variable 
    group_by(age, sex) |>                 # Group by age and sex
    summarise(count = sum(count), .groups = "drop") # Summarise total counts and ungroup

  # Process and aggregate non-poisoning deaths that occur during treatment, by age and sex, for the same year
  non_poisoning_deaths_by_age <-
    process_deaths_in_treatment(
      data = get_non_poisoning_deaths(),    # Fetch non-poisoning death data
      years = 2022,                         # Only include data for the year 2022
      by = "age",                           # Aggregate results by age
      exclude_poisoning = TRUE,             # Ensure we exclude poisoning deaths
      by_treatment_status = TRUE,           # Consider treatment status categories
      by_death_cause = FALSE,               # Do not break down by specific cause of death
      by_sex = TRUE,                        # Stratify by sex
      exclude_alcohol_specific_deaths = TRUE # Exclude alcohol-specific deaths
    ) |>
    filter(treatment_status != "Died one or more years following discharge") |> # Filter out deaths occurring more than a year post-discharge
    group_by(age, sex) |>                   # Group by age and sex
    summarise(count = sum(count), .groups = "drop") # Summarise total counts and ungroup

  # Combine poisoning and non-poisoning deaths together, then aggregate by age and sex
  deaths_by_age <-
    bind_rows(
      poisoning_deaths_by_age,    # Add the poisoning deaths
      non_poisoning_deaths_by_age # Add the non-poisoning deaths
    ) |>
    group_by(age, sex) |>         # Re-group combined data by age and sex
    summarise(count = sum(count), .groups = "drop") |> # Sum all counts for each age/sex group
    mutate(substance = "Drugs")   # Add a variable to indicate these deaths are drug-related

  # Return the final aggregated data frame
  return(deaths_by_age)
}