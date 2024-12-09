calculate_crude_yll <- function() {
  # Step 1: Merge alcohol-related deaths into a single dataset
  alcohol_deaths <- merge_alcohol_deaths()

  # Step 2: Parse the age groups in the alcohol deaths data to get lower and upper bounds
  age_df <- parse_age_groups(pull(alcohol_deaths, age_group))

  # Step 3: Retrieve life tables, which contain life expectancy (ex) by sex and age
  life_tables <- get_life_tables()

  # Step 4: Calculate years of life lost (YLL) for alcohol-related deaths
  yll_alcohol <-
    life_tables |>
    # Assign each age in life tables to an age group based on parsed age group bounds
    mutate(age_group = assign_age_group(ages = age, age_df)) |>
    # Group by sex and age group and calculate the average life expectancy (ex)
    group_by(sex, age_group) |>
    summarise(ex = mean(ex), .groups = "drop") |>
    # Merge with alcohol deaths data to match life expectancy with death counts
    right_join(alcohol_deaths, by = c("sex", "age_group")) |>
    # Calculate YLL by multiplying life expectancy (ex) with the number of deaths (count)
    mutate(yll = ex * count)

  # Step 5: Summarise alcohol-related YLL and death counts by age group and substance
  yll_alcohol <-
    yll_alcohol |>
    group_by(age_group, substance) |>
    summarise(count = sum(count), yll = sum(yll), .groups = "drop")

  # Step 6: Merge drug-related deaths into a single dataset
  drug_deaths <- merge_drug_deaths()

  # Step 7: Calculate YLL for drug-related deaths
  yll_drugs <-
    left_join(
      drug_deaths,  # Join drug deaths data
      life_tables,  # With life tables to get life expectancy (ex) by sex and age
      by = c("sex", "age")
    ) |>
    # Calculate YLL by multiplying life expectancy (ex) with the number of deaths (count)
    mutate(yll = count * ex)

  # Step 8: Re-parse the age groups in the summarised alcohol YLL data
  age_df <- parse_age_groups(pull(yll_alcohol, age_group))

  # Step 9: Assign each age in drug YLL data to an age group based on parsed age group bounds
  yll_drugs <-
    mutate(yll_drugs, age_group = assign_age_group(age, age_df)) |>
    # Summarise drug-related YLL and death counts by age group and substance
    group_by(age_group, substance) |>
    summarise(count = sum(count), yll = sum(yll), .groups = "drop")

  # Step 10: Combine alcohol-related and drug-related YLL into a single dataset
  yll <- bind_rows(yll_alcohol, yll_drugs)

  # Step 11: Return the combined dataset
  return(yll)
}
