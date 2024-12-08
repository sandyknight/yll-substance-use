calculate_substance_use_yll <- function() {
  # Merge and summarise alcohol-related deaths by age group and sex
  alcohol_deaths <- 
    merge_alcohol_deaths()

  # Create a data frame mapping age values to defined age groups, derived from alcohol_deaths
  age_df <- 
    parse_age_groups(unique(pull(alcohol_deaths, age_group)))

  # Load life tables
  life_tables <- 
    get_life_tables()
  # Incorporate age groups into life tables, then compute average age and life expectancy (ex) for each age group and sex
  life_tables <-
    life_tables |>
    mutate(age_group = assign_age_group(ages = pull(life_tables, age), age_df = age_df)) |>
    group_by(sex, age_group) |>
    summarise(mean_age = mean(age), ex = mean(ex), .groups = "drop")

  # Join alcohol deaths with the relevant life table data to calculate Years of Life Lost (YLL)
  alcohol_yll <-
    left_join(alcohol_deaths, life_tables) |>
    mutate(yll = calculate_yll(number.deaths = count, average.age.death = mean_age, model.life.expectancy = ex)) |>
    group_by(age_group, substance) |>
    summarise(count = sum(count), yll = sum(yll), .groups = "drop")

  # Merge and summarise drug-related deaths by age and sex
  drug_deaths <- merge_drug_deaths()

  # Get ONS life tables for drugs YLL calculation
  life_tables <- get_life_tables()

  # Join drug deaths data with life table data
  yll <-
    left_join(drug_deaths, life_tables)

  # Calculate YLL for drug-related deaths
  drugs_yll <-
    yll |>
    mutate(yll = calculate_yll(number.deaths = count, average.age.death = age, model.life.expectancy = ex))

  # Assign drug deaths to appropriate age groups using age_df generated above
  drugs_yll <-
    drugs_yll |>
    mutate(age_group = assign_age_group(ages = age, age_df)) |>
    group_by(age_group, substance) |>
    summarise(count = sum(count), yll = sum(yll), .groups = "drop")

  # Combine YLL estimates for both alcohol and drugs into one dataset
  substance_use_yll <- bind_rows(alcohol_yll, drugs_yll)

  # Return the final data frame containing combined YLL estimates for both alcohol and drug substances
  return(substance_use_yll)
}