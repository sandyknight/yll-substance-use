README
================

- [Required packages](#required-packages)
- [Set-up](#set-up)
- [Getting the data](#getting-the-data)
  - [Drug poisoning data](#drug-poisoning-data)
  - [NDTMS-ONS linked dataset deaths](#ndtms-ons-linked-dataset-deaths)
  - [ONS deaths related to drug
    poisoning](#ons-deaths-related-to-drug-poisoning)
  - [Non-poisoning deaths of people with contact with the treatment
    system](#non-poisoning-deaths-of-people-with-contact-with-the-treatment-system)
  - [ONS alcohol-specific deaths](#ons-alcohol-specific-deaths)
  - [Life expectancy](#life-expectancy)
- [Data processing and preparation](#data-processing-and-preparation)
  - [Drug deaths](#drug-deaths)
  - [Alcohol deaths](#alcohol-deaths)
  - [Age group parsing](#age-group-parsing)
- [Data merging](#data-merging)
- [Calculating years of life lost
  (YLL)](#calculating-years-of-life-lost-yll)
  - [Initial estimate](#initial-estimate)
  - [YLL with age-weighting and
    discounting](#yll-with-age-weighting-and-discounting)
- [Plotting results](#plotting-results)
  - [Plot crude estimate](#plot-crude-estimate)
  - [Plot discounted and weighted
    estimate](#plot-discounted-and-weighted-estimate)
- [Version information](#version-information)

## Required packages

``` r
library(dplyr)
library(ggplot2)
library(janitor)
library(openxlsx)
library(stringr)
library(arrow) 
library(tidyr)
library(glue)
library(scales)
```

## Set-up

Two of the datasets used in the analysis aren’t publicly available, so
I’ve not uploaded the `data`directory here. To get the analysis working
you’ll have to add the two original data files with this structure:

    ├── data
    │   └── raw
    │       ├── post election data for Jon- sent.xlsx
    │       └── table1_all deaths_Cocaine version 1.xlsx

Clone or download this repository Add the data files and directory
structure

I’ve included the session info with R and package versions at the end to
help with any compatability issues.

## Getting the data

Each of these functions take no arguments and return a raw dataset when
called.

### Drug poisoning data

The ONS publishes drug poisonings and classifies them as “related to
drug misuse” given certain criteria i.e. specific ICD-10 codes on the
death record.

The linkage of NDTMS and ONS data reveals that some deaths that had
insufficient information for the ONS to apply this classification are
probably related to drug misuse since they occured either in treatment
or within a year of discharge. This analysis uses data from both sources
to adjust the total number of deaths to include these additional deaths.

The functions in this section load each of the raw datasets when called.

### NDTMS-ONS linked dataset deaths

Drug poisoning deaths from the NDTMS-ONS data linkage. This function
loads the required data from an Excel workbook received by email from
EAT with the name `table1_all deaths_Cocaine version 1.xlsx`. Since
loading data this large from Excel takes a long time it is saved as a
parquet file to speed up later analysis. If this is already done the
function does nothing. Note that this is not publicly available data.

``` r
get_drug_poisoning_deaths <- function() {
  if (!file.exists("data/raw/ndtms_mortality_data.parquet")) {
    #  This file is NDTMS-ONS data linkage; received by email from Stefan and
    #  named: "table1_all deaths_Cocaine version 1.xlsx"

    df <- # Load deaths data from Excel
      openxlsx::read.xlsx("data/raw/table1_all deaths_Cocaine version 1.xlsx",
        sheet = "table1_all deaths"
      )
    # Write data to easier format
    write_parquet(df, "data/raw/ndtms_mortality_data.parquet")
  }
  # Return raw data
  read_parquet("data/raw/ndtms_mortality_data.parquet")
}
```

### ONS deaths related to drug poisoning

The ONS drug poisoning data is publicly available at the URI in this
function. This function downloads, reshapes, and returns the relevant
ONS data.

``` r
get_ons_drug_poisoning_data <-
  function() {
    url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsrelatedtodrugpoisoningenglandandwalesreferencetable/current/2023registrations.xlsx"

    ons_registrations <-
      openxlsx::read.xlsx(
        url,
        sheet = "Table 1",
        startRow = 4,
        sep.names = "_",
        fillMergedCells = TRUE,
        skipEmptyCols = TRUE,
        check.names = TRUE
      ) |>
      select(1, 2, 5, 9)

    colnames(ons_registrations) <- c("sex", "year", "all_drug_poisoning", "drug_misuse")

    ons_registrations <-
      ons_registrations |>
      slice(3:100) |>
      mutate(sex = zoo::na.locf(sex)) |>
      filter(sex == "Persons") |>
      as_tibble() |>
      filter(!is.na(year))

    return(ons_registrations)
  }
```

### Non-poisoning deaths of people with contact with the treatment system

The NDTMS-ONS data linkage also has records for people who died in
treatment or after treatment from causes other than drug poisoning. This
analysis adds those deaths to either drug- or -alcohol related deaths
depending on the cause and, in the case of those people in treatment for
drug use, time since discharge.

The treatment mortality data was received by email from EAT,(*received:
2024-07-08 from CA*) and named `post election data for Jon- sent.xlsx`.
This data is not publicly available.

``` r
get_non_poisoning_deaths <- function() {
  np_deaths <-
    openxlsx::read.xlsx("data/raw/post election data for Jon- sent.xlsx", sheet = "NDTMS_ONS") |>
    janitor::clean_names()

  np_deaths <-
    np_deaths |>
    filter(geography == "LA")

  return(np_deaths)
}
```

### ONS alcohol-specific deaths

The ONS alcohol-specific death data is publicly available at the URI in
this function. This function downloads, reshapes, and returns the
relevant ONS data.

``` r
get_ons_alcohol_specific_death_data <-
  function() {
    url <-
      "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/datasets/alcoholspecificdeathsbysexagegroupandindividualcauseofdeath/current/deathsbyindividualcause.xlsx"

    alc_specific_deaths <-
      read.xlsx(
        xlsxFile = url, rows = c(5:50), cols = c(1:24), colNames = TRUE, sheet = "Table 2"
      ) %>%
      rename_with(.fn = janitor::make_clean_names, .cols = 1:4) |>
      rename("period" = year_note_3) %>%
      filter(sex != "Persons") |>
      tidyr::pivot_longer(cols = `<1`:`90+`, names_to = "age_group", values_to = "count") |>
      group_by(sex, age_group) |>
      summarise(count = sum(count), .groups = "drop")

    return(alc_specific_deaths)
  }
```

### Life expectancy

This function downloads, re-labels, and reshapes the ONS life tables for
use in the YLL analysis.

``` r
get_life_tables <- function() {
  url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/nationallifetablesunitedkingdomreferencetables/current/nltuk198020203.xlsx"

  life_tables <-
    read.xlsx(
      xlsxFile = url,
      sheet = "2020-2022",
      startRow = 6
    ) |>
    rename_with(.cols = 1:6, ~ paste0(.x, "_male")) |>
    rename_with(.cols = 7:12, ~ paste0(.x, "_female")) |>
    select(ex_male, age_female, ex_female) |>
    pivot_longer(cols = contains("ex"), values_to = "ex", names_to = "sex") |>
    mutate(sex = str_remove(sex, "ex_")) |>
    rename("age" = age_female)

  return(life_tables)
}
```

## Data processing and preparation

### Drug deaths

The following two functions were written to allow other analyses of drug
deaths and have more flexibilty (and therefore are unfortunately long
and complicated) than is necessary for the YLL analysis. They should
both take the output of the corresponding `get_*_data()` functions as
the `data` argument. For this analysis we’ll choose to group the data by
age and sex, and use date of occurrence rather than registration.

``` r
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
  date_of_var <- switch(date_of,
    "occurrence" = "dod_year",
    "registration" = "reg_year",
    stop("Only 'occurrence' or 'registration' are valid options!")
  )

  # Handle grouping variable(s)
  if (!is.null(by)) {
    by_var <- switch(by,
      "area" = c("dat", "dat_nm"),
      "age" = "ageinyrs",
      stop("Invalid 'by' value. Use 'area', 'age', or leave as NULL.")
    )
  } else {
    by_var <- NULL
  }

  if (isTRUE(by_sex)) {
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
      mutate(sex = case_match(sex, "M" ~ "male", "F" ~ "female"))
  }

  return(result)
}
```

``` r
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
                                        exclude_alcohol_specific_deaths = TRUE) {
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
    mutate( # Deal with Excel date madness
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
```

### Alcohol deaths

This function extracts:  
1. alcohol-specific deaths in or after treatment and; 2. deaths of
people in treatment for alcohol use only from the output of
`get_non_poisoning_deaths()`

``` r
get_alcohol_specific_deaths_from_tx <- function(data) {
  data |>
    filter(drug_group == "alcohol only" | death_cause == "Alcohol-specific death") |>
    filter(treatment_status != "Died one or more years following discharge")
}
```

### Age group parsing

These functions parse the age groups in the datasets that don’t have
single-year-of-age fields to allow merging.

``` r
# Simplified parse_age_groups function
parse_age_groups <- function(age_groups) {
  # Patterns for age group formats
  pattern_range <- "^([0-9]+)-([0-9]+)$"
  pattern_under <- "^<([0-9]+)$"
  pattern_over <- "^([0-9]+)\\+$"

  # Initialize vectors
  lower <- rep(NA_real_, length(age_groups))
  upper <- rep(NA_real_, length(age_groups))

  # Identify which pattern each age group matches
  is_range <- grepl(pattern_range, age_groups)
  is_under <- grepl(pattern_under, age_groups)
  is_over <- grepl(pattern_over, age_groups)

  # For range: extract lower and upper numbers
  if (any(is_range)) {
    lower[is_range] <- as.numeric(sub(pattern_range, "\\1", age_groups[is_range]))
    upper[is_range] <- as.numeric(sub(pattern_range, "\\2", age_groups[is_range]))
  }

  # For under: "<X" means lower = -Inf, upper = X
  if (any(is_under)) {
    upper[is_under] <- as.numeric(sub(pattern_under, "\\1", age_groups[is_under]))
    lower[is_under] <- -Inf
  }

  # For over: "X+" means lower = X, upper = Inf
  if (any(is_over)) {
    lower[is_over] <- as.numeric(sub(pattern_over, "\\1", age_groups[is_over]))
    upper[is_over] <- Inf
  }

  # Create and return a data frame
  data.frame(
    age_group = age_groups,
    lower = lower,
    upper = upper,
    stringsAsFactors = FALSE
  )
}
```

``` r
# Simplified assign_age_group function
assign_age_group <- function(ages, age_df) {
  # We assume ages is already an integer vector
  age_groups <- character(length(ages))

  # Assign age groups based on intervals
  for (i in seq_len(nrow(age_df))) {
    in_interval <- ages >= age_df$lower[i] & ages <= age_df$upper[i]
    age_groups[in_interval] <- age_df$age_group[i]
  }

  return(age_groups)
}
```

## Data merging

``` r
merge_drug_deaths <- function() {
  # Process and aggregate poisoning deaths by age and sex for the specified year
  poisoning_deaths_by_age <-
    process_poisoning_data(
      data = get_drug_poisoning_deaths(), # Fetch raw drug poisoning death data
      date_of = "occurrence", # Use date of occurrence (not registration date)
      years = 2022, # Only include data for the year 2022
      by = "age", # Aggregate results by age
      by_sex = TRUE # Also stratify by sex
    ) |>
    rename("age" = ageinyrs) |> # Rename age variable
    group_by(age, sex) |> # Group by age and sex
    summarise(count = sum(count), .groups = "drop") # Summarise total counts and ungroup

  # Process and aggregate non-poisoning deaths that occur during treatment, by age and sex, for the same year
  non_poisoning_deaths_by_age <-
    process_deaths_in_treatment(
      data = get_non_poisoning_deaths(), # Fetch non-poisoning death data
      years = 2022, # Only include data for the year 2022
      by = "age", # Aggregate results by age
      exclude_poisoning = TRUE, # Ensure we exclude poisoning deaths
      by_treatment_status = TRUE, # Consider treatment status categories
      by_death_cause = FALSE, # Do not break down by specific cause of death
      by_sex = TRUE, # Stratify by sex
      exclude_alcohol_specific_deaths = TRUE # Exclude alcohol-specific deaths
    ) |>
    filter(treatment_status != "Died one or more years following discharge") |> # Filter out deaths occurring more than a year post-discharge
    group_by(age, sex) |> # Group by age and sex
    summarise(count = sum(count), .groups = "drop") # Summarise total counts and ungroup

  # Combine poisoning and non-poisoning deaths together, then aggregate by age and sex
  deaths_by_age <-
    bind_rows(
      poisoning_deaths_by_age, # Add the poisoning deaths
      non_poisoning_deaths_by_age # Add the non-poisoning deaths
    ) |>
    group_by(age, sex) |> # Re-group combined data by age and sex
    summarise(count = sum(count), .groups = "drop") |> # Sum all counts for each age/sex group
    mutate(substance = "Drugs") # Add a variable to indicate these deaths are drug-related

  # Return the final aggregated data frame
  return(deaths_by_age)
}
```

``` r
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
```

## Calculating years of life lost (YLL)

### Initial estimate

This function calculates the “crude” years of life lost.

The crude expected years of life lost is:

$$
YLL = (D_{x})(e_{x}^s)
$$

Where $D_x$ is the number of deaths in the age group and $e_{x}^s$ is
the mean standard age of death from the external life expectancy (the
ONS life tables) for the age. This is the method used by the Global
Burden of Disease Study (GBD) [^1].

``` r
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
      drug_deaths, # Join drug deaths data
      life_tables, # With life tables to get life expectancy (ex) by sex and age
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
```

### YLL with age-weighting and discounting

This function is from
[here](https://static-content.springer.com/esm/art%3A10.1186%2F1471-2458-8-116/MediaObjects/12889_2007_1086_MOESM3_ESM.pdf)[^2].
It uses the same basic method but applies discounting and age-weighting.

The formula is:

$$Y_x = d_x \left[ \frac{KCe^{r(n^a_x)}}{(r+\beta)^2} \left( e^{z[-(r+\beta)(e^s_x + a_x) - 1]} - e^{-(r+\beta)a_x[-(r+\beta)a_x - 1]} \right) + \frac{1-K}{r} (1 - e^{r(e^s_x)}) \right]$$
   
where:  
𝑎 = age of death (years)  
𝑟 = discount rate (usually 3%)  
𝛽 = age weighting constant (usually 𝛽=0.04)  
𝐾 = age-weighting modulation constant (usually 𝐾=1)  
𝐶 = adjustment constant for age-weights (usually 𝐶=0.1658)  
𝑒 = standard life expectancy at age of death (years)  

The default values for these parameters were chosen and calibrated in
the original Global Burden of Disease (GBD) study [^3]

``` r
calculate_yll <-
  function(number.deaths,
           average.age.death,
           model.life.expectancy,
           discount.rate = 0.03,
           beta.constant = 0.04,
           modulation.constant = 0,
           adjustment.constant = 0.1658) {
    ## abbreviate inputs
    N <- number.deaths
    a <- average.age.death
    L <- model.life.expectancy
    r <- discount.rate
    b <- beta.constant
    K <- modulation.constant
    CC <- adjustment.constant
    ## do calculations
    if (discount.rate == 0) {
      N * (K * CC * ((exp(-b * a)) / b^2) * ((exp(-b * L)) * (-b * (L + a) - 1) - (-b * a - 1)) + ((1 - K) * L))
    } else {
      N * (K * ((CC * exp(r * a)) / (-(r + b)^2)) * ((exp(-(r + b) * (L + a)) * (-(r + b) * (L + a) - 1)) - (exp(-(r + b) * a) * (-(r + b) * a - 1))) + ((1 - K) / r) * ((1 - exp(-r * L))))
    }
  }
```

This function uses the `calculate_yll` function to estimate the total
YLL for drug and alcohol use with discounting and age weighting.

``` r
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
```

## Plotting results

### Plot crude estimate

Produces a plot of the crude YLL estimate. Sources two scripts that
define `ggplot2` aesthetics but don’t contibute to the analysis.

``` r
plot_crude_yll_estimate <-
  function() {
    source("R/dhsc_colour_palette.R")
    source("R/themes.R")
    crude_yll <-
      calculate_crude_yll()

    p <-
      crude_yll |>
      ggplot(aes(x = age_group, y = yll)) +
      geom_col(aes(fill = substance), colour = "black") +
      scale_y_continuous(labels = scales::comma) +
      labs(
        title = "Estimated years of life lost (YLL), due to substance use",
        subtitle = glue::glue("Total YLL: {scales::comma(sum(crude_yll$yll))} (without discounting or age weighting)"),
        caption = "Data from ONS and NDTMS for deaths which occurred in 2022",
        fill = NULL,
        x = "Age group",
        y = "YLL"
      ) +
      my_theme +
      my_fill_scale +
      theme(
        axis.text.x = element_text(angle = 30, vjust = 0.5),
        legend.direction = "horizontal",
        plot.caption = element_text(face = "italic")
      )

    return(p)
  }

plot_crude_yll_estimate()
```

![](README_files/figure-gfm/function-plot_crude_yll_estimate-1.png)<!-- -->

### Plot discounted and weighted estimate

Produces a similar plot for the discounted and age-weighted YLL estimate

``` r
plot_substance_use_yll_estimate <-
  function() {
    source("R/dhsc_colour_palette.R")
    source("R/themes.R")

    yll <-
      calculate_substance_use_yll()

    yll |>
      ggplot(aes(x = age_group, y = yll)) +
      geom_col(aes(fill = substance), colour = "black") +
      scale_y_continuous(labels = scales::comma) +
      labs(
        title = "Estimated years of life lost (YLL), due to substance use",
        subtitle = glue::glue(
          "Total YLL: {scales::comma(sum(yll$yll))} (with discounting and age weighting)"
        ),
        caption = "Data from ONS and NDTMS for deaths which occurred in 2022",
        fill = NULL,
        x = "Age group",
        y = "YLL"
      ) +
      my_theme +
      my_fill_scale +
      theme(
        axis.text.x = element_text(angle = 30, vjust = 0.5),
        legend.direction = "horizontal",
        plot.caption = element_text(face = "italic")
      )
  }

plot_substance_use_yll_estimate()
```

![](README_files/figure-gfm/function-plot_substance_use_yll_estimate-1.png)<!-- -->

## Version information

    ## R version 4.4.2 (2024-10-31 ucrt)
    ## Platform: x86_64-w64-mingw32/x64
    ## Running under: Windows 10 x64 (build 19045)
    ## 
    ## Matrix products: default
    ## 
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United Kingdom.utf8 
    ## [2] LC_CTYPE=English_United Kingdom.utf8   
    ## [3] LC_MONETARY=English_United Kingdom.utf8
    ## [4] LC_NUMERIC=C                           
    ## [5] LC_TIME=English_United Kingdom.utf8    
    ## 
    ## time zone: Europe/London
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] scales_1.3.0     glue_1.8.0       tidyr_1.3.1      arrow_17.0.0.1  
    ## [5] stringr_1.5.1    openxlsx_4.2.7.1 janitor_2.2.0    ggplot2_3.5.1   
    ## [9] dplyr_1.1.4     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] bit_4.5.0         gtable_0.3.6      compiler_4.4.2    tidyselect_1.2.1 
    ##  [5] Rcpp_1.0.13-1     zip_2.3.1         assertthat_0.2.1  snakecase_0.11.1 
    ##  [9] yaml_2.3.10       fastmap_1.2.0     R6_2.5.1          labeling_0.4.3   
    ## [13] generics_0.1.3    knitr_1.49        tibble_3.2.1      munsell_0.5.1    
    ## [17] lubridate_1.9.3   tzdb_0.4.0        pillar_1.9.0      rlang_1.1.4      
    ## [21] utf8_1.2.4        stringi_1.8.4     xfun_0.49         bit64_4.5.2      
    ## [25] timechange_0.3.0  cli_3.6.3         withr_3.0.2       magrittr_2.0.3   
    ## [29] digest_0.6.37     grid_4.4.2        rstudioapi_0.17.1 lifecycle_1.0.4  
    ## [33] vctrs_0.6.5       evaluate_1.0.1    farver_2.1.2      fansi_1.0.6      
    ## [37] colorspace_2.1-1  purrr_1.0.2       rmarkdown_2.29    tools_4.4.2      
    ## [41] pkgconfig_2.0.3   htmltools_0.5.8.1

[^1]: Chudasama, Y.V., Khunti, K., Gillies, C.L., Dhalwani, N.N.,
    Davies, M.J., Yates, T., & Zaccardi, F. (2022). Estimates of years
    of life lost depended on the method used: tutorial and comparative
    investigation. *Journal of Clinical Epidemiology*, 150, pp. 42–50.
    Available at: <https://doi.org/10.1016/j.jclinepi.2022.06.012>
    \[Accessed 6 Nov. 2024\].

[^2]: Aragón, T.J., Lichtensztajn, D.Y., Katcher, B.S., Reiter, R. and
    Katz, M.H., 2007. Calculating Expected Years of Life Lost to Rank
    the Leading Causes of Premature Death in San Francisco. *San
    Francisco Department of Public Health*.

[^3]: Murray, C.J., Lopez, A.D. and Jamison, D.T., 1994. The global
    burden of disease in 1990: summary results, sensitivity analysis and
    future directions. *Bulletin of the world health organization*,
    72(3), p.495.
