get_ons_alcohol_specific_death_data <-
  function(){

    url <-
      "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/datasets/alcoholspecificdeathsbysexagegroupandindividualcauseofdeath/current/deathsbyindividualcause.xlsx"

    alc_specific_deaths <-
      read.xlsx(
        xlsxFile = url, rows = c(5:50), cols = c(1:24),colNames = TRUE, sheet = "Table 2") %>%
      rename_with(.fn = janitor::make_clean_names, .cols = 1:4)  |>
      rename("period" = year_note_3) %>%
      filter(sex != "Persons") |>
      tidyr::pivot_longer(cols = `<1`:`90+`, names_to = "age_group", values_to = "count") |>
      group_by(sex, age_group) |>
      summarise(count = sum(count), .groups = "drop")

    return(alc_specific_deaths)
  }
