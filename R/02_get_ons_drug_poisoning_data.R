get_ons_drug_poisoning_data  <-
  function(){
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
      select(1,2,5,9)

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
