get_life_tables <- function() {
  url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/nationallifetablesunitedkingdomreferencetables/current/nltuk198020203.xlsx"

  life_tables <-
    read.xlsx(
      xlsxFile = url,
      sheet = "2020-2022",
      startRow = 6
    ) |>
    rename_with(.cols = 1:6, ~paste0(.x, "_male")) |>
    rename_with(.cols = 7:12, ~paste0(.x, "_female")) |>
    select(ex_male, age_female, ex_female) |>
    pivot_longer(cols = contains("ex"), values_to = "ex", names_to = "sex") |>
    mutate(sex = str_remove(sex, "ex_")) |>
    rename("age" = age_female)

  return(life_tables)
}
