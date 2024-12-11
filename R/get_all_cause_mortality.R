get_all_cause_mortality <-
function(){

if (file.exists("data/processed/ons_leading_mortality_causes.csv")){

ons_leading_mortality_causes <-
read.xlsx(
  xlsxFile = "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsregisteredinenglandandwalesseriesdrreferencetables/2022/dr2022corrected.xlsx",
          sheet = "10b",
          rows = c(6:55)) |>
  janitor::clean_names() |>
  as_tibble() |>
  mutate(percentage_of_all_deaths_percent = percentage_of_all_deaths_percent / 100)

return(ons_leading_mortality_causes)
}
}
