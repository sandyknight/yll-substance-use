merge_alcohol_deaths  <- function(){
ons_alc_deaths <-
  get_ons_alcohol_specific_death_data()

alc_deaths_tx <-
get_alcohol_specific_deaths_from_tx(get_non_poisoning_deaths())  |>
  filter(period_range == "April 2021 to March 2022", death_cause != "Alcohol-specific death" ) |>
  group_by(sex, age) |>
  summarise(count = sum(count, na.rm = TRUE), .groups = "drop")

age_df  <<-
  parse_age_groups(pull(ons_alc_deaths, age_group))

alc_deaths_tx <-
alc_deaths_tx |>
  mutate(age_group = assign_age_group(pull(alc_deaths_tx, age), age_df = age_df)) |>
  group_by(sex, age_group) |>
  summarise(count = sum(count), .groups = "drop")

alcohol_deaths <-
bind_rows(alc_deaths_tx, ons_alc_deaths) |>
  group_by(sex, age_group) |>
  summarise(count = sum(count), .groups = "drop")


alcohol_deaths <-
alcohol_deaths |>
  mutate(sex = tolower(sex))

return(alcohol_deaths)
}

stack_drugs_and_alcohol_deaths  <-
  function(){
bind_rows(
mutate(merge_alcohol_deaths(), substance = "Alcohol"),
mutate(merge_drug_deaths(), substance  = "Drugs")
)
  }

drugs_and_alcohol_deaths <-
  stack_drugs_and_alcohol_deaths()

life_tables <-
get_life_tables()


life_tables <-
life_tables |>
  mutate(age_group = assign_age_group(pull(life_tables, age), age_df)) |>
  group_by(sex, age_group) |>
  summarise(ex = mean(ex), .groups = "drop")



drugs_and_alcohol_deaths
life_tables

yll_df <-
yll_df |>
  mutate(crude_yll = count * ex) |>
  group_by(age_group, substance) |>
  summarise(count = sum(count), crude_yll = sum(crude_yll))


yll_df |>
  ggplot(aes(x = age_group, y = crude_yll)) +
  geom_col(aes(fill = substance), colour = "black")
