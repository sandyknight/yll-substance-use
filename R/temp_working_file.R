combine_national_data_alcohol <- function(){
total_alc_spec_deaths_ons <-
get_ons_alcohol_specific_death_data()  |>
  summarise(count = sum(count)) %>%
  pull(count)


alcohol_deaths <-
get_alcohol_specific_deaths_from_tx(data = get_non_poisoning_deaths())  |>
  filter(period_range == "April 2021 to March 2022") |>
  group_by(death_cause) |>
  summarise(count = sum(count), .groups = "drop")  |>
  mutate(count = case_when(death_cause == "Alcohol-specific death" ~ total_alc_spec_deaths_ons,
                   TRUE ~ count)) %>%
  mutate(death_category = if_else(
    death_cause == "Alcohol-specific death",
    "Alcohol-specific deaths (ONS)",
    "Additional deaths (NDTMS-ONS linkage)"
    )) |>
  arrange(-count)

aggregate_alcohol_deaths <-
alcohol_deaths |>
  group_by(death_category) |>
  summarise(count = sum(count), .groups = "drop")

return(aggregate_alcohol_deaths)
}


drugs_data <- get_national_data_drugs() |>
  relabel_national_data()  |>
  mutate(substance = "Drugs")

alc_data <-
  combine_national_data_alcohol() |>
  mutate(substance = "Alcohol") |>
  mutate(year = 2022)


merged_data <-
  bind_rows(drugs_data, alc_data) |>
  mutate(year = factor(year))

merged_data <-
merged_data |>
  mutate(death_category = factor(death_category, levels = rev(c(
    "Initial alcohol-specific deaths",
    "Additional alcohol deaths",
    "Initial poisoning drug deaths",
    "Additional poisoning drug deaths",
    "Non-poisoning drug deaths: Died in treatment",
    "Non-poisoning drug deaths: Died within a year of discharge"
  )))) %>%
    arrange(rev(death_category))

df <- merged_data


df
df |>
ggplot(aes(x = 1, y = count)) +
  geom_hline(yintercept = sum(df$count), linetype = 2) +
  geom_col(aes(group = death_category, fill = substance), width = 0.5, alpha = 0.7, colour = "black") +
    geom_text(
      aes(
        label = scales::comma(count),
        group = death_category
      ),
      position = position_stack(0.5),
      size = 4,
      colour = "black",
      fontface = "bold"
    ) +
  geom_text(aes(label = death_category, group = death_category, x = 1.4, y = count),
            position = position_stack(0.5), hjust = 0) +
  my_theme +
  scale_fill_dhsc() +
  scale_x_continuous(limits = c(0.5, 5)) +
  scale_y_continuous(labels = scales::comma, breaks = c(0, 5000, 10000,15000, 20000,sum(df$count))) +
  theme(legend.position = 'none', axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(x = NULL, y = "Deaths (n)")

source("R/themes.R")
source("R/dhsc_colour_palette.R")
