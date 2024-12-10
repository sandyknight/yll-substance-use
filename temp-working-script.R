get_all_cause_mortality <-
function(){

if (file.exists("data/processed/ons_leading_mortality_causes.csv")){

readr::read_csv("data/processed/ons_leading_mortality_causes.csv")

} else {

ons_leading_mortality_causes <-
read.xlsx(
  xlsxFile = "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsregisteredinenglandandwalesseriesdrreferencetables/2022/dr2022corrected.xlsx",
          sheet = "10b",
          rows = c(6:55)) |>
  janitor::clean_names() |>
  as_tibble() |>
  mutate(percentage_of_all_deaths_percent = percentage_of_all_deaths_percent / 100)

readr::write_csv(ons_leading_mortality_causes, "data/processed/ons_leading_mortality_causes.csv")

readr::read_csv("data/processed/ons_leading_mortality_causes.csv")
}
}

all_cause_mortality <- get_all_cause_mortality()
alcohol_deaths      <- merge_alcohol_deaths()
drug_deaths         <- merge_drug_deaths()


leading_mortality_age_groups <-
  unique(pull(all_cause_mortality, age_group))

leading_mortality_age_groups <-
  leading_mortality_age_groups[4:7]



all_cause_mortality <-
all_cause_mortality |>
  filter(age_group %in% leading_mortality_age_groups) |>
  group_by(age_group) |>
  group_split()


all_cause_mortality <-
  lapply(all_cause_mortality, function(x) slice(arrange(x, -deaths), 1:5))

all_cause_mortality <-
  bind_rows(all_cause_mortality)


drug_deaths <-
drug_deaths |>
  filter(age < 80) |>
  filter(age > 18) |>
  mutate(age_group = cut(
    age,
    breaks = c(19, 34, 49, 64, 79),
    labels = c(
      "20 to 34 years",
      "35 to 49 years",
      "50 to 64 years",
      "65 to 79 years"
    ),
    right = TRUE
  )) |>
  filter(age_group %in% leading_mortality_age_groups) |>
  group_by(substance, age_group) |>
  summarise(count = sum(count), .groups = "drop") |>
  mutate(leading_cause = "Deaths associated with drug use") |>
  select(-substance)


alcohol_deaths <-
alcohol_deaths |>
  mutate(age_group =
           case_match(
             age_group,
             "20-24" ~ "20 to 34 years",
             "25-29" ~ "20 to 34 years",
             "30-34" ~ "20 to 34 years",
             "35-39" ~ "35 to 49 years",
             "40-44" ~ "35 to 49 years",
             "45-49" ~ "35 to 49 years",
             "50-54" ~  "50 to 64 years",
             "55-59" ~  "50 to 64 years",
             "60-64" ~  "50 to 64 years",
             "65-69" ~  "65 to 79 years",
             "70-74" ~  "65 to 79 years",
             "75-79" ~  "65 to 79 years"
           )
  ) |>
  filter(!is.na(age_group)) |>
  group_by(substance, age_group) |>
  summarise(count = sum(count), .groups = "drop") |>
  mutate(leading_cause = "Deaths associated with alcohol use") |>
  select(-substance)


all_cause_mortality <-
all_cause_mortality |>
  select(age_group, leading_cause,  deaths) |>
  rename("count" = deaths)


leading_cause_mortality_comparison <-
bind_rows(list(all_cause_mortality,
               alcohol_deaths,
               drug_deaths))

leading_cause_mortality_comparison <-
leading_cause_mortality_comparison |>
  group_by(age_group) |>
  group_split()


data <- leading_cause_mortality_comparison[[1]]



plot_leading_cause_comparison <- function(data, substance = "both"){

causes <-
   pull(filter(data, !str_detect(leading_cause, "drug|alcohol")), leading_cause)

if (substance == "combined") {
  data <-
  data  |>
    filter(leading_cause %in% c(
      "Deaths associated with drug use",
       "Deaths associated with alcohol use"
    )) |>
    group_by(age_group) |>
    summarise(count = sum(count), .groups = "drop") |>
    mutate(leading_cause = "Deaths associated with drug and alcohol use")  |>
    bind_rows(filter(data, !leading_cause %in% c(
      "Deaths associated with drug use",
       "Deaths associated with alcohol use"
    )))
} else {
  substance <-
    switch(substance,
           "drugs" = "Deaths associated with alcohol use",
           "alcohol" = "Deaths associated with drug use",
           "both" = c("Deaths associated with drug use", "Deaths associated with alcohol use")
    )
  data <-
    data |>
      filter(leading_cause %in% c(substance, causes))



}


data |>
  arrange(count) |>
  mutate(leading_cause = forcats::as_factor(leading_cause)) |>
  ggplot(aes(x = count, y = leading_cause, fill = leading_cause)) +
  geom_col() +
  scale_fill_manual(values = c("Deaths associated with drug use" = "red", "Deaths associated with alcohol use" = "red", "Deaths associated with drug and alcohol use" = "red")) +
   my_theme +
   theme(legend.position = "none") +
    scale_x_continuous(labels = scales::comma) +
    labs(x = "Deaths (n)", y = NULL, subtitle = unique(data$age_group))

}

plot_leading_cause_comparison(data = leading_cause_mortality_comparison[[1]], substance = "combined")

lc_plots <- lapply(leading_cause_mortality_comparison, plot_leading_cause_comparison)

outer(plot_leading_cause_comparison, data = leading_cause_mortality_comparison, substance = c("drugs", "alcohol", "both", "combined"))

  purrr::map2(.x = leading_cause_mortality_comparison, .y = c("drugs", "alcohol", "both", "combined"), .f = plot_leading_cause_comparison)


lc_plots[[1]]

cowplot::plot_grid(plotlist = lc_plots)

