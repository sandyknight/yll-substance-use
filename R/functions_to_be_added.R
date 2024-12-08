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



calculate_substance_use_yll <- function(){
alcohol_deaths <-
  merge_alcohol_deaths()

age_df <-
  parse_age_groups(unique(pull(alcohol_deaths, age_group)))

life_tables <-
life_tables |>
  mutate(age_group = assign_age_group(ages = pull(life_tables, age), age_df = age_df)) |>
  group_by(sex, age_group) |>
  summarise(mean_age = mean(age), ex = mean(ex), .groups = "drop")

alcohol_yll <-
left_join(alcohol_deaths, life_tables) |>
  mutate(yll = calculate_yll(number.deaths = count, average.age.death = mean_age, model.life.expectancy = ex)) |>
  group_by(age_group, substance) |>
  summarise(count = sum(count), yll = sum(yll), .groups = "drop")


drug_deaths <-
  merge_drug_deaths()

life_tables <-
  get_life_tables()

yll <-
  left_join(drug_deaths, life_tables)

drugs_yll <-
  yll |>
  mutate(yll = calculate_yll(number.deaths = count, average.age.death = age, model.life.expectancy = ex))

drugs_yll <-
drugs_yll |>
  mutate(age_group = assign_age_group(ages = age, age_df)) |>
  group_by(age_group, substance) |>
  summarise(count = sum(count), yll = sum(yll), .groups = "drop")


substance_use_yll <-
  bind_rows(alcohol_yll, drugs_yll)

return(substance_use_yll)

}

substance_use_yll  |>
  ggplot(aes(x = age_group, y = yll)) +
  geom_col(aes(fill = substance), colour = "black") +
  my_theme + my_fill_scale +
  scale_y_continuous(labels = scales::comma) +
  labs(
    fill = NULL,
    y = "Years of life lost",
    x = "Age group",
    title = "Years of life lost due to substance use",
    caption = "Deaths data from 2021 and 2022",
    subtitle = glue::glue("Total years of life lost: {scales::comma(sum(substance_use_yll$yll))}")
  ) +
  theme(
    axis.text.x = element_text(angle = 30, vjust = 0.5),
    legend.direction = "horizontal"
  )

