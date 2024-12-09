
# Load scripts

sapply(file.path("R", list.files("R/")), source)

# Produce and save results

crude_yll_estimate <-
  calculate_crude_yll()

yll_estimate <-
  calculate_substance_use_yll()

age_group_order <-
c(
"<1",
"01-04",
"05-09",
"10-14",
"15-19",
"20-24",
"25-29",
"30-34",
"35-39",
"40-44",
"45-49",
"50-54",
"55-59",
"60-64",
"65-69",
"70-74",
"75-79",
"80-84",
"85-89",
"90+"
)

yll_estimate %>%

  select(-count)  |>
  pivot_wider(names_from = substance, values_from = yll) %>%
  arrange(age)

# Produce and save plots

p1 <-
  plot_crude_yll_estimate()

png("plots/crude_yll_plot.png", width = 15, height = 10, units = "cm", res = 300)
p1
dev.off()

p2 <-
  plot_substance_use_yll_estimate()

png("plots/yll_plot.png", width = 15, height = 10, units = "cm", res = 300)
p2
dev.off()
