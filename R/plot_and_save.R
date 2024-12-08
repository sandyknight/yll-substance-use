plot_crude_yll_estimate <-
  function(){
source("R/dhsc_colour_palette.R")
source("R/themes.R")
crude_yll <-
  calculate_crude_yll()

p <-
crude_yll  |>
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
  my_theme + my_fill_scale +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5), legend.direction = "horizontal", plot.caption = element_text(face = "italic"))

return(p)
  }


p1 <- plot_crude_yll_estimate()

png("plots/crude_yll_plot.png", width = 15, height = 10, units = "cm", res = 300)
p1
dev.off()




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
          "Total YLL: {scales::comma(sum(crude_yll$yll))} (with discounting and age weighting)"
        ),
        caption = "Data from ONS and NDTMS for deaths which occurred in 2022",
        fill = NULL,
        x = "Age group",
        y = "YLL"
      ) +
      my_theme + my_fill_scale +
      theme(
        axis.text.x = element_text(angle = 30, vjust = 0.5),
        legend.direction = "horizontal",
        plot.caption = element_text(face = "italic")
      )

  }

p2 <-
  plot_substance_use_yll_estimate()

png("plots/yll_plot.png", width = 15, height = 10, units = "cm", res = 300)
p2
dev.off()
