yll  |>
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
    subtitle = glue::glue("Total years of life lost: {scales::comma(sum(yll$yll))}")
  ) +
  theme(
    axis.text.x = element_text(angle = 30, vjust = 0.5),
    legend.direction = "horizontal"
  )
