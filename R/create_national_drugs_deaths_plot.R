create_national_drugs_deaths_plot <- function(){
 data <- get_national_data_drugs() |>
  relabel_national_data()

p <-
  plot_national_data_drugs(data)

p <-
  add_plot_annotations(plot = p, data = data)

return(p)
}

