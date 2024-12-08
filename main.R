

sapply(file.path("R", list.files("R/")), source)

#FIXME
# Need to number scripts so they're loaded in the correct order

p1 <- plot_crude_yll_estimate()

png("plots/crude_yll_plot.png", width = 15, height = 10, units = "cm", res = 300)
p1
dev.off()

p2 <-
  plot_substance_use_yll_estimate()

png("plots/yll_plot.png", width = 15, height = 10, units = "cm", res = 300)
p2
dev.off()
