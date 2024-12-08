process_poisoning_data <- function(data, date_of = "occurrence", years = c(2022, 2023), by = NULL, by_sex = FALSE) {
  if (is.null(by)) {
    message(paste("Drug poisoning: national level data, all ages", years, sep = ", "))
  }