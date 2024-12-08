assign_age_group <- function(ages, age_df) {
  # We assume ages is already an integer vector
  age_groups <- character(length(ages))
  
  # Assign age groups based on intervals
  for (i in seq_len(nrow(age_df))) {
    in_interval <- ages >= age_df$lower[i] & ages <= age_df$upper[i]
    age_groups[in_interval] <- age_df$age_group[i]
  }