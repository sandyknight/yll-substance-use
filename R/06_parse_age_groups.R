parse_age_groups <- function(age_groups) {
  # Patterns for age group formats
  pattern_range <- "^([0-9]+)-([0-9]+)$"
  pattern_under <- "^<([0-9]+)$"
  pattern_over  <- "^([0-9]+)\\+$"

  # Initialize vectors
  lower <- rep(NA_real_, length(age_groups))
  upper <- rep(NA_real_, length(age_groups))

  # Identify which pattern each age group matches
  is_range <- grepl(pattern_range, age_groups)
  is_under <- grepl(pattern_under, age_groups)
  is_over  <- grepl(pattern_over, age_groups)

  # For range: extract lower and upper numbers
  if (any(is_range)) {
    lower[is_range] <- as.numeric(sub(pattern_range, "\\1", age_groups[is_range]))
    upper[is_range] <- as.numeric(sub(pattern_range, "\\2", age_groups[is_range]))
  }

}
