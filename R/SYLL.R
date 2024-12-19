get_esp2013 <- function() {

  url <- "https://webarchive.nationalarchives.gov.uk/ukgwa/20160106020035mp_/http://www.ons.gov.uk/ons/guide-method/user-guidance/health-and-life-events/age-standardised-mortality-rate-calculation-template-using-the-2013-esp.xls"

  esp2013 <- tempfile()

  curl::curl_download(url, destfile = esp2013)

  esp2013 <- readxl::read_xls(esp2013, sheet = "Methods", range = "B22:C43")

  return(esp2013)
}
calculate_substance_use_yll()


#' Standardised potential years of life lost
#'
#' @param d number of deaths
#' @param a average age-specific lffe expectancy
#' @param n population in age group
#' @param w number of people in standard population (ESP 2013) age group
syll <- function(d, a, n, w) {
  i((w * ((a * d) / n)) / w) * 100e03
}
