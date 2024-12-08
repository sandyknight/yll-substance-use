get_non_poisoning_deaths <- function(){
np_deaths <- 
  openxlsx::read.xlsx("data/raw/post election data for Jon- sent.xlsx", sheet = "NDTMS_ONS") |> 
  janitor::clean_names()

np_deaths <- 
np_deaths |> 
  filter(geography == "LA")

return(np_deaths )
  
}