get_alcohol_specific_deaths_from_tx <- function(data){
data |> 
  filter(drug_group == "alcohol only"|death_cause == "Alcohol-specific death") |> 
  filter(treatment_status != "Died one or more years following discharge")
  
}