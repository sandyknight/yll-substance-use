get_drug_poisoning_deaths <- function(){ 
if (!file.exists("data/raw/ndtms_mortality_data.parquet")) {
  #  This file is NDTMS-ONS data linkage; received by email from Stefan and
  #  named: "table1_all deaths_Cocaine version 1.xlsx"
  
  df <- # Load deaths data from Excel
    openxlsx::read.xlsx("data/raw/table1_all deaths_Cocaine version 1.xlsx",
                        sheet = "table1_all deaths")
  # Write data to easier format
  write_parquet(df, "data/raw/ndtms_mortality_data.parquet")
  
}