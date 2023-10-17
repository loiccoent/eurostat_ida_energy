# Prepare for potential exceptions
full_sector_base_year <- function(country, first_year){
   case_when(
        TRUE ~ first_year)
}

full_sector_last_year <- function(country, final_year){
   case_when(
        TRUE ~ final_year)
}