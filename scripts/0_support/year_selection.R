# Prepare for potential exceptions
full_sector_base_year <- function(country, first_year) {
    year <- case_when(
        TRUE ~ first_year
    )
    year
}

full_sector_last_year <- function(country, final_year) {
    year <- case_when(
        TRUE ~ final_year
    )
    year
}


industry_GVA_base_year <- function(country, first_year) {
    # if(country=="MT"){print("MT first year set to 2012")}
    year <- case_when(
        country == "MT" ~ 2012,
        TRUE ~ first_year
    )
    year
}

industry_GVA_last_year <- function(country, final_year) {
	# if(country=="BE"){print("BE last year set to 2020")}
    # if(country=="DE"){print("DE last year set to 2020")}
    # if(country=="CY"){print("CY last year set to 2020")}
    # if(country=="ES"){print("ES last year set to 2020")}
    # if(country=="FR"){print("FR last year set to 2020")}
    # if(country=="IT"){print("IT last year set to 2020")}
    # if(country=="LT"){print("LT last year set to 2020")}
    # if(country=="LV"){print("LV last year set to 2020")}
    # if(country=="PL"){print("PL last year set to 2020")}
    # if(country=="PT"){print("PT last year set to 2020")}
    # if(country=="SE"){print("SE last year set to 2020")}
    # if(country=="EU27"){print("EU27 last year set to 2020")}
	year <- case_when(
        # country == "BG" ~ 2019,
        # country == "IT" ~ 2019,
        # country == "LV" ~ 2019,
        country == "BE" ~ 2020,
        country == "DE" ~ 2020,
        country == "CY" ~ 2020,
        country == "ES" ~ 2020,
        country == "FR" ~ 2020,
        country == "IT" ~ 2020,
        country == "LT" ~ 2020,
        country == "LV" ~ 2020,
        country == "PL" ~ 2020,
        country == "PT" ~ 2020,
        country == "SE" ~ 2020,
        country == "EU27" ~ 2020,
        TRUE ~ final_year
    )
    year
}

economy_emp_base_year <- function(country, first_year) {
    year <- case_when(
        TRUE ~ first_year
    )
    year
}

economy_emp_last_year <- function(country, final_year) {
    year <- case_when(
        TRUE ~ final_year
    )
    year
}

household_base_year <- function(country, first_year){
   case_when(
        TRUE ~ first_year)
}

household_last_year <- function(country, final_year){
   case_when(
     country == "HU" ~ 2019,
        TRUE ~ final_year)
}