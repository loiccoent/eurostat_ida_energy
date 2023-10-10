print('MT first year set to 2012')
print('BE last year set to 2020')
print('DE last year set to 2020')
print('CY last year set to 2020')
print('ES last year set to 2020')
print('FR last year set to 2020')
print('IT last year set to 2020')
print('LT last year set to 2020')
print('LV last year set to 2020')
print('PL last year set to 2020')
print('PT last year set to 2020')
print('SE last year set to 2020')
print('EU27 last year set to 2020')

industry_GVA_base_year <- function(country, first_year){
   case_when(
        country == "MT" ~ 2012,
        TRUE ~ first_year)
}

industry_GVA_last_year <- function(country, final_year){
   case_when(
        #country == "BG" ~ 2019,
        #country == "IT" ~ 2019,
        #country == "LV" ~ 2019,
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
        TRUE ~ final_year)
}

economy_emp_base_year <- function(country, first_year){
   case_when(
        TRUE ~ first_year)
}

economy_emp_last_year <- function(country, final_year){
   case_when(
        TRUE ~ final_year)
}