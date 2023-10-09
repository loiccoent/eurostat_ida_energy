library(eurostat)
library(tidyr)
library(dplyr)
library(yaml)

config <- yaml.load_file("config.yml")

# Set global parameters
eu27 <- eu_countries %>% filter(code != "UK")
geo_codes <- eu27$code %>% append("EU27")

first_year <- as.double(config$year$first)
last_year <- as.double(config$year$last)
year_chart <- as.double(config$year$chart)
country <- config$country

download <- config$actions$download
clear <- config$actions$clear

# Set paths
data_path <- path(getwd(), "data/raw")
output_path <- path(getwd(), "output")