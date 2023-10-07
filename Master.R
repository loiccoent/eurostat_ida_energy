# RUN THIS FIRST TO DEFINE GLOBAL PARAMETERS

# Load required packages

#install package (if not already done)
#install.packages("tidyverse")
#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("RColorBrewer")
#install.packages("plotly")
#install.packages("waterfalls")
#install.packages("eurostat")
#install.packages("ggpubr")


# Load required packages
library(fs)
library(here)
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
#library(plotly)
library(waterfalls)
library(eurostat)
library("ggpubr")

rm()

# Paths
data_path <- path(here(), "/Data/")
chart_path <- path(here(), "/Output/")

# Set global parameters

EU_df <- eu_countries %>% filter(code != "UK")
geo_codes <- EU_df$code %>% append("EU27")

#per the project requirements, data start in 2010
first_year <- 2011
#do not take 2019 for now (economic data incomplete)
last_year <- 2021
#year shown in charts
year_chart <- 2021
# Retrieve data
retrieve_data <- F
# Clear charts
clear_charts <- F


# Source functions --------------------------------------------------------
source(path(here(), "/0-Preparation/data_extract.R"))
source(path(here(), "/0-Preparation/clear_folders.R"))
source(path(here(), "/0-Preparation/Full_Energy.R"))
source(path(here(), "/1-Industry/Industry_GVA_Final.R"))
source(path(here(), "/1-Industry/Industry_GVA_Primary.R"))
source(path(here(), "/1-Industry/Economy_Emp_Final.R"))
source(path(here(), "/2-Household/Households.R"))
source(path(here(), "/3-Transport/Transport_VKM.R"))

# Get the Eurostat data (either download it or work with the data saved locally)
if (retrieve_data) {
  # Download all the data from Eurostat website
  retrieve_eurostat_data(first_year, last_year, data_path)
}

# Clean the folder first
if (clear_charts) {
  # Download all the data from Eurostat website
  clear_all("all", chart_path)
}

# Calculate all the required indicators,
# Generate the charts by country
# For EU, generate an additional chart for the comparison among countries
full_energy_final(first_year, last_year, "EU27", data_path, chart_path)
industry_GVA_final(first_year, last_year, "EU27", data_path, chart_path)
industry_GVA_primary(first_year, last_year, "EU27", data_path, chart_path)
economy_emp_final(first_year, last_year, "EU27", data_path, chart_path)
household_final(first_year, last_year, "EU27", data_path, chart_path)
transport_final(first_year, last_year, "SI", data_path, chart_path)
