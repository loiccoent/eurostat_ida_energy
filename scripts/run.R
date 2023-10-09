# RUN THIS FIRST TO DEFINE GLOBAL PARAMETERS

# Load required packages

#install package (if not already done)
#install.packages("fs")
#install.packages("here")
#install.packages("yaml")
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
library(yaml)
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
#library(plotly)
library(waterfalls)
library("ggpubr")

rm()

# Source parameters and functions
source(path(getwd(), "scripts/0_support/parameters.R"))
source(path(getwd(), "scripts/0_support/data_download.R"))
source(path(getwd(), "scripts/0_support/clear_folders.R"))
source(path(getwd(), "scripts/1_industry/1a_industry_gva_final.R"))
source(path(getwd(), "scripts/1_industry/1b_industry_gva_primary.R"))
source(path(getwd(), "scripts/1_industry/1c_economy_emp_final.R"))
source(path(getwd(), "scripts/2_household/households.R"))
source(path(getwd(), "scripts/3_transport/transport_vkm.R"))
source(path(getwd(), "scripts/4_all_sectors/full_energy.R"))

# Get the Eurostat data (either download it or work with the data saved locally)
if (download) {
  # Download all the data from Eurostat website
  download_eurostat_data(first_year, last_year, data_path)
}

# Clean the folder first
if (clear) {
  # Download all the data from Eurostat website
  clear_all("all", output_path)
}

# Calculate all the required indicators,
# Generate the charts by country
# For EU, generate an additional chart for the comparison among countries
#full_energy_final(first_year, last_year, country, data_path, output_path)
industry_GVA_final(first_year, last_year, country, data_path, output_path)
industry_GVA_primary(first_year, last_year, country, data_path, output_path)
economy_emp_final(first_year, last_year, country, data_path, output_path)
household_final(first_year, last_year, country, data_path, output_path)
transport_final(first_year, last_year, country, data_path, output_path)
