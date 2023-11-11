# RUN THIS FIRST TO DEFINE GLOBAL PARAMETERS

# Load required packages

#install package (if not already done)
#install.packages("fs")
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
setwd(path(here()))
# Source parameters
source(path(getwd(), "scripts/0_support/parameters.R"))

# If requested, download all the data from Eurostat website
if (download) {
    source(path(getwd(), "scripts/0_support/data_download.R"))
    download_eurostat_data(first_year, last_year, data_path)
    }
# If requested, clean the folder first
if (clear) {
    source(path(getwd(), "scripts/0_support/clear_folders.R"))
    clear_all(country, output_path)
    }
# If requested, calculate all the required indicators, Generate the charts by country
if (run_context) {
    source(path(getwd(), "scripts/4_all_sectors/full_energy.R"))
    full_energy_final(first_year, last_year, country, data_path, output_path)
    }
if (run_industry_GVA_final) {
    source(path(getwd(), "scripts/1_industry/1a_industry_gva_final.R"))
    industry_GVA_final(first_year, last_year, country, data_path, output_path)
    }
if (run_industry_GVA_primary) {
    source(path(getwd(), "scripts/1_industry/1b_industry_gva_primary.R"))
    industry_GVA_primary(first_year, last_year, country, data_path, output_path)
    }
if (run_economy_emp_final) {
    source(path(getwd(), "scripts/1_industry/1c_economy_emp_final.R"))
    economy_emp_final(first_year, last_year, country, data_path, output_path)
    }
if (run_household_final) {
    source(path(getwd(), "scripts/2_household/households.R"))
    household_final(first_year, last_year, country, data_path, output_path)
    }
if (run_transport_final) {
    source(path(getwd(), "scripts/3_transport/transport_vkm.R"))
    transport_final(first_year, last_year, country, data_path, output_path)
    }
