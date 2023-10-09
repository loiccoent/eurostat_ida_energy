library(eurostat)
library(tidyr)
library(dplyr)

# THIS SCRIPT SHOULD ONLY BE RUN WHEN UPDATING THE EUROSTAT DATA

retrieve_eurostat_data <- function(id, first_year, last_year) {
  get_eurostat(id = id, time_format = "num") %>%
    filter(
      geo %in% eu27$code,
      time >= first_year,
      time <= last_year
    )
}

add_EU_27_sum <- function(df, cols) {
  df %>%
    rbind(
      df %>%
        group_by_at(cols) %>%
        # take the sum of all countries
        summarise(values = sum(values, na.rm = TRUE)) %>%
        mutate(geo = "EU27")
    )
}

add_EU_27_mean <- function(df, cols) {
  df %>%
    rbind(
      df %>%
        group_by_at(cols) %>%
        # take the average of all countries
        summarise(values = mean(values, na.rm = TRUE)) %>%
        mutate(geo = "EU27")
    )
}

# Retrieve data
download_eurostat_data <- function(first_year,
                                   last_year,
                                   data_path) {
  # Energy consumption (and supply) from the energy balance (nrg_bal_c)
  nrg_bal_c <- retrieve_eurostat_data("nrg_bal_c", first_year, last_year) %>%
    filter(
      unit %in% c("TJ", "GWH")
    )
  # Add the EU27 total and save the dataframe for later use
  nrg_bal_c <- nrg_bal_c %>%
    add_EU_27_sum(c("unit", "nrg_bal", "siec", "time"))
  save(nrg_bal_c, file = paste(data_path, "/nrg_bal_c.Rda", sep = ""))

  # Economic activity from the national account data (nama_10_a64)
  nama_10_a64 <- retrieve_eurostat_data("nama_10_a64", first_year, last_year) %>%
    filter(
      na_item == "B1G",
      unit == "CLV15_MEUR"
    )
  # Add the EU27 total and save the dataframe for later use
  nama_10_a64 <- nama_10_a64 %>%
    add_EU_27_sum(c("unit", "nace_r2", "na_item", "time"))
  save(nama_10_a64, file = paste(data_path, "/nama_10_a64.Rda", sep = ""))

  # Employment data from the national account (nama_10_a10_e)
  nama_10_a10_e <- retrieve_eurostat_data("nama_10_a10_e", first_year, last_year) %>%
    filter(
      # work with Total employment domestic concept, in Thousand persons
      na_item == "EMP_DC",
      unit == "THS_PER"
    )
  # Add the EU27 total and save the dataframe for later use
  nama_10_a10_e <- nama_10_a10_e %>%
    add_EU_27_sum(c("unit", "nace_r2", "na_item", "time"))
  save(nama_10_a10_e, file = paste(data_path, "/nama_10_a10_e.Rda", sep = ""))

  # Disaggregated final energy consumption in households (nrg_d_hhq)
  nrg_d_hhq <- retrieve_eurostat_data("nrg_d_hhq", first_year, last_year) %>%
    filter(
      # work with total energy consumption, in TJ
      siec == "TOTAL",
      unit == "TJ"
    )
  # Add the EU27 total and save the dataframe for later use
  nrg_d_hhq <- nrg_d_hhq %>%
    add_EU_27_sum(c("unit", "siec", "nrg_bal", "time"))
  save(nrg_d_hhq, file = paste(data_path, "/nrg_d_hhq.Rda", sep = ""))

  # Cooling and heating degree days (nrg_chdd_a)
  nrg_chdd_a <- get_eurostat(
    id = "nrg_chdd_a",
    time_format = "num"
  ) %>%
    # Keep all years for now (used to calculate the baseline)
    filter(geo %in% eu27$code)
  # Add the EU27 total and save the dataframe for later use
  nrg_chdd_a <- nrg_chdd_a %>%
    add_EU_27_mean(c("unit", "indic_nrg", "time")) %>%
    save(nrg_chdd_a, file = paste(data_path, "/nrg_chdd_a.Rda", sep = ""))

  # Population from the demographic balance (demo_gind)
  demo_gind <- retrieve_eurostat_data("demo_gind", first_year, last_year) %>%
    filter(# take only EU countries
      # take total population
      indic_de == "AVG"
    )
  # Add the EU27 total and save the dataframe for later use
  demo_gind <- demo_gind %>%
    add_EU_27_sum(c("indic_de", "time"))
  save(demo_gind, file = paste(data_path, "/demo_gind.Rda", sep = ""))

  # Average household size from the EU-SILC survey (ilc_lvph01)
  ilc_lvph01 <- retrieve_eurostat_data("ilc_lvph01", first_year, last_year) %>%
    filter(
      # take the average household size
      unit == "AVG"
    )
  # Add the EU27 total and save the dataframe for later use
  ilc_lvph01 <- ilc_lvph01 %>%
    add_EU_27_mean(c("unit", "time"))
  save(ilc_lvph01, file = paste(data_path, "/ilc_lvph01.Rda", sep = ""))

  # Vehicle kilometer from road transport data (road_tf_vehmov)
  road_tf_vehmov <- retrieve_eurostat_data("road_tf_vehmov", first_year, last_year) %>%
    filter(
      vehicle == "TOTAL"
    )
  # Add the EU27 total and save the dataframe for later use
  road_tf_vehmov <- road_tf_vehmov %>%
    add_EU_27_sum(c("unit", "vehicle", "regisveh", "time"))
  save(road_tf_vehmov, file = paste(data_path, "/road_tf_vehmov.Rda", sep = ""))

  # Vehicle kilometer from rail transport data (rail_tf_trainmv)
  rail_tf_trainmv <- retrieve_eurostat_data("rail_tf_trainmv", first_year, last_year) %>%
    filter(
      # work with total trains (goods + passenger), in thousand VKM
      train == "TOTAL",
      unit == "THS_TRKM"
    )
  # Add the EU27 total and save the dataframe for later use
  rail_tf_trainmv <- rail_tf_trainmv %>%
    add_EU_27_sum(c("unit", "train", "time"))
  save(rail_tf_trainmv, file = paste(data_path, "/rail_tf_trainmv.Rda", sep = ""))

  # Vehicle kilometer from water transport data (iww_tf_vetf)
  iww_tf_vetf <- retrieve_eurostat_data("iww_tf_vetf", first_year, last_year) %>%
    filter(
      # work with total (loaded and empty) and transport from all nationalities, in thousand VKM
      tra_cov == "TOTAL",
      loadstat == "TOTAL",
      unit == "THS_VESKM"
    )
  # Add the EU27 total
  iww_tf_vetf <- iww_tf_vetf %>%
    add_EU_27_sum(c("unit", "tra_cov", "loadstat", "time"))
  # Save the dataframe for later use
  save(iww_tf_vetf, file = paste(data_path, "/iww_tf_vetf.Rda", sep = ""))
}