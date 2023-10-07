# THIS SCRIPT SHOULD ONLY BE RUN WHEN UPDATING THE EUROSTAT DATA

# Retrieve data
retrieve_eurostat_data <- function(first_year,
                                   last_year, 
                                   data_path) {
  #Energy consumption (and supply) from the energy balance (nrg_bal_c)
  nrg_bal_c <- get_eurostat(id = "nrg_bal_c",
                            time_format = "num") %>%
    filter(geo %in% EU_df$code,
           time >= first_year,
           time <= last_year,
           unit %in% c("TJ", "GWH"))
  
  # Add the EU27 total
  nrg_bal_c <- nrg_bal_c %>%
    rbind(
      nrg_bal_c %>%
        group_by(unit, nrg_bal, siec, time) %>%
        summarise(values = sum(values, na.rm = TRUE)) %>%
        mutate(geo = 'EU27')
    )
  
  # Save the dataframe for later use
  save(nrg_bal_c, file = paste(data_path, "nrg_bal_c.Rda", sep = ""))
  
  #Economic activity from the national account data (nama_10_a64)
  nama_10_a64 <- get_eurostat(id = "nama_10_a64",
                              time_format = "num") %>%
    filter(
      geo %in% EU_df$code,
      time >= first_year,
      time <= last_year,
      na_item == "B1G",
      unit == "CLV15_MEUR"
    )
  
  # Add the EU27 total
  nama_10_a64 <- nama_10_a64 %>%
    rbind(
      nama_10_a64 %>%
        group_by(unit, nace_r2, na_item, time) %>%
        summarise(values = sum(values, na.rm = TRUE)) %>%
        mutate(geo = 'EU27')
    )
  
  # Save the dataframe for later use
  save(nama_10_a64, file = paste(data_path, "nama_10_a64.Rda", sep = ""))
  
  #Employment data from the national account (nama_10_a10_e)
  nama_10_a10_e <- get_eurostat(id = "nama_10_a10_e",
                                time_format = "num") %>%
    filter(
      geo %in% EU_df$code,
      time >= first_year,
      time <= last_year,
      #work with Total employment domestic concept, in Thousand persons
      na_item == "EMP_DC",
      unit == "THS_PER"
    )
  
  # Add the EU27 total
  nama_10_a10_e <- nama_10_a10_e %>%
    rbind(
      nama_10_a10_e %>%
        group_by(unit, nace_r2, na_item, time) %>%
        summarise(values = sum(values, na.rm = TRUE)) %>%
        mutate(geo = 'EU27')
    )
  
  # Save the dataframe for later use
  save(nama_10_a10_e,
       file = paste(data_path, "nama_10_a10_e.Rda", sep = ""))
  
  #Industrial Production Index data from the Industry, trade and services (ei_isin_m)
  ei_isin_m <- get_eurostat(id = "ei_isin_m",
                            time_format = "num") %>%
    filter(
      geo %in% EU_df$code,
      time >= first_year,
      time <= last_year,
      #take the Production index
      indic == "IS-IP",
      #select Seasonally and calendar adjusted data
      s_adj == "SCA"
    )
  
  # Add the EU27 total
  ei_isin_m <- ei_isin_m %>%
    rbind(
      ei_isin_m %>%
        group_by(unit, s_adj, indic, nace_r2, time) %>%
        summarise(values = mean(values, na.rm = TRUE)) %>%
        mutate(geo = 'EU27')
    )
  
  # Save the dataframe for later use
  save(ei_isin_m, file = paste(data_path, "ei_isin_m.Rda", sep = ""))
  
  # Construction Production Index data from the Industry, trade and services (ei_isbu_m)
  ei_isbu_m <- get_eurostat(id = "ei_isbu_m",
                            time_format = "num") %>%
    filter(
      geo %in% EU_df$code,
      time >= first_year,
      time <= last_year,
      #take the Production index
      indic == "IS-IP",
      #select Seasonally and calendar adjusted data
      s_adj == "SCA"
    )
  
  # Add the EU27 total
  ei_isbu_m <- ei_isbu_m %>%
    rbind(
      ei_isbu_m %>%
        group_by(unit, s_adj, indic, nace_r2, time) %>%
        summarise(values = mean(values, na.rm = TRUE)) %>%
        mutate(geo = 'EU27')
    )
  
  # Save the dataframe for later use
  save(ei_isbu_m, file = paste(data_path, "ei_isbu_m.Rda", sep = ""))
  
  # Disaggregated final energy consumption in households (nrg_d_hhq)
  nrg_d_hhq <- get_eurostat(id = "nrg_d_hhq",
                            time_format = "num") %>%
    filter(geo %in% EU_df$code,
           time >= first_year,
           time <= last_year,
           #work with total energy consumption, in TJ
           siec == "TOTAL",
           unit == "TJ")
  
  # Add the EU27 total
  nrg_d_hhq <- nrg_d_hhq %>%
    rbind(
      nrg_d_hhq %>%
        group_by(unit, siec, nrg_bal, time) %>%
        summarise(values = sum(values, na.rm = TRUE)) %>%
        mutate(geo = 'EU27')
    )
  
  # Save the dataframe for later use
  save(nrg_d_hhq, file = paste(data_path, "nrg_d_hhq.Rda", sep = ""))
  
  # Cooling and heating degree days (nrg_chdd_a)
  nrg_chdd_a <- get_eurostat(id = "nrg_chdd_a",
                             time_format = "num") %>%
    # Keep all years for now (used to calculate the baseline)
    filter(geo %in% EU_df$code)
  
  # Add the EU27 total
  nrg_chdd_a <- nrg_chdd_a %>%
    rbind(
      nrg_chdd_a %>%
        group_by(unit, indic_nrg, time) %>%
        # take the average of all countries
        summarise(values = mean(values, na.rm = TRUE)) %>%
        mutate(geo = 'EU27')
    )
  
  # Save the dataframe for later use
  save(nrg_chdd_a, file = paste(data_path, "nrg_chdd_a.Rda", sep = ""))
  
  # Population from the demographic balance (demo_gind)
  demo_gind <- get_eurostat(id = "demo_gind",
                            time_format = "num") %>%
    filter(#take only EU countries
      geo %in% EU_df$code,
      time >= first_year,
      time <= last_year,
      #take total population
      indic_de == "AVG")
  
  # Add the EU27 total
  demo_gind <- demo_gind %>%
    rbind(
      demo_gind %>%
        group_by(indic_de, time) %>%
        summarise(values = sum(values, na.rm = TRUE)) %>%
        mutate(geo = 'EU27')
    )
  
  # Save the dataframe for later use
  save(demo_gind, file = paste(data_path, "demo_gind.Rda", sep = ""))
  
  # Average household size from the EU-SILC survey (ilc_lvph01)
  ilc_lvph01 <- get_eurostat(id = "ilc_lvph01",
                             time_format = "num") %>%
    filter(geo %in%  EU_df$code,
           time >= first_year,
           time <= last_year,
           #take total population
           unit == "AVG")
  
  # Add the EU27 total
  ilc_lvph01 <- ilc_lvph01 %>%
    rbind(
      ilc_lvph01 %>%
        group_by(unit, time) %>%
        # take the average of all countries
        summarise(values = mean(values, na.rm = TRUE)) %>%
        mutate(geo = 'EU27')
    )
  
  # Save the dataframe for later use
  save(ilc_lvph01, file = paste(data_path, "ilc_lvph01.Rda", sep = ""))
  
  # Vehicle kilometer from road transport data (road_tf_vehmov)
  road_tf_vehmov <- get_eurostat(id = "road_tf_vehmov",
                                 time_format = "num") %>%
    filter(
      geo %in% EU_df$code,
      time >= first_year,
      time <= last_year,
      vehicle == "TOTAL"
    )
  
  # Add the EU27 total
  road_tf_vehmov <- road_tf_vehmov %>%
    rbind(
      road_tf_vehmov %>%
        group_by(unit, vehicle, regisveh, time) %>%
        # take the average of all countries
        summarise(values = mean(values, na.rm = TRUE)) %>%
        mutate(geo = 'EU27')
    )
  
  # Save the dataframe for later use
  save(road_tf_vehmov,
       file = paste(data_path, "road_tf_vehmov.Rda", sep = ""))
  
  # Vehicle kilometer from rail transport data (rail_tf_trainmv)
  rail_tf_trainmv <- get_eurostat(id = "rail_tf_trainmv",
                                  time_format = "num") %>%
    filter(
      geo %in% EU_df$code,
      time >= first_year,
      time <= last_year,
      #work with total trains (goods + passenger), in thousand VKM
      train == "TOTAL",
      unit == "THS_TRKM"
    )
  
  # Add the EU27 total
  rail_tf_trainmv <- rail_tf_trainmv %>%
    rbind(
      rail_tf_trainmv %>%
        group_by(unit, train, time) %>%
        # take the average of all countries
        summarise(values = sum(values, na.rm = TRUE)) %>%
        mutate(geo = 'EU27')
    )
  
  # Save the dataframe for later use
  save(rail_tf_trainmv,
       file = paste(data_path, "rail_tf_trainmv.Rda", sep = ""))
  
  # Vehicle kilometer from water transport data (iww_tf_vetf)
  iww_tf_vetf <- get_eurostat(id = "iww_tf_vetf",
                              time_format = "num") %>%
    filter(
      geo %in% EU_df$code,
      time >= first_year,
      time <= last_year,
      #work with total (loaded and empty) and transport from all nationalities, in thousand VKM
      tra_cov == "TOTAL",
      loadstat == "TOTAL",
      unit == "THS_VESKM"
    )
  
  # Add the EU27 total
  iww_tf_vetf <- iww_tf_vetf %>%
    rbind(
      iww_tf_vetf %>%
        group_by(unit, tra_cov, loadstat, time) %>%
        # take the average of all countries
        summarise(values = sum(values, na.rm = TRUE)) %>%
        mutate(geo = 'EU27')
    )
  
  # Save the dataframe for later use
  save(iww_tf_vetf, file = paste(data_path, "iww_tf_vetf.Rda", sep = ""))
  
}