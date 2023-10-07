# PRIMARY ENERGY CONSUMPTION IN ECONOMY

economy_emp_base_year <- function(country, first_year){
   case_when(
        TRUE ~ first_year)
}

economy_emp_last_year <- function(country, final_year){
   case_when(
        TRUE ~ final_year)
}

# Data preparation
economy_emp_final <- function(first_year,
                              last_year,
                              country,
                              data_path,
                              chart_path) {
  
  # Define the list as the whole list
  country_list <- geo_codes
  
  #Agriculture, forestry and fishing
  NRG_AGRI <- c("FC_OTH_AF_E",
                "FC_OTH_FISH_E")
  #Manufacturing
  NRG_MAN <- c(
    "FC_IND_FBT_E",
    "FC_IND_TL_E",
    "FC_IND_WP_E",
    "FC_IND_PPP_E",
    "NRG_PR_E",
    "FC_IND_CPC_E",
    "FC_IND_NMM_E",
    "FC_IND_IS_E",
    "NRG_CO_E",
    "NRG_BF_E",
    "FC_IND_NFM_E",
    "FC_IND_MAC_E",
    "FC_IND_TE_E",
    "NRG_PF_E",
    "NRG_BKBPB_E",
    "NRG_CL_E",
    "NRG_GTL_E",
    "NRG_CPP_E",
    "NRG_NSP_E",
    "FC_IND_NSP_E"
  )
  
  #Other industries
  NRG_OTH <- c(
    "FC_IND_MQ_E",
    "NRG_CM_E",
    "NRG_OIL_NG_E",
    #"TI_EHG_E", # removed because double counting with electricity
    "NRG_EHG_E",
    "NRG_GW_E",
    "NRG_LNG_E",
    "NRG_BIOG_E",
    "NRG_NI_E"
  )
  
  #list of end uses sectors, used in the subset
  NRG_ECO_SECTORS <- c(
    NRG_AGRI,
    NRG_MAN,
    NRG_OTH,
    "FC_IND_CON_E",
    "FC_OTH_CP_E"
  )
  
  # Coal, manufactured gases, peat and peat products
  COAL_PRODS <- c("C0110",
                  "C0121",
                  "C0129",
                  "C0210",
                  "C0220",
                  "C0311",
                  "C0312",
                  "C0320",
                  "C0330",
                  "C0340",
                  "C0350",
                  "C0360",
                  "C0371",
                  "C0379",
                  "P1100",
                  "P1200")
  
  # Oil, petroleum products, oil shale and oil sands
  OIL_PRODS <- c(
    "O4100_TOT",
    "O4200",
    "O4300",
    "O4400X4410",
    "O4500",
    "O4610",
    "O4620",
    "O4630",
    "O4640",
    "O4651",
    "O4652XR5210B",
    "O4669",
    "O4671XR5220B",
    "O4653",
    "O4661XR5230B",
    "O4680",
    "O4691",
    "O4692",
    "O4693",
    "O4694",
    "O4695",
    "O4699",
    "S2000")
  
  # Biofuels and renewable wastes
  BIO_PRODS <- c("R5110-5150_W6000RI",
                 "R5160",
                 "R5210P",
                 "R5210B",
                 "R5220P",
                 "R5220B",
                 "R5230P",
                 "R5230B",
                 "R5290",
                 "R5300",
                 "W6210")
  
  # Non-renewable wastes
  OTH_PRODS <- c("W6100",
                 "W6220")
  
  # 
  OTH_REN <- c("RA200",
               "RA300",
               "RA410",
               "RA420",
               "RA500",
               "RA600")
  
  #list of products, used for the subset of the energy balance (nrg_bal_c)
  NRG_PRODS <- c(
    COAL_PRODS,
    OIL_PRODS,
    "G3000",
    BIO_PRODS,
    OTH_PRODS,
    "RA100",
    OTH_REN,
    "N900H",
    "E7000",
    "H8000"
  )
  
  EMP_ECO_SECTORS <- c(
    "A", 
    "B-E", 
    "C", 
    "F", 
    "G-I", 
    "J", 
    "K", 
    "L", 
    "M_N", 
    "O-Q",
    "R-U")
  
  #list of end uses sectors, as they will be named in the LMDI results
  IDA_ECO_SECTORS <- c(
    #"Agriculture, forestry and fishing",
    "Agricult., forest. and fish.",
    "Manufacturing",
    "Construction",
    #"Commercial and public services"
    "Comm. and pub. services",
    "Other industries"
  )
  
  #list of products, as they will be named in the charts
  IDA_ECO_PROD <- c(
    "Coal",
    "Oil",
    "Gas",
    "Biofuels and renewable wastes",
    "Non-renewable wastes",
    #    "Nuclear",
    #    "Hydro",
    "Wind, solar, geothermal, etc.",
    "Heat",
    "Electricity"
  )
  
  # Colors
  
  ColorsIndex <- c(
    "Energy consumption" = "blue4",
    "Employment"  = "red4",
    "Energy consumption per employee" = "green4"
  )
  
  ColorsEffect <- c(
    "Activity" = "red4",
    "Intensity"  = "green4",
    "Structure" = "purple4"
  )
  
  ColorsSector <- c(
    #"Agriculture, forestry and fishing" = brewer.pal(5, "Set3")[1],
    "Agricult., forest. and fish." = brewer.pal(5, "Set3")[3],
    "Manufacturing" = brewer.pal(5, "Set3")[2],
    "Construction" = brewer.pal(5, "Set3")[1],
    "Other industries" = brewer.pal(5, "Set3")[4],
    #"Commercial and public services" = brewer.pal(5, "Set3")[5]
    "Comm. and pub. services" = brewer.pal(5, "Set3")[5]
  )
  
  ColorsProduct <- c(
    "Coal" = brewer.pal(10, "Set3")[10],
    "Oil" = brewer.pal(10, "Set3")[6],
    "Gas" = brewer.pal(10, "Set3")[8],
    "Biofuels and renewable wastes" = brewer.pal(10, "Set3")[7],
    "Non-renewable wastes" = brewer.pal(10, "Set3")[9],
    #    "Nuclear" = brewer.pal(10, "Set3")[2],
    #    "Hydro" = brewer.pal(10, "Set3")[5],
    "Wind, solar, geothermal, etc." = brewer.pal(10, "Set3")[1],
    "Heat" = brewer.pal(10, "Set3")[4],
    "Electricity" = brewer.pal(10, "Set3")[3]
  )
  
  # DATA PREPARATION
  
  # Energy consumption (and supply) from the energy balance (nrg_bal_c)
  load(paste0(data_path, "/nrg_bal_c.Rda"))
  
  # Employment data from the national account (nama_10_a10_e)
  load(paste0(data_path, "/nama_10_a10_e.Rda"))
  
  # Energy consumption by fuel
  economy_energy_breakdown <-  nrg_bal_c %>%
    filter(
      geo %in% country_list,
      #from first year
      time >= first_year,
      # to last year
      time <= last_year,
      #take industry end uses
      nrg_bal %in% NRG_ECO_SECTORS,
      #work with total energy consumption, in TJ
      siec %in% NRG_PRODS,
      unit == "TJ"
    ) %>%
    group_by(geo, time, siec) %>%
    summarise(values = sum(values, na.rm = TRUE)) %>% 
    ungroup() %>%
    #reshape to wide
    pivot_wider(names_from = siec,
                values_from = values) %>%
    #aggregate
    mutate(
      # Coal, manufactured gases, peat and peat products
      CPS = rowSums(select(., COAL_PRODS), na.rm = TRUE),
      # Oil, petroleum products, oil shale and oil sands
      OS = rowSums(select(.,  OIL_PRODS), na.rm = TRUE), 
      # Biofuels and renewable wastes
      RW = rowSums(select(., BIO_PRODS), na.rm = TRUE),
      # Non-renewable wastes
      NRW = rowSums(select(., OTH_PRODS), na.rm = TRUE),
      # Wind, solar, geothermal, etc.
      MR = rowSums(select(., OTH_REN), na.rm = TRUE)
    ) %>%
    #keep only relevant columns
    select(
      -c(
        C0110,
        C0121,
        C0129,
        C0210,
        C0220,
        C0311,
        C0312,
        C0320,
        C0330,
        C0340,
        C0350,
        C0360,
        C0371,
        C0379,
        P1100,
        P1200,
        O4100_TOT,
        O4200,
        O4300,
        O4400X4410,
        O4500,
        O4610,
        O4620,
        O4630,
        O4640,
        O4651,
        O4652XR5210B,
        O4669,
        O4671XR5220B,
        O4653,
        O4661XR5230B,
        O4680,
        O4691,
        O4692,
        O4693,
        O4694,
        O4695,
        O4699,
        S2000,
        .data[["R5110-5150_W6000RI"]],
        R5160,
        R5210P,
        R5210B,
        R5220P,
        R5220B,
        R5230P,
        R5230B,
        R5290,
        R5300,
        W6210,
        W6100, 
        W6220, 
        RA200, 
        RA300, 
        RA410, 
        RA420, 
        RA500, 
        RA600
      )
    ) %>%
    #rename to explicit names
    rename(
      "Coal" = "CPS",
      "Oil" = "OS",
      "Gas" = "G3000",
      "Biofuels and renewable wastes" = "RW",
      "Non-renewable wastes" = "NRW",
      "Nuclear" = "N900H",
      "Hydro" = "RA100",
      "Wind, solar, geothermal, etc." = "MR",
      "Heat" = "H8000",
      "Electricity" = "E7000"
    ) %>%
    #reshape to long
    pivot_longer(
      cols = -c(geo, time),
      names_to = "product",
      values_to = "energy_consumption"
    ) %>%
    mutate(product = factor(product, level = IDA_ECO_PROD)) %>%
    group_by(geo, time) %>%
    mutate(share_energy_consumption = energy_consumption / sum(energy_consumption)) %>%
    ungroup()
  
  #energy consumption (and supply) from the energy balance (nrg_bal_c)
  economy_energy_final <- nrg_bal_c %>%
    filter(
      geo %in% country_list,
      #from first year
      time >= first_year,
      # to last year
      time <= last_year,
      #take economy sectors
      nrg_bal %in% NRG_ECO_SECTORS,
      #work with total energy consumption, in TJ
      siec == "TOTAL",
      unit == "TJ"
    ) %>%
    select(c("geo", "time", "nrg_bal", "values")) %>%
    #reshape to wide
    pivot_wider(names_from = nrg_bal, 
                values_from = values) %>%
    #aggregate
    mutate(
      #Agriculture, forestry and fishing
      A = rowSums(select(., NRG_AGRI), na.rm = TRUE),
      #Manufacturing
      C = rowSums(select(., NRG_MAN), na.rm = TRUE), 
      #Other industries
      B_D_E = rowSums(select(., NRG_OTH), na.rm = TRUE)
    ) %>%
    #keep only relevant columns
    select(
      -c(
        FC_OTH_AF_E,
        FC_OTH_FISH_E,
        FC_IND_MQ_E,
        NRG_CM_E,
        NRG_OIL_NG_E,
        FC_IND_FBT_E,
        FC_IND_TL_E,
        FC_IND_WP_E,
        FC_IND_PPP_E,
        NRG_PR_E,
        FC_IND_CPC_E,
        FC_IND_NMM_E,
        FC_IND_IS_E,
        NRG_CO_E,
        NRG_BF_E,
        FC_IND_NFM_E,
        FC_IND_MAC_E,
        FC_IND_TE_E,
        NRG_PF_E,
        NRG_BKBPB_E,
        NRG_CL_E,
        NRG_GTL_E,
        NRG_CPP_E,
        NRG_NSP_E,
        FC_IND_NSP_E,
        #TI_EHG_E,
        NRG_EHG_E,
        NRG_GW_E,
        NRG_LNG_E,
        NRG_BIOG_E,
        NRG_NI_E
      )
    ) %>%
    #rename to explicit names
    rename(
      #"Agriculture, forestry and fishing" = "A",
      "Agricult., forest. and fish." = "A",
      "Construction" = "FC_IND_CON_E",
      "Manufacturing" = "C",
      "Other industries" = "B_D_E",
      #"Commercial and public services" = "FC_OTH_CP_E"
      "Comm. and pub. services" = "FC_OTH_CP_E"
    ) %>%
    #reshape to long
    pivot_longer(
      cols = -c(geo, time),
      names_to = "sector",
      values_to = "energy_consumption"
    )
  
  # prepare employment data
  economy_employment <- nama_10_a10_e %>%
    filter(
      #take only EU countries
      geo %in% country_list,
      #from first year
      time >= first_year,
      # to last year
      time <= last_year,
      #take economy sectors
      nace_r2 %in% EMP_ECO_SECTORS,
      #work with Total employment domestic concept, in Thousand persons
      na_item == "EMP_DC",
      unit == "THS_PER"
    ) %>%
    #reshape to wide
    pivot_wider(names_from = nace_r2, values_from = values) %>%
    rename(
      "B_E" = "B-E",
      "G_I" = "G-I",
      "O_Q" = "O-Q",
      "R_U" = "R-U"
    ) %>%
    #aggregate
    mutate(#Other industry
      B_D_E = B_E - C,
      #Commercial and Public Services
      G_U = rowSums(select(., c("G_I", "J", "K", "L", "M_N", "O_Q", "R_U")), 
                    na.rm = TRUE)
      ) %>%
    #keep only relevant columns
    select(-c(na_item, unit, B_E, G_I, J, K, L, M_N, O_Q, R_U)) %>%
    #rename to explicit names
    rename(
      #"Agriculture, forestry and fishing" = "A",
      "Agricult., forest. and fish." = "A",
      "Manufacturing" = "C",
      "Construction" = "F",
      "Other industries" = "B_D_E",
      #"Commercial and public services" = "G_U"
      "Comm. and pub. services" = "G_U"
    ) %>%
    #reshape to long
    pivot_longer(
      cols = -c(geo, time),
      names_to = "sector",
      values_to = "employment"
    )
  
  # Remove negative and 0 employment
  economy_employment$employment <-
    replace(
      economy_employment$employment, 
      which(economy_employment$employment <= 0), 
      NA)
  # Remove negative and 0 Energy consumption
  economy_energy_final$energy_consumption <-
    replace(
      economy_energy_final$energy_consumption,
      which(economy_energy_final$energy_consumption <= 0),
      NA)
  
  # joining datasets
  economy_emp_final_complete <- full_join(economy_energy_final,
                                 economy_employment,
                                 by = c("geo", "time", "sector")) %>% 
    mutate(
      employment = case_when(
        (employment ==0 & energy_consumption > 0) ~ NA_real_,
        TRUE ~ employment),
      energy_consumption = case_when(
        (energy_consumption == 0 & employment > 0) ~ NA_real_,
        TRUE ~ energy_consumption),
      # intensity calculated here for the charts, will be recalculated later once the totals are included
      intensity = case_when(
        (employment == 0 & energy_consumption > 0) ~ NA_real_,
        (employment == 0 & energy_consumption == 0) ~ 0, 
        TRUE ~ energy_consumption / employment)
    ) %>% 
    # For each country and each year
    group_by(geo, time) %>%
    mutate(
      # Calculate the total energy consumption and value added of the overall industry sector, as the sum of all subsectors
      total_energy_consumption = sum(energy_consumption, na.rm = TRUE),
      total_employment = sum(employment, na.rm = TRUE)
    ) %>%
    # For each country, each year and each subsector
    ungroup() %>%
    mutate(
      # Calculate the share of the subsector in the overall energy consumption and in the overall value added of the industry sector
      share_energy_consumption = energy_consumption / total_energy_consumption,
      share_employment = employment / total_employment
    )

  # filter out sectors with incomplete data
  economy_emp_final_filtered <- economy_emp_final_complete
  
  # indicators calculations
  
  #calculate the required indicators for the 3 effects
  economy_emp_final_augmented <- economy_emp_final_filtered %>%
    #for each country and each year
    group_by(geo, time) %>%
    mutate(
      #calculate the total energy consumption and value added of the overall economy, as the sum of all sectors selected
      total_energy_consumption = sum(energy_consumption, na.rm = TRUE),
      total_employment = sum(employment, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    #for each country, each year and each subsector
    mutate(
      #calculate the share of each sector in the overall energy consumption and in the overall employment of the economy
      share_energy_consumption = energy_consumption / total_energy_consumption,
      share_employment = employment / total_employment
    ) %>%
    #remove the total columns, not required any longer
    select(-c(total_energy_consumption, 
              total_employment,
              intensity)) %>%
    ungroup()
  
  economy_emp_final_total <- economy_emp_final_augmented %>%
    group_by(geo, time) %>%
    summarize(employment = sum(employment, na.rm = TRUE),
              energy_consumption = sum(energy_consumption, na.rm = TRUE),
              # the sum of shares should be one, calculated here for checking
              share_employment = sum(share_employment, na.rm = TRUE),
              share_energy_consumption = sum(share_energy_consumption, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(sector = "Total")
  
  # Calculate the indexed and differenced indicators
  
  # Copy the dataframe to store all the values indexed on the base year
  economy_emp_final_full <- economy_emp_final_augmented %>%
    rbind(economy_emp_final_total) %>%
    # calculate intensity again, to include the total intensity
    mutate(intensity = case_when(
      (employment == 0 & energy_consumption > 0) ~ NA_real_,
      (employment == 0 & energy_consumption == 0) ~ 0, 
      TRUE ~ energy_consumption / employment)) %>% 
    pivot_longer(cols = -c(geo, time, sector), 
                 names_to = "measure", 
                 values_to = "value") %>%
    group_by(geo, sector, measure) %>%
    mutate(
      value_indexed = case_when(
         value[time == economy_emp_base_year(country=.data[["geo"]], first_year = first_year)] == 0 ~ 0,
         is.na(value[time == economy_emp_base_year(country=.data[["geo"]], first_year = first_year)]) ~ NA_real_,
         TRUE ~  value / value[time == economy_emp_base_year(country=.data[["geo"]], first_year = first_year)]),
      value_delta = value - value[time == economy_emp_base_year(country=.data[["geo"]], first_year = first_year)],
      time = as.integer(time)
    ) %>%
    ungroup()
  
  # Effects calculation
  
  # Calculate the effects using the LMDI formulas
  economy_emp_final_LMDI <- economy_emp_final_full %>%
    # Reshape to wide (moving all measures calculated in Value, index and delta, all in separate columns)
    pivot_wider(
      names_from = measure,
      values_from = c(value, value_indexed, value_delta)
    ) %>%
    # Calculate the effects
    mutate(
      # The weighting factor links the effect calculated on the indexed variation to the total energy consumption variation
      weighting_factor = ifelse(
        value_delta_energy_consumption == 0,
        value_energy_consumption,
        value_delta_energy_consumption / log(value_indexed_energy_consumption)
      ),
      # Apply natural logarithm to the indexed values for each sub sectors
      activity_log = ifelse(value_indexed_employment == 0, 0, log(value_indexed_employment)),
      structure_log = ifelse(value_indexed_share_employment == 0, 0, log(value_indexed_share_employment)),
      intensity_log = ifelse(value_indexed_intensity == 0, 0, log(value_indexed_intensity))
    ) %>%
    # Keep only the relevant columns
    select(
      geo,
      time,
      sector,
      weighting_factor,
      value_energy_consumption,
      value_delta_energy_consumption,
      activity_log,
      structure_log,
      intensity_log
    ) %>%
    # The baseline figures need to be expanded across all sub sectors, and across all years
    rowwise() %>%
    mutate(base_year = economy_emp_base_year(country=.data[["geo"]], first_year = first_year)) %>%
    ungroup() %>%
    group_by(geo) %>%
    mutate(
      value_energy_consumption_total_baseline = value_energy_consumption[sector == "Total" & 
                                                                           time == base_year]
      ) %>%
    ungroup() %>%
    # Similarly, the figures calculated for the total sector and the end figures need to be expanded across all subsectors
    group_by(geo, time) %>%
    mutate(
      activity_log_total = activity_log[sector == "Total"],
      value_delta_energy_consumption_total = value_delta_energy_consumption[sector == "Total"],
      value_energy_consumption_total_end = value_energy_consumption[sector == "Total"]
      ) %>%
    ungroup() %>%
    # Now the total sector is not required any longer
    filter(sector != "Total") %>%
    # Multiply the weighting factor * log(indexed subsectors), or weighting factor * log(indexed total sector)
    mutate(
      ACT = weighting_factor * activity_log_total,
      STR = weighting_factor * structure_log,
      INT = weighting_factor * intensity_log
    ) %>%
    # Remove unnecessary columns
    select(
      -c(
        weighting_factor,
        activity_log,
        activity_log_total,
        structure_log,
        intensity_log
      )
    ) %>%
    # Now the figures calculated at subsector level need to be aggregated
    group_by(geo, time) %>%
    # The aggregation is performed differently:
    summarize(
      # Either by summing all subsectors
      activity_effect = sum(ACT), #na.rm = TRUE),
      structural_effect = sum(STR), #na.rm = TRUE),
      intensity_effect = sum(INT), #na.rm = TRUE),
      # By keeping the mean figure when only one exist across all subsectors
      energy_consumption_var_obs = mean(value_delta_energy_consumption_total),
      value_energy_consumption_total_baseline = mean(value_energy_consumption_total_baseline),
      value_energy_consumption_total_end = mean(value_energy_consumption_total_end)
    ) %>%
    ungroup() %>%
    # For checking purposes, recalculate the total energy consumption calculated as the sum of the effects
    mutate(energy_consumption_var_calc = 
             rowSums(select(., c("activity_effect", 
                                 "structural_effect", 
                                 "intensity_effect")), 
                     #na.rm = TRUE
                     ))
  
  ### CHARTS ###
  
  if (country == "all") {
    countries <- geo_codes
  } else {
    countries <- country
  }
  
  for (country_chart in countries) {
    
    # Long name for the country
    country_name <- filter(EU_df, code == country_chart)$name
    # Output charts
    outputpath <-
      paste0(chart_path, "/", country_chart, "/")
    # first and last year shown in charts
    first_year_chart <-
      economy_emp_base_year(country=country_chart, first_year = first_year)
    last_year_chart <-
      economy_emp_last_year(country = country_chart, final_year = last_year)
  
    # Country data
    economy_emp_final_country_data <- 
      economy_emp_final_complete %>%
      filter(
        geo == country_chart,
        time <= last_year_chart
        ) %>%
      mutate(sector = factor(sector, levels = IDA_ECO_SECTORS))
    
    # full data (after filtering)
    economy_emp_final_subsector <- economy_emp_final_full %>%
      filter(geo == country_chart,
             time >= first_year_chart,
             time <= last_year_chart) 
    
    # Breakdown of energy consumption by fuel
    economy_energy_breakdown_filtered <- economy_energy_breakdown %>% 
      filter(geo == country_chart,
             !energy_consumption == 0)
    
    # Table used to provide figures in the text of the report
    table_economy_energy_breakdown_filtered <- economy_energy_breakdown_filtered %>%
      mutate(energy_consumption = round(energy_consumption, 2),
             share_energy_consumption = round(share_energy_consumption, 3)) 
    
    write.csv(table_economy_energy_breakdown_filtered, paste0(outputpath, "Part3_fuel.csv"), row.names = FALSE)
    
    # Table used to provide figures in the text of the report
    table_economy_emp_final_country_data <- economy_emp_final_country_data %>%
      mutate(employment = round(employment, 2),
             energy_consumption = round(energy_consumption, 2),
             share_employment = round(share_employment, 2),
             share_energy_consumption = round(share_energy_consumption, 2))
    
    write.csv(table_economy_emp_final_country_data, paste0(outputpath, "Part3_sector.csv"), row.names = FALSE)
    
    # chart indexed variation of total economy
    
    economy_emp_final_country_indexed <- 
      economy_emp_final_subsector %>%
      filter(sector == "Total") %>%
      select(-c(value, value_delta)) %>%
      pivot_wider(
        names_from = measure, 
        values_from = value_indexed)  %>%
      select(c(
        geo, 
        time, 
        intensity, 
        energy_consumption, 
        employment)) %>%
      rename(
        "Energy consumption per employee" = "intensity",
        "Energy consumption" = "energy_consumption",
        "Employment" = "employment"
      )
  
    year <- as.Date(as.character(economy_emp_final_country_indexed$time), "%Y")
  
    p <- economy_emp_final_country_indexed %>%
      cbind(year) %>%
      select(-time) %>%
      pivot_longer(
        cols = -c(geo, year),
        names_to = "measure",
        values_to = "value"
      ) %>%
      ggplot() +
      geom_blank(aes(x = year)) +
      geom_line(aes(x = year, y = value, color = measure), size = 1) +
      scale_color_manual(values = ColorsIndex) +
      theme_classic() +
      theme(
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        text = element_text(size = 15)
      ) +
      guides(fill=guide_legend(ncol=3)) + 
      scale_y_continuous(labels = scales::number) +
      ylab(paste("Index (", economy_emp_base_year(country=country_chart, first_year = first_year), "=1)"))
    # ggtitle(paste("Indexed indicators for",country_name,"'s total energy consumption, \nproduction index and energy consumption per employee, \nall years related to",as.character(base_year)))
    
    filename <- paste0(country_chart, "_Figure17B.jpg")
    
    print(filename)
    
    jpeg(
      file = paste0(outputpath, filename),
      width = 2400,
      height = 2400,
      res = 300
    )
    
    print(p)
    
    dev.off()
    
    # Final energy consumption by fuel
    
    year <-
      as.Date(as.character(economy_energy_breakdown_filtered$time),
              "%Y")
    
    p <- economy_energy_breakdown_filtered %>%
      cbind(year) %>%
      select(-time) %>%
      mutate(year = lubridate::year(year)) %>%
      ggplot(aes(x = year, y = energy_consumption / 1000)) +
      geom_bar(aes(fill = product), stat = "identity") +
      scale_fill_manual(values = ColorsProduct, limits = force) +
      theme_classic() +
      theme(
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        text = element_text(size = 15)
      ) +
      guides(fill=guide_legend(ncol=3)) + 
      scale_x_continuous(breaks = c(first_year,round((first_year+last_year)/2), last_year)) +
      scale_y_continuous(labels = scales::number) +
      ylab(paste("Energy consumption (PJ)"))
    #ggtitle(paste("Economy energy consumption by fuel for",country_name))
    
    filename <- paste0(country_chart, "_Figure14.jpg")
    print(filename)
    
    jpeg(
      file = paste0(outputpath, filename),
      width = 2400,
      height = 2400,
      res = 300
    )
    
    print(p)
    
    dev.off()
    
    # 2 years
    
    p <- economy_energy_breakdown_filtered %>%
      filter(time %in% c(first_year, last_year)) %>%
      ggplot(aes(x = factor(time), y = share_energy_consumption, fill = product)) +
      geom_bar(position = "fill", stat = "identity") +
      scale_fill_manual(values = ColorsProduct, limits = force) +
      theme_classic() +
      theme(
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        text = element_text(size = 15)
      ) +
      guides(fill=guide_legend(ncol=3)) + 
      scale_y_continuous(labels = scales::percent) +
      geom_text(aes(label = paste0(round(share_energy_consumption*100, 0), "%")), 
                position = position_stack(vjust = 0.5)) +
      ylab(paste("Share in final energy consumption"))
    # ggtitle(paste("Economy energy consumption by fuel for",country_name))
    
    filename <- paste0(country_chart, "_Figure14B.jpg")
    print(filename)
    
    jpeg(
      file = paste0(outputpath, filename),
      width = 2400,
      height = 2400,
      res = 300
    )
    
    print(p)
    
    dev.off()
  
    # Energy consumption by subsector
    
    year <- as.Date(as.character(economy_emp_final_country_data$time), "%Y")
  
    p <- economy_emp_final_country_data %>%
      cbind(year) %>%
      select(-time) %>%
      mutate(year = lubridate::year(year)) %>%
      ggplot(aes(x = year, y = energy_consumption / 1000)) +
      geom_bar(aes(fill = sector), stat = "identity") +
      scale_fill_manual(values = ColorsSector, limits = force) +
      theme_classic() +
      theme(
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        text = element_text(size = 15)
      ) +
      guides(fill=guide_legend(ncol=3)) + 
      scale_x_continuous(breaks = c(first_year,round((first_year+last_year)/2), last_year)) +
      scale_y_continuous(labels = scales::number) +
      ylab(paste("Energy consumption (PJ)"))
    # ggtitle(paste("Total energy consumption by economy subsector for",country_name))
  
    filename <- paste0(country_chart, "_Figure15.jpg")
    
    print(filename)
    
    jpeg(
      file = paste0(outputpath, filename),
      width = 2400,
      height = 2400,
      res = 300
    )
    
    print(p)
    
    dev.off()
    
    # 2 years share
    
    p <- economy_emp_final_country_data %>%
      filter(time %in% c(first_year, last_year)) %>%
      mutate(sector = factor(sector, levels = IDA_ECO_SECTORS))  %>%
      ggplot(aes(x = factor(time), y = share_energy_consumption, fill = sector)) +
      geom_bar(position = "fill", stat = "identity") +
      scale_fill_manual(values = ColorsSector, limits = force) +
      theme_classic() +
      theme(
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        text = element_text(size = 15)
      ) +
      guides(fill=guide_legend(ncol=3)) + 
      scale_y_continuous(labels = scales::percent) +
      geom_text(aes(label = paste0(round(share_energy_consumption*100, 0), "%")), 
                position = position_stack(vjust = 0.5)) +
      ylab(paste("Share of energy consumption"))
    # ggtitle(paste("Economy energy consumption by subsector for",country_name))
    
    filename <- paste0(country_chart, "_Figure15B.jpg")
    print(filename)
    
    jpeg(
      file = paste0(outputpath, filename),
      width = 2400,
      height = 2400,
      res = 300
    )
    
    print(p)
    
    dev.off()
    
    # employment by subsector
    
    p <- economy_emp_final_country_data %>%
      cbind(year) %>%
      select(-time) %>%
      mutate(year = lubridate::year(year)) %>%
      ggplot(aes(x = year, y = employment / 1000)) +
      geom_bar(aes(fill = sector), stat = "identity") +
      scale_fill_manual(values = ColorsSector, limits = force) +
      theme_classic() +
      theme(
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        text = element_text(size = 15)
      ) +
      guides(fill=guide_legend(ncol=3)) + 
      scale_x_continuous(breaks = c(first_year,round((first_year+last_year)/2), last_year)) +
      scale_y_continuous(labels = scales::number) +
      ylab(paste("Employment (millions)"))
    # ggtitle(paste("Employment by subsector for",country_name))
    
    filename <- paste0(country_chart, "_Figure16.jpg")
    
    print(filename)
    
    jpeg(
      file = paste0(outputpath, filename),
      width = 2400,
      height = 2400,
      res = 300
    )
    
    print(p)
    
    dev.off()
    
    # 2 years share
    
    p <- economy_emp_final_country_data %>%
      filter(time %in% c(first_year, last_year)) %>%
      ggplot(aes(x = factor(time), y = share_employment, fill = sector)) +
      geom_bar(position = "fill", stat = "identity") +
      scale_fill_manual(values = ColorsSector, limits = force) +
      theme_classic() +
      theme(
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        text = element_text(size = 15)
      ) +
      guides(fill=guide_legend(ncol=3)) + 
      scale_y_continuous(labels = scales::percent) +
      geom_text(aes(label = paste0(round(share_employment*100, 0), "%")), 
                position = position_stack(vjust = 0.5)) +
      ylab(paste("Share of employment"))
    # ggtitle(paste("Economy energy consumption by subsector for",country_name))
    
    filename <- paste0(country_chart, "_Figure16B.jpg")
    print(filename)
    
    jpeg(
      file = paste0(outputpath, filename),
      width = 2400,
      height = 2400,
      res = 300
    )
    
    print(p)
    
    dev.off()
  
    # indexed variation by subsector
    
    year <-
      as.Date(as.character(economy_emp_final_subsector$time), "%Y")
    
    p <- economy_emp_final_subsector %>%
      cbind(year) %>%
      filter(sector != "Total",
             measure == "intensity") %>%
      mutate(sector = factor(sector, levels = IDA_ECO_SECTORS)) %>%
      select(-c("time", "value", "value_delta")) %>%
      ggplot() +
      geom_blank(aes(x = year)) +
      geom_line(aes(x = year, y = value_indexed, color = sector), size = 1) +
      scale_color_manual(values = ColorsSector, limits = force) +
      theme_classic() +
      theme(
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        text = element_text(size = 15)
      ) +
      guides(fill=guide_legend(ncol=3)) + 
      scale_y_continuous(labels = scales::number) +
      ylab(paste("Index (", economy_emp_base_year(country=country_chart, first_year = first_year), "=1)"))
    # ggtitle(paste("Indexed variation for",country_name,"'s energy consumption per employee in sectors of the economy, \nall years related to",as.character(base_year)))
    
    filename <- paste0(country_chart, "_Figure17C.jpg")
    
    print(filename)
    
    jpeg(
      file = paste0(outputpath, filename),
      width = 2400,
      height = 2400,
      res = 300
    )
    
    print(p)
    
    dev.off()
  
    # Simple effect decomposition
    
    # prepare data for the simple effect chart
    economy_emp_final_effects <- economy_emp_final_LMDI %>%
      filter(geo == country_chart,
             time <= last_year_chart,
             time >= first_year_chart) %>%
      rename(
        "Activity" = "activity_effect",
        "Intensity" = "intensity_effect",
        "Structure" = "structural_effect"
      ) %>%
      pivot_longer(
        cols = -c(geo, time),
        names_to = "Effect",
        values_to = "value"
      ) %>%
      filter(Effect == "Activity" |
               Effect == "Structure" |
               Effect == "Intensity")
    
    economy_emp_final_results <- economy_emp_final_LMDI %>%
      filter(geo == country_chart,
             time <= last_year_chart) %>%
      pivot_longer(
        cols = -c(geo, time),
        names_to = "measure",
        values_to = "value"
      ) %>%
      filter(measure == "energy_consumption_var_obs")
    
    # Plot the simple effect as bar chart
    p <- ggplot(data = economy_emp_final_effects,
           aes(x = factor(time),
               y = value / 1000)) +
      geom_bar(aes(fill = Effect),
               stat = "identity")  +
      scale_fill_manual(values = ColorsEffect) +
      geom_point(data = economy_emp_final_results,
                 aes(y = value / 1000),
                 size = 3) +
      theme_classic() +
      theme(axis.title.x = element_blank(),
            text = element_text(size = 15)) +
      scale_y_continuous(labels = scales::number) +
      ylab("Energy consumption variation (PJ)")
    # ggtitle(paste("Decompostion analysis of",country_name,"'s total energy consumption variation, \n  all years related to",as.character(first_year)))
    
    filename <- paste0(country_chart, "_Figure17D.jpg")
    
    print(filename)
    
    jpeg(
      file = paste0(outputpath, filename),
      width = 2400,
      height = 1600,
      res = 300
    )
    
    print(p)
    
    dev.off()
  
    # Waterfall chart
    
    Base_label <- paste0(as.character(first_year_chart), " level")
    Result_label <- paste0(as.character(last_year_chart), " level")
    
    #define the levels used in the waterfall chart
    levels_waterfall <- c(
      Base_label,
      "Activity",
      "Structure",
      "Intensity",
      Result_label)
    
    #prepare data for the waterfall chart (see plotly waterfall for explanations)
    economy_emp_final_Waterfall_data <- economy_emp_final_LMDI %>%
      filter(geo == country_chart,
             time == last_year_chart) %>%
      rename(
        "Activity" = "activity_effect",
        "Intensity" = "intensity_effect",
        "Structure" = "structural_effect",
        !!Base_label := "value_energy_consumption_total_baseline",
        !!Result_label := "value_energy_consumption_total_end"
      ) %>%
      select(!!Base_label,
             "Activity",
             "Structure",
             "Intensity",
             !!Result_label) %>%
      pivot_longer(cols = everything(),
                   names_to = "x",
                   values_to = "y") %>%
      mutate(x = factor(x, level = levels_waterfall),
             text = paste(as.character(round(y, 2)), "TJ", sep = " "),
             measure = case_when((x == !!Result_label) ~ "total",
                                 TRUE ~ "relative")
             )
    
    write.csv(economy_emp_final_Waterfall_data,
              paste0(outputpath, "Part3_waterfall.csv"),
              row.names = FALSE)
    
    p <- economy_emp_final_Waterfall_data %>%
      filter(x != Result_label) %>%
      select(x, y) %>%
      mutate(y = round(y / 1000, 2)) %>%
      waterfall(calc_total = TRUE, 
                rect_text_size = 1.5) +
      theme_classic() +
      #xlab("Effects") +
      theme(axis.title.x = element_blank(),
            text = element_text(size = 15)) +
      scale_y_continuous(labels = scales::number) +
      ylab("Energy consumption level and effect (PJ)") +
      scale_x_discrete(labels = levels_waterfall)
    
    filename <- paste0(country_chart, "_Figure17.jpg")
    
    print(filename)
    
    jpeg(
      file = paste0(outputpath, filename),
      width = 2400,
      height = 1600,
      res = 300
    )
    
    print(p)
    
    dev.off()
  
    # Intensity effect chart
    
    #prepare data for the intensity effect chart
    economy_emp_final_intensity_effect <- 
      economy_emp_final_LMDI %>%
      filter(geo == country_chart,
             time >= first_year,
             time <= last_year) %>%
      select(geo,
             time,
             value_energy_consumption_total_end,
             intensity_effect) %>%
      mutate("Without intensity effect" = value_energy_consumption_total_end - intensity_effect) %>%
      rename("Actual energy consumption" = value_energy_consumption_total_end) %>%
      select(-c(intensity_effect)) %>%
      pivot_longer(
        cols = -c(geo, time),
        names_to = "measure",
        values_to = "value"
      ) %>%
      mutate(measure = factor(
        measure,
        levels = c(
          "Without intensity effect", 
          "Actual energy consumption"
          )
      )) %>%
      arrange(measure)
    
    write.csv(economy_emp_final_intensity_effect,
              paste0(outputpath, "Part3_intensity_effect.csv"),
              row.names = FALSE)
    
    #Plot the intensity effect as area chart
    p <- economy_emp_final_intensity_effect %>%
      ggplot() +
      geom_bar(
        data = (economy_emp_final_intensity_effect %>% 
                  filter(measure == 'Actual energy consumption')),
        aes(y = value / 1000, 
            x = time,
            fill = measure), 
        stat = "identity", 
        alpha = 0.5) +
      scale_fill_manual(values = c("Actual energy consumption" = "blue4")) +
      geom_point(data = (economy_emp_final_intensity_effect %>% 
                           filter(measure == 'Without intensity effect',
                                  time >= first_year_chart,
                                  time <= last_year_chart)
      ),
      aes(y = value / 1000,
          x = time,
          color = measure),
      size = 3,
      alpha = 0.5) +
      scale_color_manual(values = c("Without intensity effect" = "green4")) +
      theme_classic() +
      theme(
        axis.title.x = element_blank(),
        legend.title = element_blank(),

        legend.position = "bottom",
        legend.box = "horizontal",
        text = element_text(size = 15)
      ) +
      guides(fill=guide_legend(ncol=3)) + 
      scale_x_continuous(breaks = c(first_year,round((first_year+last_year)/2), last_year)) +
      scale_y_continuous(labels = scales::number) +
      ylab("Energy consumption (PJ)") +
      expand_limits(y = 0)
    # ggtitle(paste("Actual energy consumption in the economy vs theoretical \n(without improvements in energy consumption per employee) for",country_name))
    
    filename <- paste0(country_chart, "_Figure18.jpg")
    
    print(filename)
    
    jpeg(
      file = paste0(outputpath, filename),
      width = 2400,
      height = 2400,
      res = 300
    )
    
    print(p)
    
    dev.off()
  
    # Total intensity comparison chart
    
    #prepare data for the intensity comparison chart
    economy_emp_final_intensity_comparison <-
      economy_emp_final_full %>%
      filter(measure == "intensity",
             time <= year_chart,
             sector == "Total") %>%
      select(-c(value_indexed, value_delta)) %>%
      pivot_wider(names_from = geo, values_from = value)
    
    min_EU <-
      apply(select_if(
        economy_emp_final_intensity_comparison[, -1], 
        is.numeric
        ),
            1,
            min,
            na.rm = TRUE)
    max_EU <-
      apply(select_if(economy_emp_final_intensity_comparison[, -1], is.numeric),
            1,
            max,
            na.rm = TRUE)
    avg_EU <-
      apply(select_if(economy_emp_final_intensity_comparison[, -1], is.numeric),
            1,
            mean,
            na.rm = TRUE)
    year <-
      as.Date(as.character(economy_emp_final_intensity_comparison$time),
              "%Y")
    
    economy_emp_final_intensity_comparison <-
      economy_emp_final_intensity_comparison %>%
      cbind(min_EU) %>%
      cbind(max_EU) %>%
      cbind(avg_EU) %>%
      cbind(year) %>%
      select(c(year,!!country_chart, min_EU, max_EU)) %>%
      mutate(year = lubridate::year(year))
    
    #Plot the intensity comparison as line chart
    p <- economy_emp_final_intensity_comparison %>%
      ggplot(aes(x = year)) +
      #geom_blank(aes(x = year)) +
      geom_ribbon(aes(
        x = year,
        ymax = max_EU,
        ymin = min_EU,
        fill = "grey"
      )) +
      geom_line(aes(x = year, y = avg_EU, color = "black"), size = 1) +
      geom_line(aes(x = year, y = .data[[!!country_chart]], color = "red"), size = 1) +
      scale_fill_identity(guide = 'legend', labels = c("Europe range")) +
      scale_colour_manual(
        values = c('black' = 'black', 'red' = 'red'),
        labels = c('Europe average', country_name)
      ) +
      theme_classic() +
      theme(
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        text = element_text(size = 15)
      ) +
      guides(fill=guide_legend(ncol=3)) + 
      scale_y_continuous(labels = scales::number) +
      ylab("Energy consumption per employee (TJ / Thousand employee)") +
    scale_x_continuous(breaks = c(first_year,round((first_year+last_year)/2), last_year))
    #ggtitle(paste("Energy consumption per employee in", country_name, "'s ", sector_chart, " subsector compared to \nother European countries"))
    
    filename <- paste0(country_chart, "_Figure19B.jpg")
    
    print(filename)
    
    jpeg(
      file = paste0(outputpath, filename),
      width = 2400,
      height = 2400,
      res = 300
    )
    
    print(p)
    
    dev.off()
  
    # Sectoral intensity comparison chart
    
    #prepare data for the intensity comparison chart
    economy_emp_final_intensity_comparison_sector <-
      economy_emp_final_complete %>%
      mutate(sector = factor(sector, levels = IDA_ECO_SECTORS)) %>%
      filter(time <= year_chart) %>%
      select(-c(employment, energy_consumption, 
                total_energy_consumption, total_employment, 
                share_energy_consumption, share_employment)) %>%
      pivot_wider(names_from = geo, values_from = intensity)
    
    min_EU <-
      apply(select_if(economy_emp_final_intensity_comparison_sector[, -1], is.numeric),
            1,
            min,
            na.rm = TRUE)
    max_EU <-
      apply(select_if(economy_emp_final_intensity_comparison_sector[, -1], is.numeric),
            1,
            max,
            na.rm = TRUE)
    avg_EU <-
      apply(select_if(economy_emp_final_intensity_comparison_sector[, -1], is.numeric),
            1,
            mean,
            na.rm = TRUE)
    year <-
      as.Date(as.character(economy_emp_final_intensity_comparison_sector$time),
              "%Y")
    
    economy_emp_final_intensity_comparison_sector <-
      economy_emp_final_intensity_comparison_sector %>%
      cbind(min_EU) %>%
      cbind(max_EU) %>%
      cbind(avg_EU) %>%
      cbind(year) %>%
      select(c(year, sector,!!country_chart, min_EU, max_EU)) %>%
      mutate(year = lubridate::year(year),
             sector = factor(sector, levels = IDA_ECO_SECTORS))
    
    #Plot the intensity comparison as line chart
    p <- economy_emp_final_intensity_comparison_sector %>%
      ggplot(aes(x = year)) +
      #geom_blank(aes(x = year)) +
      geom_ribbon(aes(
        x = year,
        ymax = max_EU,
        ymin = min_EU,
        fill = "grey"
      )) +
      geom_line(aes(x = year, y = avg_EU, color = "black"), size = 1) +
      geom_line(aes(x = year, y = .data[[!!country_chart]], color = "red"), size = 1) +
      scale_fill_identity(guide = 'legend', labels = c("Europe range")) +
      scale_colour_manual(
        values = c('black' = 'black', 'red' = 'red'),
        labels = c('Europe average', country_name)
      ) +
      theme_classic() +
      theme(
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.8, 0.2),
        text = element_text(size = 15)
      ) +
      ylab("Energy consumption per employee (MJ / employee)") +
      scale_x_continuous(breaks = c(first_year,round((first_year+last_year)/2), last_year)) +
      #scale_y_continuous(labels = scales::number) +
      expand_limits(y = 0) +
      scale_y_continuous(breaks = scales::breaks_extended(Q = c(0, 1, 2, 3))) +
      #ggtitle(paste("Energy consumption per employee in", country_name, "'s subsectors compared to \nother European countries")) +
      facet_wrap( ~ sector, scales = "free", ncol = 3)
    
    filename <- paste0(country_chart, "_Figure19.jpg")
    
    print(filename)
    
    jpeg(
      file = paste0(outputpath, filename),
      width = 2400,
      height = 1600,
      res = 300
    )
    
    print(p)
    
    dev.off()
    
  }
  
  if (country == "EU27"){
    
    outputpath <- paste0(chart_path, "/EU27/")
    
    # Data coverage chart
    p <- economy_emp_final_complete %>%
      filter(
        sector != "Total",
        geo != "EU27",
        time <= year_chart
      ) %>%
      select(c("geo", "time", "sector", "energy_consumption", "employment")) %>%
      replace(is.na(.), 0) %>%
      mutate(missing =
               case_when((energy_consumption > 0 & employment > 0) | 
                           (energy_consumption == 0 & employment == 0) ~ 0,
                         TRUE ~ 1
               )) %>%
      select(-c("energy_consumption", "employment")) %>%
      group_by(geo, time) %>%
      summarize(missing = sum(missing)) %>%
      ggplot(aes(
        x = reorder(geo, desc(geo)),
        y = factor(time),
        fill = missing
      )) +
      coord_flip() +
      geom_tile() +
      theme_classic() +
      scale_fill_gradient(
        low = "white",
        high = "red",
        limits = c(0, 5),
        breaks = scales::pretty_breaks(n = 5)(0:5)
      ) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      labs(fill = "Missing sub-sectors") 
      #ggtitle("Completeness of coverage (energy and activity data) for European countries across years")
    
    filename <- "EU27_Figure19D.jpg"
    print(filename)
    
    jpeg(
      file = paste0(outputpath, filename),
      width = 2400,
      height = 3200,
      res = 300
    )
    
    print(p)
    
    dev.off()
    
    # Prepare the data
    EU_comparison <- economy_emp_final_full %>%
      filter(
        measure == "intensity",
        time %in% c(first_year_chart, last_year_chart),
        sector == "Total",
        geo != 'EU27'
      ) %>%
      select(-c(value_indexed, value_delta)) %>%
      arrange(value) %>%
      merge(eu_countries, by.x = 'geo', by.y = 'code') %>%
      select(-c('geo', 'label'))

    write.csv(EU_comparison,
      paste0(outputpath, "Part2_EU27.csv"),
      row.names = FALSE)
    
    # Rank the countries by intensity on last year
    country_ranked <- EU_comparison %>%
      filter(time == last_year_chart) %>%
      arrange(value) %>%
      pull(name)
  
  # EU intensity comparison chart
  
  p <-  EU_comparison %>%
    ggplot(aes(
      x = factor(name, levels = country_ranked),
      y = value,
      color = as.factor(time)
    )) +
    geom_point() +
    geom_line(aes(group = name), colour = "grey") +
    coord_flip() +
    theme_classic() +
    theme(
      axis.title.y = element_blank(),
      legend.title = element_blank(),
      legend.position = c(0.8, 0.2),
      legend.box = "horizontal"
    ) +
    scale_y_continuous(labels = scales::number) +
    ylab("Energy consumption per employee (TJ / Thousand employee)")
  # ggtitle(paste("Energy consumption per employee of economic sectors of European countries"))
  
  filename <- "EU27_Figure19C.jpg"
  
  print(filename)
  
  jpeg(
    file = paste0(outputpath, filename),
    width = 2400,
    height = 1600,
    res = 300
  )
  
  print(p)
  
  dev.off()
  }
}
