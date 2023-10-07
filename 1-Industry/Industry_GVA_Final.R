# FINAL ENERGY CONSUMPTION IN INDUSTRY

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

# Data preparation
industry_GVA_final <- function(first_year,
                               last_year,
                               country,
                               data_path,
                               chart_path) {
  
  # Define the list as the whole list
  country_list <- geo_codes
  
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
  
  # list of end uses sectors, used for the industry subset the energy balance (nrg_bal_c)
  NRG_IND_SECTORS <- c(
    "NRG_CM_E",
    "NRG_OIL_NG_E",
    "NRG_PR_E",
    "NRG_CO_E",
    "NRG_BF_E",
    "FC_IND_CON_E",
    "FC_IND_CPC_E",
    "FC_IND_FBT_E",
    "FC_IND_IS_E",
    "FC_IND_MAC_E",
    "FC_IND_MQ_E",
    "FC_IND_NFM_E",
    "FC_IND_NMM_E",
    "FC_IND_NSP_E",
    "FC_IND_PPP_E",
    "FC_IND_TE_E",
    "FC_IND_TL_E",
    "FC_IND_WP_E",
    "NRG_PF_E",
    "NRG_BKBPB_E",
    "NRG_CL_E",
    "NRG_GTL_E",
    "NRG_CPP_E",
    "NRG_NSP_E"
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
  
  # Other renewables
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
  
  # list of end uses sectors, used for the industry subset of the economic data (nama_10_a64)
  GVA_IND_SECTORS <- c(
    "F",
    "B",
    "C10-C12",
    "C13-C15",
    "C16",
    "C17",
    "C18",
    "C19",
    "C20",
    "C21",
    "C22",
    "C23",
    "C24",
    "C25",
    "C26",
    "C27",
    "C28",
    "C29",
    "C30",
    "C31_C32"
  )
  
  # list of end uses sectors, as they will be named in the LMDI results
  IDA_IND_SECTOR <- c(
    "Construction",
    "Mining and quarrying",
    #"Food, beverages and tobacco",
    "Food, bev. and tobacco",
    "Textile and leather",
    "Wood and wood products",
    "Paper, pulp and printing",
    #"Coke and refined petroleum products",
    "Coke and ref. pet. products",
    #"Chemical and petrochemical",
    "Chemical and petrochem.",
    "Non-metallic minerals",
    "Basic metals",
    "Machinery",
    "Transport equipment",
    "Other manufacturing"
  )
  
  # list of products, as they will be named in the charts
  IDA_IND_PROD <- c(
    "Coal",
    "Oil",
    "Gas",
    "Biofuels and renewable wastes",
    "Non-renewable wastes",
    #   "Nuclear",
    #   "Hydro",
    "Wind, solar, geothermal, etc.",
    "Heat",
    "Electricity"
  )
  
  # Colors
  
  ColorsIndex <- c(
    "Energy consumption" = "blue4",
    "Gross Value Added" = "red4",
    "Energy intensity" = "green4"
  )
  
  ColorsEffect <- c(
    "Activity" = "red4",
    "Intensity" = "green4",
    "Structure" = "purple4"
  )
  
  ColorsSector <- c(
    "Construction" = brewer.pal(12, "Set3")[1],
    "Mining and quarrying" = brewer.pal(12, "Set3")[2],
    #"Food, beverages and tobacco" = brewer.pal(12, "Set3")[3],
    "Food, bev. and tobacco" = brewer.pal(12, "Set3")[3],
    "Textile and leather" = brewer.pal(12, "Set3")[4],
    "Wood and wood products" = brewer.pal(12, "Set3")[5],
    "Paper, pulp and printing" = brewer.pal(12, "Set3")[6],
    #"Coke and refined petroleum products" = brewer.pal(12, "Set3")[7],
    "Coke and ref. pet. products" = brewer.pal(12, "Set3")[7],
    #"Chemical and petrochemical" = brewer.pal(12, "Set3")[8],
    "Chemical and petrochem." = brewer.pal(12, "Set3")[8],
    "Non-metallic minerals" = brewer.pal(12, "Set3")[9],
    "Basic metals" = brewer.pal(12, "Set3")[10],
    "Machinery" = brewer.pal(12, "Set3")[11],
    "Transport equipment" = brewer.pal(12, "Set3")[12],
    "Other manufacturing" = "grey"
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
  
  # Economic activity from the national account data (nama_10_a64)
  load(paste0(data_path, "/nama_10_a64.Rda"))
  
  # Energy consumption by fuel
  industry_energy_breakdown <-  nrg_bal_c %>%
    filter(
      geo %in% country_list,
      #from first year
      time >= first_year,
      # to last year
      time <= last_year,
      #take industry end uses
      nrg_bal %in% NRG_IND_SECTORS,
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
    mutate(product = factor(product, level = IDA_IND_PROD)) %>%
    group_by(geo, time) %>%
    mutate(share_energy_consumption = energy_consumption / sum(energy_consumption)) %>%
    ungroup()
  
  #energy consumption (and supply) from the energy balance (nrg_bal_c)
  industry_energy_final <- nrg_bal_c %>%
    filter(
      geo %in% country_list,
      #from first year
      time >= first_year,
      # to last year
      time <= last_year,
      #take industry end uses
      nrg_bal %in% NRG_IND_SECTORS,
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
      #basic metals
      FC_MBM = rowSums(select(., c("FC_IND_IS_E", 
                                   "NRG_CO_E", 
                                   "NRG_BF_E", 
                                   "FC_IND_NFM_E")), 
                       na.rm = TRUE),
      #mining and quarrying
      FC_MQ = rowSums(select(., c("FC_IND_MQ_E", 
                                  "NRG_CM_E", 
                                  "NRG_OIL_NG_E")), 
                      na.rm = TRUE),
      #other manufacturing
      FC_NSP = rowSums(select(., c("NRG_PF_E", 
                                   "NRG_BKBPB_E", 
                                   "NRG_CL_E", 
                                   "NRG_GTL_E", 
                                   "NRG_CPP_E", 
                                   "NRG_NSP_E", 
                                   "FC_IND_NSP_E")), 
                       na.rm = TRUE)
    ) %>%
    #keep only relevant columns
    select(
      -c(
        FC_IND_IS_E,
        NRG_CO_E,
        NRG_BF_E,
        FC_IND_NFM_E,
        FC_IND_MQ_E,
        NRG_CM_E,
        NRG_OIL_NG_E,
        NRG_PF_E,
        NRG_BKBPB_E,
        NRG_CL_E,
        NRG_GTL_E,
        NRG_CPP_E,
        NRG_NSP_E,
        FC_IND_NSP_E
      )
    ) %>%
    #rename to explicit names
    rename(
      "Construction" = "FC_IND_CON_E",
      "Mining and quarrying" = "FC_MQ",
      #"Food, beverages and tobacco" = "FC_IND_FBT_E",
      "Food, bev. and tobacco" = "FC_IND_FBT_E",
      "Textile and leather" = "FC_IND_TL_E",
      "Wood and wood products" = "FC_IND_WP_E",
      "Paper, pulp and printing" = "FC_IND_PPP_E",
      #"Coke and refined petroleum products" = "NRG_PR_E",
      "Coke and ref. pet. products" = "NRG_PR_E",
      #"Chemical and petrochemical" = "FC_IND_CPC_E",
      "Chemical and petrochem." = "FC_IND_CPC_E",
      "Non-metallic minerals" = "FC_IND_NMM_E",
      "Basic metals" = "FC_MBM",
      "Machinery" = "FC_IND_MAC_E",
      "Transport equipment" = "FC_IND_TE_E",
      "Other manufacturing" = "FC_NSP"
    ) %>%
    #reshape to long
    pivot_longer(
      cols = -c(geo, time),
      names_to = "sector",
      values_to = "energy_consumption"
    )
  
  #economic activity from the national account data (nama_10_a64)
  industry_GVA <- nama_10_a64 %>%
    filter(
      geo %in% country_list,
      #from first year
      time >= first_year,
      # to last year
      time <= last_year,
      #take industry sub sectors
      nace_r2 %in% GVA_IND_SECTORS,
      # Gross Value Added in Chain linked volumes (2015), million euro
      na_item == "B1G",
      unit == "CLV15_MEUR"
    ) %>%
    select(c("geo", "time", "nace_r2", "values")) %>%
    #reshape to wide
    pivot_wider(names_from = nace_r2, 
                values_from = values) %>%
    #aggregate
    mutate(
      #Paper
      "C17-C18" = rowSums(select(., c("C17", "C18")), na.rm = TRUE),
      #Chem and petchem
      "C20-C21" = rowSums(select(., c("C20", "C21")), na.rm = TRUE),
      #Non-metallic minerals
      "C22-C23" = rowSums(select(., c("C22", "C23")), na.rm = TRUE),
      #Machinery
      "C25-C28" = rowSums(select(., c("C25", "C26", "C27", "C28")), na.rm = TRUE),
      #Transport equipment
      "C29-C30" = rowSums(select(., c("C29", "C30")), na.rm = TRUE)
    ) %>%
    #keep only relevant columns
    select(-c(C17, C18, C20, C21, C22, C23, C25, C26, C27, C28, C29, C30)) %>%
    # Rename to explicit names
    rename(
      "Construction" = "F",
      "Mining and quarrying" = "B",
      #"Food, beverages and tobacco" = "C10-C12",
      "Food, bev. and tobacco" = "C10-C12",
      "Textile and leather" = "C13-C15",
      "Wood and wood products" = "C16",
      "Paper, pulp and printing" = "C17-C18",
      #"Coke and refined petroleum products" = "C19",
      "Coke and ref. pet. products" = "C19",
      #"Chemical and petrochemical" = "C20-C21",
      "Chemical and petrochem." = "C20-C21",
      "Non-metallic minerals" = "C22-C23",
      "Basic metals" = "C24",
      "Machinery" = "C25-C28",
      "Transport equipment" = "C29-C30",
      "Other manufacturing" = "C31_C32"
    ) %>%
    # Reshape to long
    pivot_longer(
      cols = -c(geo, time),
      names_to = "sector",
      values_to = "GVA"
    ) %>%  
    # correction 
    mutate(GVA = case_when(
      (geo == 'AT'&sector == 'Coke and ref. pet. products'&time == 2013) ~ GVA * 10,
      (geo == 'AT'&sector == 'Coke and ref. pet. products'&time == 2014) ~ GVA * 100,
      (geo == 'ES'&sector == 'Coke and ref. pet. products'&time == 2020) ~ - GVA,
      (geo == 'SE'&sector == 'Coke and ref. pet. products'&time == 2020) ~ - GVA,
      (geo == 'IT'&sector == 'Coke and ref. pet. products'&time == 2014) ~ - GVA,
      (geo == 'IT'&sector == 'Coke and ref. pet. products'&time == 2020) ~ - GVA,
      (geo == 'PT'&sector == 'Coke and ref. pet. products'&time == 2020) ~ - GVA,
      (geo == 'LV'&sector == 'Basic metals'&time == 2013) ~ - GVA,
      (geo == 'LV'&sector == 'Basic metals'&time == 2014) ~ - GVA,
      (geo == 'LV'&sector == 'Coke and ref. pet. products'&time == 2020) ~ - GVA,
      (geo == 'BG'&sector == 'Coke and ref. pet. products'&time == 2014) ~ - GVA,
      (geo == 'BG'&sector == 'Coke and ref. pet. products'&time == 2020) ~ - GVA,
      (geo == 'CZ'&sector == 'Coke and ref. pet. products'&time == 2021) ~ - GVA,
      TRUE ~ GVA
      ))

  print('AT 2013, 2014 Coke and ref. pet. products GVA correction')
  print('ES 2020 Coke and ref. pet. products GVA correction')
  print('SE 2020 Coke and ref. pet. products GVA correction')
  print('IT 2014, 2020 Coke and ref. pet. products GVA correction')
  print('PT 2020 Coke and ref. pet. products GVA correction')
  print('LV 2013, 2014 Basid metals GVA correction')
  print('LV 2020 Coke and ref. pet. products GVA correction')
  print('BG 2014, 2020 Coke and ref. pet. products GVA correction')
  print('CZ 2021 Coke and ref. pet. products GVA correction')
  
  # Joining datasets
  industry_GVA_final_complete <- full_join(industry_GVA,
                                  industry_energy_final,
                                  by = c("geo", "time", "sector")) %>%
    # correcting for missing GVA / Energy
    mutate(
      GVA = case_when(
        (GVA == 0 & energy_consumption > 0) ~ NA_real_,
        TRUE ~ GVA),
      energy_consumption = case_when(
        (energy_consumption == 0 & GVA > 0) ~ NA_real_,
        TRUE ~ energy_consumption),
      # intensity calculated here for the charts, will be recalculated later once the totals are included
      intensity = case_when(
        (GVA == 0 & energy_consumption > 0) ~ NA_real_,
        (GVA == 0 & energy_consumption == 0) ~ 0, 
        TRUE ~ energy_consumption / GVA)
    ) %>% 
    # For each country and each year
    group_by(geo, time) %>%
    mutate(
      # Calculate the total energy consumption and value added of the overall industry sector, as the sum of all subsectors
      total_energy_consumption = sum(energy_consumption, na.rm = TRUE),
      total_GVA = sum(GVA, na.rm = TRUE)
    ) %>%
    # For each country, each year and each subsector
    ungroup() %>%
    mutate(
      # Calculate the share of the subsector in the overall energy consumption and in the overall value added of the industry sector
      share_energy_consumption = energy_consumption / total_energy_consumption,
      share_GVA = GVA / total_GVA
    )
  
  # filter out sectors with incomplete data
  industry_GVA_final_filtered <- industry_GVA_final_complete %>% 
    filter(!(geo == 'EE'&sector == 'Coke and ref. pet. products'),
           !(geo == 'IE'&sector %in% c('Coke and ref. pet. products',
                                       'Chemical and petrochem.',
                                       'Transport equipment')),
           !(geo == 'LV'&sector %in% c('Coke and ref. pet. products')),
           !(geo == 'LT'&sector %in% c('Coke and ref. pet. products')),
           !(geo == 'LU'&sector %in% c('Food, bev. and tobacco',
                                       'Wood and wood products',
                                       'Basic metals',
                                       'Other manufacturing',
                                       'Paper, pulp and printing',
                                       'Non-metallic minerals',
                                       'Machinery',
                                       'Transport equipment')),
           !(geo == 'MT'&sector %in% c('Mining and quarrying',
                                       'Wood and wood products',
                                       'Basic metals',
                                       'Coke and ref. pet. products',
                                       'Non-metallic minerals',
                                       'Transport equipment',
                                       #"Chemical and petrochem.",
                                       #'Paper, pulp and printing',
                                       "Other manufacturing")),
           !(geo == 'SI'&sector %in% c('Coke and ref. pet. products')),
           !(geo == 'SE'&sector %in% c('Chemical and petrochem.')))
  
  print('EE full period Coke and ref. pet. products GVA removed (missing energy consumption)')        
  print('MT full period Mining and quarrying, Coke and ref. pet. products, Wood and wood products removed (missing GVA)')
  print('IE full period Coke and ref. pet. products and Chemical and petrochem. removed (missing GVA)')
  print('LV full period Coke and ref. pet. products removed (missing energy consumption)')
  print('LT full period Coke and ref. pet. products removed (missing GVA)')
  print('LU full period Food, bev. and tobacco, Wood and wood products, 
        Basic metals, Other manufacturing, Paper, pulp and printing, 
        Non-metallic minerals, Machinery, Transport equipment removed (missing GVA)')
  print('MT full period Mining and quarrying, Wood and wood products, 
        Basic metals, Coke and ref. pet. products, Non-metallic minerals, 
        Transport equipment, Other manufacturing removed (missing GVA)')
  print('MT full period Basic metals removed (missing energy consumption)')
  print('SI full period Coke and ref. pet. products removed (missing GVA)')
  print('SE full period Chemical and petrochem. removed (missing GVA)')
  
  industry_GVA_final_augmented <- industry_GVA_final_filtered %>% 
    # For each country and each year
    group_by(geo, time) %>%
    mutate(
      # Calculate the total energy consumption and value added of the overall industry sector, as the sum of all subsectors selected
      total_energy_consumption = sum(energy_consumption, na.rm = TRUE),
      total_GVA = sum(GVA, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    # For each country, each year and each subsector
    mutate(
      # Calculate the share of the subsector in the overall energy consumption and in the overall value added of the industry sector
      share_energy_consumption = energy_consumption / total_energy_consumption,
      share_GVA = GVA / total_GVA
    ) %>%
    # Remove the total columns, not required any longer
    select(-c(total_energy_consumption,
              total_GVA,
              intensity)) %>%
    ungroup()
  
  industry_GVA_final_total <- industry_GVA_final_augmented %>%
    group_by(geo, time) %>%
    summarize(GVA = sum(GVA, na.rm = TRUE),
              energy_consumption = sum(energy_consumption, na.rm = TRUE),
              # the sum of shares should be one, calculated here for checking
              share_GVA = sum(share_GVA, na.rm = TRUE),
              share_energy_consumption = sum(share_energy_consumption, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(sector = "Total")
  
  # Calculate the indexed and indexed indicators
  
  # Copy the dataframe to store all the values indexed on the base year
  industry_GVA_final_full <- industry_GVA_final_augmented %>%
    rbind(industry_GVA_final_total) %>%
    # calculate intensity again, to include the total intensity
    mutate(intensity = case_when(
      (GVA == 0 & energy_consumption > 0) ~ NA_real_,
      (GVA == 0 & energy_consumption == 0) ~ 0, 
      TRUE ~ energy_consumption / GVA)) %>% 
    pivot_longer(cols = -c(geo, time, sector), 
                 names_to = "measure", 
                 values_to = "value") %>%
    group_by(geo, sector, measure) %>%
    mutate(
      value_indexed = case_when(
         value[time == industry_GVA_base_year(country=.data[["geo"]], first_year = first_year)] == 0 ~ 0,
         is.na(value[time == industry_GVA_base_year(country=.data[["geo"]], first_year = first_year)]) ~ NA_real_,
         TRUE ~ value / value[time == industry_GVA_base_year(country=.data[["geo"]], first_year = first_year)]),
      value_delta = value - value[time == industry_GVA_base_year(country=.data[["geo"]], first_year = first_year)],
      time = as.integer(time)
    ) %>%
    ungroup()
  
  # Effects calculation
  
  # Calculate the effects using the LMDI formulas
  industry_GVA_final_LMDI <- industry_GVA_final_full %>%
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
      activity_log = ifelse(value_indexed_GVA == 0, 0, log(value_indexed_GVA)),
      structure_log = ifelse(value_indexed_share_GVA == 0, 0, log(value_indexed_share_GVA)),
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
    mutate(base_year = industry_GVA_base_year(country=.data[["geo"]], first_year = first_year)) %>%
    ungroup() %>%
    group_by(geo) %>%
    mutate(
      value_energy_consumption_total_baseline = value_energy_consumption[sector == "Total" & time == base_year]
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
             rowSums(select(., 
                            c("activity_effect", 
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
      industry_GVA_base_year(country=country_chart, first_year = first_year)
    last_year_chart <-
      industry_GVA_last_year(country = country_chart, final_year = last_year)

    
    # Country data
    industry_GVA_final_country_data <-
      industry_GVA_final_complete %>%
      filter(
        geo == country_chart,
        time <= year_chart
      ) %>%
      mutate(sector = factor(sector, levels = IDA_IND_SECTOR))
    
    # full data (after filtering)
    industry_GVA_final_subsector <- 
      industry_GVA_final_full %>%
      filter(geo == country_chart,
             time >= first_year_chart,
             time <= last_year_chart)
    
    # Breakdown of energy consumption by fuel
    industry_energy_breakdown_filtered <- 
      industry_energy_breakdown %>% 
      filter(geo == country_chart,
             !energy_consumption == 0)
    
    # Table used to provide figures in the text of the report
    table_industry_energy_breakdown_filtered <- 
      industry_energy_breakdown_filtered %>%
      mutate(
        energy_consumption = round(energy_consumption, 2),
        share_energy_consumption = round(share_energy_consumption, 3)
        )
    
    write.csv(table_industry_energy_breakdown_filtered,
              paste0(outputpath, "Part1_fuel.csv"),
              row.names = FALSE)
    
    # Table used to provide figures in the text of the report
    table_industry_GVA_final_country_data <- 
      industry_GVA_final_country_data %>%
      mutate(GVA = round(GVA, 2),
             energy_consumption = round(energy_consumption, 2),
             share_GVA = round(share_GVA, 2),
             share_energy_consumption = round(share_energy_consumption, 2))
    
    write.csv(
      table_industry_GVA_final_country_data,
      paste0(outputpath, "Part1_sector.csv"),
      row.names = FALSE)
    
    # Chart indexed variation of total industry
    
    industry_GVA_final_country_indexed <- 
      industry_GVA_final_subsector %>%
      filter(sector == "Total") %>%
      select(-c(value, value_delta)) %>%
      pivot_wider(
        names_from = measure, 
        values_from = value_indexed) %>%
      select(c(
        geo, 
        time, 
        intensity, 
        energy_consumption, 
        GVA)) %>%
      rename(
        "Energy intensity" = "intensity",
        "Energy consumption" = "energy_consumption",
        "Gross Value Added" = "GVA"
      )
    
    year <- as.Date(as.character(industry_GVA_final_country_indexed$time), "%Y")
    
    p <- industry_GVA_final_country_indexed %>%
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
    ylab(paste("Index (", industry_GVA_base_year(country=country_chart, first_year = first_year), "=1)"))
    #ggtitle(paste("Indexed indicators for",country_chart,"'s total industry energy consumption, \ngross value added and energy intensity variation, \nall years related to",as.character(base_year)))
    
    filename <- paste0(country_chart, "_Figure05B.jpg")
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
      as.Date(as.character(industry_energy_breakdown_filtered$time),
              "%Y")
    
    p <- industry_energy_breakdown_filtered %>%
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
    #ggtitle(paste("Industry energy consumption by fuel for",country_name))
    
    filename <- paste0(country_chart, "_Figure02.jpg")
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

    p <- industry_energy_breakdown_filtered %>%
      filter(time %in% c(first_year, last_year)) %>%
      ggplot(aes(
        x = factor(time), 
        y = share_energy_consumption, 
        fill = product
        )) +
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
      geom_text(
        aes(label = paste0(round(share_energy_consumption*100, 0), "%")), 
        position = position_stack(vjust = 0.5)) +
      ylab(paste("Share in final energy consumption"))
    # ggtitle(paste("Industry energy consumption by fuel for",country_name))
    
    filename <- paste0(country_chart, "_Figure02B.jpg")
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
  
    year <- as.Date(as.character(industry_GVA_final_country_data$time), "%Y")
    
    p <- industry_GVA_final_country_data %>%
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
    #ggtitle(paste("Industry energy consumption by subsector for",country_name))
    
    filename <- paste0(country_chart, "_Figure03.jpg")
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
    
    p <- industry_GVA_final_country_data %>%
      filter(time %in% c(first_year, last_year)) %>%
      mutate(sector = factor(sector, levels = IDA_IND_SECTOR))  %>%
      ggplot(aes(x = factor(time), 
                 y = share_energy_consumption, 
                 fill = sector)) +
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
    # ggtitle(paste("Industry energy consumption by subsector for",country_name))
    
    filename <- paste0(country_chart, "_Figure03B.jpg")
    print(filename)
    
    jpeg(
      file = paste0(outputpath, filename),
      width = 2400,
      height = 2400,
      res = 300
    )
    
    print(p)
    
    dev.off()
    
    # GVA by subsector
    
    p <- industry_GVA_final_country_data %>%
      cbind(year) %>%
      select(-time) %>%
      mutate(year = lubridate::year(year)) %>%
      ggplot(aes(x = year, y = GVA / 1000)) +
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
      ylab(paste("Gross Value Added (Billion EUR)"))
    #ggtitle(paste("Industry gross value added by subsector for",country_name))
    
    filename <- paste0(country_chart, "_Figure04.jpg")
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
    
    p <- industry_GVA_final_country_data %>%
      filter(time %in% c(first_year, last_year)) %>%
      ggplot(aes(x = factor(time), 
                 y = share_GVA, 
                 fill = sector
                 )) +
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
      geom_text(aes(label = paste0(round(share_GVA*100, 0), "%")), 
                position = position_stack(vjust = 0.5)) +
      ylab(paste("Share of Gross Value Added"))
    # ggtitle(paste("Industry gross value added by subsector for",country_name))
    
    filename <- paste0(country_chart, "_Figure04B.jpg")
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
      as.Date(as.character(industry_GVA_final_subsector$time), "%Y")
    
    p <- industry_GVA_final_subsector %>%
      cbind(year) %>%
      select(-time) %>%
      filter(sector != "Total",
             measure == "intensity") %>%
      mutate(sector = factor(sector, levels = IDA_IND_SECTOR)) %>%
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
      ylab(paste("Index (", industry_GVA_base_year(country=country_chart, first_year = first_year), "=1)"))
    #ggtitle(paste("Indexed variation for",country_name,"'s energy intensity in industry subsectors, \nall years related to",as.character(base_year)))
    
    filename <- paste0(country_chart, "_Figure05C.jpg")
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
    industry_GVA_final_effects <- industry_GVA_final_LMDI %>%
      filter(geo == country_chart,
             time <= year_chart,
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
    
    industry_GVA_final_results <- industry_GVA_final_LMDI %>%
      filter(geo == country_chart,
             time <= year_chart) %>%
      pivot_longer(
        cols = -c(geo, time),
        names_to = "measure",
        values_to = "value"
      ) %>%
      filter(measure == "energy_consumption_var_obs")
    
    #Plot the simple effect as bar chart
    p <- ggplot(data = industry_GVA_final_effects,
                aes(x = factor(time),
                    y = value / 1000)) +
      geom_bar(aes(fill = Effect),
               stat = "identity")  +
      scale_fill_manual(values = ColorsEffect, limits = force) +
      geom_point(data = industry_GVA_final_results,
                 aes(y = value / 1000),
                 size = 3) +
      theme_classic() +
      theme(axis.title.x = element_blank(),
            text = element_text(size = 15)) +
      scale_y_continuous(labels = scales::number) +
      ylab("Energy consumption variation (PJ)")
    # ggtitle(paste("Decompostion analysis of",country_name,"'s industry energy consumption variation, \n  all years related to",as.character(base_year)))
    
    filename <- paste0(country_chart, "_Figure05D.jpg")
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
    levels_waterfall <- c(Base_label,
                          "Activity",
                          "Structure",
                          "Intensity",
                          Result_label)
    
    #prepare data for the waterfall chart (see plotly waterfall for explanations)
    industry_GVA_final_Waterfall_data <- industry_GVA_final_LMDI %>%
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
             text = paste(as.character(round(y,2)), "TJ", sep = " "),
             measure = case_when(
               x == !!Result_label ~ "total",
               TRUE ~ "relative")
             )
    
    write.csv(industry_GVA_final_Waterfall_data,
              paste0(outputpath, "Part1_waterfall.csv"),
              row.names = FALSE)
    
    p <- industry_GVA_final_Waterfall_data %>%
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
    
    filename <- paste0(country_chart, "_Figure05.jpg")
    
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
    
    # Prepare data for the intensity effect chart
    industry_GVA_final_intensity_effect <-
      industry_GVA_final_LMDI %>%
      filter(geo == country_chart,
             time <= last_year,
             time >= first_year) %>%
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
    
    write.csv(industry_GVA_final_intensity_effect,
              paste0(outputpath, "Part1_intensity_effect.csv"),
              row.names = FALSE)
    
    #Plot the intensity effect as area chart
    p <- industry_GVA_final_intensity_effect %>%
      ggplot() +
      geom_bar(
        data = (industry_GVA_final_intensity_effect %>% 
                filter(measure == 'Actual energy consumption')),
        aes(y = value / 1000, 
            x = time,
            fill = measure),
        stat = "identity",
        alpha = 0.5) +
      scale_fill_manual(values = c("Actual energy consumption" = "blue4")) +
      geom_point(
        data = (
          industry_GVA_final_intensity_effect %>%
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
    #ggtitle(paste("Actual energy consumption in the industry vs theoretical \n(without energy intensity improvements) for",country_name))
    
    filename <- paste0(country_chart, "_Figure06.jpg")
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
    
    # Prepare data for the intensity comparison chart
    industry_GVA_final_intensity_comparison_total <-
      industry_GVA_final_full %>%
      filter(measure == "intensity",
             time <= year_chart,
             sector == "Total") %>%
      select(-c(value_indexed, value_delta)) %>%
      pivot_wider(names_from = geo, values_from = value)
    
    min_EU <-
      apply(
        select_if(
          industry_GVA_final_intensity_comparison_total[, -1],
          is.numeric
        ),
        1,
        min,
        na.rm = TRUE
      )
    max_EU <-
      apply(
        select_if(
          industry_GVA_final_intensity_comparison_total[, -1],
          is.numeric
        ),
        1,
        max,
        na.rm = TRUE
      )
    avg_EU <-
      apply(
        select_if(
          industry_GVA_final_intensity_comparison_total[, -1],
          is.numeric
        ),
        1,
        mean,
        na.rm = TRUE
      )
    year <-
      as.Date(as.character(industry_GVA_final_intensity_comparison_total$time),
              "%Y")
    
    industry_GVA_final_intensity_comparison_total <-
      industry_GVA_final_intensity_comparison_total %>%
      cbind(min_EU) %>%
      cbind(max_EU) %>%
      cbind(avg_EU) %>%
      cbind(year) %>%
      select(c(year,!!country_chart, min_EU, max_EU)) %>%
      mutate(year = lubridate::year(year))
    
    #Plot the intensity comparison as line chart
    p <- industry_GVA_final_intensity_comparison_total %>%
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
      ylab("Energy intensity (MJ / EUR)") + 
      scale_x_continuous(breaks = c(first_year,round((first_year+last_year)/2), last_year))
    #  ggtitle(paste("Energy intensity in", country_name, "'s manufacturing industry compared to \naverage European countries"))
    
    filename <- paste0(country_chart, "_Figure07.jpg")
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
    
    # Prepare data for the intensity comparison chart
    industry_GVA_final_intensity_comparison_sector <-
      industry_GVA_final_complete %>%
      mutate(sector = factor(sector, levels = IDA_IND_SECTOR)) %>%
      filter(time <= year_chart) %>%
      select(-c(GVA, energy_consumption, 
                total_energy_consumption, total_GVA, 
                share_energy_consumption, share_GVA)) %>%
      pivot_wider(names_from = geo, values_from = intensity)
    
    min_EU <-
      apply(
        select_if(
          industry_GVA_final_intensity_comparison_sector[, -1],
          is.numeric
        ),
        1,
        min,
        na.rm = TRUE
      )
    max_EU <-
      apply(
        select_if(
          industry_GVA_final_intensity_comparison_sector[, -1],
          is.numeric
        ),
        1,
        max,
        na.rm = TRUE
      )
    avg_EU <-
      apply(
        select_if(
          industry_GVA_final_intensity_comparison_sector[, -1],
          is.numeric
        ),
        1,
        mean,
        na.rm = TRUE
      )
    
    
    year <-
      as.Date(as.character(industry_GVA_final_intensity_comparison_sector$time), "%Y")
    
    industry_GVA_final_intensity_comparison_sector <-
      industry_GVA_final_intensity_comparison_sector %>%
      cbind(min_EU) %>%
      cbind(max_EU) %>%
      cbind(avg_EU) %>%
      cbind(year) %>%
      select(c(sector, year,!!country_chart, min_EU, max_EU)) %>%
      mutate(year = lubridate::year(year),
             sector = factor(sector, levels = IDA_IND_SECTOR))
    
    #Plot the intensity comparison as line chart
    p <- industry_GVA_final_intensity_comparison_sector %>%
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
        legend.position = c(0.75, 0.05),
        legend.box = "horizontal",
        text = element_text(size = 15)
      ) +
      #scale_y_continuous(labels = scales::number) +
      expand_limits(y = 0) +
      scale_y_continuous(breaks = scales::breaks_extended(Q = c(0, 1, 2, 3))) +
      ylab("Energy intensity (MJ / EUR)") +
      scale_x_continuous(breaks = c(first_year,round((first_year+last_year)/2), last_year)) + 
      #ggtitle(paste("Energy intensity in", country_name, "'s manufacturing industry compared to \naverage European countries")) +
      facet_wrap( ~ sector, 
                  scales = "free", 
                  ncol = 3)
    
    filename <- paste0(country_chart, "_Figure08.jpg")
    print(filename)
    
    jpeg(
      file = paste0(outputpath, filename),
      width = 2400,
      height = 3200,
      res = 300
    )
    
    print(p)
    
    dev.off()
  }
  
  if (country == "EU27") {
    
    outputpath <- paste0(chart_path, "/EU27/")
    
    # Data coverage chart
    p <- industry_GVA_final_complete %>%
      filter(
        sector != "Total",
        geo != "EU27",
        time <= year_chart
        ) %>%
      select(c("geo", "time", "sector","energy_consumption", "GVA")) %>%
      replace(is.na(.), 0) %>%
      mutate(missing =
               case_when((energy_consumption > 0 & GVA > 0) |
                           (energy_consumption == 0 & GVA == 0) ~ 0,
                         TRUE ~ 1
               )) %>%
      select(-c("energy_consumption", "GVA")) %>%
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
        limits = c(0, 13),
        breaks = scales::pretty_breaks(n = 4)(0:13)
      ) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      labs(fill = "Missing sub-sectors")
      #ggtitle("Completeness of coverage (energy and activity data) for European countries")
    
    filename <- "EU27_Figure08C.jpg"
    
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
    EU_comparison <- industry_GVA_final_full %>%
      filter(
        measure == "intensity",
        time %in% c(first_year_chart, last_year_chart),
        sector == "Total",
        geo != 'EU27'
      ) %>%
      select(-c(value_indexed, value_delta)) %>%
      merge(eu_countries, by.x = 'geo', by.y = 'code') %>%
      select(-c('geo', 'label'))

    write.csv(EU_comparison,
          paste0(outputpath, "Part1_EU27.csv"),
          row.names = FALSE)

    
    # Rank the countries by intensity on last year
    country_ranked <- EU_comparison %>%
      filter(time == last_year_chart) %>%
      arrange(value) %>%
      pull(name)
    
    # Output charts
    p <- EU_comparison %>%
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
      guides(fill=guide_legend(ncol=3)) + 
      scale_y_continuous(labels = scales::number) +
      ylab("Energy intensity (TJ / million EUR)")
    #ggtitle(paste("Energy intensity of manufacturing industry of European countries"))
    
    filename <- "EU27_Figure08B.jpg"
    
    print(filename)
    
    jpeg(
      file = paste0(outputpath, filename),
      width = 2400,
      height = 2400,
      res = 300
    )
    
    print(p)
    
    dev.off()
  }
}
