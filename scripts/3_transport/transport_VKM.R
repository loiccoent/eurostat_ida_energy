# FINAL ENERGY CONSUMPTION IN INDUSTRY

transport_base_year <- function(country, first_year){
   case_when(
     country == "BE" ~ 2013,
        country == "DE" ~ 2013,
        country == "DK" ~ 2013,
        country == "EE" ~ 2013,
        country == "HU" ~ 2013,
        country == "IE" ~ 2013,
        country == "MT" ~ 2013,
        country == "NL" ~ 2013,
        country == "RO" ~ 2011,
        country == "SE" ~ 2013,
        country == "SI" ~ 2015,
        TRUE ~ first_year)
}

transport_last_year <- function(country, final_year){
   case_when(
     country == "AT" ~ 2017,
        country == "BE" ~ 2015,
        country == "CY" ~ 2011,
        country == "PL" ~ 2015,
        #country == "LU" ~ 2019,
        country == "FI" ~ 2020,
        TRUE ~ final_year)
}

# Data preparation
transport_final <- function(first_year,
                            last_year,
                            country,
                            data_path,
                            chart_path) {
  # Define the list as the whole list
  country_list <- geo_codes

  print('BE first year set to 2013')
  print('DE first year set to 2013')
  print('DK first year set to 2013')
  print('EE first year set to 2013')
  print('HU first year set to 2013')
  print('IE first year set to 2013')
  print('MT first year set to 2013')
  print('NL first year set to 2013')
  print('SE first year set to 2013')
  print('RO first year set to 2011')
  print('SI first year set to 2015')
  print('AT last year set to 2017')
  print('BE last year set to 2015')
  print('CY last year set to 2011')
  print('PL last year set to 2015')
  print('FI last year set to 2020')

  #list of transport mode, used for the transport subset of the energy balance (nrg_bal_c)
  NRG_TRA_MODES <- c("FC_TRA_RAIL_E",
                     "FC_TRA_ROAD_E",
                     "FC_TRA_DNAVI_E")

  #list of end uses modes, as they will be named in the LMDI results
  IDA_TRA_MODES <- c("Road",
                     "Rail",
                     "Navigation")

  # Coal, manufactured gases, peat and peat products
  COAL_PRODS <- c(
    "C0110",
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
    "P1200"
  )

  # Gasoline
  GASOLINE_PRODS <- c("O4651",
                     "O4652XR5210B",
                     "O4653")

  # Diesel
  DIESEL_PRODS <- c("O4671XR5220B")

  # Kerosene
  KEROSENE_PRODS <- c("O4661XR5230B",
                     "O4669")

  # LPG
  LPG_PRODS <- c("O4630")

  # Oil, petroleum products, oil shale and oil sands
  OTHER_OIL_PRODS <- c(
    "O4100_TOT",
    "O4200",
    "O4300",
    "O4400X4410",
    "O4500",
    "O4610",
    "O4620",
    "O4640",
    "O4680",
    "O4691",
    "O4692",
    "O4693",
    "O4694",
    "O4695",
    "O4699",
    "S2000"
  )

  # Bio-Gasoline
  BIOGASOLINE_PRODS <- c("R5210P",
                        "R5210B")

  # Bio-Diesel
  BIODIESEL_PRODS <- c("R5220P",
                      "R5220B")

  # Other bioliquids
  OTHER_BIOLIQUIDS_PRODS <- c("R5230P",
                             "R5230B",
                             "R5290")
  # Other biogas
  BIOGAS_PRODS <- c("R5300")

  # Other Biofuels and wastes
  OTH_PRODS <- c("R5110-5150_W6000RI",
                "R5160",
                "W6210",
                "W6100",
                "W6220")

  #list of products, used for the subset of the energy balance (nrg_bal_c)
  TRA_PRODS <- c(
    COAL_PRODS,
    GASOLINE_PRODS,
    DIESEL_PRODS,
    KEROSENE_PRODS,
    LPG_PRODS,
    OTHER_OIL_PRODS,
    "G3000",
    BIOGASOLINE_PRODS,
    BIODIESEL_PRODS,
    OTHER_BIOLIQUIDS_PRODS,
    BIOGAS_PRODS,
    OTH_PRODS,
    "E7000"
  )

  #list of products, as they will be named in the charts
  IDA_TRA_PROD <- c(
    "Coal",
    "Gasoline",
    "Biogasoline",
    "Diesel",
    "Biodiesel",
    "LPG",
    "Kerosene",
    "Other oil products",
    "Other liquid biofuels",
    "Gas",
    "Biogas",
    "Solid biofuels and wastes",
    "Electricity"
  )

  # Colors

  ColorsIndex <- c(
    "Energy consumption" = "blue4",
    "Traffic" = "red4",
    "Energy intensity" = "green4"
  )

  ColorsEffect <- c(
    "Activity" = "red4",
    "Intensity" = "green4",
    "Structure" = "purple4"
  )

  ColorsMode <- c(
    "Road" = brewer.pal(3, "Set3")[1],
    "Rail" = brewer.pal(3, "Set3")[2],
    "Navigation" = brewer.pal(3, "Set3")[3]
  )

  ColorsProduct <- c(
    "Coal" = brewer.pal(12, "Set3")[10],
    "Gasoline" = brewer.pal(12, "Paired")[1],
    "Biogasoline" = brewer.pal(12, "Paired")[2],
    "Diesel" = brewer.pal(12, "Paired")[7],
    "Biodiesel" = brewer.pal(12, "Paired")[8],
    "LPG" = brewer.pal(12, "Paired")[9],
    "Kerosene" = brewer.pal(12, "Paired")[10],
    "Other oil products" = brewer.pal(12, "Paired")[11],
    "Other liquid biofuels" = brewer.pal(12, "Paired")[12],
    "Gas" = brewer.pal(12, "Set3")[8],
    "Biogas" = brewer.pal(12, "Paired")[6],
    "Solid biofuels and wastes" = brewer.pal(12, "Paired")[4],
    #"Nuclear" = brewer.pal(10, "Set3")[2],
    #"Hydro" = brewer.pal(10, "Set3")[5],
    #"Wind, solar, geothermal, etc." = brewer.pal(10, "Set3")[1],
    #"Heat" = brewer.pal(10, "Set3")[4],
    "Electricity" = brewer.pal(12, "Set3")[3]
  )

  # DATA PREPARATION

  # Energy consumption (and supply) from the energy balance (nrg_bal_c)
  load(paste0(data_path, "/nrg_bal_c.Rda"))

  # Vehicle kilometer from road transport data (road_tf_vehmov)
  load(paste0(data_path, "/road_tf_vehmov.Rda"))

  # Vehicle kilometer from rail transport data (rail_tf_trainmv)
  load(paste0(data_path, "/rail_tf_trainmv.Rda"))

  # Vehicle kilometer from water transport data (iww_tf_vetf)
  load(paste0(data_path, "/iww_tf_vetf.Rda"))

  # Energy consumption by fuel
  transport_energy_breakdown <-  nrg_bal_c %>%
    filter(
      geo %in% country_list,
      #from first year
      time >= first_year,
      # to last year
      time <= last_year,
      #take industry end uses
      nrg_bal %in% NRG_TRA_MODES,
      #work with total energy consumption, in TJ
      siec %in% TRA_PRODS,
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
      CPS = rowSums(select(., all_of(COAL_PRODS)), na.rm = TRUE),
      # Gasoline
      GASOLINE = rowSums(select(.,  all_of(GASOLINE_PRODS)), na.rm = TRUE),
      # Biogasoline
      BIOGASOLINE = rowSums(select(.,  all_of(BIOGASOLINE_PRODS)), na.rm = TRUE),
      # Diesel
      DIESEL = rowSums(select(.,  all_of(DIESEL_PRODS)), na.rm = TRUE),
      # Biodiesel
      BIODIESEL = rowSums(select(.,  all_of(BIODIESEL_PRODS)), na.rm = TRUE),
      # LPG
      LPG = rowSums(select(.,  all_of(LPG_PRODS)), na.rm = TRUE),
      # Kerosene
      KEROSENE = rowSums(select(.,  all_of(KEROSENE_PRODS)), na.rm = TRUE),
      # Other oil
      OTHER_OIL = rowSums(select(.,  all_of(OTHER_OIL_PRODS)), na.rm = TRUE),
      # Other bioliquids
      OTHER_BIOLIQUIDS = rowSums(select(.,  all_of(OTHER_BIOLIQUIDS_PRODS)), na.rm = TRUE),
      # Biogases
      BIOGAS = rowSums(select(.,  all_of(BIOGAS_PRODS)), na.rm = TRUE),
      # Solid biofuels and wastes
      OTH = rowSums(select(., all_of(OTH_PRODS)), na.rm = TRUE)
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
        W6220
      )
    ) %>%
    #rename to explicit names
    rename(
      "Coal" = "CPS",
      "Gasoline" = "GASOLINE",
      "Biogasoline" = "BIOGASOLINE",
      "Diesel" = "DIESEL",
      "Biodiesel" = "BIODIESEL",
      "LPG" = "LPG",
      "Kerosene" = "KEROSENE",
      "Other oil products" = "OTHER_OIL",
      "Other liquid biofuels" = "OTHER_BIOLIQUIDS",
      "Gas" = "G3000",
      "Biogas" = "BIOGAS",
      "Solid biofuels and wastes" = "OTH",
      "Electricity" = "E7000"
    ) %>%
    #reshape to long
    pivot_longer(
      cols = -c(geo, time),
      names_to = "product",
      values_to = "energy_consumption"
    ) %>%
    mutate(product = factor(product, level = IDA_TRA_PROD)) %>%
    group_by(geo, time) %>%
    mutate(share_energy_consumption = energy_consumption / sum(energy_consumption)) %>%
    ungroup()

  #energy consumption (and supply) from the energy balance (nrg_bal_c)
  transport_energy <- nrg_bal_c %>%
    filter(
      geo %in% country_list,
      #from first year
      time >= first_year,
      # to last year
      time <= last_year,
      #take transport end uses
      nrg_bal %in% NRG_TRA_MODES,
      #work with total energy consumption, in TJ
      siec == "TOTAL",
      unit == "TJ"
    )  %>%
    #keep only relevant columns
    select(-c(siec, unit)) %>%
    #convert all figures to vkm
    rename(energy = values) %>%
    #reshape to wide (mode in columns)
    pivot_wider(names_from = nrg_bal,
                values_from = energy) %>%
    #rename
    rename(Road = FC_TRA_ROAD_E,
           Rail = FC_TRA_RAIL_E,
           Navigation = FC_TRA_DNAVI_E) %>%
    pivot_longer(
      cols = c(Road, Rail, Navigation),
      names_to = "mode",
      values_to = "energy_consumption"
    )

  # prepare road traffic data}

  #subset the road traffic data from road_tf_vehmov to keep only total vehicle transport in VKM, for EU28 countries from 2010.
  traffic_road <- road_tf_vehmov %>%
    filter(
      #take only EU countries
      geo %in% country_list,
      #from first year
      time >= first_year,
      # to last year
      time <= last_year,
      #work with total road vehicles on national territory (goods + passenger), in million VKM
      regisveh %in% c("D001", "D002"),
      vehicle == "TOTAL",
      unit == "MIO_VKM"
    ) %>%
    pivot_wider(names_from = regisveh,
                values_from = values) %>%
    mutate(values = ifelse(is.na(D001), D002, D001)) %>%
    select(-c("D001", "D002")) %>%
    #convert all figures to vkm, add mode
    mutate(VKM = values * 1000000,
           mode = "Road") %>%
    #keep only relevant columns
    select(-c(unit, vehicle, values)) %>%
    # correction
    mutate(VKM = case_when((geo == "BG") &
                             (time == 2010) ~ 1301900000,
                           TRUE ~ VKM))

  print('BG 2010 road traffic correction')

  # prepare rail traffic data}

  #subset the rail traffic data from rail_tf_trainmv to keep only total vehicle transport in VKM, for EU28 countries from 2010.
  traffic_rail <- rail_tf_trainmv %>%
    filter(
      #take only EU countries
      geo %in% country_list,
      #from first year
      time >= first_year,
      # to last year
      time <= last_year,
      #work with total trains (goods + passenger), in thousand VKM
      train == "TOTAL",
      unit == "THS_TRKM"
    ) %>%
    #convert all figures to vkm, add mode
    mutate(VKM = values * 1000,
           mode = "Rail") %>%
    #keep only relevant columns
    select(-c(unit, train, values))

  # prepare vessel traffic data}

  #subset the vessel traffic data from iww_tf_vetf to keep only national transport in VKM, for EU28 countries from 2010.
  traffic_water <- iww_tf_vetf %>%
    filter(
      #take only EU countries
      geo %in% country_list,
      #from first year
      time >= first_year,
      # to last year
      time <= last_year,
      #work with total (loaded and empty) and transport from all nationalities, in thousand VKM
      tra_cov == "TOTAL",
      loadstat == "TOTAL",
      unit == "THS_VESKM"
    ) %>%
    #convert all figures to vkm, add mode
    mutate(VKM = values * 1000,
           mode = "Navigation") %>%
    #keep only relevant columns
    select(-c(unit, tra_cov, loadstat, values))

  # joining datasets
  traffic <- traffic_road %>%
    bind_rows(traffic_rail) %>%
    bind_rows(traffic_water)

  transport_complete <- full_join(transport_energy,
                         traffic,
                         by = c("geo", "time", "mode")) %>%
  # correcting for missing VKM / Energy
  mutate(
    VKM = case_when(
      (VKM == 0 & energy_consumption > 0) ~ NA_real_,
      TRUE ~ VKM),
    energy_consumption = case_when(
      (energy_consumption == 0 & VKM > 0) ~ NA_real_,
      TRUE ~ energy_consumption),
    # intensity calculated here for the charts, will be recalculated later once the totals are included
    intensity = case_when(
      (VKM == 0 & energy_consumption > 0) ~ NA_real_,
      (VKM == 0 & energy_consumption == 0) ~ 0,
      TRUE ~ energy_consumption / VKM)
      ) %>%
    #for each country and each year
    group_by(geo, time) %>%
    mutate(
      #calculate the total energy consumption and VKM of the overall transport sector, as the sum of all mode selected
      total_energy_consumption = sum(energy_consumption, na.rm = TRUE),
      total_VKM = sum(VKM, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    #for each country, each year and each mode
    mutate(
      #calculate the share of the subsecto in the overall energy consumption and in the overall traffic
      share_energy_consumption = energy_consumption / total_energy_consumption,
      share_VKM = VKM / total_VKM
    )

  # filter out sectors with incomplete data
  transport_filtered <- transport_complete

  transport_augmented <- transport_filtered %>%
    # For each country and each year
    group_by(geo, time) %>%
    mutate(
      # Calculate the total energy consumption and value added of the overall industry sector, as the sum of all subsectors selected
      total_energy_consumption = sum(energy_consumption, na.rm = TRUE),
      total_VKM = sum(VKM, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    # For each country, each year and each subsector
    mutate(
      # Calculate the share of the subsector in the overall energy consumption and in the overall value added of the industry sector
      share_energy_consumption = energy_consumption / total_energy_consumption,
      share_VKM = VKM / total_VKM
    ) %>%
    #remove the total columns, not required any longer
    select(-c(total_energy_consumption,
              total_VKM,
              intensity)) %>%
    ungroup()

  transport_total <- transport_augmented %>%
    group_by(geo, time) %>%
    summarize(VKM = sum(VKM, na.rm = TRUE),
              energy_consumption = sum(energy_consumption, na.rm = TRUE),
              # the sum of shares should be one, calculated here for checking
              share_VKM = sum(share_VKM, na.rm = TRUE),
              share_energy_consumption = sum(share_energy_consumption, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(mode = "Total")

  # calculate the indexed and differenced indicators
  transport_full <- transport_augmented %>%
    rbind(transport_total) %>%
    # calculate intensity again, to include the total intensity
    mutate(intensity = case_when(
      (VKM == 0 & energy_consumption > 0) ~ NA_real_,
      (VKM == 0 & energy_consumption == 0) ~ 0,
      TRUE ~ energy_consumption / VKM)) %>%
    pivot_longer(cols = -c(geo, time, mode),
                 names_to = "measure",
                 values_to = "value") %>%
    group_by(geo, mode, measure) %>%
    mutate(
      value_indexed = value / value[time == transport_base_year(country=.data[["geo"]], first_year = first_year)],
      value_delta = value - value[time == transport_base_year(country=.data[["geo"]], first_year = first_year)],
      time = as.integer(time)
    ) %>%
    ungroup()

  # Effects calculation

  # Calculate the effects using the LMDI formulas
  transport_LMDI <- transport_full %>%
    # Reshape to wide (moving all measures calculated in Value, index and delta, all in separate columns)
    pivot_wider(
      names_from = measure,
      values_from = c(value, value_indexed, value_delta)
    ) %>%
    # Calculate the effects
    mutate(
      #the weighting factor links the effect calculated on the indexed variation to the total energy consumption variation
      weighting_factor = ifelse(
        value_delta_energy_consumption == 0,
        value_energy_consumption,
        value_delta_energy_consumption / log(value_indexed_energy_consumption)
      ),
      # Apply natural logarithm to the indexed values for each mode
      activity_log = ifelse(value_indexed_VKM == 0, 0, log(value_indexed_VKM)),
      structure_log = ifelse(value_indexed_share_VKM == 0, 0, log(value_indexed_share_VKM)),
      intensity_log = ifelse(value_indexed_intensity == 0, 0, log(value_indexed_intensity))
    ) %>%
    # Keep only the relevant columns
    select(
      geo,
      time,
      mode,
      weighting_factor,
      value_energy_consumption,
      value_delta_energy_consumption,
      activity_log,
      structure_log,
      intensity_log
    ) %>%
    # The baseline figures need to be expanded across all mode, and across all years
    rowwise() %>%
    mutate(base_year = transport_base_year(country=.data[["geo"]], first_year = first_year)) %>%
    ungroup() %>%
    group_by(geo) %>%
    mutate(value_energy_consumption_total_baseline = value_energy_consumption[mode == "Total" &
                                                                                time == base_year]) %>%
    ungroup() %>%
    # Similarly, the figures calculated for the total mode and the end figures need to be expanded across all modes
    group_by(geo, time) %>%
    mutate(
      activity_log_total = activity_log[mode == "Total"],
      value_delta_energy_consumption_total = value_delta_energy_consumption[mode == "Total"],
      value_energy_consumption_total_end = value_energy_consumption[mode == "Total"]
    ) %>%
    ungroup() %>%
    # Now the total mode is not required any longer
    filter(mode != "Total") %>%
    # Multiply the weighting factor * log(indexed mode), or weighting factor * log(indexed total modes)
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
    # Now the figures calculated at mode level need to be aggregated
    group_by(geo, time) %>%
    # The aggregation is performed differently:
    summarize(
      # Either by summing all mode
      activity_effect = sum(ACT, na.rm = TRUE),
      structural_effect = sum(STR, na.rm = TRUE),
      intensity_effect = sum(INT, na.rm = TRUE),
      # By keeping the mean figure when only one exist across all modes
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

    # Skipped because only data for 2011
    if (country_chart == "CY") next

    # Long name for the country
    country_name <- filter(eu27, code == country_chart)$name
    # Output charts
    outputpath <-
      paste0(chart_path, "/", country_chart, "/")
    # first and last year shown in charts
    first_year_chart <-
      transport_base_year(country=country_chart, first_year = first_year)
    last_year_chart <-
      transport_last_year(country = country_chart, final_year = last_year)

    # Country data
    transport_country_data <-
      transport_complete %>%
      filter(
        geo == country_chart,
        time <= last_year
      ) %>%
      mutate(mode = factor(mode, levels = IDA_TRA_MODES))

    # full data (after filtering)
    transport_mode <-
    transport_full %>%
      filter(geo == country_chart,
             time >= first_year_chart,
             time <= last_year_chart)

    # Breakdown of energy consumption by fuel
    transport_energy_breakdown_filtered <-
      transport_energy_breakdown %>%
      filter(geo == country_chart,
             !energy_consumption == 0)

    # Table used to provide figures in the text of the report
    table_transport_energy_breakdown_filtered <-
      transport_energy_breakdown_filtered %>%
      mutate(
        energy_consumption = round(energy_consumption, 2),
        share_energy_consumption = round(share_energy_consumption, 3)
      )

    write.csv(
      table_transport_energy_breakdown_filtered,
      paste0(outputpath, "Part5_fuel.csv"),
      row.names = FALSE
    )

    # Table used to provide figures in the text of the report
    table_transport_country_data <-
      transport_country_data %>%
      mutate(VKM = round(VKM, 2),
             energy_consumption = round(energy_consumption, 2),
             share_VKM = round(share_VKM, 2),
             share_energy_consumption = round(share_energy_consumption, 2))

    write.csv(
      table_transport_country_data,
      paste0(outputpath, "Part5_mode.csv"),
      row.names = FALSE
    )

    # chart indexed variation of total transport

    transport_data <-
      transport_mode %>%
      filter(mode == "Total",
             measure %in% c("intensity", "energy_consumption", "VKM")) %>%
      select(-c(
        value,
        value_delta)) %>%
      mutate(measure = case_when(
        measure == "intensity" ~ "Energy intensity",
        measure == "energy_consumption" ~ "Energy consumption",
        measure == "VKM" ~ "Traffic",
        TRUE ~ measure
        )
      )

    year <- as.Date(as.character(transport_data$time), "%Y")

    p <- transport_data %>%
      cbind(year) %>%
      select(-time) %>%
      ggplot() +
      geom_blank(aes(x = year)) +
      geom_line(aes(x = year, y = value_indexed, color = measure), size = 1) +
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
      ylab(paste("Index (", transport_base_year(country=country_chart, first_year = first_year), "=1)"))
    # ggtitle(paste("Indexed indicators for",country_name,"'s total energy consumption, \ntraffic and energy intensity variation of the transport sector, \nall years related to",as.character(first_year_chart)))

    filename <- paste0(country_chart, "_Figure29B.jpg")

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
      as.Date(as.character(transport_energy_breakdown_filtered$time),
              "%Y")

    p <- transport_energy_breakdown_filtered %>%
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
      scale_x_continuous(breaks = c(first_year, round((first_year + last_year
      ) / 2), last_year)) +
      scale_y_continuous(labels = scales::number) +
      ylab(paste("Energy consumption (PJ)"))
    #ggtitle(paste("Transport energy consumption by fuel for",country_name))

    filename <- paste0(country_chart, "_Figure26.jpg")
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

    p <- transport_energy_breakdown_filtered %>%
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
        aes(label = paste0(round(share_energy_consumption * 100, 0), "%")),
        position = position_stack(vjust = 0.5)) +
      ylab(paste("Share in final energy consumption"))
    # ggtitle(paste("Transport energy consumption by fuel for",country_name))

    filename <- paste0(country_chart, "_Figure26B.jpg")
    print(filename)

    jpeg(
      file = paste0(outputpath, filename),
      width = 2400,
      height = 2400,
      res = 300
    )

    print(p)

    dev.off()

    # energy consumption and VKM by mode

    year <-
      as.Date(as.character(transport_country_data$time), "%Y")

    # Transport energy consumption by mode
    p <- transport_country_data %>%
      cbind(year) %>%
      select(-time) %>%
      mutate(year = lubridate::year(year)) %>%
      ggplot(aes(x = year, y = energy_consumption / 1000)) +
      geom_bar(aes(fill = mode), stat = "identity") +
      scale_fill_manual(values = ColorsMode, limits = force) +
      theme_classic() +
      theme(
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        text = element_text(size = 15)
      ) +
      guides(fill=guide_legend(ncol=3)) +
      scale_x_continuous(breaks = c(first_year, round((
        first_year + last_year
      ) / 2), last_year)) +
      scale_y_continuous(labels = scales::number) +
      ylab(paste("Energy consumption (PJ)"))
    # ggtitle(paste("Transport energy consumption by mode for",country_name))

    filename <- paste0(country_chart, "_Figure27.jpg")

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

    p <- transport_country_data %>%
      filter(time %in% c(first_year, last_year)) %>%
      ggplot(aes(
        x = factor(time),
        y = share_energy_consumption,
        fill = mode
      )) +
      geom_bar(position = "fill", stat = "identity") +
      scale_fill_manual(values = ColorsMode, limits = force) +
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
      geom_text(aes(label = paste0(round(share_energy_consumption * 100, 0), "%")),
                position = position_stack(vjust = 0.5)) +
      ylab(paste("Share of energy consumption"))
    # ggtitle(paste("Energy consumption by mode for",country_name))

    filename <- paste0(country_chart, "_Figure27B.jpg")
    print(filename)

    jpeg(
      file = paste0(outputpath, filename),
      width = 2400,
      height = 2400,
      res = 300
    )

    print(p)

    dev.off()

    # Transport traffic by mode
    p <- transport_country_data %>%
      cbind(year) %>%
      select(-time) %>%
      mutate(year = lubridate::year(year)) %>%
      ggplot(aes(x = year, y = VKM / 1000000)) +
      geom_bar(aes(fill = mode), stat = "identity") +
      scale_fill_manual(values = ColorsMode, limits = force) +
      theme_classic() +
      theme(
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        text = element_text(size = 15)
      ) +
      guides(fill=guide_legend(ncol=3)) +
      scale_x_continuous(breaks = c(
        first_year,
                                    #round((first_year + last_year) / 2),
                                    last_year_chart
        )) +
      scale_y_continuous(labels = scales::number) +
      ylab(paste("Traffic (Million VKM)"))
    # ggtitle(paste("Traffic by mode for",country_name))

    filename <- paste0(country_chart, "_Figure28.jpg")

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

    p <- transport_country_data %>%
      filter(time %in% c(first_year, last_year)) %>%
      ggplot(aes(
        x = factor(time),
        y = share_VKM,
        fill = mode
      )) +
      geom_bar(position = "fill", stat = "identity") +
      scale_fill_manual(values = ColorsMode, limits = force) +
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
      geom_text(aes(label = paste0(round(share_VKM * 100, 0), "%")),
                position = position_stack(vjust = 0.5)) +
      ylab(paste("Share of traffic"))
    # ggtitle(paste("Traffic by mode for",country_name))

    filename <- paste0(country_chart, "_Figure28B.jpg")
    print(filename)

    jpeg(
      file = paste0(outputpath, filename),
      width = 2400,
      height = 2400,
      res = 300
    )

    print(p)

    dev.off()

    # Indexed variation by mode

    year <- as.Date(as.character(transport_mode$time), "%Y")

    p <- transport_mode %>%
      cbind(year) %>%
      select(-time) %>%
      filter(mode != "Total",
             measure == "intensity") %>%
      mutate(mode = factor(mode, levels = IDA_TRA_MODES)) %>%
      ggplot() +
      geom_blank(aes(x = year)) +
      geom_line(aes(x = year, y = value_indexed, color = mode), size = 1) +
      scale_color_manual(values = ColorsMode, limits = force) +
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
      ylab(paste("Index (", transport_base_year(country=country_chart, first_year = first_year), "=1)"))
    # ggtitle(paste("Indexed variation for",country_name,"'s energy intensity by transport mode, \nall years related to",as.character(base_year)))

    filename <- paste0(country_chart, "_Figure29C.jpg")

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
    transport_effects <- transport_LMDI %>%
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

    results_df <- transport_LMDI %>%
      filter(geo == country_chart,
             time <= last_year) %>%
      pivot_longer(
        cols = -c(geo, time),
        names_to = "measure",
        values_to = "value"
      ) %>%
      filter(measure == "energy_consumption_var_obs")

    #Plot the simple effect as bar chart
    p <- ggplot(data = transport_effects,
                aes(x = factor(time),
                    y = value / 1000)) +
      geom_bar(aes(fill = Effect),
               stat = "identity")  +
      scale_fill_manual(values = ColorsEffect, limits = force) +
      geom_point(data = results_df,
                 aes(y = value / 1000),
                 size = 3) +
      theme_classic() +
      theme(axis.title.x = element_blank(),
            text = element_text(size = 15)) +
      scale_y_continuous(labels = scales::number) +
      ylab("Energy consumption variation (PJ)")
    # ggtitle(paste("Decompostion analysis of",country_name,"'s transport energy consumption variation, \n  all years related to",as.character(first_year_chart)))

    filename <- paste0(country_chart, "_Figure29D.jpg")

    print(filename)

    jpeg(
      file = paste0(outputpath, filename),
      width = 2400,
      height = 1600,
      res = 300
    )

    print(p)

    dev.off()

    # waterfall chart

    Base_label <- paste0(as.character(first_year_chart), " level")
    Result_label <- paste0(as.character(last_year_chart), " level")

    #define the levels used in the waterfall chart
    levels_waterfall <- c(Base_label,
                         "Activity",
                         "Structure",
                         "Intensity",
                         Result_label)

    #prepare data for the waterfall chart (see plotly waterfall for explanations)
    transport_Waterfall_data <- transport_LMDI %>%
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
             text = paste(as.character(round(y, 2)), "PJ", sep = " "),
             measure = case_when((x == !!Result_label) ~ "total",
                                 TRUE ~ "relative"))

    write.csv(transport_Waterfall_data,
              paste0(outputpath, "Part5_waterfall.csv"),
              row.names = FALSE)

    p <- transport_Waterfall_data %>%
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

    filename <- paste0(country_chart, "_Figure29.jpg")

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
    transport_intensity_effect_data <-
      transport_LMDI %>%
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
        levels = c("Without intensity effect",
                   "Actual energy consumption")
      )) %>%
      arrange(measure)

    write.csv(transport_intensity_effect_data,
              paste0(outputpath, "Part5_intensity_effect.csv"),
              row.names = FALSE)

    #Plot the intensity effect as area chart
    p <- transport_intensity_effect_data %>%
      ggplot() +
      geom_bar(
        data = (
          transport_intensity_effect_data %>%
            filter(measure == 'Actual energy consumption')
        ),
        aes(
          y = value / 1000,
          x = time,
          fill = measure
        ),
        stat = "identity",
        alpha = 0.5
      ) +
      scale_fill_manual(values = c("Actual energy consumption" = "blue4")) +
      geom_point(
        data = (
          transport_intensity_effect_data %>%
            filter(measure == 'Without intensity effect',
                   time >= first_year_chart,
                   time <= last_year_chart)
        ),
        aes(
          y = value / 1000,
          x = time,
          color = measure
        ),
        size = 3,
        alpha = 0.5
      ) +
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
      scale_x_continuous(breaks = c(first_year, round((first_year + last_year) / 2), last_year)) +
      scale_y_continuous(labels = scales::number) +
      ylab("Energy consumption (PJ)") +
      expand_limits(y = 0)
    # ggtitle(paste("Actual energy consumption in the transport sector vs theoretical \n(without energy intensity improvements) for",country_name))

    filename <- paste0(country_chart, "_Figure30.jpg")

    print(filename)

    jpeg(
      file = paste0(outputpath, filename),
      width = 2400,
      height = 2400,
      res = 300
    )

    print(p)

    dev.off()

    # intensity comparison chart 1

    # Prepare data for the intensity comparison chart
    transport_intensity_comparison_data <- transport_full %>%
      filter(measure == "intensity",
             time <= last_year,
             mode != 'Total') %>%
      select(-c(value_indexed, value_delta)) %>%
      mutate(value = value * 1000000) %>%
      pivot_wider(names_from = geo, values_from = value)

    min_EU <-
      apply(select_if(transport_intensity_comparison_data[, -1], is.numeric),
            1,
            min,
            na.rm = TRUE)
    max_EU <-
      apply(select_if(transport_intensity_comparison_data[, -1], is.numeric),
            1,
            max,
            na.rm = TRUE)
    avg_EU <-
      apply(select_if(transport_intensity_comparison_data[, -1], is.numeric),
            1,
            mean,
            na.rm = TRUE)
    year <-
      as.Date(as.character(transport_intensity_comparison_data$time),
              "%Y")

    transport_intensity_comparison_data <-
      transport_intensity_comparison_data %>%
      cbind(min_EU) %>%
      cbind(max_EU) %>%
      cbind(avg_EU) %>%
      cbind(year) %>%
      select(c(year, mode,!!country_chart, min_EU, max_EU)) %>%
      mutate(year = lubridate::year(year))

    #Plot the intensity comparison as line chart
    p <- transport_intensity_comparison_data %>%
      ggplot(aes(x = year)) +
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
        text = element_text(size = 15)
      ) +
      ylab("Energy intensity (TJ / Million VKM)") +
      scale_x_continuous(breaks = c(first_year, round((
        first_year + last_year
      ) / 2), last_year)) +
      #scale_y_continuous(labels = scales::number) +
      expand_limits(y = 0) +
      scale_y_continuous(breaks = scales::breaks_extended(Q = c(0, 1, 2, 3))) +
      # ggtitle(paste("Energy intensity in", country_name, "'s transport by mode compared to \nother European countries")) +
      facet_wrap( ~ mode, scales = "free")

    filename <- paste0(country_chart, "_Figure31.jpg")

    print(filename)

    jpeg(
      file = paste0(outputpath, filename),
      width = 2400,
      height = 1600,
      res = 300
    )

    print(p)

    dev.off()

    # intensity comparison chart 2

    #prepare data for the intensity comparison across mode chart
    mode_comparison_data <- transport_full %>%
      filter(geo == country_chart,
             measure == "intensity",
             mode != "Total",
             time <= last_year) %>%
      mutate(value = value * 1000000)

    year <- as.Date(as.character(mode_comparison_data$time), "%Y")

    mode_comparison_data <- mode_comparison_data %>%
      cbind(year) %>%
      select(c(year, mode, value)) %>%
      mutate(year = lubridate::year(year))

    #Plot the intensity comparison as line chart
    p <- mode_comparison_data %>%
      ggplot(aes(x = year, y = value, color = mode)) +
      geom_line(size = 1) +
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
      ylab("Energy intensity (TJ / Million VKM)") +
      scale_x_continuous(breaks = c(first_year, round((
        first_year + last_year
      ) / 2), last_year))
    # ggtitle(paste("Energy intensity in", country_name, "compared by mode"))

    filename <- paste0(country_chart, "_Figure31B.jpg")

    jpeg(
      file = paste0(outputpath, filename),
      width = 2400,
      height = 2400,
      res = 300
    )

    print(p)

    dev.off()

    # intensity comparison chart 3

    #prepare data for the intensity comparison across mode chart
    mode_comparison_data <- transport_full %>%
      filter(geo == country_chart,
             measure == "intensity",
             mode != "Total",
             time <= last_year) %>%
      mutate(value = value * 1000000)

    year <- as.Date(as.character(mode_comparison_data$time), "%Y")

    mode_comparison_data <- mode_comparison_data %>%
      cbind(year) %>%
      select(c(year, mode, value)) %>%
      mutate(year = lubridate::year(year))

  }

  if (country == "EU27") {

    outputpath <- paste0(chart_path, "/EU27/")

    # Data coverage chart
    missing_data <- transport_complete %>%
      filter(
        mode != "Total",
        geo != "EU27",
        time <= last_year
        ) %>%
      select(c("geo", "time", "mode", "energy_consumption", "VKM")) %>%
      replace(is.na(.), 0) %>%
      mutate(missing =
               case_when((energy_consumption > 0 & VKM > 0) |
                           (energy_consumption == 0 & VKM == 0) ~ 0,
                         TRUE ~ 1
               )) %>%
      select(-c("energy_consumption", "VKM")) %>%
      group_by(geo, time) %>%
      summarize(missing = sum(missing))
    
    write.csv(missing_data,
              paste0(outputpath, "Part5_missing_data.csv"),
              row.names = FALSE)
      
    p <- missing_data %>%
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
        limits = c(0, 3),
        breaks = scales::pretty_breaks(n = 3)(0:3)
      ) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      labs(fill = "Missing modes")
      #ggtitle("Completeness of coverage (energy and activity data) for European countries across years")

    filename <- "EU27_Figure31D.jpg"

    print(filename)

    jpeg(
      file = paste0(outputpath, filename),
      width = 2400,
      height = 3200,
      res = 300
    )

    print(p)

    dev.off()

    # EU intensity comparison chart
    transport_intensity_EU_comparison_data <-
      transport_full %>%
      filter(
        measure == "intensity",
        time %in% c(first_year_chart, last_year_chart),
        mode != "Total",
        geo != 'EU27'
      ) %>%
      arrange(value) %>%
      merge(eu_countries, by.x = 'geo', by.y = 'code') %>%
      select(-c('geo', 'label'))

    write.csv(transport_intensity_EU_comparison_data %>%
               mutate(value = value * 1000000),
      paste0(outputpath, "Part4_EU27.csv"),
      row.names = FALSE)

    # Rank the countries by intensity on last year
    country_ranked_road <- transport_intensity_EU_comparison_data %>%
      filter(time == last_year_chart,
             mode == "Road") %>%
      arrange(value) %>%
      pull(name)

    country_ranked_rail <- transport_intensity_EU_comparison_data %>%
      filter(time == last_year_chart,
             mode == "Rail") %>%
      arrange(value) %>%
      pull(name)

    country_ranked_water <- transport_intensity_EU_comparison_data %>%
      filter(time == last_year_chart,
             mode == "Navigation") %>%
      arrange(value) %>%
      pull(name)

    road_chart <- transport_intensity_EU_comparison_data %>%
      filter(mode == "Road") %>%
      ggplot(aes(
        x = factor(name, levels = country_ranked_road),
        y = value * 1000000,
        color = as.factor(time)
      )) +
      geom_point() +
      geom_line(aes(group = name), colour = "grey") +
      coord_flip() +
      theme_classic() +
      theme(
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal"
      ) +
      ylab("Energy intensity (TJ / Million VKM)") +
      scale_y_continuous(labels = scales::number)
    #ggtitle(paste("Energy intensity of road transport in European countries"))

    rail_chart <- transport_intensity_EU_comparison_data %>%
      filter(mode == "Rail") %>%
      ggplot(aes(
        x = factor(name, levels = country_ranked_rail),
        y = value * 1000000,
        color = as.factor(time)
      )) +
      geom_point() +
      geom_line(aes(group = name), colour = "grey") +
      coord_flip() +
      theme_classic() +
      theme(
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal"
      ) +
      ylab("Energy intensity (TJ / Million VKM)") +
      scale_y_continuous(labels = scales::number)
    #ggtitle(paste("Energy intensity of rail transport in European countries"))

    water_chart <- transport_intensity_EU_comparison_data %>%
      filter(mode == "Navigation") %>%
      ggplot(aes(
        x = factor(name, levels = country_ranked_water),
        y = value * 1000000,
        color = as.factor(time)
      )) +
      geom_point() +
      geom_line(aes(group = name), colour = "grey") +
      coord_flip() +
      theme_classic() +
      theme(
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal"
      ) +
      ylab("Energy intensity (TJ / Million VKM)") +
      scale_y_continuous(labels = scales::number)
    #ggtitle(paste("Energy intensity of rail transport in European countries"))

    p <- ggarrange(
      road_chart,
      rail_chart,
      water_chart,
      ncol = 3,
      nrow = 1,
      labels = c("Road", "Rail", "Navigation"),
      label.x = 0.5,
      align = "hv",
      common.legend = TRUE,
      legend = "bottom"
    )

    filename <- "EU27_Figure31C.jpg"

    print(filename)

    jpeg(
        file = paste0(outputpath, filename),
        width = 3000,
        height = 3200,
        res = 300
    )
    
    print(p)
    
    dev.off()
  }
}