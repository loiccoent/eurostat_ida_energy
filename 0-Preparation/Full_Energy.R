# FINAL ENERGY CONSUMPTION IN INDUSTRY

# Data preparation
full_energy_final <- function(first_year,
                               last_year,
                               country,
                               data_path,
                               chart_path) {
  
  # Define the list as the whole list
  country_list <- geo_codes
  
  # Prepare for potential exceptions
  years_list <- data.frame(geo_codes) %>%
    mutate(base_year = case_when(TRUE ~ first_year),
           final_year = case_when(TRUE ~ last_year))
  
  # DATA PREPARATION
  
  # Energy consumption (and supply) from the energy balance (nrg_bal_c)
  load(paste0(data_path, "/nrg_bal_c.Rda"))
  
  #list of end uses sectors, used for the industry subset the energy balance (nrg_bal_c)
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
  
  #Agriculture, forestry and fishing
  NRG_AGRI <- c("FC_OTH_AF_E",
                "FC_OTH_FISH_E")
  
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
  
  NRG_TRA <- c(
    "FC_TRA_RAIL_E",
    "FC_TRA_ROAD_E",
    "FC_TRA_DNAVI_E"
  )
  
  #list of end uses sectors, used in the subset
  NRG_FULL_SECTORS <- c(
    NRG_AGRI,
    NRG_MAN,
    NRG_OTH,
    NRG_TRA,
    "FC_IND_CON_E",
    "FC_OTH_CP_E",
    "FC_OTH_HH_E"
  )
  
  #list of supply items, used in the context
  NRG_SUPPLY <- c(
    "PPRD",
    "IMP",
    "EXP",
    "STK_CHG"
  )
  
  #list of end uses sectors, as they will be named in the LMDI results
  IDA_FULL_SECTORS <- c(
    #"Agriculture, forestry and fishing",
    "Agricult., forest. and fish.",
    "Manufacturing",
    "Construction",
    "Other industries",
    #"Commercial and public services"
    "Comm. and pub. services",
    "Residential",
    "Transport"
  )
  
  # Colors
  
  ColorsSector <- c(
    #"Agriculture, forestry and fishing" = brewer.pal(5, "Set3")[1],
    "Agricult., forest. and fish." = brewer.pal(7, "Set3")[3],
    "Manufacturing" = brewer.pal(7, "Set3")[2],
    "Construction" = brewer.pal(7, "Set3")[1],
    "Other industries" = brewer.pal(7, "Set3")[4],
    #"Commercial and public services" = brewer.pal(5, "Set3")[5]
    "Comm. and pub. services" = brewer.pal(7, "Set3")[5],
    "Residential" = brewer.pal(7, "Set3")[7],
    "Transport" = brewer.pal(7, "Set3")[6]
  )
  
  # Energy consumption by fuel
  full_energy_context <-  nrg_bal_c %>%
    filter(
      geo %in% country_list,
      #from first year
      time >= first_year,
      # to last year
      time <= last_year,
      #take industry end uses
      nrg_bal %in% NRG_SUPPLY,
      #work with total energy consumption, in TJ
      siec == "TOTAL",
      unit == "TJ"
    ) %>%
    select(c("geo", "time", "nrg_bal", "values"))
  
  # Energy consumption by fuel
  full_energy_breakdown <-  nrg_bal_c %>%
    filter(
      geo %in% country_list,
      #from first year
      time >= first_year,
      # to last year
      time <= last_year,
      #take industry end uses
      nrg_bal %in% NRG_FULL_SECTORS,
      #work with total energy consumption, in TJ
      siec == "TOTAL",
      unit == "TJ"
    ) %>%
    select(c("geo", "time", "nrg_bal", "values")) %>%
    #reshape to wide
    pivot_wider(names_from = nrg_bal, values_from = values) %>%
    #aggregate
    mutate(
      #Agriculture, forestry and fishing
      A = rowSums(select(., all_of(NRG_AGRI)), na.rm = TRUE),
      #Manufacturing
      C = rowSums(select(., all_of(NRG_MAN)), na.rm = TRUE),
      #Other industries
      B_D_E = rowSums(select(., all_of(NRG_OTH)), na.rm = TRUE),
      #Other industries
      TRA = rowSums(select(., all_of(NRG_TRA)), na.rm = TRUE)
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
        NRG_NI_E,
        FC_TRA_RAIL_E,
        FC_TRA_ROAD_E,
        FC_TRA_DNAVI_E
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
      "Comm. and pub. services" = "FC_OTH_CP_E",
      "Residential" = "FC_OTH_HH_E",
      "Transport" = "TRA"
    ) %>%
    #reshape to long
    pivot_longer(
      cols = -c(geo, time),
      names_to = "sector",
      values_to = "energy_consumption"
    ) %>%
    # For each country and each year
    group_by(geo, time) %>%
    mutate(
      # Calculate the total energy consumption and value added of the overall industry sector, as the sum of all subsectors selected
      total_energy_consumption = sum(energy_consumption, na.rm = TRUE)) %>%
    ungroup() %>%
    # For each country, each year and each subsector
    mutate(
      # Calculate the share of the subsector in the overall energy consumption and in the overall value added of the industry sector
      share_energy_consumption = energy_consumption / total_energy_consumption
    ) 
    # Remove the total columns, not required any longer
    #select(-c(total_energy_consumption)) 
  
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
      years_list$base_year[geo_codes == country_chart]
    last_year_chart <-
      years_list$final_year[geo_codes == country_chart]
    
    # Energy consumption by subsector
    
    # Table used to provide figures in the text of the report
    table_full_energy_context <- full_energy_context %>%
      filter(
        geo == country_chart
      ) %>%
      mutate(values = round(values, 1))
    
    write.csv(table_full_energy_context, paste0(outputpath, "Context.csv"), row.names = FALSE)
    
    # Table used to provide figures in the text of the report
    table_full_energy_breakdown <- full_energy_breakdown %>%
      filter(
        geo == country_chart
      ) %>%
      mutate(energy_consumption = round(energy_consumption, 1),
             share_energy_consumption = round(share_energy_consumption, 3))
    
    write.csv(table_full_energy_breakdown, paste0(outputpath, "Intro.csv"), row.names = FALSE)
    
    full_energy_breakdown_filtered_data <-
      full_energy_breakdown %>%
      filter(
        geo == country_chart,
        sector != "Total",
        time <= year_chart
      ) %>%
      mutate(sector = factor(sector, levels = IDA_FULL_SECTORS))
    
    year <-
      as.Date(as.character(full_energy_breakdown_filtered_data$time),
              "%Y")
    
    p <- full_energy_breakdown_filtered_data %>%
      cbind(year) %>%
      select(-time) %>%
      mutate(year = lubridate::year(year)) %>%
      ggplot(aes(x = year, y = energy_consumption / 1000)) +
      geom_bar(aes(fill = sector), stat = "identity") +
      scale_fill_manual(values = ColorsSector) +
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
    
    filename <- paste0(country_chart, "_Figure01.jpg")
    print(filename)
    
    jpeg(
      file = paste0(outputpath, filename),
      width = 2400,
      height = 2400,
      res = 300
    )
    
    print(p)
    
    dev.off()
    
    p <- full_energy_breakdown_filtered_data %>%
      filter(time %in% c(first_year_chart, last_year_chart),
             geo == country_chart,
             sector != "Total") %>%
      mutate(sector = factor(sector, levels = IDA_FULL_SECTORS))  %>%
      ggplot(aes(x = factor(time), y = share_energy_consumption, fill = sector)) +
      geom_bar(position = "fill", stat = "identity") +
      scale_fill_manual(values = ColorsSector) +
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
    
    filename <- paste0(country_chart, "_Figure01B.jpg")
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