library(dplyr)

apply_gva_corrections <- function(df){
  df <- df %>%
  # correction 
  mutate(GVA = case_when(
            (geo == 'AT' & sector == 'Coke and ref. pet. products' & time == 2013) ~ GVA * 10,
            (geo == 'AT' & sector == 'Coke and ref. pet. products' & time == 2014) ~ GVA * 100,
            (geo == 'ES' & sector == 'Coke and ref. pet. products' & time == 2020) ~ -GVA,
            (geo == 'SE' & sector == 'Coke and ref. pet. products' & time == 2020) ~ -GVA,
            (geo == 'IT' & sector == 'Coke and ref. pet. products' & time == 2014) ~ -GVA,
            (geo == 'IT' & sector == 'Coke and ref. pet. products' & time == 2020) ~ -GVA,
            (geo == 'PT' & sector == 'Coke and ref. pet. products' & time == 2020) ~ -GVA,
            (geo == 'LV' & sector == 'Basic metals' & time == 2013) ~ -GVA,
            (geo == 'LV' & sector == 'Basic metals' & time == 2014) ~ -GVA,
            (geo == 'LV' & sector == 'Coke and ref. pet. products' & time == 2020) ~ -GVA,
            (geo == 'BG' & sector == 'Coke and ref. pet. products' & time == 2014) ~ -GVA,
            (geo == 'BG' & sector == 'Coke and ref. pet. products' & time == 2020) ~ -GVA,
            (geo == 'CZ' & sector == 'Coke and ref. pet. products' & time == 2021) ~ -GVA,
            TRUE ~ GVA
        )
  )

  print('AT 2013, 2014 Coke and ref. pet. products GVA correction')
  print('ES 2020 Coke and ref. pet. products GVA correction')
  print('SE 2020 Coke and ref. pet. products GVA correction')
  print('IT 2014, 2020 Coke and ref. pet. products GVA correction')
  print('PT 2020 Coke and ref. pet. products GVA correction')
  print('LV 2013, 2014 Basid metals GVA correction')
  print('LV 2020 Coke and ref. pet. products GVA correction')
  print('BG 2014, 2020 Coke and ref. pet. products GVA correction')
  print('CZ 2021 Coke and ref. pet. products GVA correction')

  df

}

filter_industry_GVA_final <- function(df){
  df <- df %>%
        filter(!(geo == 'EE' & sector == 'Coke and ref. pet. products'),
            !(geo == 'IE' & sector %in% c('Coke and ref. pet. products',
                                        'Chemical and petrochem.',
                                        'Transport equipment')),
            !(geo == 'LV' & sector %in% c('Coke and ref. pet. products')),
            !(geo == 'LT' & sector %in% c('Coke and ref. pet. products')),
            !(geo == 'LU' & sector %in% c('Food, bev. and tobacco',
                                        'Wood and wood products',
                                        'Basic metals',
                                        'Other manufacturing',
                                        'Paper, pulp and printing',
                                        'Non-metallic minerals',
                                        'Machinery',
                                        'Transport equipment')),
            !(geo == 'MT' & sector %in% c('Mining and quarrying',
                                        'Wood and wood products',
                                        'Basic metals',
                                        'Coke and ref. pet. products',
                                        'Non-metallic minerals',
                                        'Transport equipment',
  #"Chemical and petrochem.",
  #'Paper, pulp and printing',
                                        "Other manufacturing")),
            !(geo == 'SI' & sector %in% c('Coke and ref. pet. products')),
            !(geo == 'SE' & sector %in% c('Chemical and petrochem.')))

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
  
  df

}