# FINAL ENERGY CONSUMPTION IN INDUSTRY

# Data preparation
clear_all <- function(country,
                      chart_path) {
  
  
  # Define the list as the whole list
  country_list <- geo_codes
  
  if (country == "all") {
    countries <- geo_codes
  } else {
    countries <- country
  }
  
  for (country_chart in countries) {
    # Output charts
    outputpath <- paste(chart_path, "/", country_chart, "/", sep = "")
    print(paste0("Emptying the folder ", country_chart))
    unlink(paste0(outputpath,"*"))
    
  }

}