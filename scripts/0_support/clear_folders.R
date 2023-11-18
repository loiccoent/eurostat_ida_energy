# CLEAR THE REPOSITORIES
library(futile.logger)

# Data preparation
clear_all <- function(country,
                      chart_path) {
  # Define the list as the whole list
  country_list <- geo_codes

  if (country == "all") {
    countries <- geo_codes
    flog.info(paste("Empty all the country folders"))
  } else {
    countries <- country
    flog.info(paste("Emptying the folder: ", country))
  }

  for (country_chart in countries) {
    # Output charts
    outputpath <- paste(chart_path, "/", country_chart, "/", sep = "")
    tryCatch(
      {
        unlink(paste0(outputpath, "*"))
      },
      error = function(e) {
        flog.error(paste("Error clearing the folder: ", country_chart), e)
      }
    )
  }
}