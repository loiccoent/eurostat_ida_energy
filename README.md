# Indexed decomposition analysis of energy consumption for different sectors of the economy

## Introduction
The tool described here is the last deliverable in the Eurostat project “energy consumption decomposition analysis”. It consists in a series of scripts written In the R language, which allow the user to download the data, perform the calculations and generate charts and numerical outputs linked to the different decompositions that have been presented during the project. More precisely, 5 main decompositions have been introduced: 
- decomposition of the final energy consumption in the industry sector, based on value-added.
- decomposition of the primary energy consumption in the industry sector, based on value-added.
- decomposition of the final energy consumption in the different economy sector, based on employment.
- decomposition of the final energy consumption in the residential sector.
- decomposition of the final energy consumption in the industry sector, based on traffic.
The document will succinctly describe how the project is organized, how the different operations are run, and how the user should maintain the different parts in order to use it again for future cycles of data collection.

## How to run
Once the project has been saved on a computer, the easiest way is to : 
- open the project in an R-compatible IDE (e.g. R studio)
- open the file run.R in the editor
- on the first run, some of the packages used might have to be installed in your local R instance. TO do so, simply uncomment the lines (remove the #) starting with “install.packages(…)” on top of the script. This will install all the required libraries and later they will only be called by the script, so you can comment them back (add the #) right after the first run to save time on future runs.
- If you only want to perform a part of the actions, open and edit the file config.yml to select what you want to do
- If ready to run everything, simply select the file run.R and select source (the editor might have a play symbol)
- Let the process run and see the information printed in the console.


## Project organisation
### Parameters
Use the file config.yml to specify all the main parameters of the process: 
- country: country to show in charts (write "all" to run all countries + EU27)
- year:
    - first: earliest year covered
    - last: latest year covered
- actions:
    - download: download raw data, False: use existing data
    - clear: clears existing charts, False: only replace the ones generated
    - context: Generate context data (Part 1)
    - industry_GVA_final: Run the decomposition of the final energy consumption in the industry sector, based on GVA
    - industry_GVA_primary: Run the decomposition of the primary energy consumption in the industry sector, based on GVA
    - economy_emp_final: Run the decomposition of the final energy consumption in the different economy sector, based on employment
    - household_final: Run the decomposition of the final energy consumption in the residential sector
    - transport_final: Run the decomposition of the final energy consumption in the industry sector, based on VKM

### Data:
The data folder will appear as the script stores the data.
- raw: where the data extracted from Eurostat is stored as .rda data file (filled by the data_download.R script)
- output: Where the charts and csv data are be stored after processing 
    - AT: country folder (receives charts (.jpg) and tables (.csv))
    - BE
    - ...

### Scripts:
Main scripts and their location:
- 0_support
    - o	folders_management.R: procedures used to empty the output folders / create them if needed
    - data_download.R: functions to load all the data required from Eurostat
    - data_load.R: functions to load the previously downloaded Eurostat data files
    - manual_corrections.R: corrections to be applied for specific decomposition, country, sector and year
    - mapping_colors.R: colors used in charts for products and sectors
    - mapping_products.R: list of product for each aggregates
    - mapping_sectors.R: list of sectors for each aggregates
    - parameters.R: functions that reads the config file and set the corresponding parameters
    - outputs.R: functions to save charts and data files in the output folder
    - year_selection.R: used ot set country specific first and last years
- 1_industry
    - 1a_industry_gva_final.R: Decomposition of final energy consumption in manufacturing industry
    - 1b_industry_gva_primary.R: Decomposition of primary energy consumption in manufacturing industry
    - 1c_economy_emp_final.R: Decomposition of final energy consumption in whole economy
- 2_household
    - households.R: Decomposition of final energy consumption in residential sector
- 3_transport
    - transport_VKM.R: Decomposition of final energy consumption in transport sector
- 4_all_sectors
    - full_energy.R: Charts for energy consumption in the different sectors
    - shared.R: Functions shared across several decomposition
- Run.R: main script, calls all of the functions above
- config.yml: configuration file where the user can select the decomposition he wants to run, and for which country
- REAME.md: the file you are reading

## Script structure
All the decomposition scripts follow a very similar structure (there are slight differences in the industry primary energy decomposition
and in the residential sector decomposition due to the higher number of effects). 

### Script content
- Library loading
- Script sourcing (loads auxiliary functions in memory)
- MAIN FUNCTION:
    - Define the country list as the whole list _(the decomposition needs to be applied on the whole list of countries because of 
    the charts comparing intensities for all countries)_
    - DATA PREPARATION:
        - Load energy consumption from the energy balance
        - Load activity data from different sources
        - Prepare Energy consumption by fuel (not part of the decomposition but used for some charts)
        - Prepare energy consumption by end use
        - Prepare activity data by end use
        - Joining the energy consumption and activity datasets (by country, year and end use)
        - Filter out sectors with incomplete data
    - EFFECTS CALCULATION:
        - Calculate the required indicators for the calculation:
            - Recalculate total and shares after filtering
            - Calculate indexed and differenced data
        - Calculate the 3 _(or more)_ effects using the LMDI formulas
    - CHARTS: 
        - either loop over all countries **("all")** or only the country selected, and for each country retrieve:
            - long name
            - first year used for decomposition
            - last year used for decomposition
        - and generate:
            - country charts _(general charts presenting the total energy and activity data)_
            - subsectors charts (same as above with details by subsectors)
            - energy breakdown charts _(same as first with details by energy medium)_
            - effect decomposition _(charts presenting the result of the decomposition, including the waterfall chart)_
        - if running *"EU27"*, generate:
            - coverage_chart _(number of sector missing for country)_
            - comparison chart _(comparison of intensity effect between EU countries)_
- SUPPORT FUNCTIONS: all of the functions called by the decomposition scipts are either found below, or if they belong to several decomposition, in the shared.R file. Important functions include:
    - reverse_negative_gva: detects where gva is negative and reverse it (unusable with the decomposition). The country, sector and year is logged to console, for investigation
    - filter_sectors (e.g. filter_industry_gva). Detects sectors for which, for a year at least, either activity is missing or there is no energy consumption despite some activity reported. If so, excludes the sector for the entire series (check sectors for specificities, due to the number of missing data, the transport sector rule is different and only checks the first and last year)

### Logging
Logging to console is used through the process to inform the user of the coming operations (flag INFO), of data exclusion (flag WARN) and of potential errors (flag ERROR). These logs should be checked as part of the output to ensure the process ran as expected.

### Error handling
Error handling has been implemented to facilitate use of the project. In summary, in case of error, the process will attempt to resume where possible. Based on the structure above it can be seen that:
- If one of the chart functions does not go through, an error will be printed to console and the next chart function in the current decomposition (if any) will start.
- If the error occur in the data preparation, the error will be caught at the higher level, because the chart cannot be generated without the base data. In consequence, an error will be printed to console and the next decomposition script (if any) will be triggered.


## For future cycles

### Getting the new data
To download the latest data, just change the corresponding parameters in the config.yml file. Be careful doing so: it will erase all data previously stored in the data/raw folder.
If you want to potentially keep this data, store it in a subfolder for instance.

### Corrections
In the future cycles, you may have to update some of the corrections and filter to handle data issues, they can be found in:
    -  manual_corrections.R: 
        - to correct specific values (e.g. wrong units)
        - to	to check if previous corrections are still valid
    -  year_selection.R: to select the first and last year, considering they should contain only complete sectors (activity and energy)
    
When test running for a new year, it is possible the process will exclude a very high amount of sector due to missing activity data on the last year. Depending on the number of sectors involved, there is always a trade off between excluding the sectors from the decomposition or shorten the year coverage for the given country.



