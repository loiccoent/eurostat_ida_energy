# ida_energy
Indexed decomposition analysis of energy consumption for different sectors of the economy

## Organisation
data
|raw: where the data extracted from Eurostat is stored as .rda data file
|output: Where the charts and csv data will be stored after processing
    |AT
    |BE
    |...
scripts:
|0_support
    |clear_folders.R: used to empty the output folders
    |data_download.R: functions to load all the data required from Eurostat
    |data_load.R: functions to load the previously downloaded Eurostat data files
    |manual_corrections.R: corrections to be applied for specific decomposition, country, sector and year
    |mapping_colors.R: colors used in charts for products and sectors
    |mapping_products.R: list of product for each aggregates
    |mapping_sectors.R: list of sectors for each aggregates
    |parameters.R: functions that reads the config file and set the corresponding parameters
    |print_charts.R function to save charts in the output folder
|1_industry
    |1a_industry_gva_final.R: Decomposition of final energy consumption in manufacturing industry
    |1b_industry_gva_primary.R: Decomposition of primary energy consumption in manufacturing industry
    |1c_economy_emp_final.R: Decomposition of final energy consumption in whole economy
|2_household
    |households.R: Decomposition of final energy consumption in residential sector
|3_transport
    |transport_VKM.R: Decomposition of final energy consumption in transport sector
|4_all_sectors
    |full_energy.R: Charts for energy consumption in the different sectors
Run.R: main script, calls all of the functions above
config.yml: configuration file where the user can select the decomposition he wants to run, and for which country
REAME.md: the file you are reading

## For future cycles

### Getting the new data
To download the latest data, just change the parameters in the config.yml file. Be careful doing so: it will erase all data previously stored in the data/raw folder.
If you want to potentially keep this data, store it in a subfolder for instance.

### Corrections
In the future cycles, you may have to update some of the corrections and filter to handle data issues, they can be found in:
    - manual_corrections.R: 
        - to exclude specific sectors where activity is available and not energy consumption, or vice-versa
        - to correct specific values (wrong units, negatives)
    - year_selection.R: to select the first and last year, considering they should contain only complete sectors (activity and energy)
When facing a data issue, there is always a trade off between excluding a sector from the decomposition or reducing the year coverage to exclude the problematic years.



