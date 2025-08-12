# variables/paths to edit to run 'flux_shiny_app.R'

# Specify the path to your database
# TE, 2024-12-11
main_dir <- '/home/uqam-site'
#main_dir <- '/Users/saraknox/Code/local_data_cleaning/Projects/uqam-site/Database'
  
# Path for met/flux variables to display
basepath <- main_dir

# Tim Elrick, 2024-12-18: changed according to Sara
level <- c("Clean/SecondStage", "Met")
#level <- c("Clean/ThirdStage","Met") #NEED TO UPDATE FOR THIRD STAGE WHEN ALL SITES HAVE DATA

tv_input <- "clean_tv" # MAKE SURE tv is in folder

# Path to csv file with units
#UnitCSVFilePath <- '/Users/saraknox/Code/flux_shiny_plots/flux_variables.csv'
# TE, 2024-12-11 
UnitCSVFilePath <- 'data/flux_variables.csv'

# path to shiny app
# TE, 2024-12-11
arg <- "./"

# provide names for incoming shortwave radiation and incoming PPDF
var_rad <- c('SW_IN_Avg', 'PPFD_IN_Avg') 

# path and file name of site coordinates
#coordinatesXLSXFilePath <- '/Users/saraknox/Code/flux_shiny_plots/ini files/site_coordinates_UQAM.xlsx'
# TE, 2024-12-11
coordinatesXLSXFilePath <- 'data/site_coordinates_UQAM.xlsx'
