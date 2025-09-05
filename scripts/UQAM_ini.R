# variables/paths to edit to run 'flux_shiny_app.R'

# Specify the path to your database
# TE, 2024-12-11

# path to shiny app
# TE, 2024-12-11
arg <- "./"
#arg <- "/Users/saraknox/Code/RShinyServer/carbonique_RShiny_app/"

main_dir <- '/home/uqam-site'
#main_dir <- '/Users/saraknox/Code/local_data_cleaning/Projects/uqam-site/Database'

# List all directories and subdirectories
dir_list <- list.dirs(main_dir,
                      full.names = TRUE,  
                      recursive = TRUE)

# From that list, get site names
dir_length <- lengths(strsplit(dir_list, "/"))

# Tim Elrick, 2024-12-17: Fixed issue that it picked up also non-year numbers
dir_years <- dir_list[grepl("/20\\d{2}$", dir_list)]

# Tim Elrick, 2024-12-17: Select only UQAM sites greater than UQAM_0
dir_sites <- list.dirs(dir_years, full.names = TRUE, recursive = FALSE)
dir_sites <- dir_sites[grepl("_[1-9][0-9]?$", dir_sites)]
sites <- unique(sapply(strsplit(dir_sites, "/"), '[[', length(strsplit(dir_sites, "/")[[1]]))) 

# Tim Elrick, 2024:12-17: load all functions, fixed
fx_path <- file.path(arg, "functions")
fxs <- list.files(fx_path, pattern = "\\.R$", full.names = T)
for (f in fxs) source(f)

# Path for met/flux variables to display
basepath <- main_dir

# Tim Elrick, 2024-12-18: changed according to Sara
level <- c("Clean/SecondStage")
#level <- c("Clean/ThirdStage","Met") #NEED TO UPDATE FOR THIRD STAGE WHEN ALL SITES HAVE DATA

tv_input <- "clean_tv" # MAKE SURE tv is in folder

# Path to csv file with units
#UnitCSVFilePath <- '/Users/saraknox/Code/flux_shiny_plots/flux_variables.csv'
# TE, 2024-12-11 
UnitCSVFilePath <- 'data/flux_variables.csv'

# provide names for incoming shortwave radiation and incoming PPDF
var_rad <- c('SW_IN_1_1_1', 'PPFD_IN_1_1_1') 

# path and file name of site coordinates
#coordinatesXLSXFilePath <- '/Users/saraknox/Code/flux_shiny_plots/ini files/site_coordinates_UQAM.xlsx'
# TE, 2024-12-11
coordinatesXLSXFilePath <- 'data/site_coordinates_UQAM.xlsx'
