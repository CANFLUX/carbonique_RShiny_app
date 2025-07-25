# 1. Load libraries -------------------------------------------------------------------------

# Tim Elrick, 2024-12-17: removed unused packages
library(tidyverse)
library(plotly)
library(readxl)
library(hms)
library(data.table)
library(ggrepel)
# reactiveConsole(T) # allows the csv file to be used properly

# get initial paths from UQAM_ini.R
source("scripts/UQAM_ini.R")
#source("/Users/saraknox/Code/RShinyServer/carbonique_RShiny_app/scripts/UQAM_ini.R")

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


# 2. LOAD DATA ---------------------------------------------------------------------------------- 

# Load site data from first site when app initializes 
site <- sites[1]
# Find which years are available
yrs <- yrs_included(basepath,site,level[1])
data <- read_data_all_years(basepath,yrs,site,level,tv_input)
# add EBC columns
data <- create_EBC_columns(data)
data_units <- var_units(colnames(data),UnitCSVFilePath)

# CREATE DATASET TO PLOT ALL SITES AT ONCE
# Only a few variables
var_of_interest <- c('datetime',
                     'FC', 'FCH4', # Ecosystem productivity and C fluxes
                     'VPD', 'RH',  'TA', 'PPFD_IN', 'SW_IN', # Meteorological variables
                     'USTAR', # Atmospheric stability / roughness length approximation (proxy for 'good' EC conditions)
                     'LE','H', 'NETRAD', 'G' # Surface energy balance
)

data_all <- data.frame()

# Loop through all sites to create a dataframe with data from all sites

for (i in 1:length(sites)) {
  # Open data at site
  site <- sites[i]
  
  yrs <- yrs_included(basepath,site,level)
  
  # Load data
  data_in <- read_data_all_years(basepath,yrs,site,level,tv_input)
  
  cols <- colnames(data_in) # Get column names
  
  # remove HVR qualifiers
  vars <- gsub("_[[:digit:]]", "", cols) 
  colnames(data_in) <- vars
  
  # Create empty column for missing variables of interest
  data_in[var_of_interest[!(var_of_interest %in% vars)]] = NA
  
  # Get index of each column of variable of interest
  indexvar <- c()
  for (j in 1:length(var_of_interest)) {
    indexvar[j] <- which(names(data_in) %in% var_of_interest[j])
  }
  indexvar <- na.omit(indexvar)
  
  data_subset <- data_in[,indexvar]
  
  # Add 'site' variable to dataframe
  data_subset$site <- site
  
  # Merge with other sites
  if (i == 1){
    data_all <- data_subset
  } else {data_all <- merge(data_all, data_subset, all = T)
  }
  
  print(unique(data_subset$site))
}

# Define units for plot with all sites
data_units_all <- var_units(colnames(data_all),UnitCSVFilePath)

# X variable names (all sites)
xvars <- var_of_interest
# Y variable names (all sites)
yvars <- var_of_interest[-length(var_of_interest)]

# Load site coordinates
sites_coordinates <- read_excel(coordinatesXLSXFilePath,sheet=1,col_names= TRUE)

# save data to app folder
save(list = ls(), file = file.path(arg, "data", "all_data.RData"))

# Tim Elrick, 2024-12-19: save date to only run this once a day
write(as.character(today()), file = file.path(arg, "data", "updated.txt"))
