###*********************************************###
### HOTspots: Tracking Antimicrobial Resistance ###
###*********************************************###
# Author: Alys Young
# Collaborators: Saras Windecker and Nick Golding
#
# Project aim: Create a shiny app to map antimicrobial resistance 
# Script aim: Load and clean data


### Resistance Data ----------------------------------------------------------------------------------------------------------------------------

# Read in the data
hotspot_monthly_data <- clean_data(raw_file = "HOTspots_monthly.csv")
hotspot_yearly_data <- clean_data(raw_file = "HOTspots_yearly.csv")
hotspot_yearly_split <- clean_data(raw_file = "HOTspots_yearly_age&sex.csv")
hotspot_yearly_splitage <- clean_data(raw_file = "HOTspots_yearly_age.csv")
hotspot_yearly_splitsex <- clean_data(raw_file = "HOTspots_yearly_sex.csv")

# Consider a timer or a sceduler that will run the datamanipulation script every time the data is updated

# If the data changes:
# Need to add more colours to the colour palette in the 4_aesthetics.R file


### Locations -------------------------------------------------------------------------------------------------------

# Shapefile map of Austalia
# Filtered to the regions with data
SA3_data <- rgdal::readOGR("www/data/Australian_regions/Aus_regions.shp") %>%
  subset(SA3_NAME16 %in% unique(hotspot_yearly_data$region))


# Locations of cities as points
# Read in the csv file
cities_names <- read.csv("www/data/Australian_regions/Cities.csv")