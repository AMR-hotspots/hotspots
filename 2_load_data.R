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
hotspot_monthly_data <- read.csv("www/data_clean/HOTspots_monthly.csv")
hotspot_yearly_data <- read.csv("www/data_clean/HOTspots_yearly.csv")
#hotspot_yearly_split <- read.csv("www/data_clean/HOTspots_yearly_age&sex.csv")
hotspot_yearly_splitage <- read.csv("www/data_clean/HOTspots_yearly_age.csv")
hotspot_yearly_splitsex <- read.csv("www/data_clean/HOTspots_yearly_sex.csv")

hotspot_monthly_data$date_dmy <- as.Date(hotspot_monthly_data$date_dmy)


# Consider a timer or a sceduler that will run the datamanipulation script every time the data is updated

# If the data changes:
# Need to add more colours to the colour palette in the 4_aesthetics.R file


###  Data cleaning as informed by experts----------------------------------------------------------------------------------------------------------------------------

# Read in the data
hotspot_monthly_data <- cleanData_byExpertKnowledge(hotspot_monthly_data)
hotspot_yearly_data <- cleanData_byExpertKnowledge(hotspot_yearly_data)
hotspot_yearly_splitage <- cleanData_byExpertKnowledge(hotspot_yearly_splitage)
hotspot_yearly_splitsex <- cleanData_byExpertKnowledge(hotspot_yearly_splitsex)

hotspot_monthly_data$date_dmy <- as.Date(hotspot_monthly_data$date_dmy)



### Locations -------------------------------------------------------------------------------------------------------

# Shapefile map of Austalia
# Filtered to the regions with data
SA3_data <- sf::st_read("www/data_clean/Australian_regions/Aus_regions.shp")

# Locations of cities as points
# Read in the csv file
cities_names <- read.csv("www/data_clean/Australian_regions/Cities.csv")
