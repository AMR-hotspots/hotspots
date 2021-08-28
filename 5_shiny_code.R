###*********************************************###
### HOTspots: Tracking Antimicrobial Resistance ###
###*********************************************###
# Author: Alys Young
# Collaborators: Saras Windecker and Nick Golding
#
# Project aim: Create a shiny app to map antimicrobial resistance 
# Script aim: Pieces of shiny code

# Logos
show_logos <- fluidRow( 
  column(3, image_logo('Logo_HOTspots.png')),
  column(2, image_logo('Logo_Menzies.png')), 
  column(2, image_logo('Logo_HotNorth.png')),
  column(5)
)

# Summary boxes with facts
# at the top of the landing page
summary_boxes <-                       
  fluidRow(
    column(2, image_logo('HOTspots')), #, width = "250px"
    valueBoxOutput("VBox_organism", width = 2), # , width = 2
    valueBoxOutput("VBox_antibiotic", width = 2),
    valueBoxOutput("VBox_regions", width = 2),
    valueBoxOutput("VBox_year", width = 2),
    valueBoxOutput("VBox_tests", width = 2)
    #
  )


# Website url and message
website_url <- text_to_code(website_url_clean)
website_message <- text_to_code(website_message_clean)


## create the list of regions by their jurisdiction
regional_lists_by_jur  <- list_reg_by_jur(hotspot_yearly_data)

# Add an all button
ls_all <- list("All")
names(ls_all) <- "All"
regional_lists_andAll <- c(ls_all, regional_lists_by_jur)
