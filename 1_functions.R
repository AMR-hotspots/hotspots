###*********************************************###
### HOTspots: Tracking Antimicrobial Resistance ###
###*********************************************###
# Author: Alys Young
# Collaborators: Saras Windecker and Nick Golding
#
# Project aim: Create a shiny app to map antimicrobial resistance 
# Script aim: Create functions


# documentation - code > insert 

# do this
# state the packages for the files to be explicit, using ::



#' Change plain text/url for the social media share message
#'
#' @description Takes normal text and the url that will be linked to the social media share buttons, and turns it into the correct syntax. 
#' @param message Character string with a message or url.
#'
#' @return A character string with the correct syntax to feed into href.
#'
#' @examples
#' text_to_code("Hello, here is your message")
#' text_to_code("www.google.com")
text_to_code <- function(message){
  
  replaced <- message %>%
    stringr::str_replace_all( ":", "%3A") %>%
    str_replace_all( "/", "%2F") %>%
    str_replace_all( " ", "%20")
  return(replaced)
}



#' List regions by their jurisdiction
#' @description Create a list of regions, seperated into the headings by their jurisdiction. The regions are sub-parts of the jurisdiction. 
#' This is can be used as the 'choices' argument in a shiny widget to create the jurisdiction are subheading when selecting a region.
#' If only one region occurs in a jurisdiction, the region is added twice so that the subheading still appears when fed into a shiny widget. 
#'
#' @param data The dataset containing the columns "jurisdiction" and "region".
#'
#' @return List where the names are the jurisidctions and the regions are within the relevant jurisdiction. This is can be used as the 'choices' argument in a shiny widget.
#'
#' @examples
#' df <- data.frame(region  = c("Melbourne", "Geelong", "Sydney"), jurisdiction = c("Victoria", "Victoria", "New South Wales"))
#' list_reg_by_jur(data  = df)
list_reg_by_jur <- function(data){
  
  # the data
  # data <- data
  
  # empty list that the options will end up in
  regional_lists_by_jur <- c()
  
  # The unique jurisdictions
  j <- unique(data$jurisdiction)
  
if (length(j) == 0){
  regional_lists_by_jur <- list("None - choose other inputs")
  
} else if (length(j) > 0){
  
  # For loop to create a list of the regions with the jurisdictions as the names
  for(i in 1:length(j)){
    
    # the regions
    reg <- unique(data$region[data$jurisdiction == j[i]])
    if(length(reg)==1) {
      reg2 <-  c(reg, "-" = reg)
    } else {
      reg2 <- reg
    }
    reg_l <- list(sort(reg2))
    
    
    # set the names of the lists 
    if(j[i] == "FNQ"){
      names(reg_l) <- "Far North Queensland"
    } else   if(j[i] == "NT"){
      names(reg_l) <- "Northern Territory"
    } else   if(j[i] == "WA"){
      names(reg_l) <- "Western Australia"
    } else {
      names(reg_l) <- j[i]
    }
    
    # save the list
    regional_lists_by_jur <- c(regional_lists_by_jur, reg_l )
  }
}
  
  return(regional_lists_by_jur)
}



#' Set the initial value of a shiny widget to be blank with a placeholder message
#'
#' @param variable A character string for the variable being selected in the widget. This will appear in the placeholder as "please select variable"
#'
#' @return list which is used in the options argument of a shiny widget
#'
#' @examples
#' start_empty("a region")
#' selectInput(inputId = "region",
#' label = "Select a region:",
#' choices = c("Melbourne", "Geelong", "Sydney"),
#' options = start_empty("antibiotic") )
start_empty <- function(variable){
  list(
    placeholder = paste('Please select', variable),
    onInitialize = I('function() { this.setValue(""); }')
  )
}



#' Inset logo image
#' @description Creates object to be put into a shiny::fluidRow() to display the logo images together 
#'
#' @param image_name Name of the logo image to input including extension
#'
#' @return
#' 
#' @example 
#' fluidRow( 
#' column(3, image_logo('Logo_HOTspots.png')),
#' column(2, image_logo('Logo_Menzies.png')), 
#' column(2, image_logo('Logo_HotNorth.png')),
#' column(5))

image_logo <- function(image_name) {
  img(src  = image_name, align = "left", width = '100%', height = 'auto')
}



# Clean data
# raw_file must be located in the www/data folder
#' Clean antimicrobial resistance data
#' @description Takes the raw files, modifies the monthly and split by age data, changes the region names to be more descriptive, removes VRE as a bug.
#'
#' @param raw_file The name of the file containing the raw data which must be located in the www/data folder.
#'
#' @return A dataframe.

clean_data <- function(raw_file) {
  
  # Read in data
  df <- read.csv(paste0("www/data/", raw_file))
  
  # Find only the monthly data
  # Change the text coloumn for the month into a date with the day as the first of the month
  if(any(colnames(df) == "month_year")){
    df$date_dmy <- as.Date(paste("01", df$month_year), format = "%d %b %y")
  }
  
  # Find only the data split by age groups
  # and set the levels - not sure this works within the plotly environment
  if(any(colnames(df) == "age_group")){
    df$age_group <- as.factor(df$age_group)
    age_levels <- c("<NA>", "unknown", "0-5", "6-15", "16-25", "26-40", "41-60", "61-80", "81+")
    df$age_group <- factor(df$age_group, levels = age_levels)
  }
  
  # Change the region names to be more descriptive
  df$region[df$region == "North West"] <- "North Western Queensland"
  df$region[df$region == "Mid East"] <- "Mid-eastern Western Australia"
  df$region[df$region == "Mid West"] <- "Mid-western Western Australia"
  df$region[df$region == "South"] <- "Southern Western Australia"
  
  # remove VRE as an organism
  df <- df[which(df$organism != "VRE"),]
  
  return(df)
}
