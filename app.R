###*********************************************###
### HOTspots: Tracking Antimicrobial Resistance ###
###*********************************************###
# Author: Alys Young
# Collaborators: Saras Windecker and Nick Golding
#
# Project aim: Create a shiny app to map antimicrobial resistance 
# Script aim: Load packages and run shiny app with data visualisations


### Set up -----------------------------------------------------------------------------------------------------------------------------------------------

# Clear the environment
# rm(list = ls())

# Deploy app
# if (!require(rsconnect)) install.packages('rsconnect')
# rsconnect::deployApp(account = 'anti-microbe-res')


### Packages -----------------------------------------------------------------------------------------------------------------------------------------------

# Install 
# if (!require(shiny))           install.versions('shiny', '1.6.0')
# if (!require(shinythemes))     install.versions('shinythemes', '1.2.0')
# if (!require(rgdal))           install.versions('rgdal', '1.5-23')
# if (!require(ggplot2))         install.versions('ggplot2', '3.3.5')
# if (!require(dplyr))           install.versions('dplyr','1.0.7')
# if (!require(leaflet))         install.versions('leaflet', '2.0.4.1 ')
# if (!require(shinydashboard))  install.versions('shinydashboard', '0.7.1')
# if (!require(shinyWidgets))    install.versions('shinyWidgets', '0.6.0')
# if (!require(shinycssloaders)) install.versions('shinycssloaders', '1.0.0')
# if (!require(DT))              install.versions('DT', '0.18')
# if (!require(tidyr))           install.versions('tidyr', '1.1.3')
# if (!require(plotly))          install.versions('plotly', '4.9.4.1')
# if (!require(shinyalert))      install.versions('shinyalert', '2.0.0')
# if (!require(rmarkdown))       install.versions('rmarkdown', '2.10')
# if (!require(lemon))           install.versions('lemon', '0.4.5')
# if (!require(stringr)) install.versions('stringr', '1.4.0')


# Suppress summarise info in the dplyr package
options(dplyr.summarise.inform = FALSE)

# Load
library(shiny)           # to create  the shiny app
library(shinythemes)     # for the theme
library(ggplot2)         # to make plots
library(dplyr)           # to clean up code and allow piping %>%
library(leaflet)         # for interactive maps
#library(rgdal)           # to open shapefiles of areas to map
library(shinydashboard)  # for valuebox
library(shinyWidgets)    # for additional widgets 
library(shinycssloaders) # for the shiny spinning loaders
library(shinyjs)         # for shiny show and hide
library(tidyr)           # for data cleaning
library(plotly)          # for making ggplots into interactive plots
library(shinyalert)      # to create the pop up on loading the app
# library(rmarkdown)     # when making a report
library(lemon)         # to make the x axis labels show up when ggplots are facetted over multiple rows
library(stringr)         # string manipulations


### Source Code -----------------------------------------------------------------------------------------------------------------------------------------------
# These scripts are dependent on eachother
  # e.g. The load_data.R scipt required functions provided in functions.R
# Therefore, they must be loaded in this order

# The minor parts of the code that will be edited by other users can be found in 3_edit_this.R
# including the email, dates,

source("1_functions.R")
source("2_load_data.R")
source("3_optional_edits.R")
source("4_aesthetics.R")
source("5_shiny_code.R")

### Finish set up -----------------------------------------------------------------------------------------------------------------------------------------------



##*******************##
## 2. User Interface ## -------------------------------------------------------------------------------------------------------------------------------
##*******************##

ui <- fluidPage(
  
  # Load shiny pages
  useShinydashboard(),
  useShinyjs(),
  useShinyalert(),
  
  #options(shiny.sanitize.errors = TRUE), # use this to change error messages to something generic
  tags$head(tags$style(".shiny-output-error{visibility: hidden}")),
  tags$head(tags$style(".shiny-output-error:after{content: 'An error has occurred. Please try different inputs on the left hand side. Contact the website administrators on HOTspots@menzies.edu.au if the error persists.'; visibility: visible}")),
  
  # Make the 8th object in the navigation bar (the share button) go to the righthand side
  # If you add more objects to the navigation bar, then you much update this code so that nth-child(8) has the number that the share button is
  # Make the microbe names be in italics
  tags$head(tags$style(HTML("
                           .navbar-nav {float: none !important;}
                           .navbar {font-size: 15px;}
                           .navbar-nav > li:nth-child(8) {float: right;}
                           .small-box {height: 85px; margin-bottom: 0px;}
                           
                            #microbe_name+ div>.selectize-input{font-style: italic;}
                            #microbe_name+ div>.selectize-dropdown{font-style: italic;}
                            #microbe_name_spec+ div>.selectize-input{font-style: italic;}
                            #microbe_name_spec+ div>.selectize-dropdown{font-style: italic;}
                            #microbe_name_filt+ div>.selectize-input{font-style: italic;}
                            #microbe_name_filt+ div>.selectize-dropdown{font-style: italic;}
                           "))),
  
  navbarPage(title ="", # title for the title bar, could add the logo or symbols
             id="nav", selected = NULL, collapsible = TRUE, 
             theme = shinytheme("flatly"),
             #tags$style(type='text/css', '.navbar {font-size: 13px;}'),
             
             
             ## Tab 1 - Map --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
             # landing page 
             tabPanel("Map", icon = icon("map"), # Name on the navigation bar

                      summary_boxes,
                      
                      br(),
                      
                      # Sidebar layout with input and output definitions
                      sidebarLayout(
                        
                        # Sidebar ********************************************************************************************************************
                        sidebarPanel(
                          
                          # Select the location where the infection was identified
                          radioButtons(inputId = "onset",
                                       label = "Select healthcare setting:",
                                       selected = character(0),
                                       choices = rev(unique(hotspot_yearly_data$onset))), # change this when animal data is added rev(unique(hotspot_yearly_data$onset))
                          
                          
                          # Help text to prompt selection parameters
                          conditionalPanel(
                            condition = "input.onset == null", 
                            helpText("Please select healthcare setting")
                          ),
                          
                          # Select the site of the human infection
                          conditionalPanel(
                            condition = "input.onset != null",
                            selectizeInput(
                              inputId  = "isolatetype",
                              label = "Select specimen type:",
                              choices = sort(unique(hotspot_yearly_data$sample_type)),
                              options = start_empty("specimen type")
                              ) 
                          ),
                          
                          
                          # Help text to prompt selection parameters
                          conditionalPanel(
                            condition = "input.onset != null  && input.isolatetype == '' ", 
                            helpText("Please select specimen type")
                          ),
                          
                          
                          conditionalPanel(
                            condition = "input.isolatetype != '' ",
                            
                            # Select the microbe name
                            selectizeInput(inputId = "microbe_name", 
                                           label = "Select organism:",
                                           choices = sort(unique(hotspot_yearly_data$organism)),
                                           options = start_empty("organism")
                                           ) 
                            
                          ),
                          
                          # Help text to prompt selection parameters
                          conditionalPanel(
                            condition = "input.isolatetype !=  ''  &&  input.microbe_name == '' ", 
                            helpText("Please select organism")
                          ),
                          
                          conditionalPanel(
                            condition = "input.microbe_name != '' ",
                            
                            # Select the microbe name
                            selectizeInput(inputId = "antibiotic_name",
                                           label = "Select antibiotic:",
                                           choices = sort(unique(hotspot_yearly_data$antimicrobial)),
                                           options = start_empty("antibiotic")
                                           ) 
                            
                          ),
                          
                          # Help text to prompt selection parameters
                          conditionalPanel(
                            condition = "input.microbe_name !=  ''  && input.antibiotic_name == '' ", 
                            helpText("Please select antibiotic")
                          ),
                          
                          
                          
                          
                          # A single slider to select the year
                          # sliderInput(inputId = "year",
                          #             label = "Select a single year:",
                          #             value = max(hotspot_yearly_data$year), # default value
                          #             min = min(hotspot_yearly_data$year),
                          #             max = max(hotspot_yearly_data$year),
                          #             step = 1,
                          #             sep = "",
                          #             round = TRUE,
                          #             ticks = FALSE),
                          # 
                          # Alter the style so there in no tail behind
                          #tags$style(type = "text/css", ".irs-bar { background: none; border-top: none; border-bottom: none;}
                          #           .irs-bar-edge {background: none; border: none;}"), # setting the blue bar to be nothing on the slider
                          
                          conditionalPanel(
                            condition = "input.antibiotic_name != '' ",
                            
                            sliderInput(inputId = "range_year",
                                        label = "Select a time period:",
                                        value = c(max(hotspot_yearly_data$year) - 5, max(hotspot_yearly_data$year)), # default value
                                        min = min(hotspot_yearly_data$year),
                                        max = max(hotspot_yearly_data$year),
                                        step = 1,
                                        sep = "",
                                        round = TRUE,
                                        ticks = FALSE),
                            
                            # A check box to change to colour blind friendly
                            checkboxInput("load_CB_friendly_map", "Colour-blind friendly palette", FALSE),
                            
                            # A check box to change to colour blind friendly
                            checkboxInput("load_show_cities", "Show cities", FALSE),
                            
                            # A button to load the map
                            actionButton(inputId  = "load_map", label = "Load map"),
                            
                            # message only shown once load button is clicked
                            hidden(p(id="please_wait", "Please wait while the map loads.")),
                            
                            # Date of data update
                            p(paste0("Data last updated ", date_updated))
                            
                          )
                          
                          
                          
                          # When the report is ready to be uploaded
                          # ,
                          # 
                          # # Button to download the report
                          # downloadButton("report", "Generate report"),
                          # 
                          # # message only shown once the report button is clicked
                          # hidden(p(id="please_wait2", "Please wait while the report is created."))
                          
                        ), # close side panel
                        
                        
                        
                        
                        # Main panel ********************************************************************************************************************
                        mainPanel(
                          # Title
                          h4(uiOutput("map_title")),
                          
                          # Map
                          leafletOutput("leaflet_map", height=600),
                          
                          # Citation
                          p(how_to_cite)
                          ) # close main panel
                      ) # close sidebar layout
             ), # close tabPanel
             
             
             
             
             
             
             
             
             
             
             
             
             ## Tab 2 - Plots -----------------------------------------------------------------------
             
             tabPanel(("Plots"), # name in nav bar
                      icon = icon("chart-bar"), # icon in nav bar
                      
                      sidebarLayout(
                        
                        
                        ### Side panel --------------------------------------------------------------
                        
                        
                        # for displaying inputs ********************************************************************************************************************
                        sidebarPanel(
                          
                          # When the plotting page is not comparing onset locations
                          conditionalPanel(
                            condition = "input.tab_plot != 'onset'",
                            
                            # Select the location where the infection was identified
                            radioButtons(inputId = "onset_spec",
                                         label = "Select healthcare setting:",
                                         selected = character(0),
                                         choices = rev(unique(hotspot_yearly_data$onset))), 
                            
                            
                            # When onset is not selected
                            conditionalPanel(
                              condition = "input.onset_spec == null", 
                              
                              # show this Help text to prompt selection parameters 
                              helpText("Please select healthcare setting")
                            )
                            
                          ),
                          
                          
                          # When an onset location is selected and the plots are not comparing the isolate types
                          conditionalPanel(
                            condition = "input.onset_spec != null && input.tab_plot != 'sample_type' ",
                            
                            # Select the site of the human infection
                            selectizeInput( inputId = "isolatetype_spec",
                                            label = "Select specimen type:",
                                            choices = sort(unique(hotspot_yearly_data$sample_type)),
                                            options = start_empty("specimen type")
                            )
                            
                          ), # close conditional panel
                          
                          
                          # Select the microbe name
                          selectizeInput(inputId = "microbe_name_spec", 
                                         label = "Select organism:",
                                         choices = sort(unique(hotspot_yearly_data$organism)),
                                         multiple = FALSE,
                                         options = start_empty("organism")
                          ), 
                          
                          
                          ## If the tab is NOT comparing antimicrobes, allow only 1 antimicrobe to be selected                          conditionalPanel(
                          conditionalPanel(
                            condition = "input.tab_plot != 'antimicrobe' && input.microbe_name_spec != '' ",
                            
                            
                            # select an antimicrobe
                            selectizeInput(inputId = "antibiotic_name_spec1",
                                           label = "Select antibiotic:",
                                           choices = sort(unique(hotspot_yearly_data$antimicrobial)),
                                           multiple = FALSE,
                                           options = start_empty("antibiotic")
                                           )
                            
                          ),
                          
                          
                          
                          ## If the tab is comparing antimicrobe, allow multiple antimicrobes to be selected
                          conditionalPanel(
                            condition = "input.tab_plot == 'antimicrobe' && input.microbe_name_spec != '' ",
                            
                            # optional help text to tell the users about the change
                            #helpText("You may now select mutliple antimicrobials"),
                            
                            # select 1 or more antimicrobes
                            selectizeInput(inputId = "antibiotic_name_spec2",
                                           label = "Select multiple antibiotics:",
                                           choices = sort(unique(hotspot_yearly_data$antimicrobial)),
                                           multiple = TRUE,
                                           options = start_empty("antibiotics")
                                           )
                          ),
                          
                          
                          ## If the tab is NOT comparing regions, allow only 1 region to be selected                          conditionalPanel(
                          conditionalPanel(
                            condition = "input.tab_plot != 'region' && input.tab_plot != 'jurisdiction' && input.microbe_name_spec != '' ",
                            
                            # select 1 region
                            selectizeInput(inputId = "region_spec1",
                                           label = "Select region:",
                                           choices = regional_lists_by_jur,
                                           multiple = FALSE,
                                           options = start_empty("regions")
                                           )
                          ),
                          
                          # ## If the tab is comparing regions, allow 1 or more regions to be selected                          conditionalPanel(
                          # conditionalPanel(
                          #   condition = "input.tab_plot == 'region' ",
                          #   
                          #   # optional help text to tell the user about the change
                          #   #helpText("You may now select mutliple regions"),
                          #   
                          #   # select 1 or more regions
                          #   selectInput(inputId = "region_spec2",
                          #               label = "Select multiple regions:",
                          #               choices = regional_lists_by_jur,
                          #               selected = sort(as.vector(unlist(regional_lists_by_jur)))[1:10],
                          #               multiple = TRUE)
                          #   
                          #   
                          # ),
                          # 
                          
                          # if region or antimicrobial
                          conditionalPanel(
                            condition = "input.tab_plot == 'region' || input.tab_plot == 'antimicrobe'  ",
                            
                            conditionalPanel(
                              condition = "input.microbe_name_spec != ''  ",
                              
                              checkboxInput("compare_reg_AddJur", label = "Add jurisdiction average", value = TRUE)
                            )
                          ),
                          br(),
                          
                          # When comparing antimicrobes, allow year to be selected
                          conditionalPanel( 
                            condition = "input.tab_plot == 'antimicrobe' && input.antibiotic_name_spec2 != '' ",
                            
                            sliderInput(inputId = "year_spec",
                                        label = "Select a year:",
                                        value = c(max(hotspot_yearly_data$year)-4, max(hotspot_yearly_data$year)), # default value
                                        min = min(hotspot_yearly_data$year),
                                        max = max(hotspot_yearly_data$year),
                                        step = 1,
                                        sep = "",
                                        round = TRUE,
                                        ticks = FALSE)
                          ),
                          
                          # A button to load the map
                          actionButton(inputId  = "load_plot", label = "Load plots"),
                          
                          helpText(plotly_description)
                          
                          # ,
                          # # A button to load the plot
                          # actionButton(inputId  = "load_plot", label = "Load plot"),
                          
                          
                          # For a single year only
                          # sliderInput(inputId = "year_spec",
                          #             label = "Select a year:",
                          #             value = max(hotspot_yearly_data$year), # default value
                          #             min = min(hotspot_yearly_data$year),
                          #             max = max(hotspot_yearly_data$year),
                          #             step = 1,
                          #             sep = "",
                          #             round = TRUE,
                          #             ticks = FALSE)
                          
                        ), # Close side bar panel
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        ### Main panel --------------------------------------------------------------
                        
                        
                        # Main panel for displaying outputs ********************************************************************************************************************
                        mainPanel(
                          
                          # tabs on the main panel
                          tabsetPanel(id = "tab_plot",
                                      
                                      
                                      ### * By antibiotic -----------------------------------------------------------
                                      tabPanel(("By antibiotic"),
                                               value="antimicrobe",
                                               
                                               
                                               # Title
                                               h4(uiOutput("text_compare_anti")),
                                               
                                               # plot to compare antimicrobials with a spinner when loading
                                               plotlyOutput("plot_compare_anti") %>% withSpinner(type = 5),
                                               
                                               br(),
                                               
                                               h4(uiOutput("text_compare_anti2")),
                                               span(uiOutput("text_anti2_warning")), #style = "color:red"
                                               plotlyOutput("plot_compare_anti2") %>% withSpinner(type = 5) # Height is dynamic in the server, but can also be static in the UI using  - , height = "750px" in the plotlyOutput function
                                      ),
                                      
                                      
                                      ### * By jurisdiction -------------------------------------------------------------
                                      tabPanel(("By jurisdiction"),
                                               value = "jurisdiction",
                                               
                                               
                                               # Title
                                               h4(uiOutput("text_compare_jur")),
                                               
                                               # note that this plot is a plotly and is therefore interactive
                                               uiOutput("text_ggplotly"),
                                               
                                               # Plotly output comparing the jurisdictions
                                               #plotlyOutput("plot_compare_jur") %>% withSpinner(type = 5),
                                               
                                               # Plotly output comparing the jurisdictions with uncertainty
                                               plotlyOutput("plot_compare_jur_uncertain", height = 600) %>% withSpinner(type = 5)
                                               
                                      ),
                                      
                                      
                                      ### * By region -------------------------------------------------------------
                                      tabPanel(("By region"),
                                               value = "region",
                                               
                                               
                                               
                                               
                                               # Title
                                               h4(uiOutput("text_compare_reg")),
                                               p("The mean antimicrobial resistance for each reigon, grouped by jurisdiction. The coloured bars represent the regions as explained in the legend. The black line is the mean for all the regions in the jurisdiction. This jurisdictional average can be turned on or off using the checkbox on the left."),
                                               helpText("Click on the regions in the plot legend to show and hide them on the plot."),
                                               
                                               #Plot 
                                               plotlyOutput("plot_compare_reg", height = 600) %>% withSpinner(type = 5)
                                               
                                      ),
                                      
                                      
                                      
                                      ### * By sample ---------------------------------------------------------------
                                      tabPanel(("By sample"),
                                               value = "sample_type",
                                               
                                               
                                               helpText("Click on the sample types in the plot legend to show and hide them on the plot."),
                                               
                                               
                                               # Title
                                               h4(uiOutput("text_compare_sample")),
                                               
                                               # Plot of resistance
                                               plotlyOutput("plot_compare_sample", height = 300) %>% withSpinner(type = 5),
                                               
                                               br(),
                                               
                                               # Plot of number of isolates
                                               plotlyOutput("plot_compare_sample_tests", height = 300) %>% withSpinner(type = 5)
                                               
                                      ),
                                      
                                      ### * By Healthcare setting ---------------------------------------------------------------
                                      
                                      tabPanel(("By healthcare setting"),
                                               value = "onset",
                                               
                                               helpText("Click on the heathcare setting in the plot legend to show and hide them on the plot."),
                                               
                                               # Title
                                               h4(uiOutput("text_compare_onset")),
                                               
                                               # Plot of resistance
                                               plotlyOutput("plot_compare_onset", height=300) %>% withSpinner(type = 5), ## update not working
                                               
                                               br(),
                                               
                                               # Plot of number of isolates
                                               plotlyOutput("plot_compare_onset_tests", height=300) %>% withSpinner(type = 5)
                                               
                                      ), # close tab panel
                                      
                                      
                                      ### * By age ------------------------------------------------------------------
                                      tabPanel(("By age"),
                                               value = "age",
                                               
                                               
                                               helpText("Click on the age groups in the plot legend to show and hide them on the plot."),
                                               
                                               # Title
                                               h4(uiOutput("text_age")),
                                               
                                               # Warning text about low sample size
                                               uiOutput("text_age_warning"),
                                               
                                               # Plot
                                               plotlyOutput("plot_compare_age", height = 300) %>% withSpinner(type = 5),
                                               
                                               br(),
                                               
                                               # Plot of number of isolates
                                               plotlyOutput("plot_compare_age_tests", height = 300) %>% withSpinner(type = 5)
                                               
                                               
                                      ), # close tab panel
                                      
                                      
                                      ### * By sex ------------------------------------------------------------------
                                      tabPanel(("By sex"),
                                               value = "sex",
                                               
                                               # Title
                                               h4(uiOutput("text_sex")),
                                               
                                               # Plot
                                               plotlyOutput("plot_compare_sex", height = 300) %>% withSpinner(type = 5),
                                               
                                               br(),
                                               
                                               # Plot of number of isolates
                                               plotlyOutput("plot_compare_sex_tests", height = 300) %>% withSpinner(type = 5)
                                               
                                               
                                      ), # Close tab panel
                                      
                                      
                                      ### * By month ----------------------------------------------------------------
                                      
                                      
                                      tabPanel(("By month"),
                                               value = "monthly",
                                               
                                               # Warning about low ioslates
                                               span(uiOutput("text_monthly_low_isolates"), style="color:red"),
                                               
                                               # Title
                                               h4(uiOutput("text_spec")),
                                               
                                               # Plot
                                               plotlyOutput("plot_monthly", height=300) %>% withSpinner(type = 5),
                                               
                                               
                                               # Plot
                                               plotlyOutput("plot_monthly2", height=300) %>% withSpinner(type = 5)
                                               
                                      ) # close Tabpanel
                          ) # close tabset panel
                        )# close main panel
                      )# close sidebar layout
             ), # close tabpanel
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             # Tab 3 - Antibiogram  -----------------------------------------------------------------------------------------------------
             
             tabPanel(("Antibiogram"), # name in nav bar
                      icon = icon("table"), # icon in nav bar
                      value="antibiogram",
                      
                      
                      
                      # Sidebar layout with input and output definitions
                      sidebarLayout(
                        
                        # Sidebar panel for inputs on the left ********************************************************************************************************************
                        sidebarPanel(
                          
                          # Select the location where the infection was identified
                          radioButtons(inputId = "onset_table",
                                       label = "Select healthcare setting:",
                                       selected = character(0),
                                       choices = rev(unique(hotspot_yearly_data$onset))),
                          
                          # If onset is empty
                          conditionalPanel(
                            condition = "input.onset_table == null", 
                            
                            # Hlep text to prompt selected
                            helpText("Please select healthcare setting")
                          ),
                          
                          # If an onset location has been selected
                          conditionalPanel(
                            condition = "input.onset_table != null",
                            
                            # Select the site of the human infection
                            selectizeInput(inputId = "isolatetype_table",
                                           label = "Select specimen type:",
                                           choices = sort(unique(hotspot_yearly_data$sample_type)),
                                           options = start_empty("specimen type")
                            )
                          ),
                          conditionalPanel(
                            condition = "input.isolatetype_table != '' ",
                            
                            # select 1 region
                            selectizeInput(inputId = "region_table",
                                           label = "Select region:",
                                           choices = regional_lists_by_jur,
                                           multiple = FALSE,
                                           options = start_empty("region")
                            )
                          ),
                          
                          
                         
                          conditionalPanel(
                            condition = "input.region_table != '' ",
                            
                            # Slider to select a range of years
                            sliderInput(inputId = "range_table",
                                        label = "Select a time period:",
                                        value = c(max(hotspot_yearly_data$year) - 5, max(hotspot_yearly_data$year)), # default value
                                        min = min(hotspot_yearly_data$year),
                                        max = max(hotspot_yearly_data$year),
                                        step = 1,
                                        sep = "",
                                        round = TRUE,
                                        ticks = FALSE),
                            
                            # A check box to change to colour blind friendly
                            checkboxInput("load_CB_friendly_antibiogram", "Colour-blind friendly palette", FALSE),
                            
                            # A button to load the map
                            actionButton(inputId  = "load_antibiogram", label = "Load antibiogram")
                            
                            
                          )
                          
                          
                          # 
                          # 
                          # 
                          # 
                          # # Select a region
                          # selectInput(inputId = "region_table",
                          #             label = "Select region:",
                          #             choices = regional_lists_by_jur,
                          #             selected = regional_lists_by_jur[[1]][1],
                          #             multiple = FALSE),
                          # 
                          # # Slider to select a range of years
                          # sliderInput(inputId = "range_table",
                          #             label = "Select a time period:",
                          #             value = c(max(hotspot_yearly_data$year) - 5, max(hotspot_yearly_data$year)), # default value
                          #             min = min(hotspot_yearly_data$year),
                          #             max = max(hotspot_yearly_data$year),
                          #             step = 1,
                          #             sep = "",
                          #             round = TRUE,
                          #             ticks = FALSE),
                          # 
                          # # A check box to change to colour blind friendly
                          # checkboxInput("load_CB_friendly_antibiogram", "Colour-blind friendly palette", FALSE),
                          # 
                          
                          # A slider to select a single year
                          #tags$style(type = "text/css", ".irs-bar { background: none; border-top: none; border-bottom: none;}
                          #           .irs-bar-edge {background: none; border: none;}"), # setting the blue bar to be nothing on the slider
                          
                          # sliderInput(inputId = "year_table",
                          #             label = "Select a year:",
                          #             value = max(hotspot_yearly_data$year), # default value
                          #             min = min(hotspot_yearly_data$year),
                          #             max = max(hotspot_yearly_data$year),
                          #             step = 1,
                          #             sep = "",
                          #             round = TRUE,
                          #             ticks = FALSE),
                          
                          
                        ), # close side panel
                        
                        
                        
                        # Main panel for displaying outputs ********************************************************************************************************************
                        mainPanel(
                          
                          # Title
                          h4(textOutput("antibiogram_text")),
                          
                          # Table
                          DT::dataTableOutput("antibiogram_table") %>% withSpinner(type = 5),
                          br(),
                          br(),
                          
                          # Citation at the bottom of the page
                          p(how_to_cite)
                          
                        ) # close main panel
                      ) # close sidebar layout
             ), # close tab panel
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             # Tab 4 - Data -----------------------------------------------------------------------------------------------------
             tabPanel(("Data and Methods"),
                      icon = icon("list-ul"),
                      
                      ###  Data disclaimer -----------------------------------------------------------------------------------------
                      
                      # Title
                      h3("Data disclaimer"),
                      
                      # Text
                      p("Unless otherwise stated, the information contained in the dataset is provided by the laboratories Territory Pathology (Northern Territory), PathWest (Western Australia), Pathology Queensland (Queensland) and Western Diagnostics (WA and NT data). 
                        With respect to the HOTspots dataset provided by Menzies School of Health Research (‘Menzies), and to the extent permitted by law, neither Menzies nor or any of its employees, makes any warranty, express or implied, or assumes any legal liability or responsibility for the accuracy, completeness, or usefulness of any information (either isolated or in the aggregate) contained, or represents that its use would not infringe privately owned rights. 
                        While the data is provided in good faith and to the best of Menzies knowledge, Menzies does not commit to it being updated. While every effort is made to ensure the data quality, the data is provided 'as is'. Menzies or HOTspots investigators are not responsible for data management after extraction and transmission to the recipient. 
                        The data and information in the dataset provided here are intended for use by persons possessing some technical skill and knowledge in epidemiology, surveillance or data management."),
                      p("In order to use the HOTspots extracted datasets provided users must adhere to the following guidelines:"),
                      p("•	consider whether a breach of confidentiality is likely due to a low cell count and make no use of the identity of any person discovered inadvertently;"),
                      p("•	not to distribute or sell the datasets to any other individual, institution, or organization without the prior written consent of Menzies and HOTspots investigators."),
                      p("The accuracy of the users' statistical analysis and the findings they report are not the responsibility of Menzies or HOTspots investigators. Menzies or HOTspots investigators shall not be held liable for improper or incorrect use of the data. 
                        In no event shall Menzies or HOTspots be liable for any incidental, indirect, consequential or special damages of any kind, or any damages whatsoever, including, without limitation, those resulting from loss of profit, loss of contracts, goodwill, or business relationships, arising out of or in connection with the use of the data. 
                        Menzies do not warrant that the files, the servers and the databases used for data storage, extraction, management and transmission are error, virus or bug free and the recipient accepts that it is its responsibility to make adequate provision for protection against such threats."),
                      p("For more information, please see the", tags$a(href="https://www.menzies.edu.au/page/Policies/", "Menzies policy website"), ", including the", tags$a(href="https://www.menzies.edu.au/icms_docs/307159_Information_and_Privacy_Policy_-_2019.pdf", "Information and Privacy policy")),
                      
                      
                      ### Methods -------------------------------------------------------------------------------------------------------
                      
                      # Title
                      h3("Methodology"),
                      
                      # Text
                      p("Antibiotic susceptibility data have been contributed by four main pathology service providers across three jurisdictions in northern Australia. These are Territory Pathology (Northern Territory), Pathology Queensland, Western Diagnostics (Western Australia and Northern Territory) and PathWest (Western Australia)."),
                      p("Between pathology providers there were variations in the content and format of the supplied data, requiring a process of data cleaning and standardisation. This is in part due to the variation in antimicrobial susceptibility testing (AST) guidelines used (Northern Territory and Western Australia use CLSI while Queensland used CLSI to 30th  June 2012 and then moved to EUCAST), however it is also due to individual laboratory policies and processes and the availability of antimicrobial agents for testing. For example, Western Diagnostics, PathWest and Territory Pathology all use CLSI, which recommends agents that are important for routine testing against various organisms or organism groups (an antimicrobial panel), however other agents may be tested or reported selectively based on the institution's formulary or substituted with a more practical alternative where their activity is similar. Therefore, the number and type of antimicrobials tested against the same microbes varies between laboratories. The microbes reported also varied, however common pathogens were identified and these microbes are available to select from the dropdown menu."),
                      p("Data were harmonised across the three jurisdictions by standardising antimicrobial, microbe and sample type nomenclature. Regions within jurisdictions were based on classification by the Australian Bureau of Statistics, Statistical Area Level 3. The healthcare setting was determined by the type of facility at which the sample was collected. Duplicates were removed from the data by selecting the first isolate per person, per calendar year. The percentage of resistant isolates was calculated by dividing the number resistant by the total number of isolates tested. For years with <15 isolates collected and tested, these data (within the region of interest) were added to the following or previous year (or excluded if all 3 years had <15 isolates)."),
                      p("Territory Pathology provided minimum inhibitory concentrations, to which we applied the 2017 CLSI M100-S27 Performance Standards for AST (27th Edition). All other data were supplied as interpreted values: susceptible (including intermediate) and resistant. Data on age and sex was not available from Territory Pathology and PathWest, and PathWest data was only available by year."),
                      p(paste0("These methods were last updates on ", date_method_update, ".")),
                      br(),
                      
                      
                      
                      ### Explore the data ----------------------------------------------------------------------------------------------
                      
                      h3("Explore the data"),
                      
                      # Sidebar layout
                      sidebarLayout(
                        
                        # Sidebar panel for inputs on the left ********************************************************************************************************************
                        sidebarPanel(
                          
                          
                          # Select the location where the infection was identified
                          radioButtons(inputId = "onset_filt",
                                       label = "Select healthcare setting:",
                                       selected = "Overall",
                                       choices = rev(unique(hotspot_yearly_data$onset))), # change this when animal data is added rev(unique(hotspot_yearly_data$onset))
                          
                          # Select the isolate type
                          selectInput(inputId = "isolatetype_filt",
                                      label = "Select specimen type:",
                                      choices = sort(unique(hotspot_yearly_data$sample_type))), ## unique(hotspot_yearly_data$sample_type[hotspot_yearly_data$sample_oranism == "Human"])
                          
                          
                          # Select the microbe name
                          selectInput(inputId = "microbe_name_filt", 
                                      label = "Select organism:",
                                      choices = c("All", sort(unique(hotspot_yearly_data$organism))),
                                      selected = "All", # none selected as the default when the app opens
                                      multiple = FALSE), # cannot select multiple
                          
                          
                          # Select the anti-microbe
                          selectInput(inputId = "antibiotic_name_filt",
                                      label = "Select antibiotic:",
                                      choices = c("All",sort(unique(hotspot_yearly_data$antimicrobial))),
                                      selected = "All",
                                      multiple = FALSE),
                          
                          # Select the region
                          selectInput(inputId = "region_filt",
                                      label = "Select a region:",
                                      choices = regional_lists_andAll,
                                      selected = regional_lists_andAll[[1]][1],
                                      multiple = FALSE),
                          
                          br(),
                          
                          # A slider to select the year
                          # Alter the style so there in no tail behind
                          #tags$style(type = "text/css", ".irs-bar { background: none; border-top: none; border-bottom: none;}
                          #           .irs-bar-edge {background: none; border: none;}"), # setting the blue bar to be nothing on the slider
                          
                          # 
                          #                           radioButtons(inputId = "year_select_filt",
                          #                                        label = "Which years to display?",
                          #                                        selected = "All",
                          #                                        choices = c("All" = "All",
                          #                                                    "Single year" = "single")), # add in a range?
                          # 
                          # 
                          #                           conditionalPanel(  # conditional to display the year input only when the tab is antimicrobe
                          #                             condition = "input.year_select_filt == 'single'",
                          # 
                          #                             sliderInput(inputId = "year_filt",
                          #                                         label = "Select a year:",
                          #                                         value = max(hotspot_yearly_data$year), # default value
                          #                                         min = min(hotspot_yearly_data$year),
                          #                                         max = max(hotspot_yearly_data$year),
                          #                                         step = 1,
                          #                                         sep = "",
                          #                                         round = TRUE,
                          #                                         ticks = FALSE)
                          #                           ),
                          
                          
                          # Select multiple years
                          sliderInput(inputId = "year_filt",
                                      label = "Select year(s):",
                                      value = c(max(hotspot_yearly_data$year)-5, max(hotspot_yearly_data$year)), # default value
                                      min = min(hotspot_yearly_data$year),
                                      max = max(hotspot_yearly_data$year),
                                      step = 1,
                                      sep = "",
                                      round = TRUE,
                                      ticks = FALSE),
                          
                          
                          
                          # Select which data (yearly or monthly) to display
                          checkboxGroupInput(inputId = "data_investigate", label ="Timeframe:",
                                             choices = c("Yearly data" = "yearly",
                                                         "Monthly data" = "monthly"),
                                             selected = "yearly"),
                          br(),
                          
                          # Title
                          h4("Download the data"),
                          
                          # Download buttons
                          downloadButton(outputId = "downloadData_yearly", label = "Full dataset"),
                          downloadButton(outputId = "downloadData_yearly_selected", label = "Selected")
                          
                          
                        ), # close the side bar
                        
                        
                        # Main panel for displaying outputs ********************************************************************************************************************
                        mainPanel(
                          
                          # When the show yearly data checkbox is selected
                          conditionalPanel(
                            condition = "input.data_investigate.indexOf('yearly') > -1", # yearly is selected in the checkboxes called data_investigate
                            
                            # Title
                            h4("Yearly data"),
                            
                            # Table
                            DT::dataTableOutput("table_data_year"),
                            br(),
                            br()
                            
                          ),
                          
                          # When the show yearly data checkbox is selected
                          conditionalPanel(
                            condition = "input.data_investigate.indexOf('monthly') > -1", # monthly is selected in the checkboxes called data_investigate
                            
                            # Title
                            h4("Monthly data"),
                            
                            # Table
                            DT::dataTableOutput("table_data_month"),
                            br(),
                            br()
                          ) # close conditional panel
                        )# close main panel
                      ), # close side bar layout
                      
                      br(),
                      br(),
                      
                      
                      ### Terms of Use -------------------------------------------------------------------------------------------------------
                      
                      # Title
                      h3("Terms of use"),
                      
                      # Text
                      p("Users must read and adhere to the terms of the HOTspots Data Disclaimer. Users must not use the datasets in any way which is inconsistent with the Privacy Act 1988 (Cth), the Information Act 2002 (NT), the HOTSpots Data Disclaimer or the HOTspots Terms of Use."),
                      p("The data and information in the dataset downloaded are intended for use by persons possessing technical skill and knowledge in epidemiology, surveillance and data management. Commercial use of the HOTspots data is not permitted without prior written consent from Menzies."),
                      p("Except where otherwise stated, downloading and reproduction of published (in paper or electronically) HOTspots data for personal use or for further non-commercial dissemination, are authorised provided appropriate acknowledgement is given to HOTspots investigators as the source. Any publication arising from the dataset provided should credit Menzies and HOTspots investigators in the relevant parts of the publication. Please contact the lead investigator Teresa.wozniak@menzies.edu.au to discuss further."), # TO DO ## add link to the email
                      
                      br(),   
                      
                      # Citation
                      p(how_to_cite),
                      
                      # Logos
                      show_logos
                      
             ), # close tab panel
             
             
             
             
             
             
             
             # Tab 5 - ResImpact  ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
             tabPanel("Economic Burden", icon = icon("calculator"),
                      
                      # warning at top
                      span(h3("This page is in development"), style="color:red"),
                      br(),
                      
                      # Title
                      h3("ResImpact"),
                      
                      # Description
                      p(tags$a(href="https://aushsi.shinyapps.io/costresistantecoli/", "ResImpact"), ", is an open-access tool based on a validated and transparent model developed as part of the ", tags$a(href="http://www.cre-rhai.org.au/projects/health-economic-modelling-of-antimicrobial-resistance-in-australia---hemaa", "Health and Economic Modelling of Antimicrobial resistance in Australia (HEMAA) project.")),
                      p("ResImpact was created by Dr Teresa Wozniak and Prof Adrian Barnett using data from Queenland before 2018."),
                      span(p("This page does not use the HOTspots data."), style="color:red"),
                      
                      br(),
                      
                      # Sidebar layout
                      sidebarLayout(
                        
                        # Sidebar panel for inputs on the left ********************************************************************************************************************
                        sidebarPanel(
                          
                          # Select bug
                          selectInput("bug", "Organism:",
                                      c( # List of bugs. The name on the left is what is displayed to the user. the right is how shiny reads it and the value that must be used in your code
                                        "ceftriaxone-resistant E. coli" = "ecoli",
                                        "ceftriaxone-resistant K. pneumoniae" = "Klebsiella",
                                        "ceftazidime-resistant P. aeruginosa" = "Pseudomonas",
                                        "MRSA" = "MRSA",
                                        "VRE" = "VRE"),
                                      selected='ecoli'),
                          
                          # BSI resistance
                          numericInput(inputId = "pDrugResBSI",
                                       label = "Probability BSI drug resistance:",
                                       min = 0,
                                       max = 1,
                                       step = 0.01,
                                       value = 0.05),
                          
                          
                          # Respiratory resistance
                          conditionalPanel(
                            condition = "input.bug == 'Pseudomonas' | input.bug == 'MRSA'",
                            numericInput(inputId = "pDrugResResp",
                                         label = "Probability respiratory drug resistance:",
                                         min = 0,
                                         max = 1,
                                         step = 0.01,
                                         value=0.05)),
                          
                          # UTI resistance
                          conditionalPanel(
                            condition = "input.bug == 'VRE'",
                            numericInput(inputId = "pDrugResUTI",
                                         label = "Probability UTI drug resistance:",
                                         min = 0,
                                         max = 1,
                                         step = 0.01,
                                         value=0.05))
                        ), # close sidebar
                        
                        # Main panel for output on the centre and righ ********************************************************************************************************************
                        
                        mainPanel(
                          
                          # Title
                          h4('Additional cost of treatment', tags$a(href="http://www.cre-rhai.org.au/projects/antibiotic-management-of-drug-resistant-infections-a-survey-of-clinical-practice", "(bloodstream infections only)")),
                          
                          # Text
                          textOutput(outputId = 'cost_text'),
                          br(),
                          
                          # Title
                          h4('Total accounting cost of bed days and treatment'),
                          
                          # Text
                          textOutput(outputId = 'account_text'),
                          br(),
                          
                          # Title
                          h4('Total opportunity cost of bed days and treatment'),
                          
                          # Text
                          textOutput(outputId = 'opp_text')
                          
                        ) # close main panel
                      ) # close sidebar layout
             ), # close tab panel
             
             
             
             
             
             
             
             ## Tab 6 - Drop down menu  ----------------------------------------------------------------
             navbarMenu(("More Information"),
                        icon = icon("info"),
                        
                        
                        
                        ### Therapeutic guidelines --------------------------------------------------
                        
                        tabPanel(("Therapeutic Guidelines"),
                                 
                                 # Title
                                 titlePanel(h1("Links to the therapeutic guidelines")),
                                 br(),
                                 tags$div("The", tags$a(href="https://www.tg.org.au", "Therapeutic Guidelines website"), ", and the", tags$a(href="https://tgldcdp.tg.org.au/fulltext/tglcontent/quicklinks/GPSummary_v11.pdf", "pdf"), "."),
                                 tags$div("The", tags$a(href="https://www.carpa.com.au", "Central Australia Rural Practioners Association (CARPA) website"), ", and the", tags$a(href="https://healthinfonet.ecu.edu.au/healthinfonet/getContent.php?linkid=592687&title=CARPA+standard+treatment+manual%3A+a+clinic+manual+for+primary+health+care+practitioners+in+remote+and+Indigenous+health+services+in+central+and+northern+Australia", "treatment manual pdf"), "."),
                                 
                                 br(),
                                 # Logos
                                 show_logos
                                 
                        )
                        , # close table panel
                        # 
                        # 
                        ### Publications  ----------------------------------------------------------------
                        tabPanel(("Publications"),
                                 
                                 # Title
                                 titlePanel(h1("Relevant publications")),
                                 
                                 # To add more publications, use the format below
                                 #p(tags$a(href="link", "Name")),
                                 
                                 publication_list,  
                                 
                                 # p(tags$a(href="https://bmchealthservres.biomedcentral.com/articles/10.1186/s12913-017-2079-5", " Page, et al., (2017)."), ". What is a hospital bed day worth? A contingent valuation study of hospital Chief Executive Officers. BMC Health Services Research 2017;17:137."),
                                 # p(tags$a(href="https://www.nature.com/articles/s41598-020-69312-4", "TM Wozniak, W Cuningham, S Buchanan, et al., (2020)."), "Geospatial epidemiology of Staphylococcus aureus in a tropical setting: an enabling digital surveillance platform. Scientific Reports 10, 13169."),
                                 # p(tags$a(href="https://academic.oup.com/cid/advance-article/doi/10.1093/cid/ciaa1228/5895480", "X Lee, A Stewardson, L Worth, N Graves, TM Wozniak (2020)."), "Attributable length of stay, mortality risk and costs of bacterial healthcare-associated infections in Australia: a retrospective case-cohort study."),
                                 # p(tags$a(href="https://peerj.com/articles/9409/", "W Cuningham, et al., (2020)."), "Antimicrobial stewardship in remote primary healthcare across northern Australia. PeerJ, 8."),
                                 # p(tags$a(href="https://onlinelibrary.wiley.com/doi/full/10.1111/1753-6405.12876", "W Cuningham, et al., (2019)."), "High burden of infectious disease and antibiotic use in early life in Australian Aboriginal communities."),
                                 # p(tags$a(href="https://www.cambridge.org/core/journals/infection-control-and-hospital-epidemiology/article/abs/health-and-economic-burden-of-antimicrobialresistant-infections-in-australian-hospitals-a-populationbased-model/A51CA4B0F6181C891F0B406823460C30", "TM Wozniak, E Bailey, N Graves (2019)."), "Health and economic burden of antimicrobial-resistant infections in Australian hospitals: a population-based model. Infection Control & Hospital Epi 40(3)320-7."),
                                 # p(tags$a(href="https://aricjournal.biomedcentral.com/articles/10.1186/s13756-019-0472-z", "TM Wozniak, L Barnsbee, X Lee, R Pacella (2019)."), "Using the best available data to estimate the cost of antimicrobial resistance: a systematic review. Antimicrobial Resistance and Infection Control 8:26."),
                                 # p(tags$a(href="https://aricjournal.biomedcentral.com/articles/10.1186/s13756-018-0379-0", "TM Wozniak (2018)."), "Estimating the burden of antimicrobial resistance. Antimicrobial Resistance and Infection Control 7: 91."),
                                 # p(tags$a(href="https://www.sciencedirect.com/science/article/pii/S2468045117302286", "TM Wozniak (2018)."), "Clinical management of drug-resistant bacteria in Australian hospitals: an online survey of doctors' opinions. Infection, Disease & Health 23(1); 41-48."),
                                 # p(tags$a(href="https://pubmed.ncbi.nlm.nih.gov/30479305/", "TM Wozniak, N Graves, A Barnett (2018)."), "How much do superbugs cost? An evidence-based open- access tool. Infection, Disease & Health 23 (1); 54-56."),
                                 # p(tags$a(href="https://idhjournal.com/article/S2468-0451(17)30067-6/fulltext", "TM Wozniak, D Paterson, K Halton (2017)."), "Review of the epidemiological data regarding antimicrobial resistance in gram(-) bacteria in Australia. Infection, Disease & Health 22(4); 210-218."),
                                 # p(tags$a(href="https://www.idhjournal.com.au/article/S2468-0451(16)30192-4/fulltext", "J Cameron, L Hall, TM Wozniak, K Halton (2016)."), "The burden of community onset MRSA in Australia Infection, Disease & Health. 21 (3). 140."),
                                 # 
                                 
                                 #note, to see the pdf you must be in the browser
                                 tags$iframe(style="height:600px; width:100%; scrolling=yes",
                                             src="EvaluationReport_Goddard_WebsiteUpload.pdf")
                                 
                        )
                        #, # close tab panel
                        # 
                        # ### News  ----------------------------------------------------------------
                        # tabPanel(("News"), 
                        #          
                        #          # Title
                        #          titlePanel(h1("News"))
                        #          
                        # ), # close tab panel                       
                        # 
                        # 
                        # ### Other  ----------------------------------------------------------------
                        # tabPanel(("Other resources"), 
                        #          
                        #          # Title
                        #          titlePanel(h1("Links to the News articles or blogs"))
                        #          
                        # ) # close tab panel
                        
             ), # close navbar drop down menu
             
             # Tab 7 - About us ----------------------------------------------------------------
             
             tabPanel("About Us",
                      icon = icon("user-circle"),
                      
                      # Title
                      h1("Meet the team"),
                      br(),
                      
                      # to create columns
                      fluidRow( 
                        
                        ## To add a new person, follow this format with the line uncommented
                        # column(4, img(src='about_us/NAME.png', align = "center", width = 'auto', height = '200px'),
                        #        p("Title Name, Role"),
                        #        p("optional contact details"),
                        #        p("Title Name blurb")),
                        # 
                        column(4, img(src = 'Teresa.png', align = "center", width = 'auto', height = '250px'),
                               p("Dr Teresa Wozniak, Program lead"),
                               # p("Dr Teresa Wozniak is an APPRISE Research Fellow and a Research Fellow at the Menzies School of Health Research. Her research interests are in surveillance systems to inform infection prevention and control efforts and support the development of local and national treatment guidelines in northern Australia."),
                               p("teresa.wozniak@menzies.edu.au")),
                        
                        column(4, img(src = 'Will.png', align = "centre", width = 'auto', height = '250px'),
                               p("Will Cuningham, Data manager and PhD candidate")
                               # p("Will is a final-year PhD candidate at the Menzies School of Health Research in Darwin. His research focuses on the burden of bacterial infections in northern Australia, including estimates of the incidence and cost of antibiotic-susceptible and antibiotic-resistant infections in Northern Territory hospitals. Prior to commencing his PhD at Menzies, Will completed his Masters in Epidemiology at the University of Melbourne and worked as a research assistant at The Peter Doherty Institute of Infection and Immunity.")
                        ), 
                        column(4, img(src = 'Alys.png', align = "centre", width = 'auto', height = '250px'),
                               p("Alys Young, Research assistant and PhD candidate")
                        ), 
                      ), # close fluid row
                      
                      br(),
                      br(),
                      
                      h3("Collaborators"),
                      fluidRow( 
                        column(4, img(src = 'UniMelb.png', align = "center", width = 'auto', height = '250px')
                        ),
                        
                        column(4, img(src = 'Nick.png', align = "centre", width = 'auto', height = '250px'),
                               p("Prof Nick Golding"),
                               p("The University of Melbourne, Curtin University, Telethon Kids Institute")
                        ), 
                        column(4, img(src = 'Saras.png', align = "centre", width = 'auto', height = '250px'),
                               p("Dr Saras Windecker"),
                               p("The University of Melbourne")
                        )
                      ),
                      br(),
                      br(),
                      
                      # Title
                      h3("Contact the HOTspots team"),
                      p("For general information regarding the data, use of the HOTspots tool and suggestions of further aspects to implement,"),
                      p("please contact Teresa Wozniak from the Menzie's School of Health Research on teresa.wozniak@menzies.edu.au"),
                      p("For specific questions regarding the website or to report an issue,"),
                      p(paste("please email", contact_email)),
                      br(),
                      br(),
                      
                      # Logos
                      h3("Funding"),
                      p("The HOTspots platform was developed by the Improving Health Outcomes in the Tropical North (HOT North) program. The HOT North program is supported by the Australian National Health and Medical Research Council (grant number 1131932)."),
                      show_logos
             ), # close tab panel
             
             
             ## Tab 8 - Share  ----------------------------------------------------------------
             
             navbarMenu(("Share"),
                        
                        icon = icon("share-alt"),
                        
                        # For other pages, use the format
                        # tabPanel(tags$a(href = "the url to the shar page", icon("social media icon for the other page"), "Page name" )),
                        
                        # Facebook
                        # tabPanel(tags$a(href = paste0('https://www.facebook.com/sharer/sharer.php?u=', website_url), icon("facebook"), "Facebook" )),
                        
                        # Twitter
                        tabPanel(tags$a(href = paste0('https://twitter.com/intent/tweet?url=', website_url , ' &text=', website_message), icon("twitter"), "Twitter" )),
                        
                        # LinkedIn 
                        tabPanel(tags$a(href = paste0('http://www.linkedin.com/shareArticle?mini=true&url=', website_url,' &title=', website_message), icon("linkedin"), "LinkedIn" ))
                        
             )# close navbar2 
  )# close navbar
) # Close fluidpage















































##***********##
## 3. Server ## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##***********##

server <- function(input,output, session){
  
  
  # Set up ------------------------------------------------------------------
  
  # Potentially stops the warnings?
  options(warn = -1) 
  
  ## Pop up on loading the page *************************************************************************************************************************************************
  shinyalert(
    
    # Title
    title = "Welcome to HOTspots:
    a tool for tracking antimicrobial resistance",
    
    # Text
    text = "On this page, select inputs on the left hand side to map resistance.
    
    To visualise the data further or see more information, select a tab at the top of the page.",
    size = "s", 
    closeOnEsc = TRUE, # close the pop up when the escape key is clicked
    closeOnClickOutside = FALSE, # close the pop up when the screen is clicked
    html = FALSE,
    type = "", # use "info" for the icon at te top
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "",
    animation = TRUE
  )
  
  
  ## Disable buttons once clicked *************************************************************************
  # Disable the load map button
  observeEvent(input$load_map, { 
    disable("load_map")
    show("please_wait")
    
    delay(500, enable("load_map"))
    delay(500, hide("please_wait")) })
  
  
  # # When ready for reports
  # # Disable the report button
  # observeEvent(input$report, { 
  #   disable("report")
  #   show("please_wait2")
  #   
  #   delay(500, enable("report"))
  #   delay(500, hide("please_wait2")) })
  
  
  
  
  ## Landing page Headers ***********************************************************************************************************************************************************************************************************************
  
  ## Number of organisms 
  output$VBox_organism <- renderValueBox({
    valueBox(
      tags$p(paste0(length(unique(hotspot_yearly_data$organism))),style = "font-size: 80%;"),
      "organisms", 
      icon= tags$i(icon("virus"), style="font-size: 60px"),
      color = "light-blue"
    )
  })
  
  ## Number of antibiotics 
  output$VBox_antibiotic <- renderValueBox({
    valueBox(
      tags$p(paste0(length(unique(hotspot_yearly_data$antimicrobial))),style = "font-size: 80%;"),
      "antibiotics", 
      icon= tags$i(icon("pills"), style="font-size: 60px"),
      color = "light-blue"
    )
  })
  
  ## Number of regions 
  output$VBox_regions <- renderValueBox({
    valueBox(
      tags$p(paste0(length(unique(hotspot_yearly_data$region))),style = "font-size: 80%;"),
      "regions", 
      icon= tags$i(icon("globe-asia"), style="font-size: 60px"),
      color = "light-blue"
    )
  })
  
  ## Number of years 
  output$VBox_year<- renderValueBox({
    valueBox(
      tags$p(paste0(length(unique(hotspot_yearly_data$year))),style = "font-size: 80%;"),
      "years of data", 
      icon= tags$i(icon("calendar-alt"), style="font-size: 60px"),
      color = "light-blue"
    )
  })
  
  ## Number of tests 
  output$VBox_tests <- renderValueBox({
    
    # Data
    data_num_isolates <- hotspot_monthly_data %>% 
      filter(onset == "Overall",
             sample_type == "All")
    
    # Value box
    valueBox(
      tags$p(format(sum(data_num_isolates$num_of_tests_monthly_raw), big.mark=","), style = "font-size: 80%;"),
      "susceptibility tests", 
      icon= tags$i(icon("vial"), style="font-size: 60px"),
      color = "light-blue"
    )
    
  })
  
  
  
  
  
  
  # Reactive values -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Saves computational time by only calculating the values once and then using them many times
  
  
  ## The yearly data
  Data_yearly <- reactive({ hotspot_yearly_data })
  
  
  ## Data for landing page --------------------------------------------------------------------------------------
  # Note: filtering the data in multiple steps is needed to use updateSelectInput
  # which allows the options displayed for inputs to only be options available in the data. 
  # Therefore, options with no data are not shown to users.
  
  
  # 1. Filtered by the sample attributes
  hotspot_yearly_filter1 <- reactive({
    
    # Required inputs
    req( input$onset, input$isolatetype, input$microbe_name)
    
    # Filter data
    data <- Data_yearly() %>%
      filter( onset == input$onset, sample_type == input$isolatetype, organism ==  input$microbe_name)
  })
  
  
  # 2. Filtered by the antimicrobial
  hotspot_yearly_filter2 <- reactive({
    
    # Required inputs
    req( input$onset, input$isolatetype, input$microbe_name, input$antibiotic_name)
    
    # Filter data
    data <- hotspot_yearly_filter1() %>%
      filter(antimicrobial ==  input$antibiotic_name)
  })
  
  
  ## 3. Filtered by the year range
  hotspot_yearly_filter4 <- reactive({
    
    # Required inputs
    req(input$range_year, input$antibiotic_name, input$microbe_name,  input$isolatetype, input$onset)
    
    # Filter data
    data <- hotspot_yearly_filter2() %>%
      filter(year %in% seq(min(input$range_year), max(input$range_year))) # year is in the range selected
  })
  
  
  
  
  ## Data for the specific combinations ----------------------------------------------------------------------------- 
  
  
  ## Monthly data ----------------------------------------------------------------------------------------------------
  Data_monthly <- reactive({ 
    hotspot_monthly_data
  })
  
  data_monthly_spec <- reactive({
    
    # Required inputs
    req(input$isolatetype_spec, input$onset_spec, input$microbe_name_spec , input$region_spec1, input$antibiotic_name_spec1)
    
    # Filter data
    data <- Data_monthly() %>% 
      filter(sample_type == input$isolatetype_spec, onset == input$onset_spec, organism ==  input$microbe_name_spec, region == input$region_spec1, antimicrobial == input$antibiotic_name_spec1)
  })
  
  
  ## Yearly data ----------------------------------------------------------------------------------------
  # Filtered by organism only - used as the base for all filtering, but also the by sample and healthcare tabs
  data_yearly_organismonly <- reactive({
    # Required inputs
    req(input$microbe_name_spec)
    # Filter data
    data <- Data_yearly() %>% filter( organism == input$microbe_name_spec) 
    
    # print("The organism was changed")
  })
  
  # Filtered by organism, healthcare and onset - used as the base for majority of all filtering (except by and sample and healthcare tab)
  data_yearly_spec <- reactive({
    # Required inputs
    req(input$microbe_name_spec,  input$isolatetype_spec, input$onset_spec)
    # Filter data
    data <- data_yearly_organismonly() %>% filter(sample_type == input$isolatetype_spec, onset == input$onset_spec) 
  })
  
  
  ## For bugs
  data_yearly_bug2 <- reactive({
    # Required inputs
    req(input$microbe_name_spec,  input$isolatetype_spec, input$onset_spec, input$antibiotic_name_spec1)
    # Filter data
    data <- data_yearly_spec() %>% filter(antimicrobial == input$antibiotic_name_spec1)
  })
  
  
  ## For by antibiotic tab, to filter the regional list
  data_yearly_bug3 <- reactive({
    # Required inputs
    req(input$microbe_name_spec,  input$isolatetype_spec, input$onset_spec, input$antibiotic_name_spec2)
    # Filter data
    data <- data_yearly_spec() %>% filter(antimicrobial %in% input$antibiotic_name_spec2)
  })
  
  
  ## For antibiotics
  data_yearly_drug3 <- reactive({
    # Required inputs
    req(input$microbe_name_spec,  input$isolatetype_spec, input$onset_spec, input$antibiotic_name_spec2, input$region_spec1 )
    # Filter data
    data <- data_yearly_bug3() %>% filter( region == input$region_spec1)
  })
  
  
  ## For jurisdiction
  data_yearly_jur4 <- reactive({
    # Required inputs
    req(input$microbe_name_spec,  input$isolatetype_spec, input$onset_spec, input$antibiotic_name_spec2)
    # Filter data
    data <- data_yearly_spec() %>%
      filter(antimicrobial == input$antibiotic_name_spec1, region == input$region_spec1) %>%
      select(jurisdiction, year, percent_resistant_yearly, region) %>%
      rename(resistance = "percent_resistant_yearly") %>%
      mutate(min_res = resistance,
             max_res = resistance)
  })
  
  
  ## For age groups
  data_age <- reactive({
    
    # Required inputs
    req(input$isolatetype_spec, input$onset_spec, input$microbe_name_spec, input$region_spec1, input$antibiotic_name_spec1)
    
    # Filter data
    data <- as.data.frame(hotspot_yearly_splitage) %>%
      filter( sample_type == input$isolatetype_spec, onset == input$onset_spec, organism ==  input$microbe_name_spec, region == input$region_spec1, antimicrobial == input$antibiotic_name_spec1)
  })
  
  
  ## Prelim data - filtered later for the sample type and onset location
  data_yearly_sampleonset <- reactive({
    
    # Required inputs
    req(input$microbe_name_spec, input$antibiotic_name_spec1, input$region_spec1 )
    
    # Filter data
    data <- data_yearly_organismonly() %>%
      filter(antimicrobial == input$antibiotic_name_spec1, region == input$region_spec1)
  })
  
  
  ## Compare by sample type
  data_yearly_compsample <- reactive({
    
    # Required inputs
    req(input$microbe_name_spec, input$antibiotic_name_spec1, input$region_spec1, input$onset_spec)
    
    # Filter data
    data <- data_yearly_sampleonset() %>% filter(onset ==  input$onset_spec)
  })
  
  
  ## Compare by onset
  data_yearly_componset <- reactive({
    
    # Required inputs
    req(input$microbe_name_spec, input$antibiotic_name_spec1, input$region_spec1, input$isolatetype_spec )
    
    # Filter data
    data <- data_yearly_sampleonset() %>% filter(sample_type ==  input$isolatetype_spec)
  })
  
  
  ## Compare by sex
  data_sex <- reactive({
    
    data <- hotspot_yearly_splitsex %>%
      mutate(sex = replace(sex, which(sex == "M"), "Male")) %>%
      mutate(sex = replace(sex, which(sex == "F"), "Female"))
    
    data
  })
  
  data_yearly_sex <- reactive({
    
    # Required inputs
    req(input$isolatetype_spec, input$onset_spec, input$microbe_name_spec )
    
    # Filter data
    data <- data_sex() %>%
      filter( sample_type == input$isolatetype_spec, onset == input$onset_spec, organism ==  input$microbe_name_spec) %>%
      filter(region == input$region_spec1, antimicrobial == input$antibiotic_name_spec1)  %>%
      filter(sex != "") %>%
      select(year, sex, num_of_tests_yearly, percent_resistant_yearly)
    
  })
  
  
  
  
  
  
  
  
  ## For antibiogram
  
  ## select region
  data_antibiogram_prelim <- reactive({
    
    # Required inputs
    req(input$isolatetype_table, input$onset_table)
    
    # Filter data
    data <-  Data_yearly() %>%
      filter( sample_type == input$isolatetype_table, onset == input$onset_table) %>%
      distinct
  })
  
  data_antibiogram <- reactive({
    
    # Required inputs
    req(input$isolatetype_table, input$onset_table, input$region_table)
    
    # Filter data
    data <-  data_antibiogram_prelim() %>%
      filter(region == input$region_table) %>%
      distinct
  })
  
  
  ## For jurisdiction average
  # Has the 3 jurisdictions and the uncertainty
  data_jur <- reactive({
    
    data <- Data_yearly() %>%
      filter(onset == input$onset_spec, sample_type  == input$isolatetype_spec, organism == input$microbe_name_spec, antimicrobial == input$antibiotic_name_spec1) %>%
      select(jurisdiction, year, num_of_tests_yearly, resistant_yearly) %>%
      mutate(res_year =  (resistant_yearly/num_of_tests_yearly)*100) %>%
      group_by(jurisdiction, year) %>%
      summarise(sum_res = sum(resistant_yearly),
                sum_tests = sum(num_of_tests_yearly),
                min_res = min(res_year),
                max_res = max(res_year)) %>%
      group_by(jurisdiction, year) %>%
      mutate(resistance = (sum_res/sum_tests)*100,
             region = jurisdiction) %>%
      select(-sum_res, -sum_tests)
    
    
  })
  
  data_jur2 <- reactive({
    
    data_region <- Data_yearly() %>%
      filter(onset == input$onset_spec, sample_type  == input$isolatetype_spec, organism == input$microbe_name_spec, antimicrobial == input$antibiotic_name_spec1, region == input$region_spec1) %>%
      select(jurisdiction, region, year, num_of_tests_yearly, resistant_yearly) %>%
      group_by(year) %>%
      mutate(resistance = (resistant_yearly/num_of_tests_yearly)*100,
             min_res = NA,
             max_res = NA) %>%
      select(-resistant_yearly, -num_of_tests_yearly )
    
    data_combo <- rbind(data_jur(), data_region)
    return(data_combo)
    
  })
  
  
  #### Update the UI inputs  --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  ## For the landingpage map ***********************************************************************************************************************************************************************************************************************
  
  # Update the list of antimicrobials
  observe({
    
    # Required inputs
    req( input$isolatetype, input$onset, input$microbe_name)
    
    # Data
    data <- hotspot_yearly_filter1() # filtered by onset, sample_type, organism
    
    
    # Update inputs
    updateSelectizeInput(session, inputId = "antibiotic_name",
                         label = "Select antibiotic:",
                         choices = sort(unique(data$antimicrobial)),
                         options = start_empty("antibiotic")
                         ) 
    
  })
  
  # Upate the years range available
  observe({
    
    # Required inputs
    req( input$isolatetype, input$onset, input$microbe_name, input$antibiotic_name)
    
    # Data
    data <- hotspot_yearly_filter2() # filtered by onset, sample_type, organism and antimicrobial
    
    # Update input
    updateSliderInput(session, inputId = "range_year",
                      value = c(max(data$year) - 5, max(data$year)),
                      min = min(data$year),
                      max = max(data$year))
  })
  
  
  
  # # When using only a single year rather than a range:
  # # Upate the years available
  # observe({
  #   
  #   # Required inputs
  #   req( input$isolatetype, input$onset, input$microbe_name, input$antibiotic_name)
  #   
  #   # Data
  #   data <- hotspot_yearly_filter2() # filtered by onset, sample_type, organism and antimicrobial 
  #   
  #   # Update inputs
  #   updateSliderInput(session, inputId = "year",
  #                     value = max(data$year),
  #                     min = min(data$year),
  #                     max = max(data$year))
  # })
  
  
  
  
  
  ## For the plotting page  ******************************************************************************************************************************************************************************************************************
  # Update the list of antimicrobials
  observe({ 
    
    # Required inputs
    req(input$microbe_name_spec)
    
    # updateselectize for antibiotic_name_spec1, based on 3 different conditions and data. # jur and region filtered by organism, isolate and healthcare onset locations # sample filtered by healthcare onset location, organism, # healthcare onset location filtered by ioslate type, organism
    # by jur and region should use data_yearly_spec
    # by sample - data_yearly_organismonly filter onset ==  input$onset_spec
    # by healthcare data_yearly_organismonly filter sample_type ==  input$isolatetype_spec
    
    # Data
    # data <- hotspot_yearly_filter1() # filtered by onset, sample_type, organism
    
    # if (input$tab_plot == "jurisdiction" | input$tab_plot == "region" ){
    #   data <- data_yearly_spec() 
    # } else 
      
    if (input$tab_plot == "sample_type"){
      req(input$onset_spec)
     data <- data_yearly_organismonly() %>% filter(onset ==  input$onset_spec)
      # print("The organism was changed on the sample_type tab and filtered by onset location")
    } else if (input$tab_plot == "onset"){
      req(input$isolatetype_spec)
      data <- data_yearly_organismonly() %>% filter(sample_type ==  input$isolatetype_spec)
      
      # print("The organism was changed on the onset tab, using the organism tab and filtered by sample type")
    } else { # if jurisdiction or regions
      data <- data_yearly_spec()
      
      # print("The organism was changed on the jurisdiction or region tab. uses data filtered by isolate, sample and organism")
    }
    

    
    # Update input
    updateSelectizeInput(session, inputId = "antibiotic_name_spec1",
                         label = "Select antibiotic:",
                         choices = sort(unique(data$antimicrobial)), 
                         options = start_empty("antibiotic")
                         )
  })
  
  
  
  ## UPDATE HERE - CHECK
  observe({ 
    
    # Required inputs
    # req(input$antibiotic_name_spec1)
    
    # regions_lists <- list_reg_by_jur(data_yearly_bug2())
    
    
    if (input$tab_plot == "antimicrobe") {
      req(input$antibiotic_name_spec2)
      regions_lists <- list_reg_by_jur(data_yearly_bug3()) # filtered for the multiple drugs
    } else {
      req(input$antibiotic_name_spec1)
      regions_lists <- list_reg_by_jur(data_yearly_bug2())
      }

    # Update
    updateSelectizeInput(session,
                         inputId = "region_spec1",
                         label = "Select region:",
                         choices = regions_lists,
                         options = start_empty("region")
                         )
  })
  
  
  ## Update the years of the plotting based on the data selected
  observe({ 
    
    # Required objects
    req(input$region_spec1, input$antibiotic_name_spec1)
    
    # Data
    data <- data_yearly_drug3()
    
    # Update
    updateSliderInput(session, inputId = "year_spec",
                      label = "Select the years:",
                      value = c(ifelse(length(unique(data$year)) < 6, min(data$year), max(data$year) - 6 ), max(data$year)), # max(data$year) - 4
                      min = min(data$year),
                      max = max(data$year))
  })
  
  
  
  
  
  
  
  ## Antibiogram **********************************************************************************************************************
  
  # regions
  observe({ 
    
    # Required inputs
    req(input$onset_table, input$isolatetype_table)
    
    
    regions_list_table <- list_reg_by_jur(data_antibiogram_prelim())
    
    # Update
    updateSelectizeInput(session, inputId = "region_table",
                         label = "Select region:",
                         choices = regions_list_table,
                         options = start_empty("region")
                         )
  })
  
  ## Antibiogram year selector
  observe({
    
    # Required inputs
    req(  input$isolatetype_table, input$onset_table, input$region_table)
    
    # Data
    data <- data_antibiogram()
    
    # Update
    updateSliderInput(session, inputId = "year_table",
                      label = "Select the year:",
                      value= c(max(data$year)-5, max(data$year)),
                      min = min(data$year),
                      max = max(data$year))
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #### Outputs to display ---------------------------------------------------------------------------------------------------------------------
  
  ## Landing page  -------------------------------------------------------------------------------------------------------------------------
  
  # Map title ************************************************************************************************************************************************************************************************************
  # written as a reactive expression
  map_title_RV <- eventReactive( input$load_map, {
    
    # Change the title based on if 1 or 2 years are selected
    if ( length(unique(input$range_year)) == 1){
      year_text <- paste("in", input$range_year)
    } else if (length(unique(input$range_year)) == 2){
      year_text <- paste("between", min(input$range_year), "and", max(input$range_year))
    }
    
    # The Title
    paste("Resistance of", em(input$microbe_name), "to", tolower(input$antibiotic_name), year_text)
  })
  
  # turning the reactive title into text  to display
  output$map_title <- renderUI(
    HTML(map_title_RV())
  )
  
  
  
  
  
  ## Leaflet Map ****************************************************************************************************************************************************************************************************************************************
  
  ## A blank map to start
  output$leaflet_map <- renderLeaflet({
    
    leaflet() %>% # create a leaflet map
      fitBounds(lng1 = 114.303954, lat1 = -32.801883, lng2 = 150.441990, lat2 =  -9.142143) %>% # the starting position of the map
      addTiles(options = providerTileOptions(minZoom = 2)) # The background map
    
  })
  
  
  ## If data is selected, then add the shapefiles
  
  
  observeEvent(input$load_map, {  # When the load  map button is pressed
    
    # Change the colour palette based on if the colour blind friendly map checkbox is clicked
    if(input$load_CB_friendly_map == FALSE){
      col_palette <- pal_num
    } else if (input$load_CB_friendly_map == TRUE){
      col_palette <- pal_num_CBfriendly
    }
    
    
    # The AMR data
    data <- hotspot_yearly_filter4() %>%
      group_by(region) %>%
      mutate( per_res_overall = (sum(resistant_yearly) / sum(num_of_tests_yearly))*100,
              num_test_overall = sum(num_of_tests_yearly)) %>%
      select(-year, -percent_resistant_yearly,  -num_of_tests_yearly, -resistant_yearly, -susceptible_yearly) %>%
      unique()
    
    # The shapefile data
    merged_data <- merge(SA3_data, data, by.x="SA3_NAME16", by.y="region")
    
    
    # Show the cities as points on the map
    if (input$load_show_cities == FALSE){ # If the add cities check box is NOT ticked
      
      # Take the base map and add the data
      leafletProxy("leaflet_map", data = merged_data ) %>%
        clearControls() %>%
        clearShapes() %>%
        clearMarkers() %>%
        addPolygons(  fillColor = ~col_palette(merged_data$per_res_overall),
                      fillOpacity = ifelse(is.na(merged_data$per_res_overall), 0, 0.7),
                      weight = 2,
                      opacity = 1,
                      color = "white",
                      dashArray = "3",
                      popup = ifelse(is.na(merged_data$per_res_overall), # consider the option of adding a little 
                                     paste("No data available"),
                                     paste0('<strong>', merged_data$SA3_NAME16, '</strong>',
                                            '<br/>', '<strong>',"Resistance: ", '</strong>',  round(merged_data$per_res_overall,1), "%",
                                            '<br/>', '<strong>', "No. of isolates: ", '</strong>', format(merged_data$num_test_overall, big.mark = ",") )),
                      highlight = highlightOptions( weight = 5, 
                                                    color = "black", 
                                                    bringToFront = TRUE))  %>%
        addLegend("bottomright", pal = col_palette, values = ~c(0:100), # also add values as ~percent_resistant_yearly
                  title = "% resistance",
                  opacity = 1)
      
    } else if (input$load_show_cities == TRUE){ # If the add cities check box is ticked
      
      leafletProxy("leaflet_map", data = merged_data ) %>%
        clearControls() %>%
        clearShapes() %>%
        clearMarkers() %>%
        addPolygons(  fillColor = ~col_palette(merged_data$per_res_overall),
                      fillOpacity = ifelse(is.na(merged_data$per_res_overall), 0, 0.7),
                      weight = 2,
                      opacity = 1,
                      color = "white",
                      dashArray = "3",
                      popup = ifelse(is.na(merged_data$per_res_overall),
                                     paste("No data available"),
                                     paste0('<strong>', merged_data$SA3_NAME16, '</strong>',
                                            '<br/>', '<strong>',"Resistance: ", '</strong>',  round(merged_data$per_res_overall, 1), "%",
                                            '<br/>', '<strong>', "No. of isolates: ", '</strong>', format(merged_data$num_test_overall, big.mark = ","))),
                      highlight = highlightOptions( weight = 5, 
                                                    color = "black", 
                                                    bringToFront = TRUE))  %>%
        addMarkers(data = cities_names, ~long, ~lat, popup = ~as.character(name), label = ~as.character(name))  %>% # add the cities
        addLegend("bottomright", pal = col_palette, values = ~c(0:100),
                  title = "% resistance",
                  opacity = 1)
      
    } # close if else loop of the adding cities
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ## Yearly plots  --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  ### Compare antibiotic  ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  # Title 
  output$text_compare_anti <- renderUI({
    
    
    input$load_plot
    
    isolate({
    # Required input
    req(input$onset_spec, input$isolatetype_spec, input$microbe_name_spec, input$antibiotic_name_spec2, input$region_spec1, input$year_spec)
    
    # If else depending on how many years selected
    if (length(unique(input$year_spec)) == 1) {
      HTML(paste("Resistance of", em(input$microbe_name_spec), "in", input$region_spec1, "in", input$year_spec))
      
    } else if (length(unique(input$year_spec)) == 2) {
      HTML(paste("Resistance of", em(input$microbe_name_spec), "in", input$region_spec1, "between", min(input$year_spec), "and", max(input$year_spec)))
    }
    })
  })
  
  
  # Plot as dots **********************************************************************************************************************************************
  output$plot_compare_anti <- renderPlotly({
    
    input$load_plot
    
    isolate({
    # Required inputs
    req(input$onset_spec, input$isolatetype_spec, input$microbe_name_spec, input$antibiotic_name_spec2, input$region_spec1, input$year_spec)
    
    # Colour palette
    pal_num_year2 <- colorNumeric(hotspot_palette$year, domain = min(input$year_spec):max(input$year_spec))
    
    # Filter the data
    data <- data_yearly_drug3() %>%
      filter(year %in% seq(min(input$year_spec), max(input$year_spec)))  %>% # years in the range
      arrange(percent_resistant_yearly) %>%
      mutate(year = as.factor(year))
    
    if(nrow(data) == 0 ){
      data <- data.frame()
      
      p <- ggplot(data) + 
        geom_point() +
        xlim(0, 100) +
        ylim(0, 2) +
        theme_classic() +
        theme(axis.text.y = element_blank()) +
        labs(x = "Percentage resistance  (%)", y = "") +
        annotate("text", x = 50, y = 1, label = "There is no data available. Please select other inputs.")
      
      g <- ggplotly(p)
      
    } else if(nrow(data) > 0 ){
      
      # Plot 
      p <- ggplot(data) + 
        geom_point( aes(text = paste0("Year: ", year, "<br>", "Resistance: " , round(percent_resistant_yearly, 2)), x=reorder(antimicrobial, percent_resistant_yearly),  y=percent_resistant_yearly, colour = year)) + # colour factor year.  , size=5
        coord_flip() +
        labs(x = NULL, y = "Percentage resistance  (%)", colour = "") + 
        scale_y_continuous( limits = c(0, max(data$percent_resistant_yearly)),
                            breaks = seq(from = 0, to = 100, by = ifelse(max(data$percent_resistant_yearly)<30,5,10)),
                            minor_breaks = seq(from = 0, to = 100, by = ifelse(max(data$percent_resistant_yearly)<30,1,5)) ) +
        scale_colour_manual(values = pal_num_year2(min(input$year_spec):max(input$year_spec)) )+ # Option to change tihs to pal_num_year to compare when the map is changed easier (keeps the year min and max stagnent), but for more obvious comparison use pal_num_year2. pal_num_year(min(input$year_spec):max(input$year_spec))
        theme_bw() +
        theme(legend.position = "top"
              # , # legend on top
              # text = element_text(size = 15) 
        ) # larger text size
      
      g <- ggplotly(p, tooltip = c("text"))  %>% # try here year
        layout(hovermode = "y unified",
               xaxis = list(tickfont = list(size = 15)), 
               yaxis = list(tickfont = list(size = 15)))
      
    }
    
    g
    })
    
  })
  
  
  # Plot over time ************************************************************************************************************************************************
  ## plot yearly resistance as bar plot, facet wrap by the antimicrobial
  
  #  Title
  output$text_compare_anti2 <- renderUI({
    
    input$load_plot
    
    isolate({
    # Required inputs
    req(input$onset_spec, input$isolatetype_spec, input$microbe_name_spec, input$antibiotic_name_spec2, input$region_spec1, input$year_spec)
    
    # Text
    HTML(paste("Resistance of", em(input$microbe_name_spec), "in", input$region_spec1))
    })
    
  })
  
  
  # Warning text
  output$text_anti2_warning <- renderUI({
    
    
    input$load_plot
    
    isolate({
      
    # Required inputs
    req(input$onset_spec, input$isolatetype_spec, input$microbe_name_spec, input$antibiotic_name_spec2, input$region_spec1, input$year_spec)
    
    # Data
    data  <- data_yearly_drug3()
    if(min(data$percent_resistant_yearly) == 0){
      HTML("The black line is the jurisdictional average for the region selected. To turn this off or on, select the checkbox on the left. A green dot and no bar indicates the resistance is exactly 0%. The absence of a dot and bar indicates no tests were done for the region selected.")
    } else if (min(data$percent_resistant_yearly) > 0){
      HTML("The black line is the jurisdictional average for the region selected. To turn this off or on, select the checkbox on the left. The absence of a green bar indicates no tests were done for that specific year in the region selected.")
    } # close if else
    })
    
  })
  
  
  
  # Plot 
  output$plot_compare_anti2 <- renderPlotly({
    
    
    input$load_plot
    
    isolate({
      
    # Required inputs
    req(input$onset_spec, input$isolatetype_spec, input$microbe_name_spec, input$antibiotic_name_spec2, input$region_spec1, input$year_spec)
    
    # Data for the region selected
    data  <- data_yearly_drug3()  %>%
      arrange(antimicrobial)
    
    #  Data for the jurisdictional total
    jur_selected <- unique(hotspot_yearly_data$jurisdiction[ hotspot_yearly_data$region == input$region_spec1])
    
    data_jur <- data_yearly_spec() %>%
      filter(antimicrobial %in% input$antibiotic_name_spec2, jurisdiction == jur_selected) %>%
      select(antimicrobial, year, num_of_tests_yearly, resistant_yearly) %>%
      mutate(res_year =  (resistant_yearly/num_of_tests_yearly)*100) %>%
      group_by(antimicrobial, year) %>%
      summarise(sum_res = sum(resistant_yearly),
                sum_tests = sum(num_of_tests_yearly)) %>%
      group_by(antimicrobial, year) %>%
      mutate(resistance = (sum_res/sum_tests)*100) %>%
      select(-sum_res, -sum_tests)
    
    
    if(nrow(data) == 0 ){
      g <- empty_ggplotly
     
      
    } else if(nrow(data) > 0 ){
      
      # Plot
      p <- ggplot() + 
        geom_bar(data = data, aes(x=year,  y = percent_resistant_yearly, text = paste0("Year: ", year, "<br>", "Resistance in region: ", round(percent_resistant_yearly, 2))), stat="identity", position = position_dodge(width=0.7, preserve = 'single'), width = 0.7, fill = "#44AA99") + 
        geom_point(data = data[data$percent_resistant_yearly == 0,], aes(x = year,  y = percent_resistant_yearly),  size = 2.5, colour = "#44AA99")  + #
        labs(x = "Year", y = "Percentage resistance  (%)") +
        theme_bw() +
        scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = 1)) +
        theme(legend.position = "bottom") + # text = element_text(size=15)
        facet_wrap(antimicrobial~., ncol = 1, scales = "fixed") #shrink = TRUE,
      #facet_rep_wrap(~antimicrobial, ncol=1, repeat.tick.labels = TRUE)
      
      if(input$compare_reg_AddJur == TRUE){
        q <- p + 
          geom_point(data = data_jur, aes(x = year,  y = resistance, text = paste("Resistance in jurisdiction:", round(resistance, 2)))) + 
          geom_line(data = data_jur, aes(x = year,  y = resistance))
        
      }  else if(input$compare_reg_AddJur == FALSE){
        q <- p
      }
      
      l_h <- length(unique(data$antimicrobial))
      # Setting the height
      h <- ifelse(l_h < 4, 500, l_h*150) # if there are less than 4 antimicrobials select, make the height 500, else make it the number x 150
      #h <- 150 * length(unique(data$antimicrobial))
      
      g <- ggplotly(q, dynamicTicks = TRUE,  tooltip =  "text", height = h) %>%
        layout(hovermode = "x unified", 
               yaxis = list(standoff = 0.35)
               # xaxis = list(tickfont = list(size = 15)),
               # yaxis = list(tickfont = list(size = 15))
        )
    }
    
    g
    })
    
    
  })
  
  
  
  
  
  
  
  ### Compare regions  ---------------------------------------------------------------------------------------------------------
  
  # Title
  output$text_compare_reg <- renderUI({
    
    
    input$load_plot
    
    isolate({
    
    # Required inputs
    req(input$onset_spec, input$isolatetype_spec, input$microbe_name_spec, input$antibiotic_name_spec1)
    
    # Text
    HTML(paste("Resistance of", em(input$microbe_name_spec), "to", tolower(input$antibiotic_name_spec1), "by regions"))
  })
  })
  
  
  
  
  # Plot
  output$plot_compare_reg <- renderPlotly({
    
    
    input$load_plot
    
    isolate({
      
    # Required inputs
    req(input$onset_spec, input$isolatetype_spec, input$microbe_name_spec, input$antibiotic_name_spec1)
    
    # Data summarised for the jurisdictions
    data_jur1 <- data_jur() %>%
      select(-min_res, -max_res, -region) %>%
      mutate(region = paste(jurisdiction, "overall")) %>%
      rename(percent_resistant_yearly = resistance)
    
    
    # Data for the regions
    data_yearly_spec_data <- data_yearly_spec() %>%
      filter(antimicrobial == input$antibiotic_name_spec1) %>%
      #filter(region %in% input$region_spec2) %>%
      select(jurisdiction, region, year, percent_resistant_yearly) %>%
      arrange(region)
    
    
    if(nrow(data_yearly_spec_data) == 0 ){
      
      g <- empty_ggplotly
      
    } else if(nrow(data_yearly_spec_data) > 0 ){
      
      
      ## Plot
      # Bar plot for the individual regions
      # Jurisdictional average over the top
      
      p <- ggplot() + 
        geom_bar(data = data_yearly_spec_data, aes(x = year,  y = percent_resistant_yearly, fill = region, text = paste0("Year: ", year, "<br>", "Region: ", region, "<br>", "Resistance in region:", percent_resistant_yearly)), stat = "identity", position = position_dodge(width = 0.7, preserve = 'single'), width = 0.7) + 
        theme_bw() +
        labs(x = "Year", y = "Percentage resistance  (%)", fill = "Regions") + 
        theme(# text = element_text(size=15), #axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top") +  #text = element_text(size=15)
        scale_fill_manual(values = hotspot_palette$regions)  + 
        scale_y_continuous(limits = c(0, max(data_yearly_spec_data$percent_resistant_yearly))) +
        scale_x_continuous(breaks = seq(min(data_yearly_spec_data$year), max(data_yearly_spec_data$year), by = 1)) +
        facet_rep_wrap(~jurisdiction, ncol = 1, repeat.tick.labels = TRUE, labeller = labeller(jurisdiction =  c("FNQ" = "Far North Queensland", "WA" = "Western Australia", "NT" = "Northern Territory"))) 
      
      if(input$compare_reg_AddJur == TRUE){
        p2 <- p + 
          geom_point(data = data_jur1, aes(x = year,  y = percent_resistant_yearly, text = paste0("Resistance in jurisdiction:", round(percent_resistant_yearly, 2))) ) +
          geom_line(data = data_jur1, aes(x = year,  y = percent_resistant_yearly) ) 
        
      }  else if(input$compare_reg_AddJur == FALSE){
        p2 <- p
      }
      
      
      # Setting the height
      if(length(unique(data_yearly_spec_data$jurisdiction)) == 1){
        h <- 300
      } else if(length(unique(data_yearly_spec_data$jurisdiction)) == 2){
        h <- 600
      } else if(length(unique(data_yearly_spec_data$jurisdiction)) == 3){
        h <- 900
      } 
      
      ## Ggplotly
      g <- ggplotly(p2, dynamicTicks = TRUE, tooltip = "text", height = h) %>% # tooltip = c("min_res", "resistance", "max_res"),
        layout(hovermode = "x unified",
               xaxis = list(tickfont = list(size = 15),
                            standoff = 2), 
               yaxis = list(tickfont = list(size = 15),
                            standoff = 0.3),
               legend = list(orientation = "h", y = 1.2, x = 0.1)
        )
      
    }
    g
    })
  })
  
  
  
  
  
  
  ### Compare jurisdictions  -----------------------------------------------------------------
  
  # Title
  output$text_compare_jur <- renderUI({
    
    input$load_plot
    
    isolate({
    # Required inputs
    req(input$onset_spec, input$isolatetype_spec, input$microbe_name_spec, input$antibiotic_name_spec1)
    
    # text
    HTML(paste("Resistance of", em(input$microbe_name_spec), "to", tolower(input$antibiotic_name_spec1), "by jurisdiction"))
    })
  })
  
  
  # Description of the graph
  output$text_ggplotly <- renderUI({
    
    input$load_plot
    
    isolate({
    
    # Required inputs
    req(input$onset_spec, input$isolatetype_spec, input$microbe_name_spec, input$antibiotic_name_spec1)
    
    # Text
    HTML(paste("The minimum, mean and maximum resistance for all the regions within the juridictions."))
  })
  })
  
  
  
  # Plot with uncertainty
  output$plot_compare_jur_uncertain <- renderPlotly({
    
    input$load_plot
    
    isolate({
    
    # Required inputs
    req(input$onset_spec, input$isolatetype_spec, input$microbe_name_spec, input$antibiotic_name_spec1)
    
    # Data
    data <- data_jur() %>%
      mutate(region = replace(region, which(region == "FNQ"), "Far North Queensland")) %>%
      mutate(region = replace(region, which(region == "NT"), "Northern Territory")) %>%
      mutate(region = replace(region, which(region == "WA"), "Western Australia"))
    
    
    
    if(nrow(data) == 0 ){
      
      g <- empty_ggplotly
      
    } else if(nrow(data) > 0 ){
      
      ## Gglot of the jurisdictional total as a line and and area showing the min and max region for variation
      p <- ggplot(data = data, aes(x = year) ) +
        geom_ribbon(aes(ymin = min_res, ymax = max_res, fill = region, text = paste0( "Jurisdiction: ", region)), alpha = 0.2) +
        geom_point(aes( y = resistance, text =  paste0("Year: ", year, "<br><br>", "Maximum resistance: ", round(max_res,2), "<br>", "Mean resistance: ", round(resistance,2), "<br>", "Minimum resistance: ", round(min_res,2)  ) )) +
        geom_line(aes( y = resistance)) + 
        theme_bw() +
        labs( x = "Year", y = "Percentage resistance  (%)", colour = "region", fill = "region") + 
        theme(#text = element_text(size=15), #axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top") +  #text = element_text(size=15)
        scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = 1)) +
        scale_fill_manual(name = "Minimum and maximum resistances", values = hotspot_palette$jurisdiction) +
        scale_colour_manual(name = "Mean resistance") +
        facet_rep_wrap(~region, ncol = 1, repeat.tick.labels = TRUE)
      
      # Add facetting variable
      if(length(unique(data$jurisdiction)) > 1){
        p2 <- p + facet_rep_wrap(~jurisdiction, ncol = 1, repeat.tick.labels = TRUE, labeller = labeller(jurisdiction = c("FNQ" = "Far North Queensland", "WA" = "Western Australia", "NT" = "Northern Territory")))
        
      } else {
        p2  <- p
      }
      
      # Setting the height
      if(length(unique(data$jurisdiction)) == 1){
        h <- 250
      } else if(length(unique(data$jurisdiction)) == 2){
        h <- 500
      } else if(length(unique(data$jurisdiction)) > 2){
        h <- 750
      } 
      
      ## Ggplotly
      g <- ggplotly(p2, tooltip =  "text", height = h) %>% # tooltip = c("min_res", "resistance", "max_res"),
        layout(hovermode = "x unified",
               showlegend = FALSE,
               legend = list( orientation = "h", y = 1.2, x = 0.1)
        )
      
      
      for(i in 1:length(unique(data$jurisdiction))){
        g$x$data[[i]]$name <- gsub("\\(","", str_split(  g$x$data[[i]]$name, ",")[[1]][1])
      }
      
    } else {

      g <- empty_ggplotly
      
    } # close if else based on the number of data rows
    g
    
  })
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ### Compare Sample Type ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  # Title
  output$text_compare_sample <- renderUI({
    
    input$load_plot
    
    isolate({
      
    # Required inputs 
    req(input$onset_spec, input$isolatetype_spec, input$microbe_name_spec, input$antibiotic_name_spec1, input$region_spec1)
    
    # Text
    HTML(paste("Resistance of", em(input$microbe_name_spec),"to", tolower(input$antibiotic_name_spec1), "in", input$region_spec1, "by sample type"))
  })
  })
  
  
  
  # Plot of resistance
  output$plot_compare_sample <- renderPlotly({
    
    input$load_plot
    
    isolate({
      
    # Data
    data <- data_yearly_compsample()
    
    
    if(nrow(data) == 0 ){

      g <- empty_ggplotly
      
      
    } else if(nrow(data) > 0 ){
      
      
      # Plot
      p <- ggplot( data = data, aes(x = year, y = percent_resistant_yearly) ) +
        geom_line(aes(colour = sample_type)) + 
        geom_point(aes(colour = sample_type, text = paste0("Year: ", year, "<br>", "Sample type: ",  sample_type, "<br>", "Resistance: ", round(percent_resistant_yearly, 2), "<br>", "Number of isolates: ", num_of_tests_yearly)), size = 3) +
        scale_x_continuous(breaks = seq(min(data$year) - 1, max(data$year) + 1, by = 1)) +
        scale_y_continuous(limits = c(0, max(data$percent_resistant_yearly))) + 
        scale_colour_manual(values = hotspot_palette$sample) +
        labs(x = "Year", y = "Percentage resistance  (%)", colour = "") + 
        theme_bw() + 
        theme(# text = element_text(size=15), #axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top")
      
      g <- ggplotly(p, tooltip = "text") %>%
        layout(hovermode = "x unified",
               legend = list(orientation = "h", y = 100, x = 0.1),
               xaxis = list(tickfont = list(size = 15)), 
               yaxis = list(tickfont = list(size = 15))
        )
    } # close if else based on number of data rows
    g
    
  })
  })
  
  
  
  
  # Plot of test numbers
  output$plot_compare_sample_tests <- renderPlotly({
    
    input$load_plot
    
    isolate({
      
    # Required inputs
    req(input$onset_spec, input$isolatetype_spec, input$microbe_name_spec, input$antibiotic_name_spec1, input$region_spec1)
    
    # Data
    data <- data_yearly_compsample()
    
    
    if(nrow(data) == 0 ){

      g <- empty_ggplotly
      
      
    } else if(nrow(data) > 0 ){
      
      # Plot
      p <- ggplot(data = data, aes(x = year, y = num_of_tests_yearly, fill = sample_type, text = paste0("Year: ", year, "<br>", "Sample type: ",  sample_type, "<br>", "Resistance: ", round(percent_resistant_yearly, 2), "<br>", "Number of isolates: ", num_of_tests_yearly)) ) +
        geom_bar(stat="identity", position=position_dodge(width=0.7, preserve = 'single'), width=0.7) +
        scale_fill_manual(values = hotspot_palette$sample) +
        scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = 1)) +
        labs(x="Year", y="Number of isolates", fill="") + 
        theme_bw() + 
        theme( # text = element_text(size=15), #axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top")
      
      g <- ggplotly(p, tooltip = "text") %>%
        layout( # hovermode = "x unified",
          legend = list(orientation = "h", y = 100, x = 0.1),
          xaxis = list(tickfont = list(size = 15)), 
          yaxis = list(tickfont = list(size = 15))
        )
    }
    g
    
  })
  })
  
  
  
  
  
  
  
  
  
  ### Compare healthcare setting ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  # Title
  output$text_compare_onset <- renderUI({
    
    input$load_plot
    
    isolate({
      
    # Required inputs
    req(input$isolatetype_spec, input$microbe_name_spec, input$antibiotic_name_spec1, input$region_spec1)
    
    # Text
    HTML(paste("Resistance of", em(input$microbe_name_spec), "to", tolower(input$antibiotic_name_spec1), "in", input$region_spec1, "by healthcare setting"))
  })
  })
  
  
  
  # Plot of resistance
  output$plot_compare_onset <- renderPlotly({
    input$load_plot
    
    isolate({
    
    # Required inputs
    req(input$isolatetype_spec, input$microbe_name_spec, input$antibiotic_name_spec1, input$region_spec1)
    
    # Data
    data <- data_yearly_componset()
    data_axis <- Data_yearly()
    
    
    
    if(nrow(data) == 0 ){

      g <- empty_ggplotly
      
      
    } else if(nrow(data) > 0 ){
      
      # Plot
      p <- ggplot(data = data, aes(x = year, y = percent_resistant_yearly) ) +
        geom_line(aes(colour = onset)) + 
        geom_point(aes(colour = onset, text = paste0("Year: ", year, "<br>", "Healthcare setting: ", onset, "<br>", "Resistance: ", round(percent_resistant_yearly, 2), "<br>", "Number of isolates: ",  num_of_tests_yearly)), size = 3) +
        scale_colour_manual(values = hotspot_palette$onset) +
        scale_y_continuous(limits = c(0, max(data$percent_resistant_yearly))) + 
        scale_x_continuous(breaks = seq(min(data_axis$year) - 1, max(data_axis$year) + 1,  by = 1)) +
        labs(x = "Year", y = "Percentage resistance  (%)", colour = "") + 
        theme_bw() + 
        theme(# text = element_text(size=15), #axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top")
      
      g <- ggplotly(p, tooltip = "text") %>% #, dynamicTicks = TRUE
        layout(hovermode = "x unified",
               legend = list(orientation = "h", y = 100, x = 0.1),
               xaxis = list(tickfont = list(size = 15)), 
               yaxis = list(tickfont = list(size = 15))
        )
    }
    g
    
  })
  })
  
  
  
  
  # Plot of number of tests
  output$plot_compare_onset_tests <- renderPlotly({
    
    input$load_plot
    
    isolate({
      
    # Required inputs
    req(input$isolatetype_spec, input$microbe_name_spec, input$antibiotic_name_spec1, input$region_spec1)
    
    # Data
    data <- data_yearly_componset()
    
    if(nrow(data) == 0 ){

      g <- empty_ggplotly
      
      
    } else if(nrow(data) > 0 ){
      
      
      # Plot
      p <- ggplot(data = data, aes(x = year, y = num_of_tests_yearly, fill = onset) ) +
        geom_bar(aes(text = paste0("Year: ", year, "<br>", "Healthcare setting: ", onset,"<br>", "Number of isolates: ",  num_of_tests_yearly)), stat="identity", position=position_dodge(width=0.7, preserve = 'single'), width=0.7) +
        scale_fill_manual(values = hotspot_palette$onset) +
        scale_x_continuous(breaks = seq(min(data$year) - 1, max(data$year) + 1,  by = 1)) +
        labs(x="Year", y="Number of isolates", fill="") + 
        theme_bw() + 
        theme(# text = element_text(size=15), #axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top")
      
      g <- ggplotly(p,  tooltip = "text") %>% #dynamicTicks = TRUE,
        layout( # hovermode = "x unified",
          legend = list(orientation = "h", y = 100, x = 0.1),
          xaxis = list(tickfont = list(size = 15)), 
          yaxis = list(tickfont = list(size = 15))
        )
    }
    g
    
  })
  })
  
  
  
  
  
  
  
  
  
  
  ### Compare Age  -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  output$text_age <- renderUI({
    
    input$load_plot
    
    isolate({
      
    # Required Inputs
    req(input$onset_spec, input$isolatetype_spec, input$microbe_name_spec, input$antibiotic_name_spec1, input$region_spec1)
    
    # Text 
    HTML(paste("Resistance of", em(input$microbe_name_spec), "to", tolower(input$antibiotic_name_spec1), "in", input$region_spec1, "by age"))
  })
  })
  
  
  
  # Warning text
  output$text_age_warning <- renderUI({
    input$load_plot
    
    isolate({
    
    # Required inputs
    req(input$onset_spec, input$isolatetype_spec, input$microbe_name_spec, input$antibiotic_name_spec1, input$region_spec1)
    
    # Data
    data <- data_age()
    
    # Text
    if(length(unique(data$age_group)) <3) {
      text_warning <- "Please note, these criteria have data available for only a few or no age groups. If the graph is empty, no data is available. Please choose other criteria."
    } else {
      text_warning <- ""
    }
    HTML(text_warning)
    
  })
  })
  
  
  
  ## Plot
  output$plot_compare_age <- renderPlotly({
    input$load_plot
    
    isolate({
    
    # Required inputs
    req(input$onset_spec, input$isolatetype_spec, input$microbe_name_spec, input$antibiotic_name_spec1, input$region_spec1)
    
    # data
    data <- data_age() 
    
    if(nrow(data) == 0 ){

      g <- empty_ggplotly
      
    } else if(nrow(data) > 0 ){
      
      # set the levels to try and make the columns plot in order
      data$age_group <- as.factor(data$age_group)
      age_levels <- c("<NA>", "unknown", "0-5", "6-15", "16-25", "26-40", "41-60", "61-80", "81+")
      data$age_group <- factor(data$age_group, levels = age_levels)
      
      # ggplot
      p <- ggplot(data = data, aes(x=year,  y=percent_resistant_yearly, fill = age_group)) + 
        geom_bar(aes(text = paste0("Year: ", year, "<br>", "Age group: ",  age_group, "<br>", "Resistance: ",  round(percent_resistant_yearly, 2), "<br>", "Number of isolates: ",  num_of_tests_yearly)), stat="identity", position=position_dodge(width=0.7, preserve = 'single'), width=0.7) +
        theme_bw() +
        scale_y_continuous(limits = c(0, max(data$percent_resistant_yearly))) + 
        scale_x_continuous(breaks = seq(min(data$year) - 1, max(data$year) + 1,  by = 1)) +
        scale_fill_manual(values = hotspot_palette$age) + 
        labs(x="Year", y="Percentage resistance  (%)", fill="Age Group") + 
        theme(legend.position = "top") #, text = element_text(size=15)
      
      # Plotly
      g <- ggplotly(p,  tooltip = "text") %>% #text = c("percent_resistant_yearly", "age_group"),
        layout( # hovermode = "x unified",
          legend = list(orientation = "h", y = 100, x = 0.1)
        )
    }
    g
  })
  })
  
  
  
  
  # Plot of number of tests
  output$plot_compare_age_tests <- renderPlotly({
    
    input$load_plot
    
    isolate({
    # Required inputs
    req(input$onset_spec, input$isolatetype_spec, input$microbe_name_spec, input$antibiotic_name_spec1, input$region_spec1)
    
    # Data
    data <- data_age()
    
    if(nrow(data) == 0 ){

      g <- empty_ggplotly
      
      
    } else if(nrow(data) > 0 ){
      
      # Plot
      p <- ggplot(data = data, aes(x = year, y = num_of_tests_yearly, fill = age_group) ) +
        geom_bar(aes(text = paste0("Year: ", year, "<br>", "Age group: ",  age_group,  "<br>", "Number of isolates: ",  num_of_tests_yearly)), stat = "identity", position = position_dodge(width = 0.7, preserve = 'single'), width = 0.7) +
        scale_fill_manual(values = hotspot_palette$age) +
        scale_x_continuous(breaks = seq(min(data$year) - 1, max(data$year) + 1,  by = 1)) +
        labs(x="Year", y="Number of isolates", fill="") + 
        theme_bw() + 
        theme(# text = element_text(size=15), #axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top")
      
      g <- ggplotly(p,  tooltip = "text") %>% #dynamicTicks = TRUE,
        layout( # hovermode = "x unified",
          legend = list(orientation = "h", y = 100, x = 0.1),
          xaxis = list(tickfont = list(size = 15)), 
          yaxis = list(tickfont = list(size = 15))
        )
    }
    g
    
  })
  })
  
  
  
  
  ### Compare Sex ----------------------------------------------------------------------------------------------------
  
  # Text
  output$text_sex <- renderUI({
    
    input$load_plot
    
    isolate({
      
    # Required inputs
    req(input$onset_spec, input$isolatetype_spec, input$microbe_name_spec, input$antibiotic_name_spec1, input$region_spec1)
    
    # Text
    HTML(paste("Resistance of", em(input$microbe_name_spec), "to", tolower(input$antibiotic_name_spec1), "in", input$region_spec1, "by sex"))
  })
  })
  
  
  
  # Plot
  output$plot_compare_sex <- renderPlotly({
    
    input$load_plot
    
    isolate({
      
    # Required inputs
    req(input$onset_spec, input$isolatetype_spec, input$microbe_name_spec, input$antibiotic_name_spec1, input$region_spec1)
    
    # Data
    data_overall <-  data_yearly_bug2() %>%
      filter(region == input$region_spec1) %>%
      select(year, num_of_tests_yearly, percent_resistant_yearly) %>%
      mutate(sex = "Overall")
    
    data_sex <- data_yearly_sex() %>%
      select(year, sex, num_of_tests_yearly, percent_resistant_yearly)
    
    data <- rbind(data_overall, data_sex)
    
    if(nrow(data) == 0 ){
      g <- empty_ggplotly
      
      
    } else if(nrow(data) > 0 ){
      
      
      # Plot
      p  <- ggplot(data = data, aes(x = year, y=percent_resistant_yearly, colour = sex)) +
        geom_point(size=3, aes(text = paste0("Year: ", year, "<br>", "Sex: ", sex, "<br>",  "Resistance: ",  round(percent_resistant_yearly, 2), "<br>",  "Number of isolates: ", num_of_tests_yearly))) +
        geom_path() +
        scale_x_continuous(breaks = seq(min(data$year), max(data$year),  by = 1)) +
        scale_y_continuous(limits = c(0, max(data$percent_resistant_yearly))) + 
        labs(x = NULL, y = "Percentage resistance (%)", colour = "") +
        scale_color_manual(values = hotspot_palette$sex, breaks = c("Female", "Male", "Both"), labels = c("Female", "Male", "Both")) + 
        theme_bw() +
        theme( #axis.text.x = element_text(angle = 45, hjust = 1),
          #text = element_text(size=15),
          legend.position = "top", 
          legend.spacing.x = unit(1.0, 'cm'))
      
      g <- ggplotly(p, tooltip = "text")  %>%
        layout(hovermode = "x unified",
               legend = list(orientation = "h", y = 100, x = 0.1))
    }
    g
    
  })
  })
  
  
  
  
  # Plot of number of tests
  output$plot_compare_sex_tests <- renderPlotly({
    
    input$load_plot
    
    isolate({
      
    # Required inputs
    req(input$onset_spec, input$isolatetype_spec, input$microbe_name_spec, input$antibiotic_name_spec1, input$region_spec1)
    
    # Data
    data_overall <-  data_yearly_bug2() %>%
      filter(region == input$region_spec1) %>%
      select(year, num_of_tests_yearly, percent_resistant_yearly) %>%
      mutate(sex = "Overall")
    
    data_sex <- data_yearly_sex() %>%
      select(year, num_of_tests_yearly, percent_resistant_yearly, sex)
    
    data <- rbind(data_overall, data_sex)
    
    if(nrow(data) == 0 ){
      g <- empty_ggplotly
      
    } else if(nrow(data) > 0 ){
      
      
      # Plot
      p <- ggplot(data = data, aes(x = year, y = num_of_tests_yearly, fill = sex) ) +
        geom_bar(aes(text = paste0("Year: ", year, "<br>", "Sex: ", sex, "<br>",  "Number of isolates: ", num_of_tests_yearly)), stat = "identity", position = position_dodge(width = 0.7, preserve = 'single'), width = 0.7) +
        scale_fill_manual(values = hotspot_palette$sex) +
        scale_x_continuous(breaks = seq(min(data$year) - 1, max(data$year) + 1,  by = 1)) +
        labs(x="Year", y="Number of isolates", fill="") + 
        theme_bw() + 
        theme(# text = element_text(size=15), #axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top")
      
      g <- ggplotly(p,  tooltip = "text") %>% #dynamicTicks = TRUE,
        layout( # hovermode = "x unified",
          legend = list(orientation = "h", y = 100, x = 0.1),
          xaxis = list(tickfont = list(size = 15)), 
          yaxis = list(tickfont = list(size = 15))
        )
    }
    g
    
  })
  })
  
  
  
  
  
  
  
  
  
  ## Monthly plots --------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  # Title
  output$text_spec <- renderUI({
    
    input$load_plot
    
    isolate({
      
    # Required inputs
    req(input$onset_spec, input$isolatetype_spec, input$microbe_name_spec, input$antibiotic_name_spec1, input$region_spec1)
    
    # Text
    HTML(paste("Resistance of", em(input$microbe_name_spec), "to", tolower(input$antibiotic_name_spec1) , "in", input$region_spec1, "by month"))
  })
  })
  
  
  # Warning text
  output$text_monthly_low_isolates <- renderUI({
    
    input$load_plot
    
    isolate({
    # Required input
    req(input$onset_spec, input$isolatetype_spec, input$microbe_name_spec, input$antibiotic_name_spec1, input$region_spec1)
    
    # Data
    data <- data_monthly_spec()
    
    # Text
    if(any(data$num_of_tests_monthly_raw < 15)){
      text <- "Please be aware that antimicrobial resistance is hard to interpret when the number of isolates is low as the number of isolates strongly impacts the resistance. In the plot being displayed, there are months with few isolates. Too few isolates can cause sharp spikes in the resistance which is not necessarily representative of the overall trend."
    } else {
      text <- "" #  empty
    }
    
  })
  })
  
  
  # Plot of resistance
  output$plot_monthly <- renderPlotly({
    
    input$load_plot
    
    isolate({
    
    # Required inputs
    req(input$onset_spec, input$isolatetype_spec, input$microbe_name_spec, input$antibiotic_name_spec1, input$region_spec1)
    
    # Data
    data <- data_monthly_spec()
    
    if(nrow(data) == 0 ){
      g <- empty_ggplotly
      
      
    } else if(nrow(data) > 0 ){
      
      
      # Plot
      p <- ggplot(data = data, aes(x = date_dmy, y = percent_resistant_monthly_raw)) +
        geom_path() + 
        geom_point(aes(text = paste0(format(date_dmy, "%B %Y"), "<br>",  "Resistance: ", round(percent_resistant_monthly_raw, 2), "<br>", "Number of isolates: ", num_of_tests_monthly_raw))) +
        scale_x_date(date_breaks = "6 months", 
                     date_minor_breaks = "1 month",
                     date_labels = "%b-%Y") + #6 months when text size  increase
        scale_y_continuous(limits = c(0, max(data$percent_resistant_monthly_raw))) + 
        labs(x=NULL, y="Percentage resistance (%)") + 
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)
              # ,
              # text = element_text(size=15)
        )
      
      
      g <- ggplotly(p, tooltip = "text")  %>%
        layout(legend = list(orientation = "h", y = 100, x = 0.1))
    }
    g
  })
  })
  
  
  
  
  
  
  # Plot of the number of isolates
  output$plot_monthly2 <- renderPlotly({
    
    input$load_plot
    
    isolate({
    
    # Required inputs
    req(input$onset_spec, input$isolatetype_spec, input$microbe_name_spec, input$antibiotic_name_spec1, input$region_spec1)
    
    # Data 
    data <- data_monthly_spec()
    
    if(nrow(data) == 0 ){
      g <- empty_ggplotly
      
      
    } else if(nrow(data) > 0 ){
      
      # Plot
      p <- ggplot(data = data, aes(x = date_dmy )) + #, text = paste0(format(date_dmy, "%B %Y"), "<br>",  "Number of isolates: ", num_of_tests_monthly_raw))
        geom_bar(aes(y = num_of_tests_monthly_raw, text = paste0(format(date_dmy, "%B %Y"), "<br>", "Number of isolates: ", num_of_tests_monthly_raw)), stat = "identity", fill="lightgrey") +
        scale_x_date(date_breaks = "6 months",
                     date_minor_breaks = "1 month",
                     date_labels = "%b-%Y") + #6 months when text size  increase
        labs(x=NULL, y="Number of isolates") + # check if this should be colour or color
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)
              # ,
              #text = element_text(size=15)
        )
      g <- ggplotly(p, tooltip = "text")
      
    }
    
    g
    
  })
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ## Antibiogram -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  # Text
  output$antibiogram_text <- renderText({
    

    input$load_antibiogram
    
    isolate({
    # Required inputs
    req( input$isolatetype_table, input$onset_table,  input$region_table, input$range_table)
    
    
    # Text
    year_text <- "Please select inputs on the left to display the antibiogram"
    
    # Change the text based on if 1 or 2 years are selected
    if ( length(unique(input$range_table)) == 1){
      year_text <- paste("in", input$range_table)
    } else if (length(unique(input$range_table)) == 2){
      year_text <- paste("between", min(input$range_table), "and", max(input$range_table))
    }
    
    # Change the text based on which setting is selected
    if (is.null(input$onset_table)){
      paste("Please select inputs on the left to display the antibiogram")
    } else if(input$onset_table == "Overall") {
      paste("Percentage susceptible in ", input$region_table, year_text)
    } else if(input$onset_table == "Community") {
      paste("Percentage susceptible in the", input$region_table, "community", year_text)
    } else if(input$onset_table == "Hospital") {
      paste("Percentage susceptible in the", input$region_table, "hospitals", year_text)
    }
    })
    
  })
  
  
  
  
  # DT Datatable 
  output$antibiogram_table <- DT::renderDataTable({
    
    input$load_antibiogram
    
    isolate({
    # Required inputs
    req( input$isolatetype_table, input$onset_table,  input$region_table, input$range_table)
    
    # Filter the data
    data_antibiogram2 <- data_antibiogram() %>%
      filter(year %in%  seq(min(input$range_table), max(input$range_table))) %>%
      group_by(organism, antimicrobial ) %>%
      mutate(suscept = round((sum(susceptible_yearly) / sum(num_of_tests_yearly))*100, 1)) %>%
      select(organism, antimicrobial,  suscept) %>%
      unique()
    
    if(nrow(data_antibiogram2) == 0){ # if the data is empty
      
      # data_null is an empty dataframe
      dat <-  DT::datatable(data = data_null)
      
    } else if(nrow(data_antibiogram2) > 0 ){ # if the data is not empty
      
      # Change the data shape to suit the plot
      data <- data_antibiogram2 %>%
        tidyr::spread(organism, suscept) %>%
        as.data.frame() %>%
        `rownames<-`(.[,1]) %>% # make the first column into the row names
        select(-antimicrobial)
      
      
      # default is *not* for NAs to be displayed
      # if you do want NAs, use:
      options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
      
      # Set the colours and the breaks at which to set the colours
      brks <- seq(10, 100, 10) # values to break.
      #clrs <- rev(c("#fffeed", pal_num(seq(0,90,10)))) # 11 colours, # turn the heat map into 10 colours
      
      # Change the colour palette based on if the colour blind friendly map checkbox is clicked
      if(input$load_CB_friendly_antibiogram == FALSE){
        clrs <- rev(c("#fffeed", pal_num(seq(0,90,10)))) # 11 colours, # turn the heat map into 10 colours
      } else if (input$load_CB_friendly_antibiogram == TRUE){
        clrs <- rev(c("#fffeed", pal_num_CBfriendly(seq(0,90,10)))) # 11 colours, # turn the heat map into 10 colours
      }
      
    
    
    # How long to start the table
    page_Length <- nrow(data) # set as all the data
    
    # The data table
    dat <-  DT::datatable(data = data, 
                          class = 'cell-border hover', #compact
                          extensions = c('Buttons', 'FixedColumns', 'KeyTable'),
                          #extensions = "FixedColumns",
                          options = list(scrollX=TRUE,
                                         #keys = TRUE,
                                         pageLength = page_Length,
                                         buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                         headerCallback = JS(headerCallback2),
                                         columnDefs = list(
                                           list(targets = "_all", className = "dt-center")),
                                         fixedColumns = list(leftColumns = 1),
                                         dom = 'tBpl')) %>%
      DT::formatStyle(names(data), # all the columns
                      backgroundColor = DT::styleInterval(cuts = brks, values = clrs),
                      color = DT::styleInterval( 50, c('white', 'black'))) # any text with a value below 50 will be black, and above 50 will be white
    
    # Option of displaying percentage in each cell, add:
    # %>% formatPercentage(names(data), number of decimal places)  # at the end
    
    
    } # close if else  based on empty data
    
    
    dat
    
  })
  })
  
  
  
  
  
  
  
  
  
  
  ## ResImpact shiny app -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  # Source in the other files
  source('ResImpact_app/simulate.R')
  
  # reactive function of the simulation
  results = reactive({
    res = simulate(inbug=input$bug, inresUTI=input$pDrugResUTI,
                   inresResp=input$pDrugResResp, inresBSI=input$pDrugResBSI)
    res
  })
  
  
  
  # Text on treatment cost
  output$cost_text <- renderText({
    
    # calulations
    meanc = round(mean(results()$cTreatment))
    ci = round(quantile(results()$cTreatment, probs=c(0.025, 0.975)))
    
    # Text
    paste('Using a treatment cost of $',results()$tCost, ' per infection. Mean cost in 2017 = $', format(meanc, big.mark = ','), 
          ' per year, 95% confidence interval = $', format(ci[1], big.mark = ','), 
          ' to $', format(ci[2], big.mark = ','), '.', sep='')
  })
  
  
  
  # Text
  output$account_text <- renderText({
    
    # Calculations
    meanc = round(mean(results()$cBedAccount))
    ci = round(quantile(results()$cBedAccount, probs=c(0.025, 0.975)))
    
    # Text
    paste('Using a bed day cost of $', results()$cAccount, ' per day obtained from the Australian Independent Hospital Pricing Authority. Mean cost in 2014 = $', format(meanc, big.mark = ','), 
          ' per year, 95% confidence interval = $', format(ci[1], big.mark = ','), 
          ' to $', format(ci[2], big.mark = ','), '.', sep='')
  })
  
  
  
  
  
  # Text on opportunity cost
  output$opp_text <- renderText({
    meanc = round(mean(results()$cBedOppCost))
    ci = round(quantile(results()$cBedOppCost, probs=c(0.025, 0.975)))
    paste('Using a bed day cost of $', results()$cOppCost, ' per day calculated as the opportunity cost of a bed-day obtained by contingent valuation (Page et al., 2017). Mean cost = $', format(meanc, big.mark = ','), 
          ' per year, 95% confidence interval = $', format(ci[1], big.mark = ','), 
          ' to $', format(ci[2], big.mark = ','), '.', sep='')
  })
  
  
  
  
  
  
  ## Data exploration tables -------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  ## Yearly Data ************************************************************************************************************************************************************************
  data_to_download_yearly <- reactive({
    
    # The data 
    data <- Data_yearly() %>%
      filter(year %in% seq(min(input$year_filt), max(input$year_filt)))
    
    
    # Filter  ******************************************************************************************************************************************************************************
    
    # filter by onset location
    if(input$onset_filt == "All"){
      data
    } else {
      data <- data %>%
        filter(onset == input$onset_filt)
    }
    
    #filter by isolate type
    if(input$isolatetype_filt == "All"){
      data
    } else {
      data <- data %>%
        filter(sample_type == input$isolatetype_filt)
    }
    
    # filter by microbe
    if(input$microbe_name_filt == "All"){
      data
    } else {
      data <- data %>%
        filter(organism == input$microbe_name_filt)
    }
    
    # filter by antibiotic
    if(input$antibiotic_name_filt == "All"){
      data
    } else {
      data <- data %>%
        filter(antimicrobial == input$antibiotic_name_filt)
    }
    
    
    # filter by region
    if(input$region_filt == "All"){
      data
    } else {
      data <- data %>%
        filter(region == input$region_filt)
    }
    
    return(data)
  })
  
  
  
  
  ## Table
  output$table_data_year <- DT::renderDataTable({
    
    # Data
    data <- data_to_download_yearly() %>%
      select(region, sample_type, onset, organism, antimicrobial, year, num_of_tests_yearly,  percent_resistant_yearly) 
    data$percent_resistant_yearly  <- round(data$percent_resistant_yearly , 2)
    
    data <- data %>%
      rename("number of isolates" = num_of_tests_yearly,
             "percentage resistance" = percent_resistant_yearly)
    
    
    # Table
    dat <- DT::datatable(data, rownames = FALSE, #colnames =  c("Region", "Sample type", "Onset location", "Organism", "Antimicrobial", "Year", "Number of isolates", "Percent resistant"),
                         class = 'compact stripe hover',
                         options = list(headerCallback = JS(headerCallback2),
                                        dom = 'tpl',
                                        scrollX=TRUE,
                                        keys = TRUE))
    return(dat)
    
  })
  
  
  
  ## Monthly data ********************************************************************************************************************************************************************************************************************************
  
  data_to_download_monthly <- reactive({
    
    # Data
    data <- Data_monthly()%>%
      filter(year %in% seq(min(input$year_filt), max(input$year_filt)))
    
    # Filter ********************************************************************************************************************************************************************************************************************************
    
    # filter by onset location
    if(input$onset_filt == "All"){
      data
    } else {
      data <- data %>%
        filter(onset == input$onset_filt)
    }
    
    #filter by isolate type
    if(input$isolatetype_filt == "All"){
      data
    } else {
      data <- data %>%
        filter(sample_type == input$isolatetype_filt)
    }
    
    # filter by microbe
    if(input$microbe_name_filt == "All"){
      data
    } else {
      data <- data %>%
        filter(organism == input$microbe_name_filt)
    }
    
    # filter by antibiotic
    if(input$antibiotic_name_filt == "All"){
      data
    } else {
      data <- data %>%
        filter(antimicrobial == input$antibiotic_name_filt)
    }
    
    
    # filter by region
    if(input$region_filt == "All"){
      data
    } else {
      data <- data %>%
        filter(region == input$region_filt)
    }
    
    return(data)
  })
  
  
  ## Table
  output$table_data_month <- DT::renderDataTable({
    
    # Data
    data <- data_to_download_monthly() %>%
      select(region, sample_type, onset, organism, antimicrobial, month_year, num_of_tests_monthly_raw, percent_resistant_monthly_raw)
    data$percent_resistant_monthly_raw  <- round(data$percent_resistant_monthly_raw , 2)
    
    data <- data  %>%
      rename("number of tests" = num_of_tests_monthly_raw,
             "percentage resistance" = percent_resistant_monthly_raw)
    
    
    # Table
    dat <- DT::datatable(data, rownames = FALSE, #colnames =  c("Region", "Sample type", "Onset location", "Organism", "Antimicrobial", "Date", "Number of tests", "Percent resistant"),
                         class = 'compact stripe hover',
                         options = list(headerCallback = JS(headerCallback2),
                                        dom = 'tpl',
                                        scrollX=TRUE,
                                        keys = TRUE))
    
    return(dat)
    
  })
  
  
  ## Download  -----------------------------------------------------------------------------------------------------------------------
  output$downloadData_yearly <- downloadHandler(
    filename = function() {
      paste('HOTspots_AMRdata-full_', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(Data_yearly(), con)
    }
  )
  
  output$downloadData_yearly_selected <- downloadHandler(
    filename = function() {
      paste('HOTspots_AMRdata-selected_', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(data_to_download_yearly(), con)
    }
  )
  
  # # When ready to do reports
  # 
  # ## Rmarkdown reports
  # 
  # output$report <- downloadHandler(
  #   
  #   # For PDF output, change this to "report.pdf"
  #   filename = paste0("HOTspots_report_", Sys.Date(), ".html"), # try just changing this to pdf
  #   
  #   content = function(file) {
  #     
  #     # Copy the report file to a temporary directory before processing it, in
  #     # case we don't have write permissions to the current working dir (which
  #     # can happen when deployed).
  #     tempReport <- file.path(tempdir(), "report.Rmd")
  #     file.copy("report.Rmd", tempReport, overwrite = TRUE)
  #     
  #     # Set up parameters to pass to Rmd document
  #     params <- list(
  #       hotspot_yearly_data = hotspot_yearly_data,
  #       SA3_data = SA3_data,
  #       date_updated = date_updated,
  #       year_updated = year_updated,
  #       cities_names = cities_names,
  #       onset = input$onset,
  #       isolatetype = input$isolatetype,
  #       microbe_name = input$microbe_name,
  #       antibiotic_name = input$antibiotic_name,
  #       range_year_min = min(input$range_year),
  #       range_year_max = max(input$range_year),
  #       load_CB_friendly_map = input$load_CB_friendly_map,
  #       load_show_cities = input$load_show_cities
  #     )
  #     
  #     # Knit the document, passing in the `params` list, and eval it in a
  #     # child of the global environment (this isolates the code in the document
  #     # from the code in this app).
  #     rmarkdown::render(tempReport, output_file = file,
  #                       params = params,
  #                       envir = new.env(parent = globalenv())
  #     )
  #   }
  # )
  # 
  
  
} # close server function







##**************##
## 4. Shiny App ## ---------------------------------------------------------------------------------------------------
##**************##
shinyApp(server = server, ui = ui)
