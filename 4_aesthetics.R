###*********************************************###
### HOTspots: Tracking Antimicrobial Resistance ###
###*********************************************###
# Author: Alys Young
# Collaborators: Saras Windecker and Nick Golding
#
# Project aim: Create a shiny app to map antimicrobial resistance 
# Script aim: Plotting aesthetics


### Text -----------------------------------------------------------------------------------------------------------------------------------------------
# Note to the website users that the plots are interactive
plotly_description <- "The plots are interactive. Hover your mouse over the graph for information. Click and drag to zoom in on an area, and double click to zoom out. Click on items in the plot legend to show or hide them."



### Datatable -----------------------------------------------------------------------------------------------------------------------------------------------
# Make the DT::datatable column names angles
headerCallback2 <- c(
  "function(thead, data, start, end, display){",
  "  var $ths = $(thead).find('th');",
  "  $ths.css({'vertical-align': 'bottom', 'white-space': 'nowrap'});",
  "  var betterCells = [];",
  "  $ths.each(function(){",
  "    var cell = $(this);",
  "    var newDiv = $('<div>', {height: 'auto', width: 'auto'});",
  "    var newInnerDiv = $('<div>', {text: cell.text()});",
  "    newDiv.css({margin: 'auto'});",
  "    newInnerDiv.css({",
  "      transform: 'rotate(200deg)',",
  "      'writing-mode': 'tb-rl',",
  "      'white-space': 'nowrap'",
  "    });",
  "    newDiv.append(newInnerDiv);",
  "    betterCells.push(newDiv);",
  "  });",
  "  $ths.each(function(i){",
  "    $(this).html(betterCells[i]);",
  "  });",
  "}"
)


### Empty if no data is available -----------------------------------------------------------------------------------------------------------------------------------------------
# For ggplots
empty_ggplot <- ggplotly(ggplot(data.frame()) + 
                           geom_point() +
                           xlim(2000, 2020) +
                           ylim(0, 100) +
                           theme_classic() +
                           labs(y = "Percentage resistance  (%)", x = "Years") +
                           annotate("text", x = 2010, y = 50, label = "There is no data available. Please select other inputs.")
)


empty_ggplotly <- ggplotly(empty_ggplot)

# For antibiogram
data_null <- data.frame(None = c("No data available. Please select other inputs."))


### Colour palette ------------------------------------------------------------------------------------------------------------------
# The spare colours are commented out
# if new categories are added in the data, need to add them to the colour palette

hotspot_palette <- list(
  
  ## For heat map
  `heat`  = c(
    `green`         = "#629c25",
    `yellow orange` = "#FFF100", 
    `orangy yellow` = "#FFD100",
    `orange`        = "#fc8105", 
    `dark orange`   = "#e34f00",
    `red`           = "#ff0000",
    `red2`          = "#db0000",
    `deep red`      = "#C00004",
    `dark red`      = "#810000",
    `almost black`  = "#410000"),
  
  ## For heat map - colour-blind friendly
  `heat_CBfriendly` = c(
    `teal`          = "#57C4AD",
    `yellow orange` = "#E6E1BC", 
    `orangy yellow` = "#E6BD85",
    `orange`        = "#EDA247", 
    `dark orange`   = "#ED8047",
    `red`           = "#DB4325",
    `red2`          = "#db0000",
    `deep red`      = "#C00004",
    `dark red`      = "#810000",
    `almost black`  = "#410000"),
  
  ## For the reigons
  `regions` = c( 
    ## QLD
    'Cairns and Hinterland'        = "#CC6677", # rose
    'Mackay'                       = "#88CCEE", #cyan 
    'North Western Queensland'     = "#44AA99", # teal
    'Torres and Cape'              = "#117733", # green
    'Townsville'                   = "#DDCC77", # sand
    #'Outback - South' = "#332288", # indigo
    #'Central Highlands (Qld)' = "#999933", # olive
    #'Rockhampton' = "#882255", # wine
    #'Biloela' = "#AA4499", # purple
    
    ## NT
    # colours lightened
    'Alice Springs'                 = "#FF8096", # rose
    'Darwin'                        = "#CCEEFF", # cyan
    'Gove'                          = "#92EFD3", # teal
    'Katherine'                     = "#CCDDAA", # green
    'Tennant Creek'                 = "#FFE57E", # sand FFEC8C
    
    ## WA 
    # colours darkened
    'Kimberley'                     = "#78343F", # rose
    'Mid-eastern Western Australia' = "#225555", # cyan 
    'Mid-western Western Australia' = "#36877A", # teal
    'Perth'                         = "#225522", # green
    'Pilbara'                       = "#A59858", # sand
    'Southern Western Australia'    = "#222255" # indigo
    
    ## Overall - grey scale
    # 'NT overall'  = "#404040", # darkest grey
    # 'FNQ overall' = "#5e5d5d", # dark grey 
    # 'WA overall'  = "#757575", #  grey
    # 'NSW overall'  = "#8a8a8a",
    # 'ACT overall' = "#9e9e9e", 
    # 'VIC overall'  = "#b5b5b5" # light grey
    # 'SA overall' = "#cfcfcf", # lightest grey
    # 'TAS overall'  = "#242424" # deep deep dark grey
    
    ## Overall - coloured
    # 'FNQ'   = "#44AA99", # teal
    # 'NT'    = "#332288", # indigo
    # 'WA'    = "#78343F" # rose
    
    ## Other colours availabe to use
    
    # colours lightened
    #'region name' = "#8183E6", # indigo
    #'region name' = "#C5C86E", # olive
    #'region name' = "#F488EE", # purple
    
    # colours darkened
    #'region name' = "#2F345B", # indigo
    #'region name' = "#7B7B29", # olive
    #'region name' = "#781D4A", # wine
    #'region name' = "#803273", # purple
    
  ),
  
  # For the Jurisdictions
  `jurisdiction` = c( 'FNQ'   = "#44AA99", # teal
                      'NT'    = "#332288", # indigo
                      'WA'    = "#78343F", # rose
                      'Far North Queensland' = "#44AA99", # teal
                      'Nothern Australia'    = "#332288", # indigo
                      'Western Australia'    = "#78343F" # rose
                      #'NSW' = "#225555" #cyan
                      #'VIC' = "#225522" #green
                      #'ACT' = "#999933" #olive
                      #'TAS' = "#A59858" #sand
                      #''    = "#781D4A", # wine
                      #''    = "#803273", # purple
  ), 
  
  # For the Jurisdictions
  `jurisdiction_range` = c( 'FNQ' = "#92EFD3", # teal
                            'NT'  = "#8183E6", # indigo
                            'WA'  = "#FF8096"), # rose
  #'NSW'  = "#CCEEFF" #cyan
  #'VIC'  = "#CCDDAA" #green
  #'ACT'  = "#C5C86E" #olive
  #'TAS'  = "#FFE57E" #sand
  
  ## For the onset locations
  `onset` = c('Overall'   = "#332288" , # indigo
              'Hospital'  = "#88CCEE", # cyan
              'Community' = "#44AA99"), # teal
  
  
  ## For the sample types
  `sample` = c('All'                = "#332288" , # indigo
               'Blood'              = "#A0515E", # rose
               'Urine'              = "#999933",
               'Skin & soft tissue' = "#88CCEE", # cyan
               'Other'              = "#44AA99", # teal
               'Respiratory'        = "#117733"), # green
  
  ## For the age brackets
  `age` = c('0-5'     = "#332288" , # indigo
            '6-15'    = "#88CCEE", # cyan
            '16-25'   = "#44AA99", # teal
            '26-40'   = "#117733", # green
            '41-60'   = "#A0515E", # rose
            '61-80'   = "#DDCC77", # sand
            '81+'     = "#999933", # olive
            'unknown' = "#803273" # purple
            
  ), 
  
  `year` = c( 
    `light red`     = "#FFE0E2",
    `red`           = "#ff0000",
    `dark red`      = "#810000"),
  
  # For the sexes
  `sex` = c( 'M'    = "#44AA99", # teal
             'F'  = "#332288", # indigo
             'Overall' = "#999933",
             
             # repeated with the names changed
             'Male'    = "#44AA99", # teal
             'Female'  = "#332288", # indigo
             'Both' = "#999933") # olive
)

# Change heat colour palette into a gradient
pal_num <- colorNumeric(hotspot_palette$heat, domain = 0:100)
pal_num_CBfriendly <- colorNumeric(hotspot_palette$heat_CBfriendly, domain = 0:100)

# Change the year palette into a gradient
pal_num_year <- colorNumeric(hotspot_palette$year, domain = min(hotspot_yearly_data$year):max(hotspot_yearly_data$year))

## To check the colour palettes:
## Uncomment the lines below and run them
#  # changing $heat to the $name of the colour palette you want to see

# par(mar=c(0,0,0,0))
# pie(rep(1, length(hotspot_palette$heat)), col = hotspot_palette$heat)
# 
# pie(rep(1, 100), col = pal_num_CBfriendly(1:100))
#
# pie(rep(1, length(min(hotspot_yearly_data$year):max(hotspot_yearly_data$year))), col = pal_num_year(min(hotspot_yearly_data$year):max(hotspot_yearly_data$year)))

## Check all the data categories have a corresponding colour in the palettes
# if(length(unique(hotspot_monthly_data$region))  != length(hotspot_palette$regions)) print("Check all the regions in the data have a colour in the regions colour palette")
# if(length(unique(hotspot_monthly_data$onset))  != length(hotspot_palette$onset)) print("Check all the onser locatoins in the data have a colour in the onset colour palette")
# if(length(unique(hotspot_monthly_data$sample_type))  != length(hotspot_palette$sample)) print("Check all the sample types in the data have a colour in the sample colour palette")
# if(length(unique(hotspot_yearly_splitage$age))  != length(hotspot_palette$age)) print("Check all the age brackets in the data have a colour in the age colour palette")


# set the ggplot theme
# theme_set(
#   theme_bw() +
#     theme(text = element_text(size=15), #axis.text.x = element_text(angle = 45, hjust = 1),
#           legend.position = "top")
# )
