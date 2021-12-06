
# test whether shiny can be a suitable GeoCAT replacement

library(leaflet)
library(shinythemes)
library(rCAT)
library(data.table)
library(here)
library(rgbif)
library(dplyr)
library(shinyWidgets)



# things to consider:

# upload to github
# results - merge csv import and gbif
# make sure sequence works when analysis is on/off
# input button to let user choose number of gbif points
# use fluid row to separate the input from the results
# excel sheet/ powerpoint with ui and server functions mapped to keep track of logic
# add tab and batch tab, and About info
# add more items to results - Red List categories, number of occurrences?
# draw convex hull?
# add native range - POWO ID?
# add pop up metadata in leaflet map when user clicks points
# select species from GBIF should be drop down? or just go with best guess see rapid LC?
# read mastering shiny book - 
# possibly hire shiny developer?

# 1 import csv <<WORKING
# 2 validate csv < handle when csv doesn;t have lat long cols, or valid data.
# 3 Import GBIF data << selectize from list of options? or best match?
# >> clear GBIF search after each query

# 4 EOO <<WORKING
# 5 AOO <<WORKING
# 6 switch off points <<WORKING
# 7 edit points << leaflet extra <<WORKING
# 8 export csv - results and points, various formats? << see Rapid LC
# 9 batch process << see Rapid LC
# 10 Nice user interface ?? probably needs more dev - Baz, Matilda - or outsource?

# other issues
#how to deal with shiny app turning off when not in use
# RCAT2 needs to be on CRAN for shiny geocat to be deployed
# native range from POWO?

source(here("functions.R"))

# UI ----
ui <- fluidPage(
  
  #shinythemes::themeSelector(),
  theme = shinytheme("darkly"),

  # App title
  titlePanel(div("GeoCAT", tags$h5("Geospatial Conservation Assessment Tool"))
  
 
),
  
  # Sidebar layout with input and output definitions 
  sidebarLayout(
    
    # Sidebar panel for inputs 
    sidebarPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Import CSV",
                           # Input: input csv file
                           helpText("Upload a CSV with 'longitude' and 'latitude' fields"),
                           fileInput("file1", NULL, multiple = FALSE, accept = (".csv")),
                           ), #, plotOutput("plot")),
                  tabPanel("Query GBIF",
                           # Input: select a species from GBIF
                           helpText("Select a species from GBIF"),# Action: run the EOO, AOO analysis
                           textInput("searchGBIF", "Enter species name"))

      ),
      
      
      materialSwitch(inputId = "Analysis", 
                     label = "Analysis on/off", 
                     value = FALSE,
                     status = "success"),
                     

                     #status = "primary"),
      # Output: Verbatim text for data summary 
      #verbatimTextOutput("res")
      #DT::dataTableOutput("res")
      htmlOutput("res_title"),
      htmlOutput("text")
      

    ),
    
    # Main panel for displaying outputs 
    mainPanel(
      
      # Output: Leaflet map
      leaflet::leafletOutput("mymap", width = "100%", height = 600),
      
      # Ouptut the results of the GBIF search
      DT::dataTableOutput("summarytab")

      

    )
  )
)