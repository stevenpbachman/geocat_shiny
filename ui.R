

# testing whether shiny can be a suitable GeoCAT replacement

library(leaflet)
library(shinythemes)
library(rCAT)
library(data.table)
library(here)
library(rgbif)
library(dplyr)
library(shinyWidgets)

# things to sort out:

# results - merge csv import and gbif
# make sure sequence works when analysis is on/off

# add pop up metadata in leaflet map when user clicks points
#   need standard tempalte file to match to so GBIF and csv is the same?
# select species from GBIF should be drop down? or just go with best guess see rapid LC?
# validate csv < handle when csv doesn't have lat long cols, or valid data.
# Import GBIF data << selectize from list of options? or best match?
# edit points << leaflet extra has draw option, but not sure how to track points
# add loading widgets?
# change user and gbif points in map legend to match point colours

# other issues:
# how to deal with shiny app turning off when not in use
# RCAT2 needs to be on CRAN for shiny geocat to be deployed
# read mastering shiny book -
# possibly hire shiny developer?

# What other options can we add with R scripts?
# Justin's AOO sampling
# Buffer points by X distance
# Random sample of points to generate EOO - range. Make a small chart in results
# AOH -  may be hard to execute with large raster files

source(here("functions.R"))

# UI ----
ui <-
  fluidPage(# set the theme - dark is a bit like current GeoCAT, but we could customise further
    theme = shinytheme("darkly"),
    
    # App title - not sure I understand the div thing, but just wanted a subtitle
    titlePanel(div(
      "GeoCAT", tags$h5("Geospatial Conservation Assessment Tool")
    )),
    
    # Sidebar layout with input and output definitions
    sidebarLayout(
      # Sidebar panel for inputs
      sidebarPanel(
        # Output: Tab panels to split the gbif and csv imports
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Import CSV",
            # Input: input user csv file - currently only needs to have latitude and longitude columns
            helpText("Upload a CSV with 'longitude' and 'latitude' fields"),
            fileInput("file1", NULL, multiple = FALSE, accept = (".csv")),
          ),
          tabPanel(
            "Query GBIF",
            # Input: select a species from GBIF
            helpText("Enter species name to search GBIF occurrences"),
            textInput("GBIFname", "Enter species name"),
            helpText("Maximum number of GBIF occurrences (default 1000)"),
            textInput("GBIFmax", "Enter number", value = 1000, width = '100px'),
            actionButton("searchGBIF", "Query GBIF")
            
          )
          
        ),
        
        # Switch the analysis on/off - not working properly
        materialSwitch(
          inputId = "Analysis",
          label = "Analysis on/off",
          value = FALSE,
          status = "success"
        ),
        
        # Output - report the EOO and AOO results
        htmlOutput("res_title"),
        htmlOutput("text")
      ),
      
      # Main panel for displaying outputs
      # Output - leaflet map with points
      mainPanel(
        leaflet::leafletOutput(
          "mymap", width = "100%", height = 600
        ),)
    ))