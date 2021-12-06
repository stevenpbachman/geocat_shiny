# server
library(leaflet)
library(leaflet.extras)
library(shinythemes)
library(rCAT)
library(readr)
library(sf)
library(kewr)
library(dplyr)

# Server
server <- function(input, output) {
  
  # prepare the points
  csvpointsInput <- eventReactive(input$file1, {
    
    df <- read.csv(input$file1$datapath) #encoding = 'UTF-8')
    
  })
  
  gbifpointsInput <- eventReactive(input$searchGBIF, {
   
  req(input$searchGBIF)
  gbif_keys <- name_search(input$searchGBIF)
  gbif_key <- gbif_keys$GBIF_key
  get_gbif_points(gbif_key, 50)
    
  })
  
  #v <- reactiveValues(data = NULL)
  
  #observeEvent(input$Analysis, {
  #  v$data <-  csvpointsInput()
  #  lldata = thedata %>%
  #    dplyr::select(longitude, latitude)
  #  
  #  lldata = lldata %>%
  #    dplyr::rename(lat = latitude,
  #                  long = longitude)
  #  thepoints <- rCAT::simProjWiz(lldata)
  #  theEOO = rCAT::eoo(thepoints)
  #  theAOO = rCAT::aoo(thepoints)
  #  
  #  str1 <- paste("Extent of occurrence: ", format(round(as.numeric(theEOO)),big.mark = ","), "(km squared)")
  #  str2 <- paste("Area of occupancy: ", format(round(as.numeric(theAOO)), big.mark = ","), "(km squared)")
  #  HTML(paste(str1, str2, sep = '<br/>'))
  #})
  
  switchon = eventReactive(input$Analysis, {

            thedata = csvpointsInput()
            lldata = thedata %>%
              dplyr::select(longitude, latitude)
            lldata = lldata %>%
              dplyr::rename(lat = latitude,
                            long = longitude)
            thepoints <- rCAT::simProjWiz(lldata)
            theEOO = rCAT::eoo(thepoints)
            theAOO = rCAT::aoo(thepoints)
           
            str1 <- paste("Extent of occurrence: ", format(round(as.numeric(theEOO)),big.mark = ","), "(km squared)")
            str2 <- paste("Area of occupancy: ", format(round(as.numeric(theAOO)), big.mark = ","), "(km squared)")
            HTML(paste(str1, str2, sep = '<br/>'))
                 #renderUI({
                      #expression("My Title"^2))      
                 
               })
#

  # base leaflet output map
  output$mymap <- renderLeaflet({

    leaflet() %>%
      
      setView(lng=0, lat=0, zoom = 2) %>% 
      
      addSearchOSM(options = searchOptions(autoCollapse = F, 
                                           collapsed = F, 
                                           minLength = 2,
                                           position = "topright")) %>% 
    
      addScaleBar(position = "bottomright") %>%
      
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#10bae0",
        completedColor = "#241ad9"
      ) %>%
      
      addProviderTiles(providers$Esri.WorldImagery,
                       group = "ESRI World Imagery (default)",
                       options = providerTileOptions(noWrap = TRUE))  %>%
      
      addProviderTiles(providers$Esri.WorldStreetMap,
                       group = "ESRI Open Street map",
                       options = providerTileOptions(noWrap = TRUE))  %>%
      
      addProviderTiles(providers$OpenStreetMap.Mapnik,
                       group = "Open Street Map",
                       options = providerTileOptions(noWrap = TRUE))  %>%
      
      addProviderTiles(providers$OpenTopoMap,
                       group = "Open Topo Map",
                       options = providerTileOptions(noWrap = TRUE))  %>%
      
      #addDrawToolbar(
      #  targetGroup = "View Points",
      #  polylineOptions = FALSE,
      #  circleOptions = FALSE,
      #  rectangleOptions = FALSE,
      #  markerOptions = FALSE,
      #  polygonOptions = FALSE,
      #  circleMarkerOptions = drawCircleMarkerOptions(color = "#FFFFFF", 
      #                                                weight = 7,
      #                                                stroke = T,
      #                                                fillOpacity = 1,
      #                                                fill = T,
      #                                                fillColor = "#c7370f"),
      #  editOptions = editToolbarOptions(
      #    selectedPathOptions = selectedPathOptions()
      #  )
      #) %>%
      

      addLayersControl( 
        baseGroups = c("ESRI World Imagery", "Open Street Map", "Open Topo Map","ESRI Open Street map"),
        overlayGroups = ("View Points"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    
  })
  
  # proxy map to add csv points
  observeEvent(input$file1,{
  
    leafletProxy("mymap", data = csvpointsInput()) %>%

      clearMarkers() %>%

      # zoom to fit - can we buffer this a little?
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude)) %>%

      # add markers from the data
      addCircleMarkers(group = "View Points",
                       lng = ~longitude,
                       lat = ~latitude, 
                       radius = 7, 
                       color = "#FFFFFF", 
                       stroke = T,
                       fillOpacity = 1,
                       fill = T,
                       fillColor = "#0070ff") #%>%
      
      # add convex hull to illustrate EOO
      #addPolygons(hulls <- data %>%
      #              st_as_sf(thepoints, coords = c("longitude", "latitude"), crs = 4326) %>%
      #              geometry = st_combine( geometry )  %>%
      #              st_convex_hull())  
    
      
  })
  
  # proxy map to add gbif points
  observeEvent(input$searchGBIF,{
    
    leafletProxy("mymap", data = gbifpointsInput()) %>%

      clearMarkers() %>%
      
      # zoom to fit - can we buffer this a little?
      fitBounds(~min(DEC_LONG), ~min(DEC_LAT), ~max(DEC_LONG), ~max(DEC_LAT)) %>%
      
      # adjust view if smaller than 2
      #setView(lng=0, lat=0, zoom = 2) %>% zoom = 2
      
      # add markers from the data
      addCircleMarkers(group = "View Points",
                       lng = ~DEC_LONG,
                       lat = ~DEC_LAT, 
                       radius = 7, 
                       color = "#FFFFFF", 
                       stroke = T,
                       fillOpacity = 1,
                       fill = T,
                       fillColor = "#008000") #%>%
    
    # add convex hull to illustrate EOO
    #addPolygons(hulls <- data %>%
    #              st_as_sf(thepoints, coords = c("longitude", "latitude"), crs = 4326) %>%
    #              geometry = st_combine( geometry )  %>%
    #              st_convex_hull())  
    
    
  })
  
  # render the output of the EOO and AOO results 
  output$res_title <- renderUI({
    HTML(paste0("<b>","Your Results:","</b>"))
    })    
  
  output$text <- renderUI({
    
    #switchon()
    switchon()
    
    })

  # render the results of the GBIF search as a table in the side panel
  #output$summarytab <- DT::renderDataTable({
  #  gbifpointsInput()
  #    },
  #options = list(pageLength = 5),
  #selection="single"
  #)
  
  
}



  