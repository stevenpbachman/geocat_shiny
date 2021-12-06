# server
library(leaflet)
library(leaflet.extras)
library(shinythemes)
library(rCAT)
library(readr)
library(sf)
library(kewr)
library(dplyr)

server <- function(input, output) {
  # react to the user points being added
  csvpointsInput <- eventReactive(input$file1, {
    df <- read.csv(input$file1$datapath) #encoding = 'UTF-8')
    
  })
  
  # react to the GBIF search box being used
  # then trigger code to select best match and get occurrence data
  gbifpointsInput <- eventReactive(input$searchGBIF, {
    req(input$searchGBIF)
    gbif_keys <- name_search(input$searchGBIF)
    gbif_key <- gbif_keys$GBIF_key
    get_gbif_points(gbif_key, input$GBIFmax)
    
  })
  
  # controls the analysis on/off button
  # when triggered, it should take the data (only user csv at the moment)
  # and run the EOO/AOO analysis
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
    
    str1 <-
      paste("Extent of occurrence: ",
            format(round(as.numeric(theEOO)), big.mark = ","),
            "(km squared)")
    str2 <-
      paste("Area of occupancy: ",
            format(round(as.numeric(theAOO)), big.mark = ","),
            "(km squared)")
    HTML(paste(str1, str2, sep = '<br/>'))
    #renderUI({
    #expression("My Title"^2))
    
  })

  # leaflet base output map
  output$mymap <- renderLeaflet({
    leaflet() %>%
      
      setView(lng = 0,
              lat = 0,
              zoom = 2) %>%
      
      addSearchOSM(options = searchOptions(
        autoCollapse = F,
        collapsed = F,
        minLength = 2,
        position = "topright"
      )) %>%
      
      addScaleBar(position = "bottomright") %>%
      
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#10bae0",
        completedColor = "#241ad9"
      ) %>%
      
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "ESRI World Imagery (default)",
        options = providerTileOptions(noWrap = TRUE)
      )  %>%
      
      addProviderTiles(
        providers$Esri.WorldStreetMap,
        group = "ESRI Open Street map",
        options = providerTileOptions(noWrap = TRUE)
      )  %>%
      
      addProviderTiles(
        providers$OpenStreetMap.Mapnik,
        group = "Open Street Map",
        options = providerTileOptions(noWrap = TRUE)
      )  %>%
      
      addProviderTiles(providers$OpenTopoMap,
                       group = "Open Topo Map",
                       options = providerTileOptions(noWrap = TRUE))  %>%
      
      # leaflet extras has a toolbar that allows drawing markers
      # and editing points, but not sure how to control
      
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
      baseGroups = c(
        "ESRI World Imagery",
        "Open Street Map",
        "Open Topo Map",
        "ESRI Open Street map"
      ),
      overlayGroups = ("View Points"),
      options = layersControlOptions(collapsed = FALSE)
    )
    
    
  })
  
  # Porxy maps help control specific elements on the map e.g. points from csv or gbif
  # proxy map to add csv points
  observeEvent(input$file1, {
    leafletProxy("mymap", data = csvpointsInput()) %>%
      
      # clear previous markers
      clearMarkers() %>%
      
      # zoom to fit - can we buffer this a little? it is too tight
      fitBounds( ~ min(longitude),
                 ~ min(latitude),
                 ~ max(longitude),
                 ~ max(latitude)) %>%
      
      # add markers from the data
      addCircleMarkers(
        group = "View Points",
        lng = ~ longitude,
        lat = ~ latitude,
        radius = 7,
        color = "#FFFFFF",
        stroke = T,
        fillOpacity = 1,
        fill = T,
        fillColor = "#0070ff"
      ) 
    
    # TO DO - add convex hull graphics to visualise EOO
    # add convex hull to illustrate EOO
    # below not working
    #%>%
    #  addPolygons(data %>%
    #              st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    #              st_combine()  %>%
    #              st_convex_hull())
    #
      })
  
  # proxy map to add gbif points
  observeEvent(input$searchGBIF, {
    leafletProxy("mymap", data = gbifpointsInput()) %>%
      
      clearMarkers() %>%
      
      # zoom to fit - can we buffer this a little?
      fitBounds( ~ min(DEC_LONG),
                 ~ min(DEC_LAT),
                 ~ max(DEC_LONG),
                 ~ max(DEC_LAT)) %>%
      
      # add markers from the data
      addCircleMarkers(
        group = "View Points",
        lng = ~ DEC_LONG,
        lat = ~ DEC_LAT,
        radius = 7,
        color = "#FFFFFF",
        stroke = T,
        fillOpacity = 1,
        fill = T,
        fillColor = "#008000"
      ) #%>%
    
    # TO DO - add convex hull graphics to visualise EOO
    # add convex hull to illustrate EOO
    # below not working
    #%>%
    #  addPolygons(data %>%
    #              st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    #              st_combine()  %>%
    #              st_convex_hull())
    #
    #  })
    
    
  })
  
  # render the output of the EOO and AOO results
  output$res_title <- renderUI({
    HTML(paste0("<b>", "Your Results:", "</b>"))
  })
  
  output$text <- renderUI({

    switchon()
    
  })
  
  # render the results of the GBIF search as a table?
  #output$summarytab <- DT::renderDataTable({
  #  gbifpointsInput()
  #    },
  #options = list(pageLength = 5),
  #selection="single"
  #)

}



