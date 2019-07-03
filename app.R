
# from https://github.com/r-spatial/mapedit/blob/master/inst/examples/shiny_modules.R 
# from https://github.com/jrowen/rhandsontable/tree/master/inst/examples/rhandsontable_datafile
# save data: https://towardsdatascience.com/get-started-with-examples-of-reactivity-in-in-shiny-apps-db409079dd11
# Leaflet adwanced: 

library(mapedit)
library(mapview)
library(shiny)
library(rhandsontable)
library(sp)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(uuid)
source("functions.R")

# define environment and load data
user_list <- read.csv("./data/input/user_list.csv", stringsAsFactors = FALSE)

locations <- readRDS(file = "./data/input/location.rds") %>% # load location data and make spatial
  filter(county=="Sør-Trøndelag") 
coordinates(locations) = ~decimalLongitude + decimalLatitude
latlong = "+init=epsg:4326"
proj4string(locations) = CRS(latlong)

taxon_list <- read.csv("./data/input/taxon_list.csv", stringsAsFactors = FALSE) # load taxon list

m <- leaflet(locations) %>% # create leaflet for selection 
    addTiles() %>% 
    addCircleMarkers(weight = 1, layerId = 1:nrow(locations),label = locations$waterBody) %>%
    addMiniMap()

#dir.create(file.path("./data/input/"), showWarnings = FALSE)
dir.create(file.path("./data/output/"), showWarnings = FALSE)

#---------------------------------------------------------------------
# UI design 
#---------------------------------------------------------------------
ui <- navbarPage(title="Fish inventory",
  tabPanel("Record occurrences",
  sidebarLayout(
    sidebarPanel(
     titlePanel("Fish inventory data entry"),
     br(),
     p("some text explaining what it's all about..."),
     br(),
     textInput("recorded_by","Recorded by (optional: information will be made public)", value = ""),
     textInput("username","Username", value = ""),
     textInput("user_code", "User code", value = "",
                   placeholder = "enter code specified"),
     actionButton("submitt_cred", "Submitt credentials"),
     textOutput("check_cred")
     ),
     
     mainPanel(
       tabsetPanel(
       tabPanel("location and event",
     dateInput("event_date","event date", value = Sys.Date()),
      selectModUI("test-mod"),
      DT::dataTableOutput("selected")
       ),
     tabPanel("Occurrences",
      actionButton("submit", "Submit and clear form"),
      actionButton("clear", "Clear Form"),
      rHandsontableOutput("hot")
      )
     )
     )
  )
  ),
    tabPanel("output data",
             downloadButton("downloadData", "Download"),
             dataTableOutput("responses")
  ),
  tabPanel("output map",
           leafletOutput("output_map", width = "100%", height = 1000)
)
)



#---------------------------------------------------------------------
# server logic 
#---------------------------------------------------------------------
server <- function(input, output, session) {

  # # When the "submitt and clear" or the "clear form" buttons are clikcked, clear form by restarting session -----
  observeEvent(input$submit, {
    session$reload()
  })
  observeEvent(input$clear, {
    session$reload()
  })
  
  # check user credentials ------------------------------------------------------------

  
  
    observeEvent(input$submitt_cred, {
    output$check_cred <- renderText({
      req(input$user_code)
      req(input$username)
      if(input$user_code %in% user_list$user_code & input$username == user_list$username[user_list$user_code==input$user_code]){
       "user OK"
        } else {
          "wrong credentials"
          }
      })
    })
  
  
  # select locations --------------------------------------------------------
    selections <- callModule(selectMod, id="test-mod", leafmap=m)
    output$selected <- DT::renderDataTable({DT::datatable(locations@data[as.numeric(selections()$id),c(1:4)],
                                                          rownames= FALSE)})

  # Select occurrences --------------------------------------
  fname = tempfile(fileext = ".csv")
  
  observe({
    # remove button and isolate to update file automatically
    # after each table change
    input$submit
    hot = isolate(input$hot)
    if (!is.null(hot)) {
      #write.csv(hot_to_r(input$hot), fname)
      print(fname)
      print(locations@data[as.numeric(selections()$id),])
    }
  })
  
  # create input table
  output$hot = renderRHandsontable({
    
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
    } else {
      DF = data.frame(scientificName = factor(taxon_list$ScientificName),
                      occurrenceStatus = factor(rep("uncertain",length(taxon_list$ScientificName)), levels=c("uncertain","present","absent")),
                      establishmentMeans = factor(rep("native", length(taxon_list$ScientificName)), levels=c("native","introduced","uncertain")),
                      occurrenceRemarks = rep("", length(taxon_list$ScientificName)),
                      stringsAsFactors = FALSE)

                      #date = seq(from = Sys.Date(), by = "days", length.out = length(taxon_list$ScientificName)), stringsAsFactors = FALSE)
    }
    
    rhandsontable(DF, width = 600, height = 300) %>%
      hot_col("scientificName", readOnly = TRUE)
  })
  
  # save data -----------------------------------------------------
  observeEvent(input$submit, {
    # check if user credentials are OK
    if(input$user_code %in% user_list$user_code & input$username == user_list$username[user_list$user_code==input$user_code]){
      
      # Create a unique file name
      fileName <- sprintf(
        "%s_%s.csv",
        as.integer(Sys.time()),
        digest::digest(data)
      )
      
      # compile data
      occurrences <- hot_to_r(input$hot)
      #locs_selected <- data.frame(locations@data[as.numeric(selections()$id),]) 
      locs_selected <- data.frame(locations)[as.numeric(selections()$id),]
      locs_selected$eventID <- generate_UUID(dim(locs_selected)[1])
      occurrences2 <- do.call("rbind", replicate(n=dim(locs_selected)[1], occurrences, simplify = FALSE))
      locs_selected2 <- do.call("rbind", replicate(n=dim(occurrences)[1], locs_selected, simplify = FALSE)) %>%
        dplyr::arrange(locationID)
      
      data <- dplyr::bind_cols(as.data.frame(occurrences2),as.data.frame(locs_selected2)) 
      data$occurrenceID <- generate_UUID(dim(data)[1])
      data$recordedBy <- input$recorded_by
      data$eventDate <- input$event_date
      data$timeStamp <- Sys.time()
      data$username <- input$username
      
      # Write the file to the local system
      write.csv(data,
                file = file.path("./data/output", fileName)
      )
      
    }
    })


  
  # render all recorded data to table -------------------------------------------------
  output$responses <- renderDataTable({
    # update with current response when Submit or Delete are clicked
    input$submit 
    #input$delete
    
    loadData()
  })
  
  output$downloadData <- downloadHandler(
    filename = "fish_inventory_data.csv",
    content = function(file) {
      write.csv(loadData(), file, row.names = FALSE, quote= TRUE)
    }
  )
  
  # map of all recorded data
  output$output_map <- renderLeaflet({
    
    input$submit 
    
    locations_out <- loadData()
    coordinates(locations_out) = ~decimalLongitude + decimalLatitude
    latlong = "+init=epsg:4326"
    proj4string(locations_out) = CRS(latlong)
    
    leaflet(locations_out) %>% # create leaflet for selection 
      addTiles() %>% 
      addCircleMarkers(weight = 1, layerId = 1:nrow(locations),label = locations$waterBody) %>%
      addMiniMap()
  })
  
}
shinyApp(ui, server)

