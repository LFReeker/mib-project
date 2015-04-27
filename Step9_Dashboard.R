# devtools::install_github("rstudio/shinydashboard")

library(shiny)
library(shinydashboard)

# Go to http://shiny.rstudio.com/gallery/widget-gallery.html to see all the different
# possibilities of inputs

# Define the header of the dashboard
header <- dashboardHeader(title = "Tai'an's dashboard")

# Define the sidebar of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Introduction", tabName = "tab1", icon = icon("th")),
    menuItem("Step 0", tabName = "tab2", icon = icon("dashboard")),
    menuItem("Step 1", tabName = "tab3", icon = icon("th")),
    menuItem("Step 2", tabName = "tab4", icon = icon("th")),
    menuItem("Step 3", tabName = "tab5", icon = icon("th")),
    menuItem("Step 4", tabName = "tab6", icon = icon("th")),
    menuItem("Step 5", tabName = "tab7", icon = icon("th"))
  )
)

# Define the body of the dashboard
body <- dashboardBody(
  tabItems(
  
    # Tab 1
    tabItem(tabName = "tab1",
            h2("Welcome to the Tai'an Data Dashboard")
    ),
  
    # Tab 2
    tabItem(tabName = "tab2",
            h3("Load dataset"),
            fluidRow(
              box(
                title = "Inputs", 
                status = "primary",
                width = 12,
                "Box content here", br(), "More box content",
                fileInput('file1', 'Choose CSV File',
                          accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                tags$hr(),
                checkboxInput('header', 'Header', TRUE),
                radioButtons('sep', 'Separator',
                             c(Comma=',',
                               Semicolon=';',
                               Tab='\t'),
                             ','),
                radioButtons('quote', 'Quote',
                             c(None='',
                               'Double Quote'='"',
                               'Single Quote'="'"),
                             '"')
              )
            ),
            fluidRow(
              
              box(
                title = "Data", 
                status = "primary",
                width = 12,
                tableOutput('contents')
              )
            )
    ),
    
    # Tab 3
    tabItem(tabName = "tab3",
          h3("Provide shop location"),
          fluidRow(
            box(
              title = "Location Input", 
              status = "primary",
              width = 10,
              collapsible = TRUE,
              
              
              textInput("input_text_location", 
                        label = "Enter your address/location here:",
                        value = ""),
             
              helpText("This location search uses Google Maps"),
              submitButton(text = "Retrieve coordinates", 
                           icon = icon("pushpin", 
                           lib="glyphicon")),
              tags$hr(),
              
              textOutput("output_text_location_1"),
              textOutput("output_text_location_2"),
              plotOutput("ouput_map_location")
            )
          )
    ),
    
    # Tab 4
    tabItem(tabName = "tab4",
          h3("Step 2")
    ),
    
    # Tab 5
    tabItem(tabName = "tab5",
          h3("Step 3")
    ),
    
    # Tab 6
    tabItem(tabName = "tab6",
          h3("Step 4")
    ),
    
    # Tab 7
    tabItem(tabName = "tab7",
          h3("Step 5")
    )
    
  # Close tabItems
  )
# Close body  
)

# Combine the UI of dashboard
ui <- dashboardPage(header, sidebar, body)



#############################################################
##                                                        ###
##                      SERVER SIDE                       ###
##                                                        ###
#############################################################

# Create server side of dashboard
server <- function(input, output) {
  
  ## Server info
  # Set seed
  
  ## Data
  
  ## Libraries
  library("ggplot2")
  library("ggmap")
  
  
  ############
  ### Tabs ###
  ############
  
  ## Tab 1
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if(is.null(inFile)) {
      return(NULL)
    } else {
      data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
               quote=input$quote)
      head(data,20)
    }
      
  })
  
  ## Tab 2
  output$output_text_location_1 <- renderText({ 
      
    text_location <- input$input_text_location
    
    if(text_location == "") {
      return(NULL)
    } else {
      paste("You have selected ", text_location, "as your location.")
    }
  })
  
  output$output_text_location_2 <- renderText({ 
    
    text_location <- input$input_text_location
    
    if(text_location == "") {
      return(NULL)
    } else {
      coordinates <- geocode(text_location)
      paste("The longtitude is ", coordinates$lon, "and latitude is ", coordinates$lat, ".")
    }
  })
  
  output$ouput_map_location <- renderPlot({
    
    text_location <- input$input_text_location
    coordinates <- geocode(text_location)
    
    locationMap <- get_map(location=text_location,
                     source="google", maptype= "roadmap", crop=FALSE, zoom = 21)
    
    ggmap(locationMap) +
    geom_point(aes(x = lon, y = lat), data = coordinates,
               alpha = .5, color="darkred", size = 3)
  
  })
      
}


# Create the dashboard
shinyApp(ui, server)





### Load the useful functions
#source("./code/sna/plotNetwork.R")

## Load the necessary datasets
#load("./data/sna/stackoverflow_graph.RData")
