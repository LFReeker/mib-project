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
                             '"'),
                submitButton(text = "Update dataset", 
                             icon = icon("cog", 
                                         lib="glyphicon"))
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
                width = 6,
                collapsible = TRUE,
                
                
                textInput("input_text_location", 
                          label = "Enter your address/location here:",
                          value = ""),
                
                helpText("This location search uses Google Maps"),
                submitButton(text = "Retrieve coordinates", 
                             icon = icon("pushpin", 
                                         lib="glyphicon"))
              )
            ),
            fluidRow(
              box(
                title = "Location Ouput", 
                status = "primary",
                width = 12,
                height = 700,
                collapsible = TRUE,
                textOutput("output_text_location_1"),
                textOutput("output_text_location_2"),
                plotOutput("output_map_location")
              )
            )
    ),
    
    # Tab 4
    tabItem(tabName = "tab4",
            h3("Shop area"),
            fluidRow(
              box(
                title = "Radius", 
                status = "primary",
                width = 4,
                collapsible = TRUE,
                sliderInput("input_area_map", label = "Select radius",
                            min = 0, max = 5, value = 2),
                helpText("We recommend a radius of 2 or 3, which provides a good precision."),
                submitButton(text = "Compute area", 
                             icon = icon("cog", 
                                         lib="glyphicon"))
              ),
              box(
                title = "Map", 
                status = "primary",
                width = 8,
                collapsible = TRUE,
                plotOutput("output_area_map")
              )
            )
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
