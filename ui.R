library(igraph)
library(plyr)
library(ggplot2)
library(TraMineR)
library(cluster)
library(ggmap)
library(reshape2)
library(shiny)
library(shinydashboard)

# Define the header of the dashboard
header <- dashboardHeader(title = "Tai'an's dashboard")

# Define the sidebar of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Introduction", tabName = "tab0", icon = icon("info-circle")),
    menuItem("1. Dataset", tabName = "tab1", icon = icon("database")),
    menuItem("2. Location", tabName = "tab2", icon = icon("map-marker")),
    menuItem("3. Neighborhood", tabName = "tab3", icon = icon("group")),
    menuItem("4. Call density", tabName = "tab4", icon = icon("phone")),
    menuItem("5. General trend", tabName = "tab5", icon = icon("line-chart")),
    menuItem("6. People week", tabName = "tab6", icon = icon("th")),
    menuItem("7. People day", tabName = "tab7", icon = icon("th")),
    menuItem("8. Social network", tabName = "tab8", icon = icon("group")),
    menuItem("9. Important people", tabName = "tab9", icon = icon("th")),
    menuItem("10. Top clients", tabName = "tab10", icon = icon("money")),
    menuItem("11. Targeting clients", tabName = "tab11", icon = icon("th")),
    menuItem("12. test", tabName = "tab12", icon = icon("th")),
    menuItem("13. test", tabName = "tab13", icon = icon("th"))
  )
)

# Define the body of the dashboard
body <- dashboardBody(
  tabItems(
    
    # Tab 0
    tabItem(tabName = "tab0",
            h2("Welcome to the Tai'an Data Dashboard")
    ),
    
    # Tab 1
    tabItem(tabName = "tab1",
            h3("Provide a dataset to start the analysis"),
            fluidRow(
              box(
                title = "Data input", 
                status = "primary",
                width = 12,
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
                             icon = icon("cog", lib="glyphicon"))
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
    
    # Tab 2
    tabItem(tabName = "tab2",
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
                
                helpText("This location search uses Google Maps."),
                helpText("Kelaitou Restaurant Wenhua Rd Taishan, Tai'an, Shandong China"),
                submitButton(text = "Retrieve coordinates", 
                             icon = icon("pushpin", lib="glyphicon"))
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
    
    # Tab 3
    tabItem(tabName = "tab3",
            h3("Compute the neighborhood of the location"),
            fluidRow(
              box(
                title = "Radius", 
                status = "primary",
                width = 4,
                collapsible = TRUE,
                sliderInput("input_area_map", label = "Select radius",
                            min = 1, max = 5, value = 2),
                helpText("We recommend a radius of 2 or 3, which provides a good precision."),
                submitButton(text = "Compute area", 
                             icon = icon("cog", lib="glyphicon"))
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
    
    # Tab 4
    tabItem(tabName = "tab4",
            h3("Call Density"),
            fluidRow(
              box(
                title = "Radius", 
                status = "primary",
                width = 4,
                collapsible = TRUE,
                selectInput("call_density_day", label = h3("Select day:"), 
                            choices = list("Please select a day" = "",
                                           "Saturday March 1st 2008" = 20080301,
                                           "Sunday March 2nd 2008" = 20080302,
                                           "Monday March 3rd 2008" = 20080303,
                                           "Tuesday March 4th 2008" = 20080304,
                                           "Wednesday March 5th 2008" = 20080305,
                                           "Thursday March 6th 2008" = 20080306,
                                           "Friday March 7th 2008" = 20080307)
                            , selected = ""),
                sliderInput("call_density_hour", "Select hour or animate:", 0, 23, 0, step = 1, 
                            animate=animationOptions(interval=300, loop=T))
              ),
              box(
                title = "Plot", 
                status = "primary",
                width = 8,
                collapsible = TRUE,
                plotOutput("output_call_density")
              )
            )
    ),
    
    
    
    # Tab 5
    tabItem(tabName = "tab5",
            h3("General call trend throughout the week"),
            fluidRow(
              box(
                title = "Radius", 
                status = "primary",
                width = 4,
                collapsible = TRUE,
                sliderInput("input_general_trend", label = "Select radius",
                            min = 1, max = 5, value = 2),
                helpText("We recommend a radius of 2 or 3, which provides a good precision."),
                submitButton(text = "Compute area", 
                             icon = icon("cog", 
                                         lib="glyphicon"))
              ),
              box(
                title = "Plot", 
                status = "primary",
                width = 8,
                collapsible = TRUE,
                plotOutput("output_general_trend")
              )
            )
    ),
    
    # Tab 6
    tabItem(tabName = "tab6",
            h3("Potential clients by the week"),
            fluidRow(
              box(
                title = "Input", 
                status = "primary",
                width = 4,
                collapsible = TRUE,
                sliderInput("input_clients_radius", label = "Select radius:",
                            min = 1, max = 5, value = 2),
                helpText("We recommend a radius of 2 or 3, which provides a good precision."),
                textInput("input_clients_num", label = "Number of potential clients:", value = 10),
                submitButton(text = "Compute list", 
                             icon = icon("cog", 
                                         lib="glyphicon")),
                helpText("For precision, this takes a while...")
              ),
              box(
                title = "List of potential clients (week)", 
                status = "primary",
                width = 8,
                collapsible = TRUE,
                tableOutput("output_clients_week")
              )
            )
    ),
    
    # Tab 7
    tabItem(tabName = "tab7",
            h3("blabla"),
            fluidRow(
              box(
                title = "Input", 
                status = "primary",
                width = 4,
                collapsible = TRUE
              )
            )
    ),
    
    # Tab 8
    tabItem(tabName = "tab8",
            h3("blabla"),
            fluidRow(
              box(
                title = "Input", 
                status = "primary",
                width = 4,
                collapsible = TRUE
              )
            )
    ),
    
    # Tab 9
    tabItem(tabName = "tab9",
            h3("blabla"),
            fluidRow(
              box(
                title = "Input", 
                status = "primary",
                width = 4,
                collapsible = TRUE
              )
            )
    ),
    
    # Tab 10
    tabItem(tabName = "tab10",
            h3("blabla"),
            fluidRow(
              box(
                title = "Input", 
                status = "primary",
                width = 4,
                collapsible = TRUE
              )
            )
    ),
    
    # Tab 11
    tabItem(tabName = "tab11",
            h3("blabla"),
            fluidRow(
              box(
                title = "Input", 
                status = "primary",
                width = 4,
                collapsible = TRUE
              )
            )
    ),
    
    # Tab 12
    tabItem(tabName = "tab12",
            h3("blabla"),
            fluidRow(
              box(
                title = "Input", 
                status = "primary",
                width = 4,
                collapsible = TRUE
              )
            )
    ),
    
    # Tab 13
    tabItem(tabName = "tab13",
            h3("blabla"),
            fluidRow(
              box(
                title = "Input", 
                status = "primary",
                width = 4,
                collapsible = TRUE
              )
            )
    )
    
    # Close tabItems
  )
  # Close body  
)

# Combine the UI of dashboard
ui <- dashboardPage(header, sidebar, body)
