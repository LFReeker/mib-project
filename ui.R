library(igraph)
library(plyr)
library(ggplot2)
library(TraMineR)
library(cluster)
library(ggmap)
library(reshape2)
library(shiny)
library(shinydashboard)
library(lsa)


# Define the header of the dashboard
header <- dashboardHeader(title = "Tai'an Dashboard")

# Define the sidebar of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Introduction", tabName = "tab0", icon = icon("info-circle")),
    menuItem("1. Call dataset", tabName = "tab1", icon = icon("database")),
    menuItem("2. Celltower dataset", tabName = "tab2", icon = icon("database")),
    menuItem("3. Location", tabName = "tab3", icon = icon("map-marker")),
    menuItem("4. Call density", tabName = "tab4", icon = icon("phone")),
    menuItem("5. Neighborhood", tabName = "tab5", icon = icon("group")),
    menuItem("6. Clients (week)", tabName = "tab6", icon = icon("th")),
    menuItem("7. Clients (day)", tabName = "tab7", icon = icon("th")),
    menuItem("8. Social network", tabName = "tab8", icon = icon("group")),
    menuItem("9. Influencial people", tabName = "tab9", icon = icon("th")),
    menuItem("10. Top clients", tabName = "tab10", icon = icon("money")),
    menuItem("11. Targeting clients", tabName = "tab11", icon = icon("th")),
    menuItem("Conclusion", tabName = "tab12", icon = icon("th"))
  )
)

# Define the body of the dashboard
body <- dashboardBody(
  tabItems(
    
    # Tab 0
    tabItem(tabName = "tab0",
            tags$br(),
            tags$br(),
            tags$br(),
            tags$br(),
            tags$br(),
            fluidRow(
              column(12, align="center",
                     tags$img(src = "heinz2.png"),
                     h2("Welcome to the Kelaitou Restaurant dashboard!"),
                     tags$br(),
                     tags$br(),
                     tags$br(),
                     h6("Click ", tags$a(href="http://larsreeker.nl/mib/taian_march_calldata.csv", "here "), "to download the mobile phone call dataset"),
                     h6("Click ", tags$a(href="http://larsreeker.nl/mib/taian_celltower_locations.csv", "here "), "to download the celltower locations dataset")
                     )
            )
            
    ),
    
    # Tab 1
    tabItem(tabName = "tab1",
            h3("Provide a dataset to start the analysis"),
            h5("On this page the mobile phone dataset can be uploaded and processed accordingly to its format."),
            fluidRow(
              box(
                title = "Call data input", 
                status = "primary",
                width = 4,
                fileInput('file1', 'Choose CSV File',
                          accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                tags$hr(),
                checkboxInput('header1', 'Header', TRUE),
                radioButtons('sep1', 'Separator',
                             c(Comma=',',
                               Semicolon=';',
                               Tab='\t'),
                             ';'),
                radioButtons('quote1', 'Quote',
                             c(None='',
                               'Double Quote'='"',
                               'Single Quote'="'"),
                             '')
              ),
              box(
                title = "Data", 
                status = "primary",
                width = 8,
                tags$div(style="width:auto; height:auto; overflow:auto;padding:5px;",
                  dataTableOutput('contents1')
                )
              )
            )
    ),
    
    # Tab 2
    tabItem(tabName = "tab2",
            h3("Provide a dataset to start the analysis"),
            h5("On this page the mobile phone dataset can be uploaded and processed accordingly to its format."),
            fluidRow(
              box(
                title = "Cell data input", 
                status = "primary",
                width = 3,
                fileInput('file2', 'Choose CSV File',
                          accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                tags$hr(),
                checkboxInput('header2', 'Header', TRUE),
                radioButtons('sep2', 'Separator',
                             c(Comma=',',
                               Semicolon=';',
                               Tab='\t'),
                             ';'),
                radioButtons('quote2', 'Quote',
                             c(None='',
                               'Double Quote'='"',
                               'Single Quote'="'"),
                             '')
              ),
              box(
                title = "Data", 
                status = "primary",
                width = 9,
                tags$div(style="width:auto; height:auto; overflow:auto;padding:5px;",
                         dataTableOutput('contents2')
              )
            )
          )
    ),
    
    # Tab 3
    tabItem(tabName = "tab3",
            h3("Provide shop location"),
            h5("On this page the location of the subject (restaurant, shop, etc.) should be provided. This location will be used throughout the dashboard."),
            fluidRow(
              box(
                title = "Location Input", 
                status = "primary",
                width = 8,
                collapsible = TRUE,
                textInput("input_text_location", 
                          label = "Enter your address/location here:",
                          value = "Kelaitou Restaurant, Tai'an, China"),
                
                helpText("This location search uses Google Maps."),
                helpText("Kelaitou Restaurant Wenhua Rd Taishan, Tai'an, Shandong China"),
                actionButton("location_go","Retrieve location", icon = icon("map-marker", lib="font-awesome"))
              )
            ),
            fluidRow(
              box(
                title = "Location Ouput", 
                status = "primary",
                width = 12,
                height = 700,
                collapsible = TRUE,
                htmlOutput("output_text_location"),
                plotOutput("output_map_location")
              )
            )
    ),
    
    # Tab 5
    tabItem(tabName = "tab5",
            h3("Compute the neighborhood of the location"),
            h5("On this page the system let's you select the desired area of the analysis."),
            fluidRow(
              box(
                title = "Radius", 
                status = "primary",
                width = 4,
                collapsible = TRUE,
                sliderInput("input_area_map", label = "Select radius:",
                            min = 1, max = 10, value = 4),
                helpText("We recommend a radius of 2 or 3, which provides a good precision."),
                sliderInput("zoom_area_map", label = "Select zoom level:",
                            min = 10, max = 20, value = 12)
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
            h5("The systems shows here the call density in the whole area per day and per hour."),
            fluidRow(
              box(
                title = "Radius", 
                status = "primary",
                width = 4,
                collapsible = TRUE,
                selectInput("call_density_day", label = "Select day:", 
                            choices = list("Please select a day" = "",
                                           "Saturday March 1st 2008" = 20080301,
                                           "Sunday March 2nd 2008" = 20080302,
                                           "Monday March 3rd 2008" = 20080303,
                                           "Tuesday March 4th 2008" = 20080304,
                                           "Wednesday March 5th 2008" = 20080305,
                                           "Thursday March 6th 2008" = 20080306,
                                           "Friday March 7th 2008" = 20080307)
                            , selected = 20080301),
                sliderInput("call_density_hour", "Select hour:", 0, 23, 0, step = 1, 
                            animate=animationOptions(interval=3000, loop=T))
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
    
    # Tab 6
    tabItem(tabName = "tab6",
            h3("Potential clients by the week"),
            h5("Here, the system lists all the people that have been within the radius at least one time throughout the week."),
            fluidRow(
              box(
                title = "Input", 
                status = "primary",
                width = 5,
                collapsible = TRUE,
                sliderInput("input_clients_radius", label = "Select radius:",
                            min = 1, max = 5, value = 2),
                helpText("We recommend a radius of 2 or 3, which provides a good precision."),
                textInput("input_clients_num", label = "Number of potential clients:", value = 30),
                helpText("For precision, this takes a while...")
              ),
              box(
                title = "List of potential clients (week)", 
                status = "primary",
                width = 7,
                collapsible = TRUE,
                tableOutput("output_clients_week")
              )
            )
    ),
    
    # Tab 7
    tabItem(tabName = "tab7",
            h3("Potential clients by day"),
            h5("Here, the system lists all the people that have been within the radius at least one time at one particular day."),
            fluidRow(
              box(
                title = "Input", 
                status = "primary",
                width = 5,
                collapsible = TRUE,
                sliderInput("input_bad1", label = "Select radius:",
                            min = 1, max = 5, value = 2),
                helpText("We recommend a radius of 2 or 3, which provides a good precision."),
                textInput("input_bad2", label = "Number of potential clients:", value = 10),
                selectInput("call_bad3", label = "Select day:", 
                            choices = list("Please select a day" = "",
                                           "Saturday March 1st 2008" = 20080301,
                                           "Sunday March 2nd 2008" = 20080302,
                                           "Monday March 3rd 2008" = 20080303,
                                           "Tuesday March 4th 2008" = 20080304,
                                           "Wednesday March 5th 2008" = 20080305,
                                           "Thursday March 6th 2008" = 20080306,
                                           "Friday March 7th 2008" = 20080307)
                            , selected = 20080301),
                helpText("For precision, this can take a while...")
              ),
              box(
                title = "List of potential clients (day)", 
                status = "primary",
                width = 5,
                collapsible = TRUE,
                tableOutput("output_bad4")
              )
            )
    ),
    
    # Tab 8
    tabItem(tabName = "tab8",
            h3("Plot connections between people"),
            h5("Here the social network diagram computed from all the connections within the specified range of the location."),
            fluidRow(
              box(
                title = "Input", 
                status = "primary",
                width = 6,
                collapsible = TRUE,
                sliderInput("input_social_radius", label = "Select radius:",
                            min = 1, max = 5, value = 2),
                helpText("We recommend a radius of 2 or 3, which provides a good precision."),
                selectInput("input_social_day", label = "Select day:", 
                            choices = list("Please select a day" = "",
                                           "Saturday March 1st 2008" = 20080301,
                                           "Sunday March 2nd 2008" = 20080302,
                                           "Monday March 3rd 2008" = 20080303,
                                           "Tuesday March 4th 2008" = 20080304,
                                           "Wednesday March 5th 2008" = 20080305,
                                           "Thursday March 6th 2008" = 20080306,
                                           "Friday March 7th 2008" = 20080307)
                            , selected = 20080301),
                helpText("For precision, this takes a while...")
              ),
              box(
                title = "Social Network", 
                status = "primary",
                width = 12,
                height = 700,
                collapsible = TRUE,
                plotOutput("output_social_network")
              )
            )
    ),
    
    # Tab 9
    tabItem(tabName = "tab9",
            h3("List the important people for the location"),
            h5("This page lists the people that are the most influencial in the social network (see 8. Social Network)."),
            fluidRow(
              box(
                title = "Input", 
                status = "primary",
                width = 4,
                collapsible = TRUE,
                sliderInput("input_important_radius", label = "Select radius:",
                            min = 1, max = 5, value = 2),
                helpText("We recommend a radius of 2 or 3, which provides a good precision."),
                textInput("input_important_num", label = "Number users:", value = 200),
                selectInput("input_important_day", label = "Select day:", 
                            choices = list("Please select a day" = "",
                                           "Saturday March 1st 2008" = 20080301,
                                           "Sunday March 2nd 2008" = 20080302,
                                           "Monday March 3rd 2008" = 20080303,
                                           "Tuesday March 4th 2008" = 20080304,
                                           "Wednesday March 5th 2008" = 20080305,
                                           "Thursday March 6th 2008" = 20080306,
                                           "Friday March 7th 2008" = 20080307)
                            , selected = 20080301)
              ),
              box(
                title = "List", 
                status = "primary",
                width = 8,
                collapsible = TRUE,
                tableOutput("output_important_list")
              )
            )
    ),
    
    # Tab 10
    tabItem(tabName = "tab10",
            h3("Top users"),
            h5("This analysis shows the list of people that are likely to be in the area of the location at the given time."),
            fluidRow(
              box(
                title = "Input", 
                status = "primary",
                width = 4,
                collapsible = TRUE,
                sliderInput("input_top_radius", label = "Select radius:",
                            min = 1, max = 15, value = 5),
                helpText("We recommend a radius of 2 or 3, which provides a good precision."),
                textInput("input_top_num", label = "Number users:", value = 15),
                selectInput("input_top_week", label = "Select weekday or weekend:", 
                            choices = list("Please select weekday or weekend" = "",
                                           "Weekday" = "Weekday",
                                           "Weekend" = "Weekend")
                            , selected = "Weekday"),
                selectInput("input_top_meal", label = "Select meal type:",
                            choices = list("Please select a meal type" = "",
                                           "Breakfast" = "Breakfast",
                                           "Lunch" = "Lunch",
                                           "Snacks" = "Snacks",
                                           "Dinner" = "Dinner")
                            , selected = "Lunch")
              ),
              box(
                title = "List", 
                status = "primary",
                width = 8,
                collapsible = TRUE,
                tableOutput("output_top_users")
              )
            )
    ),
    
    # Tab 11
    tabItem(tabName = "tab11",
            h3("Recommendation"),
            h5("This final analysis shows the potential clients that are connected to the userID provided. These potential clients are generated from the social network."),
            fluidRow(
              box(
                title = "Input", 
                status = "primary",
                width = 4,
                collapsible = TRUE,
                sliderInput("input_recomm_radius", label = "Select radius:",
                            min = 1, max = 15, value = 10),
                helpText("We recommend a radius of 2 or 3, which provides a good precision."),
                textInput("input_recomm_num", label = "Number users:", value = 5),
                textInput("input_recomm_number", label = "User ID:", value = "77969815431"),
                helpText("User: 77969815431")
              ),
              box(
                title = "List", 
                status = "primary",
                width = 8,
                collapsible = TRUE,
                tableOutput("output_recomm")
              )
            )
    ),
    
    # Tab 12
    tabItem(tabName = "tab12")
            
            
    
    
  # Close tabItems
  )
  
# Close body  
)

# Combine the UI of dashboard
ui <- dashboardPage(header, sidebar, body)
