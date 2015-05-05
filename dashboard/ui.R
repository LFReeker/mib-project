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
    menuItem("Welcome", tabName = "tab0"),
    
    menuItem("Introduction", tabName = "tab0",
      menuSubItem("Introduction", tabName = "tab01", icon = icon("info-circle")),
      menuSubItem("Instructions", tabName = "tab02", icon = icon("info-circle"))),
    
    menuItem("Provide information", tabName = "tab0",
      menuSubItem("1. Phone calls dataset", tabName = "tab1", icon = icon("database")),
      menuSubItem("2. Cell Tower dataset", tabName = "tab2", icon = icon("database")),
      menuSubItem("3. Location", tabName = "tab3", icon = icon("map-marker"))),
    
    menuItem("General insights", tabName = "tab0",
     menuSubItem("4. Call density", tabName = "tab4", icon = icon("phone")),
     menuSubItem("5. Neighborhood", tabName = "tab5", icon = icon("home")),
     menuSubItem("6. Social network", tabName = "tab8", icon = icon("group"))),
    
    menuItem("Analysis", tabName = "tab0",
      menuSubItem("7. People in area (week)", tabName = "tab6", icon = icon("users")),
      menuSubItem("8. People in area (day)", tabName = "tab7", icon = icon("users")),
      menuSubItem("9. People in area (time)", tabName = "tab10", icon = icon("users")),
      menuSubItem("10. Highly connected people", tabName = "tab9", icon = icon("male")),
      menuSubItem("11. Similar people in area", tabName = "tab11", icon = icon("user"))),
    
    menuItem("Take aways", tabName = "tab12",
      menuSubItem("Take aways", tabName = "tab12", icon = icon("info-circle")))
  )
)

# Define the body of the dashboard
body <- dashboardBody(
  tabItems(
    
    # Tab 01
    tabItem(tabName = "tab01",
            fluidRow(
                tags$br(),
                column(8, 
                       tags$div(class = "header", checked = NA,
                                h3("Introduction"),
                                h5("Welcome to the Tai'an dashboard! This dashboard is designed and build to provide 
                                    valuable insights in mobile phone call data. The dashboard needs two datasets in 
                                    order to be functional."),
                                tags$p(),
                                h5("Along with the report, this dashboard is the final deliverable for the course 
                                    Mobile Intelligence & Business at the Carnegie Mellon Univeristy, Heinz College faculty.
                                    In this project, we conduct a social network analysis and spatial trajectory analysis 
                                    of on a dataset consisting of 58 million calls, gathered from the area of the Chinees 
                                    city of Tai'an. Furthermore, we designed and created an interactive dashboard which 
                                    you and other users can use by yourself. With this dashboard, we are able to show the
                                    different sequential analysis we performed while maintaining user interactivity and 
                                    flexibility. With this, we think that one is able to understand and digest the 
                                    different (outcomes of the) analysis even more."),
                                tags$p(
                                h5("In the case you have questions about the dashboard or project, please reach 
                                   out to one of us!"),
                                HTML("
                                  <p><i>Subramaniam Balasubramaniam</i><br>
                                   <i>William Liu</i><br>
                                   <i>Praveen Kumar Thoranathula Radhesyam</i><br>
                                   <i>Lars Reeker</i><p/>
                                "))
                       )  
                ),
                column(4,
                  tags$div(class = "header", checked = NA, align = "right", 
                         tags$img(src = "heinz2.png", width = 200))
                )
              
            )
    ),
    
    # Tab 02
    tabItem(tabName = "tab02",
            fluidRow(
              column(8, 
                     tags$div(class = "header", checked = NA,
                              h3("Intructions"),
                              h5("In order to use this dashboard properly, make sure you have at least two separate datasets (in csv format). 
                                 One dataset should contain all the calls, with for each call defined at least:
                                 "),
                              HTML("
                               <ul>
                                    <li>Caller ID, the person calling</li>
                                    <li>Callee ID, the person being called</li>
                                    <li>Date (yyyymmdd) of when the call is made</li>
                                    <li>Time (hh:mm:ss) when the call is made</li>
                                    <li>Duration (numeric) of the call</li>
                                    <li>Longitude of the cell tower</li>
                                    <li>Latitude of the cell tower</li>
                                    <li>Hour the call is made</li>
                               </ul> 
                              "),
                              tags$p(),
                              h5("The second dataset should contain the cell towers in the particular area. For each
                                  defined at least:
                                 "),
                              HTML("
                               <ul>
                                   <li>Cell tower ID</li>
                                   <li>Longitude of the cell tower</li>
                                   <li>Latitude of the cell tower</li>
                               </ul> 
                                   "),
                              tags$p(),
                              h5("Please refer to the two datasets provided with this dashboard to make sure you have your datasets in the right format. The call dataset is a subset of an actual dataset that consists of about 58 million rows. This subset consists of about 52 thousand rows."),
                              tags$p(),
                              HTML("
                                <h5>Furthermore, please have a look at our recorded demo that guides you through all the different components of the dashboards.<h5>

                                "),
                              HTML("
                                   <p><i>Subramaniam Balasubramaniam</i><br>
                                   <i>William Liu</i><br>
                                   <i>Praveen Kumar Thoranathula Radhesyam</i><br>
                                   <i>Lars Reeker</i><p/>
                                   ")
                              )
                     ),
              column(4,
                     tags$div(class = "header", checked = NA, align = "right", 
                              tags$img(src = "heinz2.png", width = 200))
              )
            )
            
    ),
    
    
    # Tab 0
    tabItem(tabName = "tab0",
            tags$br(),
            tags$br(),
            tags$br(),
            fluidRow(
              column(12, align="center",
                     tags$img(src = "heinz2.png"),
                     h1("Welcome to the Tai'an dashboard!"),
                     tags$br(),
                     tags$br(),
                     tags$br(),
                     h5("Click ", tags$a(href="http://larsreeker.nl/mib/taian_march_calldata.csv", "here "), "to download the mobile phone call dataset"),
                     h5("Click ", tags$a(href="http://larsreeker.nl/mib/taian_celltower_locations.csv", "here "), "to download the celltower locations dataset")
                     )
            )
            
    ),
    
    # Tab 1
    tabItem(tabName = "tab1",
            h3("Provide the call dataset"),
            h5("On this page the (mobile phone) call dataset can be uploaded and processed accordingly to its format. Make sure each variable has its own column, and that all values are clear of any quotationmarks. This dataset will be used throughout the dashboard."),
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
            h3("Provide the cell tower dataset"),
            h5("On this page the cell tower locations dataset can be uploaded and processed accordingly to its format. Make sure each variable has its own column, and that all values are clear of any quotationmarks. This dataset will be used throughout the dashboard."),
            fluidRow(
              box(
                title = "Cell data input", 
                status = "primary",
                width = 4,
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
                width = 8,
                tags$div(style="width:auto; height:auto; overflow:auto;padding:5px;",
                         dataTableOutput('contents2')
              )
            )
          )
    ),
    
    # Tab 3
    tabItem(tabName = "tab3",
            h3("Provide shop location"),
            h5("On this page the geo location of the subject (restaurant, shop, etc.) should be provided. This location will be used throughout the dashboard. This location search uses Google Maps."),
            fluidRow(
              box(
                title = "Location input", 
                status = "primary",
                width = 8,
                collapsible = TRUE,
                textInput("input_text_location", 
                          label = "Enter your address/location here:",
                          value = ""),
                helpText("For the analysis of Kelaitou Restaurant, search for 'Kelaitou Restaurant, Tai'an, China'"),
                actionButton("location_go","Retrieve location", icon = icon("map-marker", lib="font-awesome"))
              )
            ),
            fluidRow(
              box(
                title = "Location on map", 
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
            h3("Neighborhood of the location"),
            h5("This page shows the area around (neighborhood of) the location provided. It displays all the cell towers in within the area."),
            fluidRow(
              box(
                title = "Input", 
                status = "primary",
                width = 4,
                collapsible = TRUE,
                sliderInput("input_area_map", label = "Select radius:",
                            min = 1, max = 10, value = 4),
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
            h3("Call density for the city area"),
            h5("This page shows the call density in the whole city area per day and per hour. You can click the 'play' button to automaticly change the hour."),
            fluidRow(
              box(
                title = "Input", 
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
            h3("People in the area throughout the week"),
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
                textInput("input_clients_num", label = "Number of potential clients:", value = 20),
                helpText("For precision, this takes a while...")
              ),
              box(
                title = "List", 
                status = "primary",
                width = 7,
                collapsible = TRUE,
                tableOutput("output_clients_week")
              )
            )
    ),
    
    # Tab 7
    tabItem(tabName = "tab7",
            h3("People in the area at one particular day"),
            h5("Here, the system lists all the people that have been within the radius at least one time at one particular day."),
            fluidRow(
              box(
                title = "Input", 
                status = "primary",
                width = 5,
                collapsible = TRUE,
                sliderInput("input_clients_radius2", label = "Select radius:",
                            min = 1, max = 5, value = 2),
                helpText("We recommend a radius of 2 or 3, which provides a good precision."),
                textInput("input_clients_num2", label = "Number of potential clients:", value = 20),
                selectInput("input_clients_day", label = "Select day:", 
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
                title = "List", 
                status = "primary",
                width = 5,
                collapsible = TRUE,
                tableOutput("output_clients_day")
              )
            )
    ),
    
    # Tab 8
    tabItem(tabName = "tab8",
            h3("Social network of the area"),
            h5("Here, the social network diagram computed from all the connections within the specified range of the location is displayed. It shows the different communities identified in different colors. This graphical visualization gives insights how all people in the dataset are related to each other. For a valuable analysis, this social network should consists preferably of many commnuties of about the same size."),
            fluidRow(
              box(
                title = "Input", 
                status = "primary",
                width = 6,
                collapsible = TRUE,
                sliderInput("input_social_radius", label = "Select radius:",
                            min = 1, max = 5, value = 2),
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
            h3("Highly connected people"),
            h5("This page lists the people that have the most connections in the social network, eg. have the highest betweenness centrality value."),
            fluidRow(
              box(
                title = "Input", 
                status = "primary",
                width = 4,
                collapsible = TRUE,
                sliderInput("input_important_radius", label = "Select radius:",
                            min = 1, max = 5, value = 2),
                helpText("We recommend a radius of 2 or 3, which provides a good precision."),
                textInput("input_important_num", label = "Number users:", value = 20),
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
            h3("People in the area on a particular day and time"),
            h5("This analysis shows the list of people that are likely to be in the area of the location at the given time."),
            fluidRow(
              box(
                title = "Input", 
                status = "primary",
                width = 4,
                collapsible = TRUE,
                sliderInput("input_top_radius", label = "Select radius:",
                            min = 1, max = 15, value = 5),
                textInput("input_top_num", label = "Number users:", value = 20),
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
            h3("Similar people in the area"),
            h5("This final analysis shows the people that are connected to the person (user id) provided. These people are located close in the social network to the person and have a similar mobility pattern, which is calculated and compared using the cosine similarity measure on the frequency location matrix of the two users."),
            fluidRow(
              box(
                title = "Input", 
                status = "primary",
                width = 4,
                collapsible = TRUE,
                sliderInput("input_recomm_radius", label = "Select radius:",
                            min = 1, max = 15, value = 10),
                textInput("input_recomm_number", label = "User ID:", value = ""),
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
    tabItem(tabName = "tab12",
            fluidRow(
              tags$br(),
              column(8, 
                     tags$div(class = "header", checked = NA,
                              h3("Take aways"),
                              h5("blablabla"),
                              tags$p(
                                h5("In the case you have questions about the dashboard or project, please reach 
                                   out to one of us!"),
                                HTML("
                                     <p><i>Subramaniam Balasubramaniam</i><br>
                                     <i>William Liu</i><br>
                                     <i>Praveen Kumar Thoranathula Radhesyam</i><br>
                                     <i>Lars Reeker</i><p/>
                                     "))
                                )  
                                ),
              column(4,
                     tags$div(class = "header", checked = NA, align = "right", 
                              tags$img(src = "heinz2.png", width = 200))
              )
              
          )
      )
            
            
    
    
  # Close tabItems
  )
  
# Close body  
)

# Combine the UI of dashboard
ui <- dashboardPage(header, sidebar, body)
