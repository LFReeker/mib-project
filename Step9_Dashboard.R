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
            h3("Step 0"),
            
            fluidRow(
              box(plotOutput("plot1", height = 250)),
              
              box(
                title = "Controls",
                sliderInput("slider", "Number of observations:", 1, 100, 50)
              )
            )
    ),
    
    # Tab 3
    tabItem(tabName = "tab3",
          h3("Step 1")
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

# Create server side of dashboard
server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

# Create the dashboard
shinyApp(ui, server)
