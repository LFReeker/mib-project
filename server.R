library(shiny)
library(shinydashboard)

options(shiny.maxRequestSize = 100*1024^2)

# Create server side of dashboard
server <- function(input, output) {
  
  ## Libraries
  library("ggplot2")
  library("ggmap")
  
  
  #############
  ### Tab 1 ###
  #############
  
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
      inputData <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                       quote=input$quote)
      head(inputData,20)
    }
    
  })
  
  #############
  ### Tab 2 ###
  #############
  
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
  
  output$output_map_location <- renderPlot({
    
    text_location <- input$input_text_location
    
    if(text_location == "") {
      return(NULL)
    } else {
      coordinates <- geocode(text_location)
      
      locationMap <- get_map(location=text_location,
                             source="google", maptype= "roadmap", crop=FALSE, zoom = 17, scale = 2)
      
      ggmap(locationMap) +
        geom_point(aes(x = lon, y = lat), data = coordinates,
                   alpha = .5, color="darkred", size = 3)
    }
    
  }, width = 600, height = 600)
  
  #############
  ### Tab 3 ###
  #############
  
  output$output_area_map <- renderPrint({
    
    text_location <- input$input_text_location
    radius <- input$input_area_map
    
    if(text_location == "") {
      return(NULL)
    } else {
    
      coordinates <- geocode(text_location)
      longitude <- coordinates$lon
      latitude  <- coordinates$lat
      
      
      lat_dist <- (1/110.54)*radius
      long_dist <- (radius/(111.32*cospi(latitude)))
      
      lat_lim_top <- latitude + lat_dist
      lat_lim_bot <- latitude - lat_dist
      long_lim_right <- longitude + long_dist
      long_lim_left <- longitude - long_dist
      
      
      cell.loc.map <- get_map(location = c(lon = longitude,
                                           lat = latitude),
                              zoom = 13, scale = 2) # scale specifies the resolution of the map
      
      cell.subset <- subset(calldata, latitude <= lat_lim_top)
      cell.subset <- subset(cell.subset, latitude >= lat_lim_bot)
      cell.subset <- subset(cell.subset, longitude <= long_lim_right)
      cell.subset <- subset(cell.subset, longitude >= long_lim_left)
      
      
      map2 <- ggmap(cell.loc.map, extent='panel', base_layer=ggplot(calldata, aes(x=calldata$longitude, y=calldata$latitude)))
      
      # create a data frame with the dimensions of the rectangle. 
      #the xmin and xmax are the longitude boundaries of the box, while ymin and ymax are the latitude boundaries.
      rect <- data.frame(xmin=long_lim_left, xmax=long_lim_right, ymin=lat_lim_bot, ymax=lat_lim_top)
      
      
      # now add the rectangle data frame into ggplot using geom_rect() and specify the color and size 
      map.scan <- map2 + geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="gray20", alpha=0.4, inherit.aes = FALSE) 
      
      # add labels to the plot 
      map.scan <- map.scan + labs(title = "Region to be focussed") 
      # add title theme 
      map.scan <- map.scan + theme(plot.title = element_text(hjust = 0, vjust = 1, face = c("bold"))) 
      map.scan <- map.scan + geom_point(data=cell.subset, aes(x = longitude, y = latitude, size=5, alpha=0.5),shape=17, col="red") + guides(alpha = FALSE,size = FALSE) 
      
      map.scan <- map.scan + geom_point(x=longitude, y=latitude, size=5, shape=25, alpha=1, aes(fill="blue")) + scale_fill_manual(values=c("blue")) + guides(fill=FALSE)
      
      print(map.scan)
    
    }
  })
  
}
