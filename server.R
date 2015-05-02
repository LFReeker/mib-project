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

options(shiny.maxRequestSize = 500*1024^2) # 500 MB limit upload



# Create server side of dashboard
server <- function(input, output) {
  
  
  ## Data Import
  callData <- reactive({
    inFile1 <- input$file1
    
    if(is.null(inFile1)) {
      return(NULL)
    } else {
      read.csv(inFile1$datapath, header=input$header1, sep=input$sep1, quote=input$quote1)
    }
  })
  
  cellData <- reactive({
    inFile2 <- input$file2
    
    if(is.null(inFile2)) {
      return(NULL)
    } else {
      read.csv(inFile2$datapath, header=input$header2, sep=input$sep2, quote=input$quote2)
    }
  })
  
  output$contents1 <- renderDataTable({
    
    callData()
    
  }, options = list(pageLength = 10))
  
  output$contents2 <- renderDataTable({
    
    cellData()    
    
  }, options = list(pageLength = 10))
  
  output$dataSummary1 <- renderUI({
    str1 <- paste("Dataset structure = ", class(callData()))
    str2 <- NULL
    HTML(paste(str1, str2, sep = '<br/>'))
  })
  
  
  ## Location 
  getLocation <- reactive({
    
    text_location <- input$input_text_location
    
    if(text_location == "") {
      return(NULL)
    } else {
      coordinates <- geocode(text_location)
      str1 <- paste("You have selected ","<b>",text_location,"</b>","as your location.")
      str2 <- paste("The longtitude is ","<b>",coordinates$lon,"</b>","and latitude is ","<b>",coordinates$lat,"</b>",".")
      HTML(paste(str1, str2, sep = '<br/>'))
    }
  })
  
  output$output_text_location <- renderUI({ 
    getLocation()
  })
  
  getMap <- reactive({
    
    text_location <- input$input_text_location
    
    if(text_location == "") {
      return(NULL)
    } else {
      coordinates <- geocode(text_location)
      
      locationMap <- get_map(location=text_location,
                             source="google", maptype= "roadmap", crop=FALSE, zoom = 16, scale = 2)
      
      ggmap(locationMap) +
        geom_point(aes(x = lon, y = lat, fill = "blue"), data = coordinates,
                   alpha = 1, size = 10, shape = 25) + guides(fill=FALSE) + scale_fill_manual(values = c("blue"))
    
    }
  })
  
  output$output_map_location <- renderPlot({
    
    input$location_go
    isolate(getMap())
    
  }, width = 600, height = 600)
  
  
  ## Area map
  areaMap <- reactive({
    
    text_location <- input$input_text_location
    radius <- input$input_area_map
    
    if(text_location == "") {
      return("Please provide a location in Step 2")
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
      
      
      cell.loc.map2 <- get_map(location = c(lon = longitude,
                                            lat = latitude),
                               zoom = input$zoom_area_map, scale = 2) # scale specifies the resolution of the map
      
      cell.subset <- subset(cellData(), latitude <= lat_lim_top)
      cell.subset <- subset(cell.subset, latitude >= lat_lim_bot)
      cell.subset <- subset(cell.subset, longitude <= long_lim_right)
      cell.subset <- subset(cell.subset, longitude >= long_lim_left)
      
      
      map2 <- ggmap(cell.loc.map2, extent='panel') #, base_layer=ggplot(cellData(), aes(x=cellData()$longitude, y=cellData()$latitude)))
      
      # create a data frame with the dimensions of the rectangle. 
      #the xmin and xmax are the longitude boundaries of the box, while ymin and ymax are the latitude boundaries.
      rect <- data.frame(xmin=long_lim_left, xmax=long_lim_right, ymin=lat_lim_bot, ymax=lat_lim_top)
      
      
      # now add the rectangle data frame into ggplot using geom_rect() and specify the color and size 
      map.scan <- map2 + geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="gray20", alpha=0.4, inherit.aes = FALSE) 
      
      # add labels to the plot 
      map.scan <- map.scan + labs(title = "Region to be focussed") 
      # add title theme 
      map.scan <- map.scan + theme(plot.title = element_text(hjust = 0, vjust = 1, face = c("bold"))) 
      map.scan <- map.scan + geom_point(data=cell.subset, aes(x = longitude, y = latitude), size=5, alpha=0.5, shape=17, col="red") + guides(alpha = FALSE, size = FALSE) 
      
      map.scan <- map.scan + geom_point(x=longitude, y=latitude, size=5, shape=25, alpha=1, aes(fill="blue")) + scale_fill_manual(values=c("blue")) + guides(fill=FALSE)
      
      map.scan
    }
  })
  
  output$output_area_map <- renderPlot({
    areaMap()
  })
  
  
  ## Call density
  cell.loc.map <- reactive({
    #Retrieve a map from Google Maps with center at the means of all the coordinates
    get_map(location = c(lon = mean(cellData()$longitude),
                         lat = mean(cellData()$latitude)),
            zoom = 10, scale = 2) # scale specifies the resolution of the map
  })
  
  output$output_call_density <- renderPlot({ 
    
    text_location <- input$input_text_location
    coordinates <- geocode(text_location)
    
    call_density_day <- input$call_density_day
    hour <- input$call_density_hour
    
    ## Load the merged data
    singleDay_with_coord <- subset(march, date == call_density_day)
    
    ## Group the data by longitude, latitude and hour
    lon.lat.hr <- ddply(singleDay_with_coord, c("longitude","latitude", "hr"), summarise, count=length(c(longitude,latitude,hr)))#, duration=sum(call_duration))
    
    lon.lat.hr$count <- as.numeric(lon.lat.hr$count)
    
    lon.lat <- lon.lat.hr[which(lon.lat.hr$hr == hour),]
    
    cell.loc.map <- ggmap(cell.loc.map()) + geom_point(data=lon.lat, aes(x = longitude, y = latitude, size=count, alpha=0.5, fill="red"), shape=21) + scale_size(range=c(3,30)) + guides(fill = FALSE, alpha = FALSE, size = FALSE) 
    ###+ geom_density2d(data = celldata, aes(x = longitude, y = latitude))
    
    cell.loc.map <- cell.loc.map + geom_point(data = coordinates, aes(x = lon, y = lat), fill = "blue", alpha = 1, size = 4, shape = 25) + guides(fill = FALSE, alpha = FALSE, size = FALSE)
    
    
    title <- paste("Locations of Cell Towers on ",call_density_day," at ",hour," hrs",sep="")
    
    cell.loc.map <- cell.loc.map + ggtitle(title) 
    
    ## Print the map
    print(cell.loc.map)
    
    })

  
  ## Clients per week
  clientsWeek <- reactive({
    
    text_location <- input$input_text_location
    radius <- input$input_clients_radius
    num_of_user <- as.numeric(input$input_clients_num)
    coordinates <- geocode(text_location)
    longitude <- coordinates$lon
    latitude <- coordinates$lat
    
    if(text_location == "") {
      return("")
    } else if(num_of_user == 0) {
      return("")
    } else {
      
      lat_dist <- (1/110.54)*radius
      long_dist <- (radius/(111.32*cospi(latitude)))
      
      lat_lim_top <- latitude + lat_dist
      lat_lim_bot <- latitude - lat_dist
      long_lim_right <- longitude + long_dist
      long_lim_left <- longitude - long_dist
      
      march_ll <- ddply(callData(), c("latitude","longitude"), summarize, len=length(callee_id))
      
      march_ll_sub <- subset(march_ll, latitude <= lat_lim_top)
      march_ll_sub <- subset(march_ll_sub, latitude >= lat_lim_bot)
      march_ll_sub <- subset(march_ll_sub, longitude <= long_lim_right)
      march_ll_sub <- subset(march_ll_sub, longitude >= long_lim_left)
      
      march_sub <- subset(callData(), callData()$latitude %in% march_ll_sub$latitude)
      march_sub <- subset(march_sub, march_sub$longitude %in% march_ll_sub$longitude)
      
      march_sub_c <- ddply(march_sub,"caller_id", summarize, len = length(callee_id))
      
      march_sub_c <- march_sub_c[order(-march_sub_c$len),]
      
      march_sub_c <- march_sub_c[1:num_of_user,]
      
      march_new <- march_sub_c
      
      march_new$caller_id <- as.character(march_new$caller_id)
      
      rownames(march_new) <- 1:nrow(march_new)
      colnames(march_new) <- c("Caller ID", "Number of calls")
      
      march_new
    }
  })
  
  output$output_clients_week <- renderTable({
    clientsWeek()
  })
  
  
  ## Clients per day
  
  
  ## Social Network
  socialNetwork <- reactive({
    
    text_location <- input$input_text_location
    coordinates <- geocode(text_location)
    longitude <- coordinates$lon
    latitude <- coordinates$lat
    radius <- input$input_social_radius
    num_of_users <- as.numeric(input$input_social_num)
    day <- input$input_social_day
    
    if(day == 20080301){
      march_od <- subset(callData(), date == 20080301)
    }
    if(day == 20080302){
      march_od <- subset(callData(), date == 20080302)
    }
    if(day == 20080303){
      march_od <- subset(callData(), date == 20080303)
    }
    if(day == 20080304){
      march_od <- subset(callData(), date == 20080304)
    }
    if(day == 20080305){
      march_od <- subset(callData(), date == 20080305)
    }
    if(day == 20080306){
      march_od <- subset(callData(), date == 20080306)
    }
    if(day == 20080307){
      march_od <- subset(callData(), date == 20080307)
    }
    
    square_side <- radius*1.414
    
    lat_dist <- (1/110.54)*radius
    long_dist <- (radius/(111.32*cos(latitude)))
    
    lat_lim_top <- latitude + lat_dist
    lat_lim_bot <- latitude - lat_dist
    long_lim_right <- longitude + long_dist
    long_lim_left <- longitude - long_dist
    
    march_ll <- ddply(march_od, c("latitude","longitude"), summarize, len=length(callee_id))
    
    march_ll_sub <- subset(march_ll, latitude <= lat_lim_top)
    march_ll_sub <- subset(march_ll_sub, latitude >= lat_lim_bot)
    march_ll_sub <- subset(march_ll_sub, longitude <= long_lim_right)
    march_ll_sub <- subset(march_ll_sub, longitude >= long_lim_left)
    
    march_sub <- subset(march, march$latitude %in% march_ll_sub$latitude)
    march_sub <- subset(march, march$longitude %in% march_ll_sub$longitude)
    
    march_sub_cc <- ddply(march_sub, c("caller_id","callee_id"), summarize, len = length(callee_id))
    
    march_gr <- graph.data.frame(march_sub_cc, directed=T)
    
    E(march_gr)$weight <- march_sub_cc$len
    
    #Community  detection
    march_fc <- fastgreedy.community(as.undirected(march_gr))
    march_fc.colors <- sample(colors(), length(march_fc),replace = TRUE)
    for(i in 1:length(march_fc.colors)) {
      V(march_gr)[membership(march_fc)==i]$color <- march_fc.colors[i]
    }
  
    E(march_gr)$weight <- march_sub_cc$len
    
    E(march_gr)$color <- ifelse(E(march_gr)$weight > 4, 'darkorange', 'azure4')
    #E(march_gr)$width <- E(march_gr)$weight / weight.factor
    V(march_gr)$size <- degree(march_gr, mode="out") / 8
    E(march_gr)$weight <- march_sub_cc$len
    
    par(mar = c(0.1, 0.1, 0.1, 0.1))
    
    #2d graph
    march_plot <- plot.igraph(march_gr, vertex.label=NA, layout=layout.fruchterman.reingold, vertex.color = V(march_gr)$color, vertex.size=2, edge.arrow.size=0.2, edge.color=E(march_gr)$color)
    
    march_plot
  })
  
  output$output_social_network <- renderPlot({
    socialNetwork()
  }, width = 600, height = 600)
  
  
  ## Influencial people
  importantClients <- reactive({
    
    text_location <- input$input_text_location
    coordinates <- geocode(text_location)
    longitude <- coordinates$lon
    latitude <- coordinates$lat
    radius <- input$input_important_radius
    num_of_users <- as.numeric(input$input_important_num)
    day <- input$input_important_day
    
    if(day == 20080301){
      march_od <- subset(callData(), date == 20080301)
    }
    if(day == 20080302){
      march_od <- subset(callData(), date == 20080302)
    }
    if(day == 20080303){
      march_od <- subset(callData(), date == 20080303)
    }
    if(day == 20080304){
      march_od <- subset(callData(), date == 20080304)
    }
    if(day == 20080305){
      march_od <- subset(callData(), date == 20080305)
    }
    if(day == 20080306){
      march_od <- subset(callData(), date == 20080306)
    }
    if(day == 20080307){
      march_od <- subset(callData(), date == 20080307)
    }
    
    square_side <- radius*1.414
    
    march <- march_od
    
    radius <- 20
    
    lat_dist <- (1/110.54)*radius
    long_dist <- (radius/(111.32*cos(latitude)))
    
    lat_lim_top <- latitude + lat_dist
    lat_lim_bot <- latitude - lat_dist
    long_lim_right <- longitude + long_dist
    long_lim_left <- longitude - long_dist
    
    march_ll <- ddply(march_od, c("latitude","longitude"), summarize, len=length(callee_id))
    
    march_ll_sub <- subset(march_ll, latitude <= lat_lim_top)
    march_ll_sub <- subset(march_ll_sub, latitude >= lat_lim_bot)
    march_ll_sub <- subset(march_ll_sub, longitude <= long_lim_right)
    march_ll_sub <- subset(march_ll_sub, longitude >= long_lim_left)
    
    
    march_sub <- subset(march, march$latitude %in% march_ll_sub$latitude)
    march_sub <- subset(march_sub, march$longitude %in% march_ll_sub$longitude)
    
    march_sub_cc <- ddply(march_sub,c("caller_id","callee_id"), summarize, len = length(callee_id))
    
    march_gr <- graph.data.frame(march_sub_cc, directed=T)
    march1_btw <- betweenness(march_gr, directed = TRUE)
    sorted.btw <- sort(march1_btw, decreasing = TRUE)
    
    march_sub_c <- sorted.btw[1:num_of_users]
    march_sub_c <- march_sub_c[march_sub_c > 0]
    
    march_sub_c <- as.data.frame(march_sub_c)
    
    callerid <- row.names(march_sub_c)
    factor <- march_sub_c$march_sub_c
    
    march_new <- NULL
    march_new$caller_id <- callerid
    march_new$factor <- factor
    
    march_new <- as.data.frame(march_new)
    
    colnames(march_new) <- c("Caller ID", "Influence factor")
    
    march_new
  })
  
  output$output_important_list <- renderTable({
    importantClients()
  })
  
  
  ## Top users
  topUsers <- reactive({
    
    text_location <- input$input_text_location
    coordinates <- geocode(text_location)
    longitude <- coordinates$lon
    latitude <- coordinates$lat
    radius <- input$input_top_radius
    num_of_users <- as.numeric(input$input_top_num)
    week <- input$input_top_week
    meal_type <- input$input_top_meal
    
    meal_type <- "Breakfast"
    week <- "Weekend"
    
    march1 <- subset(callData(), date == 20080301)
    march2 <- subset(callData(), date == 20080302)
    march3 <- subset(callData(), date == 20080303)
    march4 <- subset(callData(), date == 20080304)
    march5 <- subset(callData(), date == 20080305)
    march6 <- subset(callData(), date == 20080306)
    march7 <- subset(callData(), date == 20080307)

    if(meal_type == "Breakfast"){
      time <- c(8,9,10,11)
    }
    if(meal_type == "Lunch"){
      time <- c(12,13,14,15)
    }
    if(meal_type == "Snacks"){
      time <- c(16,17,18)
    }
    if(meal_type == "Dinner"){
      time <- c(19,20,21,22)
    }
    
    if(week == "Weekend"){
      march_w <- rbind(march1,march2)
    }
    if(week == "Weekday"){
      march_w <- rbind(march3,march4)
      march_w <- rbind(march_w,march5)
      march_w <- rbind(march_w,march6)
      march_w <- rbind(march_w,march7)
    }
    
    lat_dist <- (1/110.54)*radius
    long_dist <- (radius/(111.32*cospi(latitude)))
    
    lat_lim_top <- latitude + lat_dist
    lat_lim_bot <- latitude - lat_dist
    long_lim_right <- longitude + long_dist
    long_lim_left <- longitude - long_dist
    
    march_ll <- ddply(march_w, c("latitude","longitude"), summarize, len=length(callee_id))
    
    march_ll_sub <- subset(march_ll, latitude <= lat_lim_top)
    march_ll_sub <- subset(march_ll_sub, latitude >= lat_lim_bot)
    march_ll_sub <- subset(march_ll_sub, longitude <= long_lim_right)
    march_ll_sub <- subset(march_ll_sub, longitude >= long_lim_left)
    
    march_sub <- subset(march_w, march_w$latitude %in% march_ll_sub$latitude)
    march_sub <- subset(march_sub, march_sub$longitude %in% march_ll_sub$longitude)
    
    march_w_sub <- subset(march_sub, march_sub$hr %in% as.integer(time))
    
    march_w_sub_g <- ddply(march_w_sub, "caller_id", summarize, len = length(callee_id))
    
    march_w_sub_g <- march_w_sub_g[order(-march_w_sub_g$len),]
    
    march_w_sub_g_ret <- march_w_sub_g[1:num_of_users,]
    
    colnames(march_w_sub_g_ret) <- c("Caller_id", "Number of calls")
    rownames(march_w_sub_g_ret) <- 1:nrow(march_w_sub_g_ret)
    march_w_sub_g_ret$Caller_id <- as.character(march_w_sub_g_ret$Caller_id)
    colnames(march_w_sub_g_ret) <- c("Caller ID", "Number of calls")
    
    march_w_sub_g_ret
  })
  
  output$output_top_users<- renderTable({
    topUsers()
  })
  
  
  ## Recommendation
  recommendation <- reactive({
    
    text_location <- input$input_text_location
    coordinates <- geocode(text_location)
    longitude <- coordinates$lon
    latitude <- coordinates$lat
    radius <- input$input_recomm_radius
    num_of_users <- as.numeric(input$input_recomm_num)
    number <- as.character(input$input_recomm_number)
    
    if(number == "") {
      return(NULL)
    } else {
      
      
      longitude <- 117.7728
      latitude <- 35.90445
      #radius <- 10
      #number <- "396416538"
      
      lat_dist <- (1/110.54)*radius
      long_dist <- (radius/(111.32*cospi(latitude)))
      
      lat_lim_top <- latitude + lat_dist
      lat_lim_bot <- latitude - lat_dist
      long_lim_right <- longitude + long_dist
      long_lim_left <- longitude - long_dist
      
      march_ll <- ddply(callData(), c("latitude","longitude"), summarize, len=length(callee_id))
      
      march_ll_sub <- subset(march_ll, latitude <= lat_lim_top)
      march_ll_sub <- subset(march_ll_sub, latitude >= lat_lim_bot)
      march_ll_sub <- subset(march_ll_sub, longitude <= long_lim_right)
      march_ll_sub <- subset(march_ll_sub, longitude >= long_lim_left)
      
      march_sub <- subset(callData(), callData()$latitude %in% march_ll_sub$latitude)
      march_sub <- subset(march_sub, march_sub$longitude %in% march_ll_sub$longitude)
      
      march_sub_g <- ddply(march_sub, c("caller_id","callee_id"), summarize, len=length(callee_id))
      march_sub_g <- na.omit(march_sub_g)
      march_sub_g_sub <- subset(march_sub_g, caller_id == as.numeric(as.character(number)))
      
      march_gr_sub <- graph.data.frame(march_sub_g, directed=T)
      
      E(march_gr_sub)$weight = rep(1,nrow(march_sub_g))
      tmp3 <- shortest.paths(march_gr_sub,v=number)
      tmp3 <- as.data.frame(tmp3)
      
      nodes<-NULL
      for(i in 1:ncol(tmp3)){
        if((tmp3[1,i] <= 3) & (tmp3[1,i] >= 1)){
          nodes <- rbind(nodes,colnames(tmp3[i]))
        }
      }
      
      x = nodes[,1]
      
      nodes <- as.data.frame(nodes)
      
      colnames(nodes) <- "caller_id"
      
      #return(nodes)
      
      # Cosine Similarity
      call.data <- callData() 
      
      # create their location vector's
      user_input <- user_vector(number)
      
      recomm_users <- NULL
      
      nodes$caller_id <- as.numeric(levels(nodes$caller_id))[nodes$caller_id]
      
      #cat("Test:", nodes$caller_id)
      
      for(i in 1:8) {    # length(nodes$caller_id)
        #cat("Test:", length(nodes$caller_id), "  ")
        user <- nodes$caller_id[i]
        #cat("Test:", i," ",user, "  ")
        
        vector_user <- user_vector(user)
        
        recomm_users$caller_id[i] <- as.character(user)
        recomm_users$similarity[i] <- cosine_similarity(user_input, vector_user)
      }
      
      recomm_users <- as.data.frame(recomm_users)
      recomm_users <- subset(recomm_users, similarity >= 0.3)
      
      recomm_users_new <- recomm_users[order(-recomm_users$similarity),]
      
      colnames(recomm_users_new) <- c("Caller ID", "Similarity factor")
      rownames(recomm_users_new) <- 1:nrow(recomm_users_new)
      
      recomm_users_new[1:num_of_users,]
    }
  })
  
  output$output_recomm <- renderTable({
    recommendation()
  })
  
  
}
