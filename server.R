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

options(shiny.maxRequestSize = 110*1024^2)



# Create server side of dashboard
server <- function(input, output) {
  
  ## Retrieve a map from Google Maps with center at the means of all the coordinates
  cell.loc.map <- get_map(location = c(lon = mean(celldata$longitude),
                                       lat = mean(celldata$latitude)),
                          zoom = 10, scale = 2) # scale specifies the resolution of the map
  
  #############
  ### Tab 0 ###
  #############
  
  
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
                             source="google", maptype= "roadmap", crop=FALSE, zoom = 14, scale = 2)
      
      ggmap(locationMap) +
        geom_point(aes(x = lon, y = lat, fill = "blue"), data = coordinates,
                   alpha = 1, size = 10, shape = 25) + guides(fill=FALSE) + scale_fill_manual(values = c("blue"))
    }
    
  }, width = 600, height = 600)
  
  #############
  ### Tab 3 ###
  #############
  
  output$output_area_map <- renderPlot({
    
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
      
      
      cell.loc.map <- get_map(location = c(lon = longitude,
                                           lat = latitude),
                              zoom = 13, scale = 2) # scale specifies the resolution of the map
      
      cell.subset <- subset(celldata, latitude <= lat_lim_top)
      cell.subset <- subset(cell.subset, latitude >= lat_lim_bot)
      cell.subset <- subset(cell.subset, longitude <= long_lim_right)
      cell.subset <- subset(cell.subset, longitude >= long_lim_left)
      
      
      map2 <- ggmap(cell.loc.map, extent='panel', base_layer=ggplot(celldata, aes(x=celldata$longitude, y=celldata$latitude)))
      
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
      
      print(map.scan)
    
    }
  })
  
  #############
  ### Tab 4 ###
  #############
  
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
    
    cell.loc.map <- ggmap(cell.loc.map) + geom_point(data=lon.lat, aes(x = longitude, y = latitude, size=count, alpha=0.5, fill="red"), shape=21) + scale_size(range=c(3,30)) + guides(fill = FALSE, alpha = FALSE, size = FALSE) 
    ###+ geom_density2d(data = celldata, aes(x = longitude, y = latitude))
    
    cell.loc.map <- cell.loc.map + geom_point(data = coordinates, aes(x = lon, y = lat), fill = "blue", alpha = 1, size = 4, shape = 25) + guides(fill = FALSE, alpha = FALSE, size = FALSE)
    
    
    title <- paste("Locations of Cell Towers on ",call_density_day," at ",hour," hrs",sep="")
    
    cell.loc.map <- cell.loc.map + ggtitle(title) 
    
    ## Print the map
    print(cell.loc.map)
    
    })

  
  #############
  ### Tab 5 ###
  #############
  
  output$output_general_trend <- renderPlot({
    
    text_location <- input$input_text_location
    radius <- 2#input$input_general_trend
    coordinates <- geocode(text_location)
    longitude <- coordinates$lon
    latitude <- coordinates$lat
    
    if(text_location == "") {
      return("")
    } else {
      
      lat_dist <- (1/110.54)*radius
      long_dist <- (radius/(111.32*cospi(latitude)))
      
      lat_lim_top <- latitude + lat_dist
      lat_lim_bot <- latitude - lat_dist
      long_lim_right <- longitude + long_dist
      long_lim_left <- longitude - long_dist
      
      march_ll <- ddply(march, c("latitude","longitude"), summarize, len=length(callee_id))
      
      march_ll_sub <- subset(march_ll, latitude <= lat_lim_top)
      march_ll_sub <- subset(march_ll_sub, latitude >= lat_lim_bot)
      march_ll_sub <- subset(march_ll_sub, longitude <= long_lim_right)
      march_ll_sub <- subset(march_ll_sub, longitude >= long_lim_left)
      
      march_sub <- subset(march, march$latitude %in% march_ll_sub$latitude)
      march_sub <- subset(march, march$longitude %in% march_ll_sub$longitude)
      
      march_ll <- ddply(march_sub, c("latitude", "longitude"), summarize, len = length(callee_id))
      march_ll$latlong <- paste(latitude, ":", longitude)
      ggplot(data=march_ll, mapping=aes(x=latlong,y=len))+geom_line()
      
      march_h <- ddply(march, "hr", summarize, len = length(callee_id))
      ggplot(data=march_h, mapping=aes(x=hr,y=len))+geom_line()
      
      march_d <- ddply(march, "date", summarize, len = length(callee_id))
      ggplot(data=march_d, mapping=aes(x=date,y=len))+geom_line()
    }
  })
  
  #############
  ### Tab 6 ###
  #############
  
  output$output_clients_week <- renderTable({
    
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
      
      march_ll <- ddply(march, c("latitude","longitude"), summarize, len=length(callee_id))
      
      march_ll_sub <- subset(march_ll, latitude <= lat_lim_top)
      march_ll_sub <- subset(march_ll_sub, latitude >= lat_lim_bot)
      march_ll_sub <- subset(march_ll_sub, longitude <= long_lim_right)
      march_ll_sub <- subset(march_ll_sub, longitude >= long_lim_left)
      
      march_sub <- subset(march, march$latitude %in% march_ll_sub$latitude)
      march_sub <- subset(march, march$longitude %in% march_ll_sub$longitude)
      
      march_sub_c <- ddply(march_sub,"caller_id", summarize, len = length(callee_id))
      
      march_sub_c <- march_sub_c[order(-march_sub_c$len),]
      
      march_sub_c <- march_sub_c[1:num_of_user,]
      
      march_new <- march_sub_c
      
      march_new$caller_id <- as.character(march_new$caller_id)
      
      rownames(march_new) <- 1:nrow(march_new)
      
      return(march_new)
    }
    
  })
  
  #############
  ### Tab 7 ### People Day
  #############
  
  #############
  ### Tab 8 ### Social network
  #############
  
  output$output_social_network <- renderPlot({
    
    text_location <- input$input_text_location
    coordinates <- geocode(text_location)
    longitude <- coordinates$lon
    latitude <- coordinates$lat
    radius <- input$input_social_radius
    num_of_users <- as.numeric(input$input_social_num)
    day <- input$input_social_day
    
    if(day == 20080301){
      march_od <- march1
    }
    if(day == 20080302){
      march_od <- march2
    }
    if(day == 20080303){
      march_od <- march3
    }
    if(day == 20080304){
      march_od <- march4
    }
    if(day == 20080305){
      march_od <- march5
    }
    if(day == 20080306){
      march_od <- march6
    }
    if(day == 20080307){
      march_od <- march7
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
    
    #march_membership <- as.data.frame(membership(march_fc))
    #colnames(march_membership) <- c("x")
    #march_membership_g <- ddply(march_membership, "x", summarize, len = length(node))
    #march_membership_g_sub <- subset(march_membership_g, len > 50)
    
    #march_membership_sub <- subset(march_membership, x %in%march_membership_g_sub)
    
    #march_sub_cc_sub <- subset(march_sub_cc, march_sub_cc$caller_id %in%  march_membership_sub$node)
    
    E(march_gr)$weight <- march_sub_cc$len
    
    #Community  detection
    #march_fc_sub <- fastgreedy.community(as.undirected(march_gr_sub))
    #march_fc.colors_sub <- sample(colors(), length(march_fc_sub),replace = TRUE)
    #for(i in 1:length(march_fc.colors_sub)) {
      #V(march_gr_sub)[membership(march_fc_sub)==i]$color <- march_fc.colors_sub[i]
    #}
    
    E(march_gr)$color <- ifelse(E(march_gr)$weight > 4, 'darkorange', 'azure4')
    #E(march_gr)$width <- E(march_gr)$weight / weight.factor
    V(march_gr)$size <- degree(march_gr, mode="out") / 8
    E(march_gr)$weight <- march_sub_cc$len
    
    par(mar = c(0.1, 0.1, 0.1, 0.1))
    #2d graph
    march_plot <- plot.igraph(march_gr, vertex.label=NA, layout=layout.fruchterman.reingold, vertex.color = V(march_gr)$color, vertex.size=2, edge.arrow.size=0.2, edge.color=E(march_gr)$color)
    
    #march_btw <- betweenness(march_gr, directed = TRUE)
    
    #sorted.btw <- sort(march_btw, decreasing = TRUE)
    
    #march_sub_c <- sorted.btw[1:num_of_users]
    
    return(march_plot)
  
  }, width = 600, height = 600)
  
  
  #############
  ### Tab 9 ### Important people
  #############
  
  output$output_important_list <- renderTable({
    
    text_location <- input$input_text_location
    coordinates <- geocode(text_location)
    longitude <- coordinates$lon
    latitude <- coordinates$lat
    radius <- input$input_important_radius
    num_of_users <- as.numeric(input$input_important_num)
    day <- input$input_important_day
    
    if(day == 20080301){
      march_od <- march1
    }
    if(day == 20080302){
      march_od <- march2
    }
    if(day == 20080303){
      march_od <- march3
    }
    if(day == 20080304){
      march_od <- march4
    }
    if(day == 20080305){
      march_od <- march5
    }
    if(day == 20080306){
      march_od <- march6
    }
    if(day == 20080307){
      march_od <- march7
    }
    
    #cat("Test: ", day)
    
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
    
    return(as.data.frame(march_sub_c))
    
  })
  
  ##############
  ### Tab 10 ### Top clients
  ##############
  
  output$output_top_users<- renderTable({
  
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
    
    #march_new <- march_w_sub_g_ret
    #march_new$caller_id <- as.character(march_new$caller_id)
    #rownames(march_new) <- 1:nrow(march_new)
    #return(march_new)
  
    return(march_w_sub_g_ret)
  })
  
  ##############
  ### Tab 11 ### Recommendation
  ##############
  
  output$output_recomm <- renderTable({
    
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
    
    march_ll <- ddply(march, c("latitude","longitude"), summarize, len=length(callee_id))
    
    march_ll_sub <- subset(march_ll, latitude <= lat_lim_top)
    march_ll_sub <- subset(march_ll_sub, latitude >= lat_lim_bot)
    march_ll_sub <- subset(march_ll_sub, longitude <= long_lim_right)
    march_ll_sub <- subset(march_ll_sub, longitude >= long_lim_left)
    
    march_sub <- subset(march, march$latitude %in% march_ll_sub$latitude)
    march_sub <- subset(march_sub, march$longitude %in% march_ll_sub$longitude)
    
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
    call.data <- march 
    
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
    
    return(recomm_users[order(-recomm_users$similarity),])
    
    }
    
  })
  
  
}
