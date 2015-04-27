library(igraph)
library(plyr)
library(ggplot2)
library(TraMineR)
library(cluster)
library(ggmap)
library(reshape2)

march1 <- read.csv("march1_final.csv")
march2 <- read.csv("march2_final.csv")
march3 <- read.csv("march3_final.csv")
march4 <- read.csv("march4_final.csv")
march5 <- read.csv("march5_final.csv")
march6 <- read.csv("march6_final.csv")
march7 <- read.csv("march7_final.csv")

march <- march1

march <- rbind(march,march2)
march <- rbind(march,march3)
march <- rbind(march,march4)
march <- rbind(march,march5)
march <- rbind(march,march6)
march <- rbind(march,march7)


latitude <- 36.20198  
longitude <- 117.117
radius <- 1
num_of_user <- 100


general_trends <- function(latitude, longitude, radius){

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

whole_week_top_n <- function(latitude, longitude, radius, num_of_user){

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

return(march_sub_c)
}


one_day_top_n <- function(latitude, longitude, radius, num_of_user, day){

  if(day = 20080301){
    march_od = <- march1
  }
  if(day = 20080302){
    march_od = <- march2
  }
  if(day = 20080303){
    march_od = <- march3
  }
  if(day = 20080304){
    march_od = <- march4
  }
  if(day = 20080305){
    march_od = <- march5
  }
  if(day = 20080306){
    march_od = <- march6
  }
  if(day = 20080307){
    march_od = <- march7
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
  
  march_sub_c <- ddply(march_sub,"caller_id", summarize, len = length(callee_id))
  
  march_sub_c <- march_sub_c[order(-march_sub_c$len),]
  
  march_sub_c <- march_sub_c[1:num_of_user,]
  
  return(march_sub_c)
}

get_social_network <- function(latitude, longitude, radius, num_of_user){
  
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
  
  march_sub_cc <- ddply(march_sub,c("caller_id","callee_id"), summarize, len = length(callee_id))
  
  march_gr <- graph.data.frame(march_sub_cc, directed=T)
  
  E(march_gr)$weight <- march_sub_cc$len
  
  #Community  detection
  march_fc <- fastgreedy.community(as.undirected(march_gr))
  march_fc.colors <- sample(colors(), length(march_fc),replace = TRUE)
  for(i in 1:length(march_fc.colors)) {
    V(march_gr)[membership(march_fc)==i]$color <- march_fc.colors[i]
  }
  
  march_membership <- as.data.frame(membership(march_fc))
  colnames(march_membership) <- c("node","x")
  march_membership_g <- ddply(march_membership, "x", summarize, len = length(node))
  march_membership_g_sub <- subset(march_membership_g, len > 50)
  
  march_membership_sub <- subset(march_membership, x %in%march_membership_g_sub)
  
  march_sub_cc_sub <- subset(march_sub_cc, march_sub_cc$caller_id %in%  march_membership_sub$node)
  march_gr_sub <- graph.data.frame(march_sub_cc_sub, directed=T)
  
  E(march_gr_sub)$weight <- march_sub_cc_sub$len
  
  #Community  detection
  march_fc_sub <- fastgreedy.community(as.undirected(march_gr_sub))
  march_fc.colors_sub <- sample(colors(), length(march_fc_sub),replace = TRUE)
  for(i in 1:length(march_fc.colors_sub)) {
    V(march_gr_sub)[membership(march_fc_sub)==i]$color <- march_fc.colors_sub[i]
  }
  
  E(march1_gr_sub)$color <- ifelse(E(march1_gr_sub)$weight > 6, 'darkorange', 'azure4')
  E(march1_gr_sub)$width <- E(march1_gr_sub)$weight / weight.factor
  V(march1_gr_sub)$size <- degree(march1_gr_sub, mode="out") / 8
  E(march1_gr_sub)$weight <- march_sub_cc_sub$len
  
  #2d graph
  march_plot <- plot.igraph(march1_gr_sub, vertex.label=NA, layout=layout.fruchterman.reingold, vertex.color = V(march1_gr_sub)$color, vertex.size=2, edge.arrow.size=0.2, edge.color="black")
  
  march1_btw <- betweenness(march1_gr, directed = TRUE)
  sorted.btw <- sort(march1_btw, decreasing = TRUE)
  
  march_sub_c <- sorted.btw[1:num_of_users,]
  
  return(march_sub_c)
}


time_frame <- function(latitude, longitude, radius, num_of_user, week, meal_type){
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
  
  if(week == "Weekday"){
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
  
  march_ll <- ddply(march, c("latitude","longitude"), summarize, len=length(callee_id))
  
  march_ll_sub <- subset(march_ll, latitude <= lat_lim_top)
  march_ll_sub <- subset(march_ll_sub, latitude >= lat_lim_bot)
  march_ll_sub <- subset(march_ll_sub, longitude <= long_lim_right)
  march_ll_sub <- subset(march_ll_sub, longitude >= long_lim_left)
  
  march_sub <- subset(march, march$latitude %in% march_ll_sub$latitude)
  march_sub <- subset(march, march$longitude %in% march_ll_sub$longitude)
  
  
  march_w_sub <- subset(march_sub, march_w$hr %in% time)
  
  march_w_sub_g <- ddply(march_w_sub, "caller_id", summarize, len = length(callee_id))
  
  march_w_sub_g <- march_w_sub_g[order(-march_w_sub_g$caller_id),]
  
  march_w_sub_g_ret <- march_w_sub_g[1:n,]
  
  return(march_w_sub_g_ret)
}
  

recommendations <- function(latitude, longitude, radius, num_of_user, number){
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
  
  march_sub_g <- ddply(march_sub, c("caller_id","callee_id"), summarize, len=length(callee_id))
    
  march_sub_g_sub <- subset(march_sub_g, caller_id == number)
  
  march_gr_sub <- graph.data.frame(march_sub_g, directed=T)
  
  E(march_sub_g)$weight = rep(1,nrow(march_sub_g))
  tmp3 <- shortest.paths(march_gr_sub,v=number)
  tmp3<-as.data.frame(tmp3)
  
  nodes<-NULL
  for(i in 1:ncol(tmp3)){
    if((tmp3[1,i] <= 5) & tmp3[1,i] >= 1){
      nodes <- rbind(nodes,colnames(tmp[i]))
    }
  }
  
  tmp3_sub <- subset(tmp3, tmp3 <= 4)
  nodes <- rbind(nodes, march_sub_g_sub$callee_id)
  
  #Calculate cosine similarity between number and all caller ids in nodes.
  #Sort by cosine similarity
  #Take top n nodes and return
}

  
