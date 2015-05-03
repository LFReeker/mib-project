library(igraph)
library(plyr)
library(ggplot2)
library(TraMineR)
library(cluster)
library(ggmap)
library(reshape2)

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
    if((tmp3[1,i] <= 3) & (tmp3[1,i] >= 1)){
      nodes <- rbind(nodes,colnames(tmp3[i]))
    }
  }
  
  #Calculate cosine similarity between number and all caller ids in nodes.
  #Sort by cosine similarity
  #Take top n nodes and return
  
  x = nodes[,1]
  
  nodes <- as.data.frame(nodes)
  
  colnames(nodes) <- "caller_id"
  
  # For cosine similarity function
  call.data <- callData() 
  
  # create their location vector's
  user_input <- user_vector(number) # see user_vector function in 'support'
  
  recomm_users <- NULL
  
  nodes$caller_id <- as.numeric(levels(nodes$caller_id))[nodes$caller_id]
  
  #cat("Test:", nodes$caller_id)
  
  for(i in 1:20) {   # better: nrow(nodes$caller_id) but limited due to the use of a subset of the whole dataset
    user <- nodes$caller_id[i]
    vector_user <- user_vector(user) # see user_vector function in 'support'
    
    recomm_users$caller_id[i] <- as.character(user)
    recomm_users$similarity[i] <- cosine_similarity(user_input, vector_user) # see cosine_similarity function in 'support'
  }
  
  recomm_users <- as.data.frame(recomm_users)
  recomm_users <- subset(recomm_users, similarity >= 0.3)
  
  recomm_users_new <- recomm_users[order(-recomm_users$similarity),]
  
  colnames(recomm_users_new) <- c("Caller ID", "Similarity factor")
  rownames(recomm_users_new) <- 1:nrow(recomm_users_new)
  
  return(recomm_users_new[1:num_of_users,])
}
