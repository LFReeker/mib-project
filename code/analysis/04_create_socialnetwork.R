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
