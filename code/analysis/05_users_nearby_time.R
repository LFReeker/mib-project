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
