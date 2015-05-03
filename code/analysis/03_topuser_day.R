library(igraph)
library(plyr)
library(ggplot2)
library(TraMineR)
library(cluster)
library(ggmap)
library(reshape2)

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
