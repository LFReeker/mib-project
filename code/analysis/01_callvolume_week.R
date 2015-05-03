library(igraph)
library(plyr)
library(ggplot2)
library(TraMineR)
library(cluster)
library(ggmap)
library(reshape2)

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
