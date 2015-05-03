##########################################################################
#                                                                        #
# CREATE JPG's FOR ALL CELL TOWERS HOUR WISE                             #
# THEN CREATE A GIF WITH ALL THESE JPS's TO PLOT A MOTION GRAPH          #
#                                                                        #
##########################################################################

## Load required libraries
library(plyr)

## Load the merged data
singleDay_with_coord <- read.csv("march1.merged.csv",sep=",",header=T)
cell.loc.data <- read.csv("cell.loc.data.csv",sep=",",header=T)

## Group the data by longitude, latitude and hour
lon.lat.hr <- ddply(singleDay_with_coord, c("longitude","latitude", "hr"), summarise, count=length(c(longitude,latitude,hr)), duration=sum(call_duration))
lon.lat.hr$count <- as.numeric(lon.lat.hr$count)

## Loop for each hour
for (hour in 0:23) {
  #hour <- 23
  lon.lat <- lon.lat.hr[which(lon.lat.hr$hr == hour),]

  
  ## Retrieve a map from Google Maps with center at the means of all the coordinates
  cell.loc.map <- get_map(location = c(lon = mean(cell.loc.data$longitude),
                                       lat = mean(cell.loc.data$latitude)),
                          zoom = 10, scale = 2) # scale specifies the resolution of the map
  
  cell.loc.map <- ggmap(cell.loc.map) + geom_point(data=lon.lat, aes(x = longitude, y = latitude, size=count, alpha=0.5, fill="red"), shape=21) + scale_size(range=c(3,30)) + guides(fill = FALSE, alpha = FALSE,size = FALSE) 
  ###+ geom_density2d(data = cell.loc.data, aes(x = longitude, y = latitude))
  
  title <- paste("Locations of Cell Towers on 03/01 at ",hour," hrs",sep="")
  cell.loc.map <- cell.loc.map + ggtitle(title)
  ## Print the map
  print(cell.loc.map)
  
  name <- paste("cell_tower_hr_",hour,".jpg",sep="") 
  ggsave(filename=name, width=8, height=8)

}

## N O T E:
## Now create a gif image with all these images to get a motion graph from 0 to 23 hours for particular day

##############################################################################################
#                                                                                            #
#                             E N D    O F    P R O G R A M                                  #
#                                                                                            #
##############################################################################################
