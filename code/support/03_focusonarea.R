##############################################################################
#                                                                            #
#     M A P   T H E   A R E A    T O   B E    F O C U S S E D                #
#                                                                            #
##############################################################################

## Load the libraries
library(ggmap)


## Load the required data
calldata <- read.csv("cell.loc.data.csv",header=T)


# Get the latitude, longitude and radius of area to be focussed (in Km)

#longitude <- 117.089599
#latitude  <- 36.199165
longitude <- 117.141226
latitude  <- 36.195291
radius <- 2


source("./getRectangle.R")

#36.195291, 117.141226

cell.loc.map <- get_map(location = c(lon = longitude,
                                     lat = latitude),
                        zoom = 13, scale = 2) # scale specifies the resolution of the map

rect <- getRectangle(latitude,longitude,radius)

cell.subset <- subset(calldata, latitude <= rect$ymax)#lat_lim_top)
cell.subset <- subset(cell.subset, latitude >= rect$ymin)#lat_lim_bot)
cell.subset <- subset(cell.subset, longitude <= rect$xmax)#long_lim_right)
cell.subset <- subset(cell.subset, longitude >= rect$xmin)#long_lim_left)


map2 <- ggmap(cell.loc.map, extent='panel', base_layer=ggplot(calldata, aes(x=longitude, y=latitude)))

# now add the rectangle data frame into ggplot using geom_rect() and specify the color and size 
map.scan <- map2 + geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="gray20", alpha=0.4, inherit.aes = FALSE) 

# add labels to the plot 
map.scan <- map.scan + labs(title = "Region to be focussed") 
# add title theme 
map.scan <- map.scan + theme(plot.title = element_text(hjust = 0, vjust = 1, face = c("bold"))) 
map.scan <- map.scan + geom_point(data=cell.subset, aes(x = longitude, y = latitude, size=5, alpha=0.5),shape=17, col="red") + guides(alpha = FALSE,size = FALSE) 

map.scan <- map.scan + geom_point(x=longitude, y=latitude, size=5, shape=25, alpha=1, aes(fill="blue")) + scale_fill_manual(values=c("blue")) + guides(fill=FALSE)
print(map.scan)


########################################################################################
#                                                                                      #
#                   E N D   O F   P R O G R A M                                        #
#                                                                                      #
########################################################################################
