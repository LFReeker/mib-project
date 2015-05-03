##############################################################################
#                                                                            #
#     P L O T   T H E   U S E R S   T R A J E C T O R Y                      #
#              INPUT FROM COSINE SIMILARITY                                  #
#                                                                            #
##############################################################################
#install.packages("grid")
#install.packages("mapproj")

library(mapproj) # For Arrows
library(grid) # For Arrows
library(ggmap)
library(plyr)
library(ggplot2)

longitude <- 117.141226
latitude  <- 36.195291
radius    <- 2

source("./01_createRectangle.R")
### Select a particular user
##load data (Replace with Actual Data Set)
call.data <- read.csv("march1_final.csv",sep=",",header=T)



march1 <- read.csv("march1_done.csv")
march2 <- read.csv("march2_done.csv")
march3 <- read.csv("march3_done.csv")
march4 <- read.csv("march4_done.csv")
march5 <- read.csv("march5_done.csv")
march6 <- read.csv("march6_done.csv")
march7 <- read.csv("march7_done.csv")

calldata <- read.csv("cell.loc.data.csv",header=T)



call.data <- march1
call.data <- rbind(call.data,march2)
call.data <- rbind(call.data,march3,march4)
call.data <- rbind(call.data,march5,march6,march7)

## reduce dataset of call.data to run the codes
#call.data <- call.data[1:100000,]

#m <- ddply(call.data,c("caller_id","longitude","latitude"),summarize,Frew=length(c(caller_id,longitude,latitude)))

## Generate person of interest
# here I picked a random person
person_int <- 96314538
  
  #396416538
  
  #96314538 ## INPUT USER 1
  #14317538
#sample(call.data$caller_id, 1) 
person_int2 <- 997310538
  
  #75587410538
  #997310538 ## INPUT USER 1
#sample(call.data$caller_id, 1)

## Subset the dataset by the person of interest
call.data.person <- subset(call.data, call.data$caller_id == person_int)
#names(call.data.person)

call.data.person2 <- subset(call.data, call.data$caller_id == person_int2)

## Sort the data by time
sort.call.data.person <- call.data.person[order(call.data.person$hr), ]
sort.call.data.person2 <- call.data.person2[order(call.data.person2$hr), ]

#sort.call.data.person2 <- ddply(sort.call.data.person2,c("longitude","latitude"))
## Mapping
# location
location <- c(lon = mean(call.data$longitude),
              lat = mean(call.data$latitude))
# map from google
call.data.person.map <- get_map(location, zoom = 10, scale = 2) 

# produce the map, red for person1 and blue for person2
#map <- ggmap(call.data.person.map, extent = 'device', legend = 'none')
map <- ggmap(call.data.person.map, extent='panel',base_layer=ggplot(calldata, aes(x=longitude, y=latitude)))


map <- map + geom_line(data = sort.call.data.person2, aes(x = longitude, y = latitude),colour="red", alpha=0.80, arrow=arrow(ends = "last"),size=2) + geom_line(data = sort.call.data.person, aes(x = longitude, y = latitude),colour="blue", alpha=0.80, arrow=arrow(ends = "last"), size=2)

map  <- map + geom_point(x=longitude, y=latitude, size=5, shape=25, alpha=1, aes(fill="blue")) + scale_fill_manual(values=c("blue")) + guides(fill=FALSE)

rect <- getRectangle(latitude,longitude,radius)

map  <- map + geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="gray20", alpha=0.4, inherit.aes = FALSE)

print(map)

########################################################################################
#                                                                                      #
#                   E N D   O F   P R O G R A M                                        #
#                                                                                      #
########################################################################################
