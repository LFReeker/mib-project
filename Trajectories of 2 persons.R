### Select a particular user
##load data
call.data <- read.csv("march1.merged.csv",sep=",",header=T)
head(march1)

## reduce dataset of call.data to run the codes
call.data <- call.data[1:100000,]

## generate person of interest
# here I picked a random person
person_int <- sample(call.data$caller_id, 1) 
person_int2 <- sample(call.data$caller_id, 1)

## Subset the dataset by the person of interest
call.data.person <- subset(call.data, call.data$caller_id == person_int)
names(call.data.person)

call.data.person2 <- subset(call.data, call.data$caller_id == person_int2)

## Sort the data by time
sort.call.data.person <- call.data.person[order(call.data.person$hr), ]
sort.call.data.person2 <- call.data.person2[order(call.data.person2$hr), ]

## Mapping
# location
location <- c(lon = mean(sort.call.data.person$longitude),
            lat = mean(sort.call.data.person$latitude))
# map from google
call.data.person.map <- get_map(location, zoom = 13, scale = 2) 

# produce the map, red for person1 and blue for person2
map <- ggmap(call.data.person.map, extent = 'device', legend = 'none')
map <- map + geom_line(data = call.data.person.map, aes(x = longitude, y = latitude, size = call.duration),
                                          colour="red", alpha=0.80)
map2 <- map + geom_line(data = call.data.person.map, aes(x = longitude, y = latitude, size = call.duration),
                           colour="blue", alpha=0.80)

