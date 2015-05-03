library(igraph)
library(plyr)
library(ggplot2)
library(TraMineR)
library(cluster)
library(ggmap)

######################################
#Data
######################################

#Reading all the data sets
march1 <- read.csv("March1_Merged.csv")
march2 <- read.csv("March2_Merged.csv")
march3 <- read.csv("March3_Merged.csv")
march4 <- read.csv("March4_Merged.csv")
march5 <- read.csv("March5_Merged.csv")
march6 <- read.csv("March6_Merged.csv")
march7 <- read.csv("March7_Merged.csv")

#Removing the primary key
march1 <- march1[,-1]
march3 <- march3[,-1]
march4 <- march4[,-1]
march5 <- march5[,-1]
march6 <- march6[,-1]
march7 <- march7[,-1]

#Group by latitude and longiturde
march_ll <- ddply(march1, c("latitude", "longitude"), summarize, len = length(callee_id))
march2_ll <- ddply(march2, c("latitude", "longitude"), summarize, len = length(callee_id))
march3_ll <- ddply(march3, c("latitude", "longitude"), summarize, len = length(callee_id))
march4_ll <- ddply(march4, c("latitude", "longitude"), summarize, len = length(callee_id))
march5_ll <- ddply(march5, c("latitude", "longitude"), summarize, len = length(callee_id))
march6_ll <- ddply(march6, c("latitude", "longitude"), summarize, len = length(callee_id))
march7_ll <- ddply(march7, c("latitude", "longitude"), summarize, len = length(callee_id))

#creating march_ll dataset
march_ll <- rbind(march_ll,march2_ll)
march_ll <- rbind(march_ll,march3_ll)
march_ll <- rbind(march_ll,march4_ll)
march_ll <- rbind(march_ll,march5_ll)
march_ll <- rbind(march_ll,march6_ll)
march_ll <- rbind(march_ll,march7_ll)

#Total number of latitudes and longitudes : 865

options(scipen=999)

#deciding which latitudes and longitudes toremove
march_ll <- ddply(march_ll, c("latitude", "longitude"), summarize, len = sum(len))
march_ll <- march_ll[order(march_ll$len),]
march_ll$cum_len <- cumsum(march_ll$len)
march_ll$per <- (march_ll$cum_len/sum(march_ll$len))*100

#Plotting the length histogram
ggplot(march_ll, aes(x=len)) + geom_histogram(binwidth = 10000)

#Retaining 90% of the lengths.  Done to reduce size of the dataset, retaining only the most valuble features.
march_ll_sub <- subset(march_ll, per > 10)

#Removing from each days dataset, the inconsequential latitudes and longitudes
march1 <- subset(march1, (march1$latitude %in% march_ll_sub$latitude) & (march1$longitude %in% march_ll_sub$longitude))
march2 <- subset(march2, (march2$latitude %in% march_ll_sub$latitude) & (march2$longitude %in% march_ll_sub$longitude))
march3 <- subset(march3, (march3$latitude %in% march_ll_sub$latitude) & (march3$longitude %in% march_ll_sub$longitude))
march4 <- subset(march4, (march4$latitude %in% march_ll_sub$latitude) & (march4$longitude %in% march_ll_sub$longitude))
march5 <- subset(march5, (march5$latitude %in% march_ll_sub$latitude) & (march5$longitude %in% march_ll_sub$longitude))
march6 <- subset(march6, (march6$latitude %in% march_ll_sub$latitude) & (march6$longitude %in% march_ll_sub$longitude))
march7 <- subset(march7, (march7$latitude %in% march_ll_sub$latitude) & (march7$longitude %in% march_ll_sub$longitude))

#Grouping by caller id
march1_c <- ddply(march1, c("caller_id"), summarize, len = length(callee_id))
march2_c <- ddply(march2, c("caller_id"), summarize, len = length(callee_id))
march3_c <- ddply(march3, c("caller_id"), summarize, len = length(callee_id))
march4_c <- ddply(march4, c("caller_id"), summarize, len = length(callee_id))
march5_c <- ddply(march5, c("caller_id"), summarize, len = length(callee_id))
march6_c <- ddply(march6, c("caller_id"), summarize, len = length(callee_id))
march7_c <- ddply(march7, c("caller_id"), summarize, len = length(callee_id))

march_c <-march1_c

#Combining all grouped datasets into one
march_c <- rbind(march_c,march2_c)
march_c <- rbind(march_c,march3_c)
march_c <- rbind(march_c,march4_c)
march_c <- rbind(march_c,march5_c)
march_c <- rbind(march_c,march6_c)
march_c <- rbind(march_c,march7_c)

march_c_c <- ddply(march_c, c("caller_id"), summarize, len = sum(len))

#Retaining the most important users. The number of users retained can vary deending on context. 
#For our requirement, we wanted to maximise the probbability with which we can claim that a 
#potential customer is in a particular location at a particular time
march_c_c_sub <- subset(march_c, (len >= 7)

#Removing the unwanted callees from the original dataset                        
march1 <- subset(march1, march1$caller_id %in% march_c_c_sub$caller_id)
march2 <- subset(march2, march2$caller_id %in% march_c_c_sub$caller_id)
march3 <- subset(march3, march3$caller_id %in% march_c_c_sub$caller_id)
march4 <- subset(march4, march4$caller_id %in% march_c_c_sub$caller_id)
march5 <- subset(march5, march5$caller_id %in% march_c_c_sub$caller_id)
march6 <- subset(march6, march6$caller_id %in% march_c_c_sub$caller_id)
march7 <- subset(march7, march7$caller_id %in% march_c_c_sub$caller_id)
                        
#Retaining only callers who have made calls on both weekends, for the respective dataset
march1_sub <- subset(march1, (march1$caller_id %in% march2$caller_id))
march2_sub <- subset(march2, (march2$caller_id %in% march1$caller_id))
                        
#Retaining callers who have made calls on all weekdays for the respective datasets
march3_sub <- subset(march3, (march3$caller_id %in% march4$caller_id) & (march3$caller_id %in% march5$caller_id) & (march3$caller_id %in% march6$caller_id) & (march3$caller_id %in% march7$caller_id))
march4_sub <- subset(march4, (march4$caller_id %in% march3$caller_id) & (march4$caller_id %in% march5$caller_id) & (march4$caller_id %in% march6$caller_id) & (march4$caller_id %in% march7$caller_id))
march5_sub <- subset(march5, (march5$caller_id %in% march3$caller_id) & (march5$caller_id %in% march4$caller_id) & (march5$caller_id %in% march6$caller_id) & (march5$caller_id %in% march7$caller_id))
march6_sub <- subset(march6, (march6$caller_id %in% march3$caller_id) & (march6$caller_id %in% march4$caller_id) & (march6$caller_id %in% march5$caller_id) & (march6$caller_id %in% march7$caller_id))
march7_sub <- subset(march7, (march7$caller_id %in% march3$caller_id) & (march7$caller_id %in% march4$caller_id) & (march7$caller_id %in% march5$caller_id) & (march7$caller_id %in% march6$caller_id))
                        
#The final datasets are the marchx_sub files
                        