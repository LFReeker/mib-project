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

march1_c <- ddply(march1, c("caller_id"), summarize, len = length(callee_id))
march2_c <- ddply(march2, c("caller_id"), summarize, len = length(callee_id))
march3_c <- ddply(march3, c("caller_id"), summarize, len = length(callee_id))
march4_c <- ddply(march4, c("caller_id"), summarize, len = length(callee_id))
march5_c <- ddply(march5, c("caller_id"), summarize, len = length(callee_id))
march6_c <- ddply(march6, c("caller_id"), summarize, len = length(callee_id))
march7_c <- ddply(march7, c("caller_id"), summarize, len = length(callee_id))

march_c <-march1_c

march_c <- rbind(march_c,march2_c)
march_c <- rbind(march_c,march3_c)
march_c <- rbind(march_c,march4_c)
march_c <- rbind(march_c,march5_c)
march_c <- rbind(march_c,march6_c)
march_c <- rbind(march_c,march7_c)

ggplot(data = march_c,mapping = aes(x=len)) + geom_histogram()

march1_c <-subset(march1_c, len > 1)
march1_c <-subset(march1_c, len <= 20)
march1 <- subset(march1, march1$caller_id %in% march1_c$caller_id)

march2_c <-subset(march2_c, len > 1)
march2_c <-subset(march2_c, len <= 20)
march2 <- subset(march2, march2$caller_id %in% march2_c$caller_id)

march3_c <-subset(march3_c, len > 1)
march3_c <-subset(march3_c, len <= 20)
march3 <- subset(march3, march3$caller_id %in% march3_c$caller_id)

march4_c <-subset(march4_c, len > 1)
march4_c <-subset(march4_c, len <= 20)
march4 <- subset(march4, march4$caller_id %in% march4_c$caller_id)

march5_c <-subset(march5_c, len > 1)
march5_c <-subset(march5_c, len <= 20)
march5 <- subset(march5, march5$caller_id %in% march5_c$caller_id)

march6_c <-subset(march6_c, len > 1)
march6_c <-subset(march6_c, len <= 20)
march6 <- subset(march6, march6$caller_id %in% march6_c$caller_id)

march7_c <-subset(march7_c, len > 1)
march7_c <-subset(march7_c, len <= 20)
march7 <- subset(march7, march7$caller_id %in% march7_c$caller_id)


march_ll <- ddply(march1, c("latitude", "longitude"), summarize, len = length(callee_id))
march2_ll <- ddply(march2, c("latitude", "longitude"), summarize, len = length(callee_id))
march3_ll <- ddply(march3, c("latitude", "longitude"), summarize, len = length(callee_id))
march4_ll <- ddply(march4, c("latitude", "longitude"), summarize, len = length(callee_id))
march5_ll <- ddply(march5, c("latitude", "longitude"), summarize, len = length(callee_id))
march6_ll <- ddply(march6, c("latitude", "longitude"), summarize, len = length(callee_id))
march7_ll <- ddply(march7, c("latitude", "longitude"), summarize, len = length(callee_id))

march_ll <- rbind(march_ll,march2_ll)
march_ll <- rbind(march_ll,march3_ll)
march_ll <- rbind(march_ll,march4_ll)
march_ll <- rbind(march_ll,march5_ll)
march_ll <- rbind(march_ll,march6_ll)
march_ll <- rbind(march_ll,march7_ll)

march_11_sub <- subset(march_ll, march_ll$len > 100)

march1 <- subset(march1, (march1$latitude %in% march_11_sub$latitude) & (march1$longitude %in% march_11_sub$longitude))
march2 <- subset(march2, (march2$latitude %in% march_11_sub$latitude) & (march2$longitude %in% march_11_sub$longitude))
march3 <- subset(march3, (march3$latitude %in% march_11_sub$latitude) & (march3$longitude %in% march_11_sub$longitude))
march4 <- subset(march4, (march4$latitude %in% march_11_sub$latitude) & (march4$longitude %in% march_11_sub$longitude))
march5 <- subset(march5, (march5$latitude %in% march_11_sub$latitude) & (march5$longitude %in% march_11_sub$longitude))
march6 <- subset(march6, (march6$latitude %in% march_11_sub$latitude) & (march6$longitude %in% march_11_sub$longitude))
march7 <- subset(march7, (march7$latitude %in% march_11_sub$latitude) & (march7$longitude %in% march_11_sub$longitude))

write.csv(march1,"march2_final.csv")
write.csv(march2,"march3_final.csv")
write.csv(march3,"march4_final.csv")
write.csv(march4,"march5_final.csv")
write.csv(march5,"march5_final.csv")
write.csv(march6,"march6_final.csv")
write.csv(march7,"march7_final.csv")
