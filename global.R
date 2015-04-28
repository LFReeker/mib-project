library("ggplot2")
library("ggmap")

#setwd("~/Dashboard/")

calldata <- read.csv("cell.loc.data.csv", header=T)
calldata$longitude <- as.numeric(as.character(calldata$longitude))
calldata$latitude <- as.numeric(as.character(calldata$latitude))
