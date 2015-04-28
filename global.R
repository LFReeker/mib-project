library(igraph)
library(plyr)
library(ggplot2)
library(TraMineR)
library(cluster)
library(ggmap)
library(reshape2)
library(shiny)
library(shinydashboard)

## Local for Lars
#setwd("~/Dashboard/")

calldata <- read.csv("cell.loc.data.csv", header=T)
calldata$longitude <- as.numeric(as.character(calldata$longitude))
calldata$latitude <- as.numeric(as.character(calldata$latitude))

## Use Load()
#march <- read.csv("march.csv", header=T)
#march$longitude <- as.numeric(as.character(march$longitude))
#march$latitude <- as.numeric(as.character(march$latitude))

load("march_data.RData")

