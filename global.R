library(igraph)
library(plyr)
library(ggplot2)
library(TraMineR)
library(cluster)
library(ggmap)
library(reshape2)
library(shiny)
library(shinydashboard)
library(lsa)

#setwd("~/Dashboard/")

# Load cell tower data
celldata <- read.csv("cell.loc.data.csv", header=T)
celldata$longitude <- as.numeric(as.character(celldata$longitude))
celldata$latitude <- as.numeric(as.character(celldata$latitude))

march1 <- read.csv("march1_done.csv", header=T)
march2 <- read.csv("march2_done.csv", header=T)
march3 <- read.csv("march3_done.csv", header=T)
march4 <- read.csv("march4_done.csv", header=T)
march5 <- read.csv("march5_done.csv", header=T)
march6 <- read.csv("march6_done.csv", header=T)
march7 <- read.csv("march7_done.csv", header=T)

# Load week call data
load("march_data.RData")

#march <- read.csv("march.csv", header=T)




# Formulas

user_vector <- function(user_id) {  # function for creating a user's location vector
  
  call.data <- march
  
  # retrieve all the unique locations in the dataset
  locations <- data.frame(cell_id = unique(call.data$cell_id))
  #locations <- data.frame(cell_id = unique(celldata$cell))
  
  # create an empty location vector, all values to 0
  user_vector <- rep(list(c(rep(0, 24))), nrow(locations))
  
  # create a subset of the data for the user, containing all its calls made
  user_subset <- subset(call.data, caller_id == user_id)
  
  # retrieve all hour and cell_id's of all unique calls made
  user_calls <- ddply(user_subset, "caller_id", summarize, hour = hr, location = cell_id)
  
  for(call in 1:nrow(user_calls)) {
    ## for each call..
    # print(call)
    
    # ..store the cell_id of the call
    user_cell <- user_calls$location[call]
    # print(user_cell)
    
    # ..store the hour the call was made
    user_hour <- user_calls$hour[call]
    # print(user_hour)
    
    # ..retrieve the cell_id's index in the list of all unique locations
    cell_number <- which(locations$cell_id == user_calls$location[call])
    # cat(cell_number)
    
    # ..set the value of the particular location in the particular hour to 1
    user_vector[[cell_number]][user_hour] <- user_vector[[cell_number]][user_hour]+1
    
  }
  
  # return the filled location vector
  return(unlist(user_vector))
  
}

cosine_similarity <- function(vector_user1, vector_user2) {
  cosine <- cosine(vector_user1,vector_user2) # calculate cosine similarity between two vectors
  #return(as.factor(cosine))
  return(cosine)
}


## Example 

# set two user_id's
#user1 <- 91460716651
#user2 <- 87149815355

# create their location vector's
#vector_user1 <- user_vector(user1)
#vector_user2 <- user_vector(user2)

# compute their cosine similarity
#cosine_similarity(vector_user1, vector_user2)
