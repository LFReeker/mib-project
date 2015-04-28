## Load the required packages, assuming they have been installed
library(ggplot2)
library(ggmap) # for plotting maps
library(igraph) # for plotting graphs
library(popgraph) # for plotting population graphs
library(scales) # for plot formatting 
library(plyr) # for data manipulation

## Load the merged set 
march1 <- read.csv("march1.merged.csv",sep=",",header=T)
head(march1)

## Group the data by longitude, latitude and hour
caller.lon.lat.hr <- ddply(march1, c("caller_id", "longitude","latitude", "hr"), summarise, count=length(c(longitude,latitude,hr)), duration=sum(call_duration))
caller.lon.lat.hr <- caller.lon.lat.hr[order(caller.lon.lat.hr$caller_id,caller.lon.lat.hr$hr),]

### 5b)
call.data <- march1
## reduce dataset of call.data to run the codes
call.data <- call.data[1:100000,]
  
  ## Select the callers that are also callees
  call_ids <- intersect(call.data$caller_id, call.data$callee_id)
  subset.call.data <- subset(call.data, caller_id %in% call_ids)
  subset.call.data <- subset(subset.call.data, callee_id %in% call_ids)
  
  ## Associate each caller with their **vector** of locations.
  caller_id.cell_id <- new.env()
  for(a_call_id in call_ids) {
    subset.caller_id <- subset(subset.call.data, caller_id == a_call_id)
    if(nrow(subset.caller_id) > 0) {
      ## Find the most frequented location
      cell_id.table <- table(subset.caller_id$cell_id)
      cell_id.table <- subset(cell_id.table, cell_id.table > 0)
      caller_id.cell_id[[toString(a_call_id)]] <- names(cell_id.table)
    }
  }
  
  lower.bound <- 0 
  upper.bound <- 50
  ## Create a data frame that stores the relationships (i.e., edges)
  ## among the agents. Strength of the relationship is represented by call frequency.
  edges <- data.frame(caller_id = subset.call.data$caller_id,
                      callee_id = subset.call.data$callee_id)
  weightedEdges <- data.frame(table(edges))
  weightedEdges <- subset(weightedEdges, Freq > lower.bound)
  weightedEdges <- subset(weightedEdges, Freq < upper.bound)
  
  ## Map each agent to all of their spatial coordinates during the duration
  caller_location <- vector()
  callee_location <- vector()
  freq <- vector()
  for(i in 1:nrow(weightedEdges)) {
    a_freq <- weightedEdges$Freq[i]
    caller_id <- toString(weightedEdges$caller_id[i])
    caller_loc <- caller_id.cell_id[[caller_id]]
    callee_id <- toString(weightedEdges$callee_id[i])
    callee_loc <- caller_id.cell_id[[callee_id]]
    
    if(!is.null(caller_loc) && !is.null(callee_loc)) {
      if(length(caller_loc) == 1 && length(callee_loc) == 1) {
        ## Create an edge between the pair of locations
        caller_location <- c(caller_location, caller_loc)
        callee_location <- c(callee_location, callee_loc)
        freq <- c(freq, a_freq)
      } else { # at least one has multiple locations
        n_edges <-length(caller_loc) * length(callee_loc)
        a_freq <- ceiling(a_freq / n_edges)
        freq <- c(freq, rep(a_freq, n_edges))
        
        if(length(caller_loc) > 1 && length(callee_loc) > 1) {
          ## Create complete bipartite graph connecting the locations of callers
          ## to the locations of callees.
          for(j in 1:length(caller_loc)) {
            for(k in 1:length(callee_loc)) {
              caller_location <- c(caller_location, caller_loc[j])
              callee_location <- c(callee_location, callee_loc[k])
            }
          }
        } else {
          ## Either caller or callee has only one location (and the other has multiple)
          if(length(caller_loc) > 1) {
            for(j in 1:length(caller_loc)) {
              caller_location <- c(caller_location, caller_loc[j])
            }
            callee_location <- c(callee_location, rep(callee_loc, length(caller_loc)))
          } else { # length(callee_loc) > 1
            caller_location <- c(caller_location, rep(caller_loc, length(callee_loc)))
            for(j in 1:length(callee_loc)) {
              callee_location <- c(callee_location, callee_loc[j])
            }
          }
        }
      }
    }
  }
  
  ## Combine the attributes into a **new** data frame and return
  weightedEdges <- data.frame(caller_loc = caller_location, callee_loc = callee_location,
                              freq = freq)

### 6 b)
## Map all users to their location (cell tower) for 3:00
time_int <- 3 #time of interest 
subset_march1 <- subset(march1, select = c("cell_id", "caller_id", "callee_id", "call_duration", "longitude", "latitude"), subset=(hr == time_int))

----code written by Praveen on the subset_march1


## Average duration time 
mean(subset_march1$call_duration)

## Sum of duration time
sum(subset_march1$call_duration >= 0)

## How many users are using the cell_tower for 3:00
sum(as.numeric(subset_march1$caller_id))

### 6c)
d <- density(caller.lon.lat.hr$duration)
plot(d, main="Kernel Density of Call_duration Per Caller")
polygon(d, col="red", border="blue") 


### 7a) 
## Select a particular user
person_int <- sample(march1$caller_id, 1) #person of interest, in this case, a random person
subset_march1_person <- subset(march1, select = c("cell_id", "callee_id", "call_duration", "longitude", "latitude"), subset=(caller_id == person_int))


### 7b)
## Take a subset of the data, by eliminating all calls which were made outside the city center
