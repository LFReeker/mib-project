### Authors: Subramaniam Balasubramaniam, William Liu, Praveen Kumar Thoranathula Radhesyam, Lars Reeker
### Course: 95789 Mobile intelligence and Business
### Institution: Carnegie Mellon Univeristy, Heinz College

### Many thanks and credits to Truc Viet Le for sharing his code

### Content:
###   Step 1.1  Connect to the server 
###   Step 1.2  Retrieve a sample of the raw data
###   Step 1.3  Retrieve all unique users and their connections
###   Step 1.4  Order and transform the retrieved dataset and calculate frequency total and frequency of pairs
###   Step 1.8  Close the server connection


### Step 1.1
############

## Clear the environment
rm(list = ls()) 

## Load all necessary libraries
library(rmongodb)
library(ggplot2)
library(scales)
library(plyr)

## Login credentials to connect to data server
host <- "heinz-tjle.heinz.cmu.edu"
username <- "student"
password <- "helloWorld"
db <- "admin"

## Connect to MongoDB remote server
mongo <- mongo.create(host = host, db = db, username = username, password = password)
## Check if we are successfully connected
mongo.is.connected(mongo)

## The database we're working with is 'admin' and the collection is 'cellular'
collection <- "cellular"
namespace <- paste(db, collection, sep=".")

## Check what's in the database
mongo.get.databases(mongo)
## Take a look at the collections (tables) of one of the db's
mongo.get.database.collections(mongo, db="admin")

## Count the number of docs (rows) we have in a collection
mongo.count(mongo, namespace)

## Step 1.2
###########

## Get a sample the data to see how the data looks like
sample <- mongo.find.one(mongo, namespace)
sample
## Convert data in BSON object to a list
sample.list <- mongo.bson.to.list(sample)
sample.list

## Define the fields to be returned
fields <- mongo.bson.buffer.create()
# '1L' means we want to turn this field on, '0L' to turn it off
mongo.bson.buffer.append(fields, "_id", 0L)
mongo.bson.buffer.append(fields, "imei", 1L)
mongo.bson.buffer.append(fields, "caller_id", 1L)
mongo.bson.buffer.append(fields, "callee_id", 1L)
mongo.bson.buffer.append(fields, "date", 1L)
mongo.bson.buffer.append(fields, "time", 1L)
mongo.bson.buffer.append(fields, "call_duration", 1L)
mongo.bson.buffer.append(fields, "cell_id", 1L)
# Make object from buffer
fields <- mongo.bson.from.buffer(fields)


### Step 1.3
############

## Retrieve all unique users in the dataset
## March 2, 2008 is a Sunday
## March 5, 2008 is a Wednesday

## Create a progress bar
progress.bar <- create_progress_bar("text")
progress.bar$init(100)
call.data <- data.frame(stringsAsFactors=FALSE)

## Define the query
query <- mongo.bson.from.list(list('date'=20080302))

## Create the query cursor
cursor <- mongo.find(mongo, namespace, query=query, fields=fields)  

## Iterate over the cursor
while(mongo.cursor.next(cursor)) {
  ## Iterate and grab the next record
  value <- mongo.cursor.value(cursor)
  call <- mongo.bson.to.list(value)
  ## Make it a data frame
  call.df <- as.data.frame(t(unlist(call)), stringsAsFactors=FALSE)
  ## Bind to the master data frame
  call.data <- rbind.fill(call.data, call.df)
  progress.bar$step()
}

# Release the resources attached to cursor on both client and server
done <- mongo.cursor.destroy(cursor)

## Show the data in a dataframe
call.data

## Write dataset to csv file
write.csv(call.data, "call_data_description.csv")


### Step 1.4
############

## Group the calls per user in the retrieved dataset
call.data.grouped <- ddply(call.data, "caller.id", summarize, freq=length(callee.id))
nrow(call.data.grouped)

### Step 1.5
############

# call.data <- read.csv("call_data_weekend_33462obs.csv", header = TRUE)
sunday_grouped <- ddply(call.data, c("caller_id", "callee_id"), summarize, weight=(length(callee_id)*sum(call_duration)))
write.csv(sunday_grouped, "sunday_freq.csv")

### Compute cosine similarity
############
call.data <- read.csv("call_data_sunday_32908obs.csv", header = TRUE)

library(plyr)
freq <- ddply(call.data, "caller_id", summarize, freq=length(caller_id))

user1 <- 20460812053 # 10
user2 <- 17614317538 # 6

rec_towers <- unique(call.data$cell_id) #2568 cell towers recorded

users <- unique(call.data$caller_id) # retrieve all unique callers (callees do not have location)



for(user in call.data) {
  index <- call.data$caller_id %in% user1 # create a logical vector of all instances with caller_id = user
  list <- call.data[index, ] # list all connections of user
  list
  
  list_time <- list$time # list all times a connection is made
  list_time
  data.frame(date=list$time, time=format(x, "%H:%M"))
  
  
  
  
  list_tower <- list$cell_id # retrieve list with all cell tower id's of user
  index_tower <- rec_towers %in% list_tower # create logical vector with all visited cell towers
  index_tower
  
  

  

detect_hour <- function(list_time) {  
  output <- data.frame(index = numeric(length(list_time)), times = factor(length(list_time)), hour = numeric(length(list_time)))
  output$times <- factor(list_time)
  
  x0 <- as.POSIXct("2010-12-01 00:00:00", tz = "GMT")
  x1 <- as.POSIXct("2010-12-01 01:00:00", tz = "GMT")
  x2 <- as.POSIXct("2010-12-01 02:00:00", tz = "GMT")
  x3 <- as.POSIXct("2010-12-01 03:00:00", tz = "GMT")
  x4 <- as.POSIXct("2010-12-01 04:00:00", tz = "GMT")
  x5 <- as.POSIXct("2010-12-01 05:00:00", tz = "GMT")
  x6 <- as.POSIXct("2010-12-01 06:00:00", tz = "GMT")
  x7 <- as.POSIXct("2010-12-01 07:00:00", tz = "GMT")
  x8 <- as.POSIXct("2010-12-01 08:00:00", tz = "GMT")
  x9 <- as.POSIXct("2010-12-01 09:00:00", tz = "GMT")
  x10 <- as.POSIXct("2010-12-01 10:00:00", tz = "GMT")
  x11 <- as.POSIXct("2010-12-01 11:00:00", tz = "GMT")
  x12 <- as.POSIXct("2010-12-01 12:00:00", tz = "GMT")
  x13 <- as.POSIXct("2010-12-01 13:00:00", tz = "GMT")
  x14 <- as.POSIXct("2010-12-01 14:00:00", tz = "GMT")
  x15 <- as.POSIXct("2010-12-01 15:00:00", tz = "GMT")
  x16 <- as.POSIXct("2010-12-01 16:00:00", tz = "GMT")
  x17 <- as.POSIXct("2010-12-01 17:00:00", tz = "GMT")
  x18 <- as.POSIXct("2010-12-01 18:00:00", tz = "GMT")
  x19 <- as.POSIXct("2010-12-01 19:00:00", tz = "GMT")
  x20 <- as.POSIXct("2010-12-01 20:00:00", tz = "GMT")
  x21 <- as.POSIXct("2010-12-01 21:00:00", tz = "GMT")
  x22 <- as.POSIXct("2010-12-01 22:00:00", tz = "GMT")
  x23 <- as.POSIXct("2010-12-01 23:00:00", tz = "GMT")
  
  for(i in 1:length(list_time)) { 
      print(i)
      y <- as.POSIXct(paste("2010-12-01 ",as.character(list_time[i])), tz = "GMT")
      print(y)
      print(as.character(list_time[i]))
      
      if(y > x0 & y < x1) {
        hour <- 1
      } else if(y > x1 & y < x2) {
        hour <- 2
      } else if(y > x2 & y < x3) {
        hour <- 3
      } else if(y > x3 & y < x4) {
        hour <- 4
      } else if(y > x4 & y < x5) {
        hour <- 5
      } else if(y > x5 & y < x6) {
        hour <- 6
      } else if(y > x6 & y < x7) {
        hour <- 7
      } else if(y > x7 & y < x8) {
        hour <- 8
      } else if(y > x8 & y < x9) {
        hour <- 9
      } else if(y > x9 & y < x10) {
        hour <- 10
      } else if(y > x10 & y < x11) {
        hour <- 11
      } else if(y > x11 & y < x12) {
        hour <- 12
      } else if(y > x12 & y < x13) {
        hour <- 13
      } else if(y > x13 & y < x14) {
        hour <- 14
      } else if(y > x14 & y < x15) {
        hour <- 15
      } else if(y > x15 & y < x16) {
        hour <- 16
      } else if(y > x16 & y < x17) {
        hour <- 17
      } else if(y > x17 & y < x18) {
        hour <- 18
      } else if(y > x18 & y < x19) {
        hour <- 19
      } else if(y > x19 & y < x20) {
        hour <- 20
      } else if(y > x20 & y < x21) {
        hour <- 21
      } else if(y > x21 & y < x22) {
        hour <- 22
      } else if(y > x22 & y < x23) {
        hour <- 23
      } else if(y > x23 & y < x0) {
        hour <- 24
      } else {
        print("error!")
      }
      
      output$index[i] <- i
      output$times[i] <- list_time[i]
      output$hour[i] <- hour
     } # check in which hour the connection is made
  return(output)
}
time_df <- detect_hour(list_time)
time_df <- cbind(time_df, cell=list$cell_id)

v_location <- rep(0, 300) # 300 = length(rec_towers)
v_time <- list(list1=v_location, list2=v_location, list3=v_location, list4=v_location, 
               list5=v_location, list6=v_location, list7=v_location, list8=v_location, 
               list9=v_location, list10=v_location, list11=v_location, list12=v_location, 
               list13=v_location, list14=v_location, list15=v_location, list16=v_location, 
               list17=v_location, list18=v_location, list19=v_location, list20=v_location, 
               list21=v_location, list22=v_location, list23=v_location, list24=v_location)

time_freq <- ddply(time_df, c("hour", "cell"), summarize, freq=length(index))

for(i in time_freq$hour) {
  y <- paste("list", i, sep = "")
  v_time[y] <- time_freq$freq
}
v_time



# fake matrix creation
user1 <- c(1,1,1,0,2,0,0,1,0)
user2 <- c(1,0,1,0,1,0,0,1,0)
matrix1 <- matrix(data = user1, nrow = 3, ncol = 3)
matrix2 <- matrix(data = user2, nrow = 3, ncol = 3)

# concatenate the matrices into vectors
vector1 <- c(matrix1)
vector2 <- c(matrix2)

library(lsa) # package for cosine similarity
cosine(vector1,vector2) # calculate cosine similarity between two vectors

## Close the connection
mongo.disconnect(mongo)
mongo.destroy(mongo)
