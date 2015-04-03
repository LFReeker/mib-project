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


## Step 1.2
###########

## Count the number of docs (rows) we have in a collection
mongo.count(mongo, namespace)

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
progress.bar$init(length(10000))
call.data <- data.frame(stringsAsFactors=FALSE)
for(i in 1:length(10000)) {  
  ## Define the query
  query <- mongo.bson.from.list(list('date'=20080302))
  ## Create the query cursor
  cursor <- mongo.find(mongo, namespace, query=query, fields=fields, limit=10000L)  
  ## Iterate over the cursor
  while(mongo.cursor.next(cursor)) {
    ## Iterate and grab the next record
    value <- mongo.cursor.value(cursor)
    call <- mongo.bson.to.list(value)
    ## Make it a data frame
    call.df <- as.data.frame(t(unlist(call)), stringsAsFactors=FALSE)
    ## Bind to the master data frame
    call.data <- rbind.fill(call.data, call.df)
  }
  progress.bar$step()
}

# Release the resources attached to cursor on both client and server
done <- mongo.cursor.destroy(cursor)

## Show the data in a dataframe
call.data

## Write dataset to csv file
write.csv(call.data, "call_data_weekend_23414obs.csv")

### Step 1.4
############

## Group the calls per user in the retrieved dataset
call.data.grouped <- ddply(call.data, "caller.id", summarize, freq=length(callee.id))
nrow(call.data.grouped)





## Close the connection
mongo.disconnect(mongo)
mongo.destroy(mongo)
