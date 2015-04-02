### Authors: Subramaniam Balasubramaniam, William Liu, Praveen Kumar Thoranathula Radhesyam, Lars Reeker
### Course: 95789 Mobile intelligence and Business
### Institution: Carnegie Mellon Univeristy, Heinz College

### Many thanks and credits to Truc Viet Le for sharing his code

### Content:
###   Step 1.1  Connect to the server 
###   Step 1.2  Retrieve a sample of the raw data
###   Step 1.3  Retrieve all unique users and their connections
###   Step 1.4
###   Step 1.5  Retrieve the celltower locations


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


## Define the query
query <- mongo.bson.from.list(list('date'=20080301))
query

## Define the fields to be returned
fields <- mongo.bson.buffer.create()
# '1L' means we want to turn this field on, '0L' to turn it off
mongo.bson.buffer.append(fields, "cell_id", 1L)
mongo.bson.buffer.append(fields, "caller_id", 1L)
mongo.bson.buffer.append(fields, "callee_id", 1L)
mongo.bson.buffer.append(fields, "_id", 0L)
# Make object from buffer
fields <- mongo.bson.from.buffer(fields)

## Create the query cursor, limit to 100 rows only
cursor <- mongo.find(mongo, namespace, query=query, fields=fields, limit=100L)
# Iterate over the cursor
call.data <- data.frame(stringsAsFactors=FALSE)
while(mongo.cursor.next(cursor)) {
  ## Iterate and grab the next record
  value <- mongo.cursor.value(cursor)
  call <- mongo.bson.to.list(value)
  ## Make it a data frame
  call.df <- as.data.frame(t(unlist(call)), stringsAsFactors=FALSE)
  ## Bind to the master data frame
  call.data <- rbind.fill(call.data, call.df)
}
# Release the resources attached to cursor on both client and server
done <- mongo.cursor.destroy(cursor)

## Show the data in a dataframe
call.data

## Alternative: show the data in a list
calls <- mongo.find.all(mongo, namespace, query=query, limit=100L)


### Step 1.3
############

## Retrieve all unique users in the dataset





### Step 1.5
############

## Get the cell tower locations
loc <- mongo.distinct(mongo, namespace, "cell_id")
## Convert into vector
loc <- unlist(loc)
## Convert from hex into decimal
loc.dec <- strtoi(loc, 16L)

## Use the aggregation framework to find the distribution of all cell towers
pipe_1 <- mongo.bson.from.JSON(
  '{"$group":
    {"_id": "$cell_id", "count": {"$sum": 1}}
  }'
)
## Sort by frequency in descending order
pipe_2 <- mongo.bson.from.JSON(
  '{"$sort": {"count": -1}}'
)
pipeline <- list(pipe_1, pipe_2)
loc.distr <- mongo.aggregation(mongo, namespace, pipeline)

## Reshape the data to fit into an R data frame
lloc.distr <- mongo.bson.value(loc.distr, "result")
mloc.distr <- sapply(lloc.distr, function(x) return(c(toString(x["_id"]),
                                                      as.numeric(x["count"]))))
dloc.distr <- as.data.frame(t(mloc.distr))
colnames(dloc.distr) <- c("cell_id", "freq")
dloc.distr$freq <- as.numeric(dloc.distr$freq)

## Load code for markup
source("./code/util/fivethirtyeight_theme.R")

## Visualize the result: plot the top 10 locations
dloc.distr.top <- head(dloc.distr, 10)
(ggplot(dloc.distr.top, aes(cell_id, freq, fill=freq)) + guides(fill=FALSE) +
  geom_bar(stat = "identity", color = "white") + xlab("cell_id") + ylab("freq") +
   ggtitle("Top 10 Locations by Frequency"))

## Plot the histogram of the cell tower locations
(ggplot(dloc.distr, aes(freq)) + geom_histogram(binwidth=50, fill="#c0392b", alpha=0.75) +
  fivethirtyeight_theme() +
  labs(title="Distribution of Cell Tower Frequencies",
       x="Count", y="Frequency") + scale_x_continuous(labels=comma) +
  scale_y_continuous(labels=comma) + geom_hline(yintercept=0, size=0.4, color="black"))

## Close the connection
mongo.disconnect(mongo)
mongo.destroy(mongo)
