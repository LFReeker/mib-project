##################################################################
#                                                                #
# PRE-PROCESS THE RAW CSV DATA                                   #
# REMOVE THE RECORDS FOR WHICH CELL TOWER INTO IS NOT AVAILABLE  #
# USE data.table & fread TO LOAD THE HUGE DATA IN EFFICIENT WAY  #
#                                                                #
##################################################################

##Load the libraries
library("data.table")


## Load the RAW csv exported from mongo server
# Set the directory where the raw file is present
#setwd("C:/Users/Praveen/Study Materials/Spring-15/Mobile Intelligence/Project/New")

## Use fread function to read bulk data 
singleDay.data <- fread("march6.csv",sep=',',header=T, colClasses=c(caller_id="character",callee_id="character",imei="character"),na.strings = c("NA", ""))  

## Remove the records which has NULL/NA values
singleDay.data <- na.omit(singleDay.data) ##But there is no null, the string is empty


#returns string w/o leading or trailing whitespace
#trim <- function (x) gsub("^\\s+|\\s+$", "", x)
#To use one of these functions on myDummy$country:
#myDummy$country <- trim(myDummy$country)


######### CELL ID INFO #################

cell.towers <- read.csv(file="cell_coord.csv", stringsAsFactors=FALSE)
cell.names  <- cell.towers$Cell # retrieve the cell names
## Take only the last 4 letters for the cell names
cell.names.short <- vector()
for(i in 1:length(cell.names)) {
  cell.names.short[i] <- substr(cell.names[i], start=4, stop=nchar(cell.names[i]))
}
## Convert from hexadecimal into decimal format
cell.names.dec <- strtoi(cell.names.short, 16L)


## Write to a file
#write.csv(cell.loc, file = "cell_loc.csv")
cell.loc.csv <- read.csv(file="cell_loc.csv", stringsAsFactors=FALSE)

cell.loc <- as.character(cell.loc.csv[,1])
## Convert from hexadecimal into decimal
cell.loc.dec <- strtoi(cell.loc, 16L)


## Create a mapping from each cell_id to the corresponding cell.towers table's row index
cell_id.coord.rowIndex <- new.env() # this is how a hash table is declared in R
## Create a data frame to store and visualize the locations
cell.loc.data <- data.frame(stringsAsFactors=FALSE)
counter <- 0 # this counter keeps track of the number of matches
for(i in 1:length(cell.loc)) { # iterate through each retrieved cell location
  if(cell.loc.dec[i] %in% cell.names.dec) { # check if there is a match
    rowIndices <- which(cell.names.dec == cell.loc.dec[i]) # find the matched index
    if(length(rowIndices) == 1) { # if there is one unique match
      rowIndex <- rowIndices
    }
    else { # otherwise, matched with more than one row
      ## Take the first one that's matched
      rowIndex <- rowIndices[1]
    }
    
    ## Get the long and lat coordinates
    longitude <- as.numeric(cell.towers$Longitude[rowIndex])
    latitude <- as.numeric(cell.towers$Latitude[rowIndex])
    if(!is.na(longitude) && !is.na(latitude)) { # some of the coords are corrupted
      cell_id.coord.rowIndex[[toString(cell.loc[i])]] <- rowIndex
      loc.row.data <- data.frame(cell=cell.loc[i], longitude=longitude, latitude=latitude)
      cell.loc.data <- rbind(cell.loc.data, loc.row.data)
      counter <- counter + 1
    }
  }
}
## How much percent is matched?
(round(counter / length(cell.loc) * 100, 2))


#cell_id <- "1451" # suppose we know this cell_id
#rowIndex <- cell_id.coord.rowIndex[[cell_id]]
#(longitude <- cell.towers$Longitude[rowIndex])
#(latitude <- cell.towers$Latitude[rowIndex])


#### SUMMARIZE DATA ACCORDING TO CELL ID's ###

#library(plyr)
#call.data <- read.csv("call_data_weekday_20080307_fri.csv",sep="," ,header=TRUE)
#call.data.wed <- read.csv("call_data_weekday_wednesday.csv",sep="," ,header=TRUE)
#call.data.sun <- read.csv("call_data_sunday_23414obs.csv",sep="," ,header=TRUE)



#sun.grouped.data <- ddply(call.data, "cell_id", summarise, count=length(cell_id), duration=sum(call_duration))

###############################################


#### FILTER THE DATA ONLY FOR CELL ID's THAT WE HAVE ####

cell_ids <- as.character(cell.loc.data$cell)

final<-subset(singleDay.data, subset = singleDay.data$cell_id %in% cell_ids)
#singleDay_filtered <- read.csv("March7_Filtered.csv",sep=",",header=T)
singleDay_filtered <- as.data.frame(final)
singleDay_filtered <- singleDay_filtered[,-1]


colnames(cell.loc.data) <- c("cell_id","longitude","latitude")
singleDay_merged_Lat_Lon <- merge(singleDay_filtered,cell.loc.data,by=c("cell_id"))
singleDay_merged_Lat_Lon <- singleDay_merged_Lat_Lon[,-2]
singleDay_merged_Lat_Lon$time <- as.character(singleDay_merged_Lat_Lon$time)
singleDay_merged_Lat_Lon$hr <- substr(singleDay_merged_Lat_Lon$time,1,2)
singleDay_merged_Lat_Lon$hr <- as.numeric(singleDay_merged_Lat_Lon$hr)
write.csv(singleDay_merged_Lat_Lon, file="March7_Merged.csv")

##############################################################################################
#                                                                                            #
#                             E N D    O F    P R O G R A M                                  #
#                                                                                            #
##############################################################################################
