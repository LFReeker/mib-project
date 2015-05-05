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



cosine_similarity <- function(vector_user1, vector_user2) {
  cosine <- cosine(vector_user1,vector_user2) # calculate cosine similarity between two vectors
  #return(as.factor(cosine))
  return(cosine)
}

