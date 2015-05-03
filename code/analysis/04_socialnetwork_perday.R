library(igraph)
library(plyr)
library(ggplot2)
library(TraMineR)
library(cluster)
library(ggmap)
library(reshape2)

######################################
#Graph
######################################
#March1
#Reading the file
march1 <- read.csv("march1_final.csv")

#Edge Table
march1_cc <- ddply(march1, c("caller_id", "callee_id"), summarize, len = length(callee_id))

#Create graph data frame
march1_gr<-graph.data.frame(march1_cc, directed=T)

#Global Clustering Coeffecient
march1_gcc <- transitivity(march1_gr, type="global")

#All degrees, indegrees, outdegrees
degrees <- degree(march1_gr, mode = "all")
degrees <- subset(degrees, degrees > 0)

indegrees <- degree(march1_gr, mode = "in")
indegrees <- subset(indegrees, indegrees > 0)

outdegrees <- degree(march1_gr, mode = "out")
outdegrees <- subset(outdegrees, outdegrees > 0)

march1_cumy <- c()
y <- tabulate(degrees)
x <- 1:length(y)
for(i in 1:length(x)) {
  march1_cumy[i] <- sum(y[i:length(x)]) / sum(y)
}
## For indegrees
march1_cumy.in <- c()
y.in <- tabulate(indegrees)
x.in <- 1:length(y.in)
for(i in 1:length(x.in)) {
  march1_cumy.in[i] <- sum(y.in[i:length(x.in)]) / sum(y.in)
}
## For outdegrees
march1_cumy.out <- c()
y.out <- tabulate(outdegrees)
x.out <- 1:length(y.out)
for(i in 1:length(x.out)) {
  march1_cumy.out[i] <- sum(y.out[i:length(x.out)]) / sum(y.out)
}

# Histogram and cumulative distribution function for degrees
hist(degrees, freq = FALSE, xlab = "Degree k", main = "Histogram of All Degrees", breaks = 50, col = "gray")

plot(x, march1_cumy, log = "xy", xlab = "Degree k", ylab = expression(Pr(x) >= k), cex = 0.5, main = "Degree Power-law Distribution")

# Histogram and cumulative distribution function for Indegrees
hist(indegrees, freq = FALSE, xlab = "Indegree k",
     main = "Histogram of Indegrees", breaks = 50, col = "gray")
plot(x.in, march1_cumy.in, log = "xy", xlab = "Indegree k",
     ylab = expression(Pr(x) >= k), cex = 0.5, main = "Indegree Power-law Distr")

# Histogram and cumulative distribution function for Outdegrees
hist(outdegrees, freq = FALSE, xlab = "Outdegree k",
     main = "Histogram of Outdegrees", breaks = 50, col = "gray")
plot(x.out, march1_cumy.out, log = "xy", xlab = "Outdegree k",
     ylab = expression(Pr(x) >= k), cex = 0.5, main = "Outdegree Power-law Distr")


#Betweenness centrality is an indicator of a node's centrality in a network. 
#It is equal to the number of shortest paths from all vertices to all others that pass 
#through that node. A node with high betweenness centrality has a large influence on the 
#transfer of items through the network, under the assumption that item transfer follows the 
#shortest paths.

march1_btw <- betweenness(march1_gr, directed = TRUE)
sorted.btw <- sort(march1_btw, decreasing = TRUE)

#PageRank works by counting the number and quality of edge to a node to determine 
#a rough estimate of how important the node is. The underlying assumption is 
#that more important nodes are likely to receive more edges from other nodes
pr <- page.rank(march1_gr, directed = TRUE)
sorted.pr <- sort(pr$vector, decreasing = TRUE)

#Hyperlink-Induced Topic Search (HITS; also known as hubs and authorities) is a 
#link analysis algorithm that rates Web pages, developed by Jon Kleinberg. 
#The idea behind Hubs and Authorities stemmed from a particular insight into 
#the creation of web pages when the Internet was originally forming; that is, 
#certain web pages, known as hubs, served as large directories that were not 
#actually authoritative in the information that it held, but were used as compilations 
#of a broad catalog of information that led users directly to other authoritative pages. 
#In other words, a good hub represented a page that pointed to many other pages, and a 
#good authority represented a page that was linked by many different hubs.
authority <- authority.score(march1_gr)$vector
sorted.authority <- sort(authority, decreasing = TRUE)


#subsetting the data for the graph
march1_cc_new1 <-subset(march1_cc, len > 5)
march1_cc_new1 <-subset(march1_cc_new1, len <= 8)
march1_gr<-graph.data.frame(march1_cc_new1, directed=T)

#setting weights for graph
E(march1_gr)$weight <- march1_cc_new1$len

#Community  detection
march1_fc <- fastgreedy.community(as.undirected(march1_gr))
march1_fc.colors <- sample(colors(), length(march1_fc))
for(i in 1:length(march1_fc.colors)) {
  V(march1_gr)[membership(march1_fc)==i]$color <- march1_fc.colors[i]
}

@setting weight for edges
max.weight <- max(E(march1_gr)$weight)
weight.factor <- max.weight / 5
edgeWeight.theta =6
if(edgeWeight.theta == 0) {
  summ.edgeWeight <- summary(E(march1_gr)$weight)
  edgeWeight.theta <- exp(1) * summ.edgeWeight[3] # the median
}
E(march1_gr)$color <- ifelse(E(march1_gr)$weight >= edgeWeight.theta, 'darkorange', 'azure4')
E(march1_gr)$width <- E(march1_gr)$weight / weight.factor
V(march1_gr)$size <- degree(march1_gr, mode="out") / 8
E(march1_gr)$weight <- march1_cc_new1$len

#2d graph
march1_plot <- plot.igraph(march1_gr, vertex.label=NA, layout=layout.fruchterman.reingold, vertex.color = V(march1_gr)$color, vertex.size=2, edge.arrow.size=0.2, edge.color="black")

#interactive graph
tkplot(march1_gr, vertex.label=NA, vertex.color = V(march1_gr)$color, layout=layout.fruchterman.reingold, vertex.size=2, edge.arrow.size=0.2, edge.color="black")

#3d graph
rglplot(march1_gr, vertex.label=NA, vertex.color = V(march1_gr)$color, layout=layout.fruchterman.reingold, vertex.size=2, edge.arrow.size=0.2, edge.color="black")

pdf('march1_graph.pdf')

########################################################################################
#March2
#Reading the file
march2 <- read.csv("march2_final.csv")

#Edge Table
march2_cc <- ddply(march2, c("caller_id", "callee_id"), summarize, len = length(callee_id))

#Create graph data frame
march2_gr<-graph.data.frame(march2_cc, directed=T)

#Global Clustering Coeffecient
march2_gcc <- transitivity(march2_gr, type="global")

#All degrees, indegrees, outdegrees
degrees <- degree(march2_gr, mode = "all")
degrees <- subset(degrees, degrees > 0)

indegrees <- degree(march2_gr, mode = "in")
indegrees <- subset(indegrees, indegrees > 0)

outdegrees <- degree(march2_gr, mode = "out")
outdegrees <- subset(outdegrees, outdegrees > 0)

march2_cumy <- c()
y <- tabulate(degrees)
x <- 1:length(y)
for(i in 1:length(x)) {
  march2_cumy[i] <- sum(y[i:length(x)]) / sum(y)
}
## For indegrees
march2_cumy.in <- c()
y.in <- tabulate(indegrees)
x.in <- 1:length(y.in)
for(i in 1:length(x.in)) {
  march2_cumy.in[i] <- sum(y.in[i:length(x.in)]) / sum(y.in)
}
## For outdegrees
march2_cumy.out <- c()
y.out <- tabulate(outdegrees)
x.out <- 1:length(y.out)
for(i in 1:length(x.out)) {
  march2_cumy.out[i] <- sum(y.out[i:length(x.out)]) / sum(y.out)
}

# Histogram and cumulative distribution function for degrees
hist(degrees, freq = FALSE, xlab = "Degree k", main = "Histogram of All Degrees", breaks = 50, col = "gray")

plot(x, march2_cumy, log = "xy", xlab = "Degree k", ylab = expression(Pr(x) >= k), cex = 0.5, main = "Degree Power-law Distribution")

# Histogram and cumulative distribution function for Indegrees
hist(indegrees, freq = FALSE, xlab = "Indegree k",
     main = "Histogram of Indegrees", breaks = 50, col = "gray")
plot(x.in, march2_cumy.in, log = "xy", xlab = "Indegree k",
     ylab = expression(Pr(x) >= k), cex = 0.5, main = "Indegree Power-law Distr")

# Histogram and cumulative distribution function for Outdegrees
hist(outdegrees, freq = FALSE, xlab = "Outdegree k",
     main = "Histogram of Outdegrees", breaks = 50, col = "gray")
plot(x.out, march2_cumy.out, log = "xy", xlab = "Outdegree k",
     ylab = expression(Pr(x) >= k), cex = 0.5, main = "Outdegree Power-law Distr")


#Betweenness centrality is an indicator of a node's centrality in a network. 
#It is equal to the number of shortest paths from all vertices to all others that pass 
#through that node. A node with high betweenness centrality has a large influence on the 
#transfer of items through the network, under the assumption that item transfer follows the 
#shortest paths.

march2_btw <- betweenness(march2_gr, directed = TRUE)
sorted.btw <- sort(march2_btw, decreasing = TRUE)

#PageRank works by counting the number and quality of edge to a node to determine 
#a rough estimate of how important the node is. The underlying assumption is 
#that more important nodes are likely to receive more edges from other nodes
pr <- page.rank(march2_gr, directed = TRUE)
sorted.pr <- sort(pr$vector, decreasing = TRUE)

#Hyperlink-Induced Topic Search (HITS; also known as hubs and authorities) is a 
#link analysis algorithm that rates Web pages, developed by Jon Kleinberg. 
#The idea behind Hubs and Authorities stemmed from a particular insight into 
#the creation of web pages when the Internet was originally forming; that is, 
#certain web pages, known as hubs, served as large directories that were not 
#actually authoritative in the information that it held, but were used as compilations 
#of a broad catalog of information that led users directly to other authoritative pages. 
#In other words, a good hub represented a page that pointed to many other pages, and a 
#good authority represented a page that was linked by many different hubs.
authority <- authority.score(march2_gr)$vector
sorted.authority <- sort(authority, decreasing = TRUE)


#subsetting the data for the graph
march2_cc_new1 <-subset(march2_cc, len > 5)
march2_cc_new1 <-subset(march2_cc_new1, len <= 8)
march2_gr<-graph.data.frame(march2_cc_new1, directed=T)

#setting weights for graph
E(march2_gr)$weight <- march2_cc_new1$len

#Community  detection
march2_fc <- fastgreedy.community(as.undirected(march2_gr))
march2_fc.colors <- sample(colors(), length(march2_fc))
for(i in 1:length(march2_fc.colors)) {
  V(march2_gr)[membership(march2_fc)==i]$color <- march2_fc.colors[i]
}

@setting weight for edges
max.weight <- max(E(march2_gr)$weight)
weight.factor <- max.weight / 5
edgeWeight.theta =6
if(edgeWeight.theta == 0) {
  summ.edgeWeight <- summary(E(march2_gr)$weight)
  edgeWeight.theta <- exp(1) * summ.edgeWeight[3] # the median
}
E(march2_gr)$color <- ifelse(E(march2_gr)$weight >= edgeWeight.theta, 'darkorange', 'azure4')
E(march2_gr)$width <- E(march2_gr)$weight / weight.factor
V(march2_gr)$size <- degree(march2_gr, mode="out") / 8
E(march2_gr)$weight <- march2_cc_new1$len

#2d graph
march2_plot <- plot.igraph(march2_gr, vertex.label=NA, layout=layout.fruchterman.reingold, vertex.color = V(march2_gr)$color, vertex.size=2, edge.arrow.size=0.2, edge.color="black")

#interactive graph
tkplot(march2_gr, vertex.label=NA, vertex.color = V(march2_gr)$color, layout=layout.fruchterman.reingold, vertex.size=2, edge.arrow.size=0.2, edge.color="black")

#3d graph
rglplot(march2_gr, vertex.label=NA, vertex.color = V(march2_gr)$color, layout=layout.fruchterman.reingold, vertex.size=2, edge.arrow.size=0.2, edge.color="black")

pdf('march2_graph.pdf')

######################################################
#March3
#Reading the file
march3 <- read.csv("march3_final.csv")

#Edge Table
march3_cc <- ddply(march3, c("caller_id", "callee_id"), summarize, len = length(callee_id))

#Create graph data frame
march3_gr<-graph.data.frame(march3_cc, directed=T)

#Global Clustering Coeffecient
march3_gcc <- transitivity(march3_gr, type="global")

#All degrees, indegrees, outdegrees
degrees <- degree(march3_gr, mode = "all")
degrees <- subset(degrees, degrees > 0)

indegrees <- degree(march3_gr, mode = "in")
indegrees <- subset(indegrees, indegrees > 0)

outdegrees <- degree(march3_gr, mode = "out")
outdegrees <- subset(outdegrees, outdegrees > 0)

march3_cumy <- c()
y <- tabulate(degrees)
x <- 1:length(y)
for(i in 1:length(x)) {
  march3_cumy[i] <- sum(y[i:length(x)]) / sum(y)
}
## For indegrees
march3_cumy.in <- c()
y.in <- tabulate(indegrees)
x.in <- 1:length(y.in)
for(i in 1:length(x.in)) {
  march3_cumy.in[i] <- sum(y.in[i:length(x.in)]) / sum(y.in)
}
## For outdegrees
march3_cumy.out <- c()
y.out <- tabulate(outdegrees)
x.out <- 1:length(y.out)
for(i in 1:length(x.out)) {
  march3_cumy.out[i] <- sum(y.out[i:length(x.out)]) / sum(y.out)
}

# Histogram and cumulative distribution function for degrees
hist(degrees, freq = FALSE, xlab = "Degree k", main = "Histogram of All Degrees", breaks = 50, col = "gray")

plot(x, march3_cumy, log = "xy", xlab = "Degree k", ylab = expression(Pr(x) >= k), cex = 0.5, main = "Degree Power-law Distribution")

# Histogram and cumulative distribution function for Indegrees
hist(indegrees, freq = FALSE, xlab = "Indegree k",
     main = "Histogram of Indegrees", breaks = 50, col = "gray")
plot(x.in, march3_cumy.in, log = "xy", xlab = "Indegree k",
     ylab = expression(Pr(x) >= k), cex = 0.5, main = "Indegree Power-law Distr")

# Histogram and cumulative distribution function for Outdegrees
hist(outdegrees, freq = FALSE, xlab = "Outdegree k",
     main = "Histogram of Outdegrees", breaks = 50, col = "gray")
plot(x.out, march3_cumy.out, log = "xy", xlab = "Outdegree k",
     ylab = expression(Pr(x) >= k), cex = 0.5, main = "Outdegree Power-law Distr")


#Betweenness centrality is an indicator of a node's centrality in a network. 
#It is equal to the number of shortest paths from all vertices to all others that pass 
#through that node. A node with high betweenness centrality has a large influence on the 
#transfer of items through the network, under the assumption that item transfer follows the 
#shortest paths.

march3_btw <- betweenness(march3_gr, directed = TRUE)
sorted.btw <- sort(march3_btw, decreasing = TRUE)

#PageRank works by counting the number and quality of edge to a node to determine 
#a rough estimate of how important the node is. The underlying assumption is 
#that more important nodes are likely to receive more edges from other nodes
pr <- page.rank(march3_gr, directed = TRUE)
sorted.pr <- sort(pr$vector, decreasing = TRUE)

#Hyperlink-Induced Topic Search (HITS; also known as hubs and authorities) is a 
#link analysis algorithm that rates Web pages, developed by Jon Kleinberg. 
#The idea behind Hubs and Authorities stemmed from a particular insight into 
#the creation of web pages when the Internet was originally forming; that is, 
#certain web pages, known as hubs, served as large directories that were not 
#actually authoritative in the information that it held, but were used as compilations 
#of a broad catalog of information that led users directly to other authoritative pages. 
#In other words, a good hub represented a page that pointed to many other pages, and a 
#good authority represented a page that was linked by many different hubs.
authority <- authority.score(march3_gr)$vector
sorted.authority <- sort(authority, decreasing = TRUE)


#subsetting the data for the graph
march3_cc_new1 <-subset(march3_cc, len > 5)
march3_cc_new1 <-subset(march3_cc_new1, len <= 8)
march3_gr<-graph.data.frame(march3_cc_new1, directed=T)

#setting weights for graph
E(march3_gr)$weight <- march3_cc_new1$len

#Community  detection
march3_fc <- fastgreedy.community(as.undirected(march3_gr))
march3_fc.colors <- sample(colors(), length(march3_fc))
for(i in 1:length(march3_fc.colors)) {
  V(march3_gr)[membership(march3_fc)==i]$color <- march3_fc.colors[i]
}

@setting weight for edges
max.weight <- max(E(march3_gr)$weight)
weight.factor <- max.weight / 5
edgeWeight.theta =6
if(edgeWeight.theta == 0) {
  summ.edgeWeight <- summary(E(march3_gr)$weight)
  edgeWeight.theta <- exp(1) * summ.edgeWeight[3] # the median
}
E(march3_gr)$color <- ifelse(E(march3_gr)$weight >= edgeWeight.theta, 'darkorange', 'azure4')
E(march3_gr)$width <- E(march3_gr)$weight / weight.factor
V(march3_gr)$size <- degree(march3_gr, mode="out") / 8
E(march3_gr)$weight <- march3_cc_new1$len

#2d graph
march3_plot <- plot.igraph(march3_gr, vertex.label=NA, layout=layout.fruchterman.reingold, vertex.color = V(march3_gr)$color, vertex.size=2, edge.arrow.size=0.2, edge.color="black")

#interactive graph
tkplot(march3_gr, vertex.label=NA, vertex.color = V(march3_gr)$color, layout=layout.fruchterman.reingold, vertex.size=2, edge.arrow.size=0.2, edge.color="black")

#3d graph
rglplot(march3_gr, vertex.label=NA, vertex.color = V(march3_gr)$color, layout=layout.fruchterman.reingold, vertex.size=2, edge.arrow.size=0.2, edge.color="black")

pdf('march3_graph.pdf')

#########################################################
#March4
#Reading the file
march4 <- read.csv("march4_final.csv")

#Edge Table
march4_cc <- ddply(march4, c("caller_id", "callee_id"), summarize, len = length(callee_id))

#Create graph data frame
march4_gr<-graph.data.frame(march4_cc, directed=T)

#Global Clustering Coeffecient
march4_gcc <- transitivity(march4_gr, type="global")

#All degrees, indegrees, outdegrees
degrees <- degree(march4_gr, mode = "all")
degrees <- subset(degrees, degrees > 0)

indegrees <- degree(march4_gr, mode = "in")
indegrees <- subset(indegrees, indegrees > 0)

outdegrees <- degree(march4_gr, mode = "out")
outdegrees <- subset(outdegrees, outdegrees > 0)

march4_cumy <- c()
y <- tabulate(degrees)
x <- 1:length(y)
for(i in 1:length(x)) {
  march4_cumy[i] <- sum(y[i:length(x)]) / sum(y)
}
## For indegrees
march4_cumy.in <- c()
y.in <- tabulate(indegrees)
x.in <- 1:length(y.in)
for(i in 1:length(x.in)) {
  march4_cumy.in[i] <- sum(y.in[i:length(x.in)]) / sum(y.in)
}
## For outdegrees
march4_cumy.out <- c()
y.out <- tabulate(outdegrees)
x.out <- 1:length(y.out)
for(i in 1:length(x.out)) {
  march4_cumy.out[i] <- sum(y.out[i:length(x.out)]) / sum(y.out)
}

# Histogram and cumulative distribution function for degrees
hist(degrees, freq = FALSE, xlab = "Degree k", main = "Histogram of All Degrees", breaks = 50, col = "gray")

plot(x, march4_cumy, log = "xy", xlab = "Degree k", ylab = expression(Pr(x) >= k), cex = 0.5, main = "Degree Power-law Distribution")

# Histogram and cumulative distribution function for Indegrees
hist(indegrees, freq = FALSE, xlab = "Indegree k",
     main = "Histogram of Indegrees", breaks = 50, col = "gray")
plot(x.in, march4_cumy.in, log = "xy", xlab = "Indegree k",
     ylab = expression(Pr(x) >= k), cex = 0.5, main = "Indegree Power-law Distr")

# Histogram and cumulative distribution function for Outdegrees
hist(outdegrees, freq = FALSE, xlab = "Outdegree k",
     main = "Histogram of Outdegrees", breaks = 50, col = "gray")
plot(x.out, march4_cumy.out, log = "xy", xlab = "Outdegree k",
     ylab = expression(Pr(x) >= k), cex = 0.5, main = "Outdegree Power-law Distr")


#Betweenness centrality is an indicator of a node's centrality in a network. 
#It is equal to the number of shortest paths from all vertices to all others that pass 
#through that node. A node with high betweenness centrality has a large influence on the 
#transfer of items through the network, under the assumption that item transfer follows the 
#shortest paths.

march4_btw <- betweenness(march4_gr, directed = TRUE)
sorted.btw <- sort(march4_btw, decreasing = TRUE)

#PageRank works by counting the number and quality of edge to a node to determine 
#a rough estimate of how important the node is. The underlying assumption is 
#that more important nodes are likely to receive more edges from other nodes
pr <- page.rank(march4_gr, directed = TRUE)
sorted.pr <- sort(pr$vector, decreasing = TRUE)

#Hyperlink-Induced Topic Search (HITS; also known as hubs and authorities) is a 
#link analysis algorithm that rates Web pages, developed by Jon Kleinberg. 
#The idea behind Hubs and Authorities stemmed from a particular insight into 
#the creation of web pages when the Internet was originally forming; that is, 
#certain web pages, known as hubs, served as large directories that were not 
#actually authoritative in the information that it held, but were used as compilations 
#of a broad catalog of information that led users directly to other authoritative pages. 
#In other words, a good hub represented a page that pointed to many other pages, and a 
#good authority represented a page that was linked by many different hubs.
authority <- authority.score(march4_gr)$vector
sorted.authority <- sort(authority, decreasing = TRUE)


#subsetting the data for the graph
march4_cc_new1 <-subset(march4_cc, len > 5)
march4_cc_new1 <-subset(march4_cc_new1, len <= 8)
march4_gr<-graph.data.frame(march4_cc_new1, directed=T)

#setting weights for graph
E(march4_gr)$weight <- march4_cc_new1$len

#Community  detection
march4_fc <- fastgreedy.community(as.undirected(march4_gr))
march4_fc.colors <- sample(colors(), length(march4_fc))
for(i in 1:length(march4_fc.colors)) {
  V(march4_gr)[membership(march4_fc)==i]$color <- march4_fc.colors[i]
}

@setting weight for edges
max.weight <- max(E(march4_gr)$weight)
weight.factor <- max.weight / 5
edgeWeight.theta =6
if(edgeWeight.theta == 0) {
  summ.edgeWeight <- summary(E(march4_gr)$weight)
  edgeWeight.theta <- exp(1) * summ.edgeWeight[3] # the median
}
E(march4_gr)$color <- ifelse(E(march4_gr)$weight >= edgeWeight.theta, 'darkorange', 'azure4')
E(march4_gr)$width <- E(march4_gr)$weight / weight.factor
V(march4_gr)$size <- degree(march4_gr, mode="out") / 8
E(march4_gr)$weight <- march4_cc_new1$len

#2d graph
march4_plot <- plot.igraph(march4_gr, vertex.label=NA, layout=layout.fruchterman.reingold, vertex.color = V(march4_gr)$color, vertex.size=2, edge.arrow.size=0.2, edge.color="black")

#interactive graph
tkplot(march4_gr, vertex.label=NA, vertex.color = V(march4_gr)$color, layout=layout.fruchterman.reingold, vertex.size=2, edge.arrow.size=0.2, edge.color="black")

#3d graph
rglplot(march4_gr, vertex.label=NA, vertex.color = V(march4_gr)$color, layout=layout.fruchterman.reingold, vertex.size=2, edge.arrow.size=0.2, edge.color="black")

pdf('march4_graph.pdf')


################################################
#March5
#Reading the file
march5 <- read.csv("march5_final.csv")

#Edge Table
march5_cc <- ddply(march5, c("caller_id", "callee_id"), summarize, len = length(callee_id))

#Create graph data frame
march5_gr<-graph.data.frame(march5_cc, directed=T)

#Global Clustering Coeffecient
march5_gcc <- transitivity(march5_gr, type="global")

#All degrees, indegrees, outdegrees
degrees <- degree(march5_gr, mode = "all")
degrees <- subset(degrees, degrees > 0)

indegrees <- degree(march5_gr, mode = "in")
indegrees <- subset(indegrees, indegrees > 0)

outdegrees <- degree(march5_gr, mode = "out")
outdegrees <- subset(outdegrees, outdegrees > 0)

march5_cumy <- c()
y <- tabulate(degrees)
x <- 1:length(y)
for(i in 1:length(x)) {
  march5_cumy[i] <- sum(y[i:length(x)]) / sum(y)
}
## For indegrees
march5_cumy.in <- c()
y.in <- tabulate(indegrees)
x.in <- 1:length(y.in)
for(i in 1:length(x.in)) {
  march5_cumy.in[i] <- sum(y.in[i:length(x.in)]) / sum(y.in)
}
## For outdegrees
march5_cumy.out <- c()
y.out <- tabulate(outdegrees)
x.out <- 1:length(y.out)
for(i in 1:length(x.out)) {
  march5_cumy.out[i] <- sum(y.out[i:length(x.out)]) / sum(y.out)
}

# Histogram and cumulative distribution function for degrees
hist(degrees, freq = FALSE, xlab = "Degree k", main = "Histogram of All Degrees", breaks = 50, col = "gray")

plot(x, march5_cumy, log = "xy", xlab = "Degree k", ylab = expression(Pr(x) >= k), cex = 0.5, main = "Degree Power-law Distribution")

# Histogram and cumulative distribution function for Indegrees
hist(indegrees, freq = FALSE, xlab = "Indegree k",
     main = "Histogram of Indegrees", breaks = 50, col = "gray")
plot(x.in, march5_cumy.in, log = "xy", xlab = "Indegree k",
     ylab = expression(Pr(x) >= k), cex = 0.5, main = "Indegree Power-law Distr")

# Histogram and cumulative distribution function for Outdegrees
hist(outdegrees, freq = FALSE, xlab = "Outdegree k",
     main = "Histogram of Outdegrees", breaks = 50, col = "gray")
plot(x.out, march5_cumy.out, log = "xy", xlab = "Outdegree k",
     ylab = expression(Pr(x) >= k), cex = 0.5, main = "Outdegree Power-law Distr")


#Betweenness centrality is an indicator of a node's centrality in a network. 
#It is equal to the number of shortest paths from all vertices to all others that pass 
#through that node. A node with high betweenness centrality has a large influence on the 
#transfer of items through the network, under the assumption that item transfer follows the 
#shortest paths.

march5_btw <- betweenness(march5_gr, directed = TRUE)
sorted.btw <- sort(march5_btw, decreasing = TRUE)

#PageRank works by counting the number and quality of edge to a node to determine 
#a rough estimate of how important the node is. The underlying assumption is 
#that more important nodes are likely to receive more edges from other nodes
pr <- page.rank(march5_gr, directed = TRUE)
sorted.pr <- sort(pr$vector, decreasing = TRUE)

#Hyperlink-Induced Topic Search (HITS; also known as hubs and authorities) is a 
#link analysis algorithm that rates Web pages, developed by Jon Kleinberg. 
#The idea behind Hubs and Authorities stemmed from a particular insight into 
#the creation of web pages when the Internet was originally forming; that is, 
#certain web pages, known as hubs, served as large directories that were not 
#actually authoritative in the information that it held, but were used as compilations 
#of a broad catalog of information that led users directly to other authoritative pages. 
#In other words, a good hub represented a page that pointed to many other pages, and a 
#good authority represented a page that was linked by many different hubs.
authority <- authority.score(march5_gr)$vector
sorted.authority <- sort(authority, decreasing = TRUE)


#subsetting the data for the graph
march5_cc_new1 <-subset(march5_cc, len > 5)
march5_cc_new1 <-subset(march5_cc_new1, len <= 8)
march5_gr<-graph.data.frame(march5_cc_new1, directed=T)

#setting weights for graph
E(march5_gr)$weight <- march5_cc_new1$len

#Community  detection
march5_fc <- fastgreedy.community(as.undirected(march5_gr))
march5_fc.colors <- sample(colors(), length(march5_fc))
for(i in 1:length(march5_fc.colors)) {
  V(march5_gr)[membership(march5_fc)==i]$color <- march5_fc.colors[i]
}

@setting weight for edges
max.weight <- max(E(march5_gr)$weight)
weight.factor <- max.weight / 5
edgeWeight.theta =6
if(edgeWeight.theta == 0) {
  summ.edgeWeight <- summary(E(march5_gr)$weight)
  edgeWeight.theta <- exp(1) * summ.edgeWeight[3] # the median
}
E(march5_gr)$color <- ifelse(E(march5_gr)$weight >= edgeWeight.theta, 'darkorange', 'azure4')
E(march5_gr)$width <- E(march5_gr)$weight / weight.factor
V(march5_gr)$size <- degree(march5_gr, mode="out") / 8
E(march5_gr)$weight <- march5_cc_new1$len

#2d graph
march5_plot <- plot.igraph(march5_gr, vertex.label=NA, layout=layout.fruchterman.reingold, vertex.color = V(march5_gr)$color, vertex.size=2, edge.arrow.size=0.2, edge.color="black")

#interactive graph
tkplot(march5_gr, vertex.label=NA, vertex.color = V(march5_gr)$color, layout=layout.fruchterman.reingold, vertex.size=2, edge.arrow.size=0.2, edge.color="black")

#3d graph
rglplot(march5_gr, vertex.label=NA, vertex.color = V(march5_gr)$color, layout=layout.fruchterman.reingold, vertex.size=2, edge.arrow.size=0.2, edge.color="black")

pdf('march5_graph.pdf')


############################################
#March6
#Reading the file
march6 <- read.csv("march6_final.csv")

#Edge Table
march6_cc <- ddply(march6, c("caller_id", "callee_id"), summarize, len = length(callee_id))

#Create graph data frame
march6_gr<-graph.data.frame(march6_cc, directed=T)

#Global Clustering Coeffecient
march6_gcc <- transitivity(march6_gr, type="global")

#All degrees, indegrees, outdegrees
degrees <- degree(march6_gr, mode = "all")
degrees <- subset(degrees, degrees > 0)

indegrees <- degree(march6_gr, mode = "in")
indegrees <- subset(indegrees, indegrees > 0)

outdegrees <- degree(march6_gr, mode = "out")
outdegrees <- subset(outdegrees, outdegrees > 0)

march6_cumy <- c()
y <- tabulate(degrees)
x <- 1:length(y)
for(i in 1:length(x)) {
  march6_cumy[i] <- sum(y[i:length(x)]) / sum(y)
}
## For indegrees
march6_cumy.in <- c()
y.in <- tabulate(indegrees)
x.in <- 1:length(y.in)
for(i in 1:length(x.in)) {
  march6_cumy.in[i] <- sum(y.in[i:length(x.in)]) / sum(y.in)
}
## For outdegrees
march6_cumy.out <- c()
y.out <- tabulate(outdegrees)
x.out <- 1:length(y.out)
for(i in 1:length(x.out)) {
  march6_cumy.out[i] <- sum(y.out[i:length(x.out)]) / sum(y.out)
}

# Histogram and cumulative distribution function for degrees
hist(degrees, freq = FALSE, xlab = "Degree k", main = "Histogram of All Degrees", breaks = 50, col = "gray")

plot(x, march6_cumy, log = "xy", xlab = "Degree k", ylab = expression(Pr(x) >= k), cex = 0.5, main = "Degree Power-law Distribution")

# Histogram and cumulative distribution function for Indegrees
hist(indegrees, freq = FALSE, xlab = "Indegree k",
     main = "Histogram of Indegrees", breaks = 50, col = "gray")
plot(x.in, march6_cumy.in, log = "xy", xlab = "Indegree k",
     ylab = expression(Pr(x) >= k), cex = 0.5, main = "Indegree Power-law Distr")

# Histogram and cumulative distribution function for Outdegrees
hist(outdegrees, freq = FALSE, xlab = "Outdegree k",
     main = "Histogram of Outdegrees", breaks = 50, col = "gray")
plot(x.out, march6_cumy.out, log = "xy", xlab = "Outdegree k",
     ylab = expression(Pr(x) >= k), cex = 0.5, main = "Outdegree Power-law Distr")


#Betweenness centrality is an indicator of a node's centrality in a network. 
#It is equal to the number of shortest paths from all vertices to all others that pass 
#through that node. A node with high betweenness centrality has a large influence on the 
#transfer of items through the network, under the assumption that item transfer follows the 
#shortest paths.

march6_btw <- betweenness(march6_gr, directed = TRUE)
sorted.btw <- sort(march6_btw, decreasing = TRUE)

#PageRank works by counting the number and quality of edge to a node to determine 
#a rough estimate of how important the node is. The underlying assumption is 
#that more important nodes are likely to receive more edges from other nodes
pr <- page.rank(march6_gr, directed = TRUE)
sorted.pr <- sort(pr$vector, decreasing = TRUE)

#Hyperlink-Induced Topic Search (HITS; also known as hubs and authorities) is a 
#link analysis algorithm that rates Web pages, developed by Jon Kleinberg. 
#The idea behind Hubs and Authorities stemmed from a particular insight into 
#the creation of web pages when the Internet was originally forming; that is, 
#certain web pages, known as hubs, served as large directories that were not 
#actually authoritative in the information that it held, but were used as compilations 
#of a broad catalog of information that led users directly to other authoritative pages. 
#In other words, a good hub represented a page that pointed to many other pages, and a 
#good authority represented a page that was linked by many different hubs.
authority <- authority.score(march6_gr)$vector
sorted.authority <- sort(authority, decreasing = TRUE)


#subsetting the data for the graph
march6_cc_new1 <-subset(march6_cc, len > 5)
march6_cc_new1 <-subset(march6_cc_new1, len <= 8)
march6_gr<-graph.data.frame(march6_cc_new1, directed=T)

#setting weights for graph
E(march6_gr)$weight <- march6_cc_new1$len

#Community  detection
march6_fc <- fastgreedy.community(as.undirected(march6_gr))
march6_fc.colors <- sample(colors(), length(march6_fc))
for(i in 1:length(march6_fc.colors)) {
  V(march6_gr)[membership(march6_fc)==i]$color <- march6_fc.colors[i]
}

@setting weight for edges
max.weight <- max(E(march6_gr)$weight)
weight.factor <- max.weight / 5
edgeWeight.theta =6
if(edgeWeight.theta == 0) {
  summ.edgeWeight <- summary(E(march6_gr)$weight)
  edgeWeight.theta <- exp(1) * summ.edgeWeight[3] # the median
}
E(march6_gr)$color <- ifelse(E(march6_gr)$weight >= edgeWeight.theta, 'darkorange', 'azure4')
E(march6_gr)$width <- E(march6_gr)$weight / weight.factor
V(march6_gr)$size <- degree(march6_gr, mode="out") / 8
E(march6_gr)$weight <- march6_cc_new1$len

#2d graph
march6_plot <- plot.igraph(march6_gr, vertex.label=NA, layout=layout.fruchterman.reingold, vertex.color = V(march6_gr)$color, vertex.size=2, edge.arrow.size=0.2, edge.color="black")

#interactive graph
tkplot(march6_gr, vertex.label=NA, vertex.color = V(march6_gr)$color, layout=layout.fruchterman.reingold, vertex.size=2, edge.arrow.size=0.2, edge.color="black")

#3d graph
rglplot(march6_gr, vertex.label=NA, vertex.color = V(march6_gr)$color, layout=layout.fruchterman.reingold, vertex.size=2, edge.arrow.size=0.2, edge.color="black")

pdf('march6_graph.pdf')


########################################
#March7
#Reading the file
march7 <- read.csv("march7_final.csv")

#Edge Table
march7_cc <- ddply(march7, c("caller_id", "callee_id"), summarize, len = length(callee_id))

#Create graph data frame
march7_gr<-graph.data.frame(march7_cc, directed=T)

#Global Clustering Coeffecient
march7_gcc <- transitivity(march7_gr, type="global")

#All degrees, indegrees, outdegrees
degrees <- degree(march7_gr, mode = "all")
degrees <- subset(degrees, degrees > 0)

indegrees <- degree(march7_gr, mode = "in")
indegrees <- subset(indegrees, indegrees > 0)

outdegrees <- degree(march7_gr, mode = "out")
outdegrees <- subset(outdegrees, outdegrees > 0)

march7_cumy <- c()
y <- tabulate(degrees)
x <- 1:length(y)
for(i in 1:length(x)) {
  march7_cumy[i] <- sum(y[i:length(x)]) / sum(y)
}
## For indegrees
march7_cumy.in <- c()
y.in <- tabulate(indegrees)
x.in <- 1:length(y.in)
for(i in 1:length(x.in)) {
  march7_cumy.in[i] <- sum(y.in[i:length(x.in)]) / sum(y.in)
}
## For outdegrees
march7_cumy.out <- c()
y.out <- tabulate(outdegrees)
x.out <- 1:length(y.out)
for(i in 1:length(x.out)) {
  march7_cumy.out[i] <- sum(y.out[i:length(x.out)]) / sum(y.out)
}

# Histogram and cumulative distribution function for degrees
hist(degrees, freq = FALSE, xlab = "Degree k", main = "Histogram of All Degrees", breaks = 50, col = "gray")

plot(x, march7_cumy, log = "xy", xlab = "Degree k", ylab = expression(Pr(x) >= k), cex = 0.5, main = "Degree Power-law Distribution")

# Histogram and cumulative distribution function for Indegrees
hist(indegrees, freq = FALSE, xlab = "Indegree k",
     main = "Histogram of Indegrees", breaks = 50, col = "gray")
plot(x.in, march7_cumy.in, log = "xy", xlab = "Indegree k",
     ylab = expression(Pr(x) >= k), cex = 0.5, main = "Indegree Power-law Distr")

# Histogram and cumulative distribution function for Outdegrees
hist(outdegrees, freq = FALSE, xlab = "Outdegree k",
     main = "Histogram of Outdegrees", breaks = 50, col = "gray")
plot(x.out, march7_cumy.out, log = "xy", xlab = "Outdegree k",
     ylab = expression(Pr(x) >= k), cex = 0.5, main = "Outdegree Power-law Distr")


#Betweenness centrality is an indicator of a node's centrality in a network. 
#It is equal to the number of shortest paths from all vertices to all others that pass 
#through that node. A node with high betweenness centrality has a large influence on the 
#transfer of items through the network, under the assumption that item transfer follows the 
#shortest paths.

march7_btw <- betweenness(march7_gr, directed = TRUE)
sorted.btw <- sort(march7_btw, decreasing = TRUE)

#PageRank works by counting the number and quality of edge to a node to determine 
#a rough estimate of how important the node is. The underlying assumption is 
#that more important nodes are likely to receive more edges from other nodes
pr <- page.rank(march7_gr, directed = TRUE)
sorted.pr <- sort(pr$vector, decreasing = TRUE)

#Hyperlink-Induced Topic Search (HITS; also known as hubs and authorities) is a 
#link analysis algorithm that rates Web pages, developed by Jon Kleinberg. 
#The idea behind Hubs and Authorities stemmed from a particular insight into 
#the creation of web pages when the Internet was originally forming; that is, 
#certain web pages, known as hubs, served as large directories that were not 
#actually authoritative in the information that it held, but were used as compilations 
#of a broad catalog of information that led users directly to other authoritative pages. 
#In other words, a good hub represented a page that pointed to many other pages, and a 
#good authority represented a page that was linked by many different hubs.
authority <- authority.score(march7_gr)$vector
sorted.authority <- sort(authority, decreasing = TRUE)


#subsetting the data for the graph
march7_cc_new1 <-subset(march7_cc, len > 5)
march7_cc_new1 <-subset(march7_cc_new1, len <= 8)
march7_gr<-graph.data.frame(march7_cc_new1, directed=T)

#setting weights for graph
E(march7_gr)$weight <- march7_cc_new1$len

#Community  detection
march7_fc <- fastgreedy.community(as.undirected(march7_gr))
march7_fc.colors <- sample(colors(), length(march7_fc))
for(i in 1:length(march7_fc.colors)) {
  V(march7_gr)[membership(march7_fc)==i]$color <- march7_fc.colors[i]
}

@setting weight for edges
max.weight <- max(E(march7_gr)$weight)
weight.factor <- max.weight / 5
edgeWeight.theta =6
if(edgeWeight.theta == 0) {
  summ.edgeWeight <- summary(E(march7_gr)$weight)
  edgeWeight.theta <- exp(1) * summ.edgeWeight[3] # the median
}
E(march7_gr)$color <- ifelse(E(march7_gr)$weight >= edgeWeight.theta, 'darkorange', 'azure4')
E(march7_gr)$width <- E(march7_gr)$weight / weight.factor
V(march7_gr)$size <- degree(march7_gr, mode="out") / 8
E(march7_gr)$weight <- march7_cc_new1$len

#2d graph
march7_plot <- plot.igraph(march7_gr, vertex.label=NA, layout=layout.fruchterman.reingold, vertex.color = V(march7_gr)$color, vertex.size=2, edge.arrow.size=0.2, edge.color="black")

#interactive graph
tkplot(march7_gr, vertex.label=NA, vertex.color = V(march7_gr)$color, layout=layout.fruchterman.reingold, vertex.size=2, edge.arrow.size=0.2, edge.color="black")

#3d graph
rglplot(march7_gr, vertex.label=NA, vertex.color = V(march7_gr)$color, layout=layout.fruchterman.reingold, vertex.size=2, edge.arrow.size=0.2, edge.color="black")

pdf('march7_graph.pdf')

#################################################
