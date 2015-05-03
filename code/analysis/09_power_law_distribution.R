##############################################################################
#                                                                            #
#     1.  P O W E R   L A W   D I S T R I B U T I O N                        #
#     2.  H I S T O G R A M S                                                #
#     3.  W E E K L Y   P L O T S                                            #
#                                                                            #
##############################################################################

library(plyr)
library(poweRlaw)
library(ggplot2)


########## G E T   A L L   T H E   D A T A #######################

march1 <- read.csv("March1_Merged.csv")
march2 <- read.csv("March2_Merged.csv")
march3 <- read.csv("March3_Merged.csv")
march4 <- read.csv("March4_Merged.csv")
march5 <- read.csv("March5_Merged.csv")
march6 <- read.csv("March6_Merged.csv")
march7 <- read.csv("March7_Merged.csv")

march1_c <- ddply(march1, c("caller_id"), summarize, len = length(callee_id))
march2_c <- ddply(march2, c("caller_id"), summarize, len = length(callee_id))
march3_c <- ddply(march3, c("caller_id"), summarize, len = length(callee_id))
march4_c <- ddply(march4, c("caller_id"), summarize, len = length(callee_id))
march5_c <- ddply(march5, c("caller_id"), summarize, len = length(callee_id))
march6_c <- ddply(march6, c("caller_id"), summarize, len = length(callee_id))
march7_c <- ddply(march7, c("caller_id"), summarize, len = length(callee_id))

march_c <-march1_c

march_c <- rbind(march_c,march2_c)
march_c <- rbind(march_c,march3_c)
march_c <- rbind(march_c,march4_c)
march_c <- rbind(march_c,march5_c)
march_c <- rbind(march_c,march6_c)
march_c <- rbind(march_c,march7_c)

write.csv(march_c,file="march.csv")

########## P O W E R   L A W   D I S T R I B U T I O N #############
set.seed(1)

#we load the package and create a discrete power-law object:
march_displ = displ$new(march_c$len)

#Next, we set xmin and alpha for each power-law and add it to the graph using lines:
#estimate uses mle
march_displ$setXmin(estimate_xmin(march_displ))

#To convert exponential values in axes labels
options(scipen=999) 

#Plot Sunday
plot(march_displ, main="Caller Degree Power-law Distribution", xlab="Degree k", ylab="Pr(x)>=k")
lines(march_displ, col=2)

#####################################################################


#############  H I S T O G R A M S   &   P L O T S ##################

### HISTOGRAMS ###
ggplot(data = march_c,mapping = aes(x=len)) + geom_histogram() + xlim(0,50) + xlab("No. of Calls") + ylab("Caller Frequency") + ggtitle("Histogram of Caller frequency")



### Weekly Plot ###

### Needs Modification ###

#library(lattice)

#weekly_calls <- data.frame( Day = c("Saturday","Sunday","Monday","Tuesday","Wednesday","Thursday","Friday"),
#                            Freq = c(nrow(march1),nrow(march2),nrow(march3),nrow(march4),nrow(march5),nrow(march6),nrow(march7))
#                           )


#ggplot(data=weekly_calls, x=Day,y=Freq) + geom_line()

#plot(data = weekly_calls, aes(x=Day, y = Freq))
#plot(weekly_calls)

########################################################################################
#                                                                                      #
#                   E N D   O F   P R O G R A M                                        #
#                                                                                      #
########################################################################################








