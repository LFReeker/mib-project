######################################
#Exploratory Graphs
######################################

#Count by hour
march1_h <- ddply(march1, "hr", summarize, len = length(callee_id))
march2_h <- ddply(march2, "hr", summarize, len = length(callee_id))
march3_h <- ddply(march3, "hr", summarize, len = length(callee_id))
march4_h <- ddply(march4, "hr", summarize, len = length(callee_id))
march5_h <- ddply(march5, "hr", summarize, len = length(callee_id))
march6_h <- ddply(march6, "hr", summarize, len = length(callee_id))
march7_h <- ddply(march7, "hr", summarize, len = length(callee_id))

march_h <- march1_h

march_h <- rbind(march_h,march2_h)
march_h <- rbind(march_h,march3_h)
march_h <- rbind(march_h,march4_h)
march_h <- rbind(march_h,march5_h)
march_h <- rbind(march_h,march6_h)
march_h <- rbind(march_h,march7_h)

march_h <- ddply(march_h, "hr", summarize, len = sum(len))

ggplot(data=march_h, mapping=aes(x=hr,y=len))+geom_line()

#Count by day
march1_d <- ddply(march1, "date", summarize, len = length(callee_id))
march2_d <- ddply(march2, "date", summarize, len = length(callee_id))
march3_d <- ddply(march3, "date", summarize, len = length(callee_id))
march4_d <- ddply(march4, "date", summarize, len = length(callee_id))
march5_d <- ddply(march5, "date", summarize, len = length(callee_id))
march6_d <- ddply(march6, "date", summarize, len = length(callee_id))
march7_d <- ddply(march7, "date", summarize, len = length(callee_id))

march_d <- march1_d

march_d <- rbind(march_d,march2_d)
march_d <- rbind(march_d,march3_d)
march_d <- rbind(march_d,march4_d)
march_d <- rbind(march_d,march5_d)
march_d <- rbind(march_d,march6_d)
march_d <- rbind(march_d,march7_d)

march_d <- ddply(march_d, "date", summarize, len = sum(len))

ggplot(data=march_d, mapping=aes(x=date,y=len))+geom_line()

#Count by latitude longitude
march_ll <- march1_ll

march_ll <- rbind(march_ll,march2_ll)
march_ll <- rbind(march_ll,march3_ll)
march_ll <- rbind(march_ll,march4_ll)
march_ll <- rbind(march_ll,march5_ll)
march_ll <- rbind(march_ll,march6_ll)
march_ll <- rbind(march_ll,march7_ll)

march_ll <- ddply(march_ll, c("latitude","longitude"), summarize, len = sum(len))

march_ll$latlong <- paste(latitude, ":", longitude)

ggplot(data=march_ll, mapping=aes(x=latlong,y=len))+geom_line()

#Callers ids of people who have communicated the most over the week
sorted_march_c <- march_c[order(-len),]
march_c_top20 <- march_c$caller_id[1:20,]


#Callers ids of people who received the most number of calls over the week
march1_ce <- ddply(march1, c("callee_id"), summarize, len = length(caller_id))
march2_ce <- ddply(march2, c("callee_id"), summarize, len = length(caller_id))
march3_ce <- ddply(march3, c("callee_id"), summarize, len = length(caller_id))
march4_ce <- ddply(march4, c("callee_id"), summarize, len = length(caller_id))
march5_ce <- ddply(march5, c("callee_id"), summarize, len = length(caller_id))
march6_ce <- ddply(march6, c("callee_id"), summarize, len = length(caller_id))
march7_ce <- ddply(march7, c("callee_id"), summarize, len = length(caller_id))

march_ce <-march1_ce

march_ce <- rbind(march_ce,march2_ce)
march_ce <- rbind(march_ce,march3_ce)
march_ce <- rbind(march_ce,march4_ce)
march_ce <- rbind(march_ce,march5_ce)
march_ce <- rbind(march_ce,march6_ce)
march_ce <- rbind(march_ce,march7_ce)

sorted_march_ce <- march_ce[order(-len),]
march_ce_top20 <- march_ce$callee_id[1:20,]
