#####
# Loading and preprocessing the data

library(reshape2)
activity <- read.csv("activity.csv", stringsAsFactors=FALSE)

activity <- activity[,c(2,3,1)]
activity <- activity[order(activity$date,activity$interval),]

# table(activity$date,useNA = "ifany")
# activity$date2 <- as.Date(activity$date,"%Y-%m-%d")
# stepsByDayAndInterval <- dcast(activity,interval + date ~ steps)


#####
# What is mean total number of steps taken per day?

attach(activity)
stepsByDay <- aggregate(steps,by=list(date),sum,na.rm=T)
#stepsByDayAndInterval <- aggregate(steps,by=list(date,interval),sum,na.rm=T)
detach(activity)
names(stepsByDay) <- c("date2","steps")
#names(stepsByDayAndInterval) <- c("date2","interval","steps")

hist(stepsByDay$steps,breaks=20)

steps_mean <- mean(stepsByDay$steps,na.rm=T)
steps_median <- median(stepsByDay$steps,na.rm=T)

stepsByDayAndInterval <- dcast(activity,interval ~ date)
# rm(stepsByDayAndInterval2)
# rm(tmp)

save.image("RepData_PeerAssessment1.RData")
