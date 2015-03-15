#####
# Loading and preprocessing the data

library(reshape2)
activity <- read.csv("activity.csv", stringsAsFactors=FALSE)

activity <- activity[,c(2,3,1)]
activity <- activity[order(activity$date,activity$interval),]

# table(activity$date,useNA = "ifany")
activity$date <- as.Date(activity$date,"%Y-%m-%d")
# stepsByDayAndInterval <- dcast(activity,interval + date ~ steps)


#####
# What is mean total number of steps taken per day?

attach(activity)
stepsByDay <- aggregate(steps,by=list(date),sum,na.rm=T)
stepsByInterval <- aggregate(steps,by=list(interval),mean,na.rm=T)
#stepsByDayAndInterval <- aggregate(steps,by=list(date,interval),sum,na.rm=T)
detach(activity)
names(stepsByDay) <- c("date","steps")
names(stepsByInterval) <- c("interval","steps")
#names(stepsByDayAndInterval) <- c("date2","interval","steps")

hist(stepsByDay$steps,breaks=20)

steps_mean <- mean(stepsByDay$steps,na.rm=T)
steps_median <- median(stepsByDay$steps,na.rm=T)

#stepsByDayAndInterval <- dcast(activity,interval ~ date)
# rm(stepsByDayAndInterval2)
# rm(tmp)


#####
# What is the average daily activity pattern?

plot(stepsByInterval$interval,stepsByInterval$steps,type = "l")

max_steps <- max(stepsByInterval$steps)
max_interval <- stepsByInterval[stepsByInterval$steps==max_steps,]$interval

#####
# Imputing missing values

na_total <- sum(is.na(activity$steps))

newActivity <- activity
newActivity$stepsNewMedian <- newActivity$steps
newActivity$stepsNewMean <- newActivity$steps

for(i in 1:dim(newActivity)[1]){
  if(is.na(newActivity[i,]$steps)){
    current_date <-newActivity[i,]$date
    current_interval <-newActivity[i,]$interval
    similars <- newActivity[newActivity$date==current_date | newActivity$interval==current_interval,]
    # print(dim(similars))
    newValueMedian <- median(similars$steps,na.rm = T)
    if(newValueMedian!=0) print(newValueMedian)
    newValueMean <- mean(similars$steps,na.rm = T)
    newActivity[i,]$stepsNewMedian <- newValueMedian
    newActivity[i,]$stepsNewMean <- newValueMean
  }
}

# newActivity <- activity
# # newActivity$stepsNewMedian <- newActivity$steps
# # newActivity$stepsNewMean <- newActivity$steps
# # newActivity$steps <- as.double(newActivity$steps)
# 
# for(i in 1:dim(newActivity)[1]){
#   if(is.na(newActivity[i,]$steps)){
#     current_date <-newActivity[i,]$date
#     current_interval <-newActivity[i,]$interval
#     similars <- newActivity[newActivity$date==current_date | newActivity$interval==current_interval,]
#     #print(dim(similars))
#     newValueMedian <- mean(similars$steps,na.rm = T)
#     if(newValueMedian!=0) print(newValueMedian)
#     # newValueMean <- mean(similars$steps,na.rm = T)
#     newActivity[i,]$steps <- newValueMedian
#     # newActivity[i,]$stepsNewMean <- newValueMean
#   }
# }
# 
# newActivity$oldSteps <- activity$steps

# control <- newActivity[is.na(newActivity$steps),]
# table(control$steps)

attach(newActivity)
byMean <- aggregate(stepsNewMean,by=list(date),sum,na.rm=T)
byMedian <- aggregate(stepsNewMedian,by=list(date),sum,na.rm=T)
ByInterval <- aggregate(stepsNewMedian,by=list(interval),sum,na.rm=T)
#stepsByDayAndInterval <- aggregate(steps,by=list(date,interval),sum,na.rm=T)
detach(newActivity)
names(byMean) <- c("date","steps")
names(byMedian) <- c("date","steps")
names(ByInterval) <- c("interval","steps")

hist(byMean$steps,breaks=20)
hist(byMedian$steps,breaks=20)

steps_mean2 <- mean(byMean$steps,na.rm=T)
steps_median2 <- median(byMean$steps,na.rm=T)
steps_mean3 <- mean(byMedian$steps,na.rm=T)
steps_median3 <- median(byMedian$steps,na.rm=T)

steps_mean2 / steps_mean
steps_mean3 / steps_mean

steps_median2 / steps_median
steps_median3 / steps_median

# Are there differences in activity patterns between weekdays and weekends?
newActivity$date <- as.Date(newActivity$date,"%Y-%m-%d")

newActivity$weekDay <- weekdays((newActivity$date),T)
table(newActivity$weekDay,useNA = "ifany")
newActivity$typeDay <- ifelse(newActivity$weekDay=="Sat" | newActivity$weekDay=="Sun","weekend","weekday")
table(newActivity$weekDay,newActivity$typeDay,useNA = "ifany")

attach(newActivity)
ByInterval <- aggregate(stepsNewMedian,by=list(interval,typeDay),mean,na.rm=T)
detach(newActivity)
names(ByInterval) <- c("interval","typeDay","steps")

#plot(stepsByInterval$interval,stepsByInterval$steps,type = "l")

par( mfrow=c(2,1) )
tmp <- ByInterval[ByInterval$typeDay=="weekend",]
plot(tmp$interval,tmp$steps,type = "l")
tmp <- ByInterval[ByInterval$typeDay=="weekday",]
plot(tmp$interval,tmp$steps,type = "l")
par(mfrow=c(1,1) )


save.image("RepData_PeerAssessment1.RData")

