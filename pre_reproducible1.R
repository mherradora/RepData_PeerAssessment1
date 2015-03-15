#####
# Loading and preprocessing the data

#library(reshape2)
activity <- read.csv("activity.csv", stringsAsFactors=FALSE)
activity <- activity[,c(2,3,1)]
activity <- activity[order(activity$date,activity$interval),]
activity$date <- as.Date(activity$date,"%Y-%m-%d")

#####
# What is mean total number of steps taken per day?
attach(activity)
stepsByDay <- aggregate(steps,by=list(date),sum,na.rm=T)
detach(activity)
names(stepsByDay) <- c("date","steps")

hist(stepsByDay$steps,breaks=20)

steps_mean <- mean(stepsByDay$steps,na.rm=T)
steps_median <- median(stepsByDay$steps,na.rm=T)

#####
# What is the average daily activity pattern?
attach(activity)
stepsByInterval <- aggregate(steps,by=list(interval),mean,na.rm=T)
detach(activity)
names(stepsByInterval) <- c("interval","steps")
plot(stepsByInterval$interval,stepsByInterval$steps,type = "l")
max_steps <- max(stepsByInterval$steps)
max_interval <- stepsByInterval[stepsByInterval$steps==max_steps,]$interval

#####
# Imputing missing values
na_total <- sum(is.na(activity$steps))
newActivity <- activity
newActivity$stepsNewMedian <- newActivity$steps

for(i in 1:dim(newActivity)[1]){
  if(is.na(newActivity[i,]$steps)){
    current_date <-newActivity[i,]$date
    current_interval <-newActivity[i,]$interval
    similars <- newActivity[newActivity$date==current_date | newActivity$interval==current_interval,]
    newValueMedian <- median(similars$steps,na.rm = T)
    newActivity[i,]$stepsNewMedian <- newValueMedian
  }
}
newActivity$steps <- newActivity$stepsNewMedian
newActivity$stepsNewMedian <- NULL

attach(newActivity)
byMedian <- aggregate(steps,by=list(date),sum,na.rm=T)
detach(newActivity)
names(byMedian) <- c("date","steps")

hist(byMedian$steps,breaks=20)

steps_mean2 <- mean(byMedian$steps,na.rm=T)
steps_median2 <- median(byMedian$steps,na.rm=T)

dif_mean <- 100*((steps_mean2 / steps_mean)-1)
dif_median <- 100*((steps_median2 / steps_median)-1)

#####
# Are there differences in activity patterns between weekdays and weekends?
newActivity$weekDay <- weekdays((newActivity$date),T)
newActivity$typeDay <- ifelse(newActivity$weekDay=="Sat" | newActivity$weekDay=="Sun","weekend","weekday")

attach(newActivity)
ByInterval <- aggregate(stepsNewMedian,by=list(interval,typeDay),mean,na.rm=T)
detach(newActivity)
names(ByInterval) <- c("interval","typeDay","steps")

par( mfrow=c(2,1) )
tmp <- ByInterval[ByInterval$typeDay=="weekend",]
plot(tmp$interval,tmp$steps,type = "l",main="Average of steps by interval \n-weekend-")
tmp <- ByInterval[ByInterval$typeDay=="weekday",]
plot(tmp$interval,tmp$steps,type = "l",main="Average of steps by interval \n-weekday-")
par(mfrow=c(1,1) )

save.image("RepData_PeerAssessment1.RData")

