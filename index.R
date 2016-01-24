## ------------------------------------------------------------------------
  data<-read.csv("./Data/activity.csv")

## ------------------------------------------------------------------------
  dt<-read.table(file = "./Data/activity.csv", header = TRUE, sep = ",")

## ------------------------------------------------------------------------
 dt_wo_NA <- na.omit(dt) 

## ------------------------------------------------------------------------
StepPerDay<- aggregate(steps ~ date, dt_wo_NA, sum)    
hist(StepPerDay$steps, col=1, main="Histogram of total number of steps per day", 
     xlab="Total number of steps in a day")

## ------------------------------------------------------------------------
mean(StepPerDay$steps)
median(StepPerDay$steps)

## ------------------------------------------------------------------------
StepMeanPerInterval <- aggregate(steps ~ interval, dt_wo_NA, mean)
plot(StepMeanPerInterval$interval, StepMeanPerInterval$steps, type='l', col=1, 
     main="Average number of steps averaged over all days", xlab="Interval", 
     ylab="Average number of steps")

## ------------------------------------------------------------------------
StepMeanPerInterval$interval[which.max(StepMeanPerInterval$steps)]

## ------------------------------------------------------------------------
  dt_NA <- dt[is.na(dt)]
  length(dt_NA)

## ------------------------------------------------------------------------
dt_Modified_NA<-dt;
MeanStepPerInterval<-aggregate(steps ~ interval, dt, mean) ;
for (i in 1:nrow(dt_Modified_NA)){
    if (is.na(dt_Modified_NA$steps[i])){
      inter <- dt_Modified_NA$interval[i];
      row_id <- which(MeanStepPerInterval$interval == inter);
      if(length(row_id)>0){
        steps_val <- MeanStepPerInterval[row_id,2];
        dt_Modified_NA$steps[i] <- steps_val;
      }else{dt_Modified_NA$steps[i] <- 0}
      
    }
  } 

## ------------------------------------------------------------------------
StepPerDay_ModifiedNA<- aggregate(steps ~ date, dt_Modified_NA, sum)    
hist(StepPerDay_ModifiedNA$steps, col=1, main="Histogram of total number of steps per day", 
     xlab="Total number of steps in a day")
mean(StepPerDay_ModifiedNA$steps)
median(StepPerDay_ModifiedNA$steps)

## ------------------------------------------------------------------------
library(knitr)
## ------------------------------------------------------------------------
dt_WDayEnd<-dt_Modified_NA
# convert date to string
dt_WDayEnd$date <- as.Date(dt_WDayEnd$date, "%Y-%m-%d")
# add a new column indicating day of the week 
dt_WDayEnd$day <- weekdays(dt_WDayEnd$date)
# add a new column for "weekday" or "weekend", initialised with "weekday"
dt_WDayEnd$day_type <- c("weekday")
# If day is "samedi" or "dimanche, make day_type as weekend
for (i in 1:nrow(dt_WDayEnd)){
  if (dt_WDayEnd$day[i] == "dimanche" || dt_WDayEnd$day[i] == "samedi"){
    dt_WDayEnd$day_type[i] <- "weekend"
  }
}
# convert day_time to factor
dt_WDayEnd$day_type <- as.factor(dt_WDayEnd$day_type)
StepMeanPerInterval_WDayEnd <- aggregate(steps ~ interval+day_type, dt_WDayEnd, mean)


## ------------------------------------------------------------------------
par(mfrow = c(2, 1))
for (type in c("weekend", "weekday")) {
    steps.type <- aggregate(steps ~ interval, data = StepMeanPerInterval_WDayEnd , subset = StepMeanPerInterval_WDayEnd$day_type ==  type, FUN = mean)
    plot(steps.type, type = "l", main = type)
}


