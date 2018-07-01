
###reading and cleaning dataset
activity<- read.csv("C:/Users/geovez/Documents/activity.csv", sep=",")
var_cleaning<-!is.na(activity$steps)
var_na<-is.na(activity$steps)
activity_clear<- activity[var_cleaning,]
activity_na<-activity[var_na,]
activity_na<-activity_na[,c(2,3)]



###Histogram of total number of steps
hist1<-aggregate(x=activity_clear["steps"], FUN = sum, by = list(date = activity_clear$date))
hist(hist1$steps, xlab="Steps per Day", ylab="Frequency", main= "Total number of steps taken per day", col="green" )


#### Steps mean and median
steps_mean_all<-aggregate(x=activity_clear["steps"], FUN = mean, by = list(date = activity_clear$date))

plot(activity_na$interval~activity_na$date )

na_days<-unique(activity_na[,2])


steps_mean<-mean(hist1$steps)
steps_median<-median(hist1$steps)


######### Plot average steps / day

steps_interval_mean<-aggregate(steps ~ interval, data = activity_clear, FUN = mean)

plot(steps_interval_mean, type="l", main="Average steps during All days without NAs")



############# Max intervals

steps_max<-max(steps_interval_mean$steps)

max_interval<- steps_interval_mean$interval[which(steps_interval_mean$steps == steps_max)]

##Imputing Missing Data


library(dplyr)
match_dataset=left_join(activity_na, steps_interval_mean, by="interval")

imputed_data<-match_dataset[,c(3,1,2)]

imputed_data$steps<-as.integer(imputed_data$steps)

activity_all=rbind(imputed_data, activity_clear)

#activity_imp<- with(activity, impute(activity$steps,mean))

#activity2<-activity
#activity2$steps = activity_imp


## Histogram after imputing the missing values

hist2<-aggregate(x=activity_all["steps"], FUN = sum, by = list(date = activity_all$date))
hist(hist2$steps, xlab="Steps per Day", ylab="Frequency", main= "Total number of steps taken per day", col="green" )


############# Weekdays - Weekends

library(timeDate)

activity_all$date<-as.Date(activity_all$date)

activity_all["Day_flag"]<-isWeekday(activity_all$date, wday=1:5)


activity_weekdays<-aggregate(x=activity_all["steps"], FUN = sum, by = list(date = activity_all$date) )

activity_weekdays <- aggregate(steps~interval, subset(activity_all, Day_flag == TRUE), mean)
activity_weekends<- aggregate(steps~interval, subset(activity_all, Day_flag == FALSE), mean)

plot(activity_weekdays, type="l", main="Average steps during Weekdays")
plot(activity_weekends, type="l", main="Average steps during Weekends")





