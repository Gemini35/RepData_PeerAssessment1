library(dplyr)
library(lubridate)
library(Hmisc)
library(ggplot2)

## Loading and preprocessing the data
# 1. Load the data
activity<-read.csv("activity.csv")

# 2. Process/transform the data (if necessary) into a format suitable for your analysis

## What is mean total number of steps taken per day?
# For this part of the assignment, you can ignore the missing values in the dataset.

# 1. Calculate the total number of steps taken per day



activity.1<-filter(activity,!is.na(steps)) %>%
  mutate(Date=ymd(date)) %>%
  select(Date,steps) %>%
  group_by(Date) %>% 
  summarise(StepsSumDly=sum(steps))


# 2. Make a histogram of the total number of steps taken each day

hist(activity.1$StepsSumDly)


# 3. Calculate and report the mean and median of the total number of steps taken per day

summary(activity.1$StepsSumDly)

#What is the average daily activity pattern?

#1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, 
#averaged across all days (y-axis)

activity.2<-filter(activity,!is.na(steps)) %>%
  select(interval,steps) %>%
  group_by(interval) %>% 
  summarise(StepsInt5min=mean(steps))

ggplot(activity.2,aes(interval,StepsInt5min))+geom_line()

#2, Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

  #with(activity.2,plot(interval,StepsInt5min,type="l"))
  #ggplot(activity.2,aes(interval,StepsInt5min))+geom_line()
  

  which.max(activity.2$StepsInt5min)
  activity.2$interval[which.max(activity.2$StepsInt5min)]


  ## Imputing missing values
  
  # Note that there are a number of days/intervals where there are missing values (coded as #  NA). 
  #The presence of missing days may introduce bias into some calculations or summaries of the data.
  
#1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).  There are 2,304 NA values.
  
  
sum(is.na(activity))
summary(activity)


#2, Devise a strategy for filling in all of the missing values in the dataset. 
#The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, 
#or the mean for that 5-minute interval, etc.

  #for this we use the hmisc thatn contains a useful impute function that can use the median of values to fill in NA values.
library(Hmisc)



#3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


activity.3<-read.csv("activity.csv")
activity.3$steps<-with(activity.3, impute(steps, median))

summary(activity.3)

# 4. Make a histogram of the total number of steps taken each day 
#Calculate and report the mean and median total number of steps taken per day. 
#Do these values differ from the estimates from the first part of the assignment? 
#What is the impact of imputing missing data on the estimates 
# of the total daily number of steps?


activity.4<-mutate(activity.3,Date=ymd(date)) %>%
  select(Date,steps) %>%
  group_by(Date) %>% 
  summarise(StepsSumDly=sum(steps))

hist(activity.4$StepsSumDly)

summary(activity.4$StepsSumDly)


#Are there differences in activity patterns between weekdays and weekends?

# 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” 
#indicating whether a given date is a weekday or weekend day.

activity.5<-mutate(activity.3,Week=weekdays(ymd(date)))
activity.5$Week[activity.5$Week=="Saturday" | activity.5$Week=="Saturday"]<-"Weekend"
activity.5$Week[activity.5$Week!="Weekend"]<-"Weekday"
table(activity.5$Week)

# 2. Make a panel plot containing a time series plot (i.e.type="l") of the 5-minute interval (x-axis) and the average number of steps taken, 
#averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository 
#to see an example of what this plot should look like using simulated data.

activity.6<-select(activity.5,interval,steps,Week) %>%
  group_by(interval,Week) %>% 
  summarise(StepsInt5min=mean(steps))

ggplot(activity.6,aes(interval,StepsInt5min))+geom_line()+facet_wrap((~Week))


