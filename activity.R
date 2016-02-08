# extract zip file
file_name <- "activity.zip"

if(!file.exists("activity.text")) {
  unzip(file_name)
}

#read file into dataframe
activity <- read.csv("activity.csv", header = TRUE, colClasses = c(date = "Date"))


library(dplyr)
library(ggplot2)
library(scales)

## total numbers of steps each day
steps_day <- aggregate(steps ~ date, data=activity, na.rm=FALSE, FUN = sum)

ggplot(data=steps_day, aes(steps)) + 
  geom_histogram(fill="orange", col = "black") +
  ggtitle("Total steps per day") 


step_mean <- mean(steps_day$steps)
step_median <- median(steps_day$steps)

## Average daily activity pattern
avg_steps_per_interval <- activity %>% group_by(interval) %>% summarise(avg_steps = mean(steps, na.rm=TRUE))

## Time Series Plot
with(avg_steps_per_interval, plot(strptime(sprintf("%04d", interval), format="%H%M"), y=avg_steps, type="l", col = "red", 
                                  main="Time Series Plot of Steps Taken in 5-Minute Intervals, Average Cross All Days", 
                                  xlab="Time of Day (HH:MM)", 
                                  ylab="Average number steps"))

## 5-Min interval contains the maximum number of steps
max_interval <- filter(avg_steps_per_interval, avg_steps == max(avg_steps))

## Inputting missing data

## figuring number of missing value
sum(is.na(activity$steps))

## replacing with avg steps per interval calculated in avg_steps_per_interval
activity.filled  <- inner_join(activity, avg_steps_per_interval, by = "interval") %>%
    mutate(steps=ifelse(is.na(steps), round(avg_steps), steps)) %>%
    select(date, steps, interval)

#Total steps each day after the missing value is filled
steps_day_filled <- aggregate(steps ~ date, data=activity.filled, FUN=sum)

## Histogram of the total number of steps taken each day after the missing value is filled
ggplot(data=steps_day_filled, aes(steps))+geom_histogram(fill="orange", col="black")+
  ggtitle("Total number of steps taken each day") + xlab("Total steps taken each day") + ylab("Frequency")

mean(steps_day_filled$steps)

median(steps_day_filled$steps)

## Panel plot comparing the avg number of steps taken per 5-min interval across weekdays and weekends

activity_new <- activity.filled%>%mutate(daytype=as.factor(ifelse(weekdays(date) 
                        %in% c("Saturday", "Sunday"), "weekend", "weekday")))


activity_new_interval <- aggregate(steps ~ interval + daytype, activity_new, mean)

## Set xlim (Have to change the date to reflect today's date and tomorrow's date in order to work, trying to figure out a better solution)
xlim <- as.POSIXct(c("2016-02-07 00:00", "2016-02-08 00:00"), format="%Y-%m-%d %H:%M", tz = "EST")
ggplot(data=activity_new_interval, aes(strptime(sprintf("%04d", interval), format="%H%M"), steps, color =daytype)) +
  geom_line() +
  facet_wrap(~daytype, ncol=1) + 
  xlab("Hour of Day") + 
  ggtitle("Average Daily Steps Weekdays Vs. Weekend") +
  scale_x_datetime(labels = date_format("%H:%M",tz="EST"), breaks = date_breaks("2 hour"), limits=xlim) + 
  scale_y_continuous("Average Steps Taken")
  
