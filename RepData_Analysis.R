# LOADING AND PREPROCESSING THE DATA
# 1. Load the data (i.e. read.csv())
unzip("repdata%2Fdata%2Factivity.zip")
activity <- read.csv("activity.csv", colClasses = c("numeric", "character", "integer"))

# 2. Process/transform the data (if necessary) into a format suitable for your analysis
dim(activity)
head(activity)
tail(activity)
summary(activity)
names(activity)
str(activity)

library("plyr")
library("dplyr")
library("ggplot2")

activity$date <- as.POSIXct(activity$date, format="%Y-%m-%d")

# WHAT IS THE MEAN TOTAL NUMBER OF STEPS TAKEN PER DAY?
# 1. Calculate the total number of steps taken per day
steps <- activity %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps=sum(steps)) %>%
  print()

# 2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
ggplot(steps, aes(x=date, y=steps))+geom_histogram(stat="identity")+xlab("Dates")+ylab("Steps")+labs(title="Total Number of Steps per Day")

# 3. Calculate and report the mean and median of the total number of steps taken per day
total.steps <- tapply(activity$steps, activity$date, FUN = sum, na.rm=TRUE)
mean(total.steps)
median(total.steps)

# WHAT IS THE AVERAGE DAILY ACTIVITY PATTERN?
# 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
daily <- activity %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarize(steps=mean(steps))
print(daily)

plot(daily, type = "l")

# 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
daily[which.max(daily$steps), ]$interval

# INPUTING MISSING VALUES
# 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
missing <- sum(is.na(activity))
missing

# 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
# 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
new <- activity %>%
  group_by(interval) %>%
  mutate(steps=ifelse(is.na(steps), mean(steps, na.rm=TRUE), steps))
summary(new)

# 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
new.steps <- new %>%
  group_by(date) %>%
  summarize(steps=sum(steps))
print(new.steps)

ggplot(new.steps, aes(x=date, y=steps))+geom_histogram(stat="identity")+xlab("Dates")+ylab("Imputed Steps")+labs(title="Total Number of Steps per Day")

imput.steps <- tapply(new$steps, new$date, FUN=sum, na.rm=TRUE)
mean(imput.steps)
median(imput.steps)

mean(total.steps)==mean(imput.steps)
median(total.steps)==median(imput.steps)
summary(total.steps)
summary(imput.steps)

par(mfrow=c(2,1))
hist(total.steps,col="blue")
hist(imput.steps,col="red")

# ARE THERE DIFFERENCES IN ACTIVITY PATTERNS BETWEEN WEEKDAYS AND WEEKENDS?
# 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
dayofweek <- function(date) {
  if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
    "weekend"
  } else {
    "weekday"
  }
}
new$daytype <- as.factor(sapply(new$date, dayofweek))

# 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
par(mfrow = c(2, 1))
for (type in c("weekend", "weekday")) {
  steps.type <- aggregate(steps ~ interval, data = new, subset = new$daytype == 
                            type, FUN = mean)
  plot(steps.type, type = "l", main = type)
}
