# Reproducible Research: Peer Assessment 1
`r format(Sys.time(), '%d %B, %Y')`  


## Loading and preprocessing the data

```r
library(knitr)
library(ggplot2)
library(lattice)

filename = "../data/activity.csv"
data = read.csv(filename, sep = ",")
```

We see that there are three variables, 'steps', 'date' and 'interval'. We have 17568 observations in total but there are also 2305 NA's under the 'steps' column. The maximum number of steps taken in a single interval is 806 and the minimum is 0. Moreover we have data of 61 days with 288 intervals a day.


```r
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(data)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

## What is mean total number of steps taken per day?  


```r
# Ignore missing values
data_no_NA <- data[complete.cases(data),]
# 1. calculate total number of steps per day
total_steps <- aggregate(steps ~ date, data_no_NA, sum)
# 2. Make a histogram of the total number of steps taken each day
ggplot(total_steps, aes(x = steps)) + 
       geom_histogram(fill = "blue", binwidth = 1000) + 
        labs(title="Histogram of Steps Taken per Day", 
             x = "Number of Steps per Day", y = "Number of times in a day(Count)")
```

![](PA1_template_files/figure-html/Total steps per day-1.png)
     
The mean of the total number of steps per day is:

```r
# mean of total steps per day
mean(total_steps$steps)
```

```
## [1] 10766.19
```

The median of the total number of steps per day is:

```r
# median of total steps per day
median(total_steps$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
# 1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
average_steps <- aggregate(steps ~ interval, data_no_NA, mean)
plot(average_steps, type = "l", xlab = "Interval", ylab = "Average steps")
```

![](PA1_template_files/figure-html/Daily activity-1.png)

The interval with the maximum average steps across all days is: 

```r
#2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
average_steps[average_steps$steps == max(average_steps$steps), 1]
```

```
## [1] 835
```

## Imputing missing values
The total number of rows with missing values is:

```r
#1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
nrow(data[!complete.cases(data),])
```

```
## [1] 2304
```

Replace all missing values with the average value of that interval.

```r
# 2.Fill in all of the missing values in the dataset by using the mean value for that interval.
# merge original data with the average steps per interval data frame
data_temp <- merge(data, average_steps, by = 'interval', all.y = F)
data_temp$steps.x[is.na(data_temp$steps.x)] <- round(data_temp$steps.y[is.na(data_temp$steps.x)])
#3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
data_replaced_NA <- data_temp[, 1:3]
colnames(data_replaced_NA) <- c("interval", "steps", "date")
```

Calculate again the mean and median of the total steps taken per day.

```r
total_steps_new <- aggregate(steps ~ date, data_replaced_NA, sum)
# 2. Make a histogram of the total number of steps taken each day
ggplot(total_steps_new, aes(x = steps)) + 
       geom_histogram(fill = "blue", binwidth = 1000) + 
        labs(title="Histogram of Steps Taken per Day", 
             x = "Number of Steps per Day", y = "Number of times in a day(Count)")
```

![](PA1_template_files/figure-html/total steps per day new-1.png)
     
The mean of the total number of steps per day is:

```r
# mean of total steps per day
mean(total_steps_new$steps)
```

```
## [1] 10765.64
```

The median of the total number of steps per day is:

```r
# median of total steps per day
median(total_steps_new$steps)
```

```
## [1] 10762
```

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

The new mean and median values do not significantly differ from the values calculated with data where the missing values where ignored. This is explained by the fact that we used a mean value calculated with th rest of the data to replace the missing values.

## Are there differences in activity patterns between weekdays and weekends?

```r
#1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
data_replaced_NA$date <- as.Date(data_replaced_NA$date)
data_replaced_NA$day <- "weekday"
data_replaced_NA$day[weekdays(data_replaced_NA$date, abb=T) %in% c("Sat","Sun")] <- "weekend"
data_replaced_NA$day <- as.factor(data_replaced_NA$day)
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
#2. Make a panel plot containing a time series plot
average_steps_day <- aggregate(steps ~ interval + day, data_replaced_NA, mean)
xyplot(steps ~ interval | day, average_steps_day, type="l", grid=T, layout=c(1,2), ylab="Number of steps", xlab="interval")
```

![](PA1_template_files/figure-html/Make a panel plot containing a time series plot-1.png)

From the plot made in de code chunk here above it can be seen that there are differences between weekdays and weekends. During the weekend there are more steps taken during the day while on weekdays the most steps are taking in the beginning of the day.
