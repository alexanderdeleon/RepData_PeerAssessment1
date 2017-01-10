# Reproducible Research: Peer Assessment 1
Alexander de Leon  
5 January 2017  

## Loading and preprocessing the data


```r
setwd("~/Documents/R")
if(!file.exists("activity.csv")){
    unzip("activity.zip")
}
activityData <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

### 1. Calculate the total number of steps taken per day

```r
steps_day <- tapply(activityData$steps, activityData$date, FUN = sum, na.rm = TRUE)
```

### 2. Make a histogram of the total number of steps taken each day

```r
library(ggplot2)
qplot(steps_day, binwidth = 2000, xlab = "Total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

### 3. Calculate and report the mean and median of the total number of steps taken per day

```r
steps_day_mean <- round(mean(steps_day), 2)
steps_day_median <- median(steps_day)
```

The mean is 9354.23 and the median is 10395

## What is the average daily activity pattern?

### 1. Make a time series plot

```r
averageStepsPerInterval <- aggregate(x = list(avgSteps = activityData$steps), 
                                     by = list(interval = activityData$interval),
                                     FUN = mean, na.rm = TRUE)
ggplot(data = averageStepsPerInterval, aes(x = interval, y = avgSteps)) + geom_line() + xlab("5-minute interval") + ylab("Average number of steps taken") 
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

### 2. Which 5-minute interval, on average across all the days, contains the maximum number of steps?

```r
averageStepsPerInterval[which.max(averageStepsPerInterval$avgSteps),]
```

```
##     interval avgSteps
## 104      835 206.1698
```

## Imputing missing values

### 1. Calculate and report the total number of missing values in the dataset

```r
MissingValues <- length(which(is.na(activityData$steps)))
```

The number of missing values is 2304

### 2. Devise a strategy for filling in all of the missing values in the dataset.
### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
library(Hmisc)
```

```
## Loading required package: lattice
```

```
## Loading required package: survival
```

```
## Loading required package: Formula
```

```
## 
## Attaching package: 'Hmisc'
```

```
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
```

```r
activityDataImputed <- activityData
activityDataImputed$steps <- impute(activityData$steps, fun = mean)
```

### 4.1 Make a histogram of the total number of steps taken each day

```r
steps_day_imputed <- tapply(activityDataImputed$steps, activityDataImputed$date, FUN = sum)
qplot(steps_day_imputed, binwidth = 2000, xlab = "Total number of steps taken each day (Imputed)")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

### 4.2 Calculate and report the mean and median total number of steps taken per day.

```r
steps_day_imputed_mean <- round(mean(steps_day_imputed), 2)
steps_day_imputed_median <- median(steps_day_imputed)
```

The mean is 1.076619\times 10^{4} and the median is 1.0766189\times 10^{4}

## Are there differences in activity patterns between weekdays and weekends?

### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
activityDataImputed$dateType <- ifelse(as.POSIXlt(activityDataImputed$date)$wday %in% c(0,6), 'weekend', 'weekday')
```

### 2. Make a panel plot containing a time series plot "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
averageStepsPerIntervalImputed <- aggregate(steps ~ interval + dateType, data = activityDataImputed, FUN = mean)
ggplot(averageStepsPerIntervalImputed, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(dateType ~ .) +
    xlab("5-minute interval") + 
    ylab("Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->








