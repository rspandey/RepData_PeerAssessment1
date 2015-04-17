---
title: "PA1_template.Rmd"
output: PA1_template.html
---
# Reproducible Research: Peer assessment 1
#### Ravi S Pandey


## Loading and preprocessing the data

### Load required libraries

```r
library(knitr)
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.3
```

```r
library(lattice)
```

### Set global options

```r
opts_chunk$set(echo = TRUE, results = 'hold')
```

### Load the data

```r
#Download the zip file
fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileurl, "./data/activity.zip")
#unzip the file
unzip("./data/activity.zip")
#read the file
data <- read.csv("activity.csv")

#convert the date field to Date class and interval field to Factor class.
data$date <- as.Date(data$date, format = "%Y-%m-%d")
data$interval <- as.factor(data$interval)
```

## What is mean total number of steps taken per day?

### Calculate the total number of steps taken per day

```r
stepsTotalPerDay <- aggregate(steps ~ date, data, sum)
colnames(stepsTotalPerDay) <- c("date","steps")
head(stepsTotalPerDay)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

### Make a histogram of the total number of steps taken per day

```r
STPD <- stepsTotalPerDay$steps
hist(STPD, main = "Histogram of number of steps per day", 
xlab = "Number of steps per day", ylab = "Frequency", col = "blue")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

### Calculate the mean and median of the number of steps taken per day.

```r
STPD_mean   <- mean(STPD, na.rm=TRUE)
STPD_median <- median(STPD, na.rm=TRUE)
```

## What is the average daily activity pattern?

#### Calculate the aggregation of steps by intervals of 5-minutes and convert the intervals as integers and save them in a data frame called StepsPerInterval

```r
StepsPerInterval <- aggregate(data$steps, 
                              by = list(interval = data$interval),
                              FUN=mean, na.rm=TRUE)
#convert to integers
StepsPerInterval$interval <- 
     as.integer(levels(StepsPerInterval$interval)[StepsPerInterval$interval])
colnames(StepsPerInterval) <- c("interval", "steps")
```

### Plot the average number of steps taken (averaged across all days) versus the 5-minute intervals

```r
plot(StepsPerInterval, 
     type = "l", 
     xlab = "5-minute Interval", 
     ylab = "Average number of steps taken", 
     main = "Avg number of steps across 5-minute interval")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

### Find the 5-minute interval with the containing the maximum number of steps:

```r
interval_maxsteps <- StepsPerInterval[which.max(StepsPerInterval$steps),]
```

## Imputing missing values

### Total number of missing values:

```r
Totalmissingvals <- sum(is.na(data$steps))
```

### Strategy for filling in all of the missing values in the dataset

```r
missingfill <- function(actdata, stprinterval) {
     na_index <- which(is.na(actdata$steps))
     na_replace <- unlist(lapply(na_index, FUN=function(idx){
          interval = actdata[idx,]$interval
          stprinterval[stprinterval$interval == interval,]$steps
     }))
     fill_steps <- actdata$steps
     fill_steps[na_index] <- na_replace
     fill_steps
}

data_fill <- data.frame(  
     steps = missingfill(data, StepsPerInterval),  
     date = data$date,  
     interval = data$interval)
str(data_fill)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```

### We check that are there any missing values remaining or not

```r
sum(is.na(data_fill$steps))
```

```
## [1] 0
```

### A histogram of the total number of steps taken each day

```r
fill_StepsPerDay <- aggregate(steps ~ date, data_fill, sum)
colnames(fill_StepsPerDay) <- c("date","steps")
hist(fill_StepsPerDay$steps, breaks = 6, main = "Total number of steps taken per day", 
     xlab = "Number of steps per day", ylab = "Frequency", col = "red")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 

### Compute the new mean and median

```r
fill_STPDmean   <- mean(fill_StepsPerDay$steps, na.rm=TRUE)
fill_STPDmedian <- median(fill_StepsPerDay$steps, na.rm=TRUE)
```

## What is the impact of imputing missing data on the estimates of the total daily number of steps?

### Create a factor variable with two levels (weekday, weekend)

```r
weekdays_steps <- function(actdata) {
     weekdays_steps <- aggregate(actdata$steps, by=list(interval = actdata$interval),
                                 FUN=mean, na.rm=T)
     # convert to integers for plotting
     weekdays_steps$interval <- 
          as.integer(levels(weekdays_steps$interval)[weekdays_steps$interval])
     colnames(weekdays_steps) <- c("interval", "steps")
     weekdays_steps
}

data_by_weekdays <- function(actdata) {
     actdata$weekday <- 
          as.factor(weekdays(actdata$date)) # weekdays
     weekend_data <- subset(actdata, weekday %in% c("Saturday","Sunday"))
     weekday_data <- subset(actdata, !weekday %in% c("Saturday","Sunday"))
     
     weekend_steps <- weekdays_steps(weekend_data)
     weekday_steps <- weekdays_steps(weekday_data)
     
     weekend_steps$dayofweek <- rep("weekend", nrow(weekend_steps))
     weekday_steps$dayofweek <- rep("weekday", nrow(weekday_steps))
     
     data_by_weekdays <- rbind(weekend_steps, weekday_steps)
     data_by_weekdays$dayofweek <- as.factor(data_by_weekdays$dayofweek)
     data_by_weekdays
}

data_weekdays <- data_by_weekdays(data_fill)
```

### Plot the number of steps by weekdays vs weekend

```r
stepsByDayType <- aggregate(steps ~ interval + dayofweek, data = data_weekdays, mean)
xyplot(steps ~ interval | dayofweek, stepsByDayType, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png) 


Enter file contents here
