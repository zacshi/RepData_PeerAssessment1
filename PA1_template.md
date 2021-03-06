---
title: "Project One for Reproducible Research"
date: "7/21/2017"
output: html_document
---



## Introduction

This is a single R markdown document that can be processed by knitr and be transformed into an HTML file in fulfiling the assignment in week 2. THe analysis steps were described in multiple parts. Answers to all the questions were detailed below.The related items were also commented inside the R code blocks. 


## A: Loading and preprocessing the data


```r
# Code for reading in the dataset and/or processing the data 
library("tidyverse")
library("lubridate")
activity <- read.csv("./activity/activity.csv")
```

## B: What is mean total number of steps taken per day?


```r
# Calculate the total number of steps taken per day
TotalStepsPerDay <- tapply(activity$steps, activity$date, sum, na.rm=TRUE)
```


Make a histogram of the total number of steps taken each day


```r
# Histogram of the total number of steps taken each day
qplot(TotalStepsPerDay, xlab='Total Steps Per Day', ylab='Frequency', binwidth=600)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)


## C: What is the average daily activity pattern?

Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis). 


```r
#making a time series plot
averageStepsPerTimeBlock <- aggregate(x=list(meanSteps=activity$steps), by=list(interval=activity$interval), FUN=mean, na.rm=TRUE)
library(ggplot2)
ggplot(data=averageStepsPerTimeBlock, aes(x=interval, y=meanSteps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken") 
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)



Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
mostSteps <- which.max(averageStepsPerTimeBlock$meanSteps)
timeMostSteps <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1\\2", averageStepsPerTimeBlock[mostSteps,'interval'])
# So maximum number of steps is
timeMostSteps
```

```
## [1] "835"
```

Calculate and report the mean and median of the total number of steps taken per day


```r
# Mean
stepsByDayMean <- mean(TotalStepsPerDay)
stepsByDayMean
```

```
## [1] 9354.23
```

```r
# Median
stepsByDayMedian <- median(TotalStepsPerDay)
stepsByDayMedian
```

```
## [1] 10395
```



## D: Imputing missing values

The presence of missing days may introduce bias into some calculations or summaries of the data. Calculate and report the total number of missing values in the dataset. I used the impute and mean functions to create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
#Calulate and report the total number of missing values in the dataset
missing <- is.na(activity$steps)
#total number of missing values
table(missing)
```

```
## missing
## FALSE  TRUE 
## 15264  2304
```

```r
# Create a new dataset that is equal to the original dataset but with the missing data filled in.
library(scales)
library(Hmisc)
activityImputed <- activity
activityImputed$steps <- impute(activity$steps, fun=mean) # computing the (single) imputed value from the non-NAs
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.


```r
# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day
stepsByDayImputed <- tapply(activityImputed$steps, activityImputed$date, sum)
qplot(stepsByDayImputed, xlab='Total Steps Per Day from Imputed Dataset', ylab='Frequency', binwidth=600)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

```r
# Calculate and report the mean and median total number of steps taken per day.
#Mean
(stepsByDayMeanImputed <- mean(stepsByDayImputed))
```

```
## [1] 10766.19
```

```r
#Median
(stepsByDayMedianImputed <- median(stepsByDayImputed))
```

```
## [1] 10766.19
```


Results from the imputed data are different from the first part of this assignment. 


## E: Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – “weekdays” and “weekends” 


```r
# Create a new factor variable in the dataset with two levels – “weekdays” and “weekends” 
activityImputed$Type <-  ifelse(as.POSIXlt(activityImputed$date)$wday %in% c(0,6), 'weekends', 'weekdays')
# Make a panel plot containing a time series plot
averagedActivityImputed <- aggregate(steps ~ interval + Type, data=activityImputed, mean)
ggplot(averagedActivityImputed, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(. ~ Type) +
    xlab("5-minute interval") + 
    ylab("Avarage Number of Steps") +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(title = "Comparison of the Parterns between Weekdays and Weekends")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)



