---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

```r
knitr::opts_chunk$set(echo = TRUE)
```


## Loading and preprocessing the data

Here we are going to load the data from activity.csv file using csv method, then we are going to take a look at the head


```r
act <- read.csv("activity/activity.csv")
act$date <- as.Date(as.character(act$date) , "%Y-%m-%d")
head(act)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```


Just our of curious we want to know how many days we have and how many intervals per day


```r
length(table(act$date))
```

```
## [1] 61
```

```r
length(act$interval) / length(table(act$date))
```

```
## [1] 288
```

## What is mean total number of steps taken per day?

Here, first, we are going to calculate the total number of steps per day, so we will have some table that has the column names as date of that spesific day, and under each of these dates we will have the total steps walked in that day


```r
steps_day <- aggregate(steps ~ date , FUN = sum,data = act)
head(steps_day)
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

Let's look at the type of data that we produced so far


```r
str(steps_day)
```

```
## 'data.frame':	53 obs. of  2 variables:
##  $ date : Date, format: "2012-10-02" "2012-10-03" ...
##  $ steps: int  126 11352 12116 13294 15420 11015 12811 9900 10304 17382 ...
```


We create a histogram of the data we got


```r
hist(steps_day$steps , main = "Histogram of total steps per day" , xlab = "Total number of steps per day" , ylab = "Freq")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


Calculating:

- Mean of total steps per day:

```r
mean(steps_day$steps)
```

```
## [1] 10766.19
```

- Median of total steps per day:

```r
median(steps_day$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

So, to figure out the daily activity pattern we need two things basically:
- The intervals of each days,let's take a lookt at it

```r
range(act$interval)
```

```
## [1]    0 2355
```
we can see that every day contains 288 intervales, that are distrbuted from 0 - 2355 , so this range will be our x-axis

- Then we need to calculate the average of steps of each intervals accross the days, as we said before each interval of 0-2355 is repeated through all days, and the average of these intervals will be our x-axis

We will also load plyr and dply libraries to use them

```r
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.6.3
```

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.6.3
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
x <- aggregate(steps ~ interval , data = act , FUN = mean)
names(x) <- c("interval" , "avg_step")
f <- join(act , x , by = "interval")
```

So, the code above would just basically create a new dataset, that holds our main dataset -*act*- in addition too a new column that indecate the average of each interval.

Now we simply plot what we have:


```r
plot(f$interval , f$avg_step , main="Average of steps per intervals accross days" , xlab="Intervals" , ylab = "Averaged steps" , type = "l" )
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

Now, we can simply find the interval that correspnds to the maximum value of averaged steps:


```r
f[which.max(f$avg_step) , "interval"]
```

```
## [1] 835
```


## Imputing missing values

Here we are going to generate data for the missing values in our dataset

### Find the total missing values


```r
sum(!complete.cases(act))
```

```
## [1] 2304
```


### Imputing values 

We are going to depend on the mean of the 5-minute interval for each missing value in the dataset, so dispite the date , our startegy will just look at the interval number, if it has a missing value there, then it will fill it with the average of that interval.


```r
j <- f
j[is.na(j$steps) ,]$steps <-  j[is.na(j$steps) ,]$avg_step
new_act <- j[,1:3]
new_step_day<- aggregate(steps ~ date , FUN = sum,data = new_act)
```

Let's now plot our dataset with our new imputed data


```r
hist(new_step_day$steps ,  main ="Histogram of new Imputed data" , xlab = "total steps per day" , ylab = "Freq.")
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

Let's calculate now the mean and the median and see how these two can differ from our previous results with NA's 


```r
mean(new_step_day$steps)
```

```
## [1] 10766.19
```

```r
median(new_step_day$steps)
```

```
## [1] 10766.19
```

We can see the values didn't differ from the previous one, except that we didn't use *na.rm* arrgument



## Are there differences in activity patterns between weekdays and weekends?

Now, we just need to create a new factor or a column that indicates whether each of the dates in the dataset is a weekday or a weekend.  For that one we need to use **dplyr** library and we already loaded it.

First, let me difine two things I'm going to use here:


```r
'%!in%' <- function(x,y)!('%in%'(x,y))
weekends <- c("Saturday" , "Sunday")
```

So, I made a vector of weekends, so when we go through the dataset we can test each date, if it is in the weekends vector or it is not, that's why I alos defined the *not in* function.

Now, we create a new column or factor indicates whether each data is a weekday or a weekend


```r
new_act_week <- mutate(new_act , weeky = factor(case_when(weekdays(date) %in% weekends ~ "end" , 
                                                          weekdays(date) %!in% weekends ~ "day",
                                                           TRUE                ~ NA_character_)))
```

Let's now plot the same thing we ploted before but this time we are going to use *ggplot2* so we can have multipule panels, depending on the *weeky* factor in our dataset:


```r
axx <- aggregate(steps ~ interval + weeky , data = new_act_week , FUN = mean)
```

Let's now plot our data using *ggplot2*


```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.6.3
```

```r
qplot(x = interval , steps , data = axx , facets = weeky~. , main="Time Series plot factorized by day type" , xlab = "Intervals" , ylab = "Averaged steps per interval") + geom_line(stat = "identity", aes(colour = weeky))
```

![](PA1_template_files/figure-html/unnamed-chunk-20-1.png)<!-- -->
