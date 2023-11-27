---
title: "Coursera: rmarkdown; Reproducible Research"
author: "Reza A.Khoei"
date: "2023-11-27"
output: 
  html_document:
    keep_md: true
---



# Loading and preprocessing the data

First of all, we need to import the data. To do so, we called the **readr** package and then used **read_csv()** function. As well, we called **tidyverse package** to do some pre-processing.


```r
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.4     ✔ readr     2.1.4
## ✔ forcats   1.0.0     ✔ stringr   1.5.1
## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
## ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
## ✔ purrr     1.0.2     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

```r
library(readr)
activity <- read_csv("activity.csv")
```

```
## Rows: 17568 Columns: 3
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## dbl  (2): steps, interval
## date (1): date
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

# Histogram of the total number of steps taken each day


```r
Activity = na.omit(activity)
group_sum <- activity %>%
     group_by(date) %>%
     summarise_at(vars(steps),
                  list(Sum_Frequency = sum))
group_sum
```

```
## # A tibble: 61 × 2
##    date       Sum_Frequency
##    <date>             <dbl>
##  1 2012-10-01            NA
##  2 2012-10-02           126
##  3 2012-10-03         11352
##  4 2012-10-04         12116
##  5 2012-10-05         13294
##  6 2012-10-06         15420
##  7 2012-10-07         11015
##  8 2012-10-08            NA
##  9 2012-10-09         12811
## 10 2012-10-10          9900
## # ℹ 51 more rows
```

```r
ggplot(group_sum, aes(x=Sum_Frequency)) +   
     geom_histogram(color="black", fill="white") 
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```
## Warning: Removed 8 rows containing non-finite values (`stat_bin()`).
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

# Mean number of steps taken each day

Mean of the total number of steps taken per day is shown below.


```r
group_mean <- Activity %>%
    group_by(date) %>%
    summarise_at(vars(steps),
                 list(Mean = mean))
group_mean
```

```
## # A tibble: 53 × 2
##    date         Mean
##    <date>      <dbl>
##  1 2012-10-02  0.438
##  2 2012-10-03 39.4  
##  3 2012-10-04 42.1  
##  4 2012-10-05 46.2  
##  5 2012-10-06 53.5  
##  6 2012-10-07 38.2  
##  7 2012-10-09 44.5  
##  8 2012-10-10 34.4  
##  9 2012-10-11 35.8  
## 10 2012-10-12 60.4  
## # ℹ 43 more rows
```

# median number of steps taken each day

Median of the total number of steps taken per day is shown below.


```r
group_median <- Activity %>%
    group_by(date) %>%
    summarise_at(vars(steps),
                 list(Median = median))
group_median
```

```
## # A tibble: 53 × 2
##    date       Median
##    <date>      <dbl>
##  1 2012-10-02      0
##  2 2012-10-03      0
##  3 2012-10-04      0
##  4 2012-10-05      0
##  5 2012-10-06      0
##  6 2012-10-07      0
##  7 2012-10-09      0
##  8 2012-10-10      0
##  9 2012-10-11      0
## 10 2012-10-12      0
## # ℹ 43 more rows
```

# Time series plot of the average number of steps taken

We used **ggplot** function to plot time series of the average number of steps taken.


```r
ggplot(group_mean, aes(x = date, y = Mean)) +
    geom_line() +
    geom_point() + 
    ylab("average number of steps taken") +
    xlab("the 5-minute interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

# The 5-minute interval that, on average, contains the maximum number of steps


```r
group_interval <- Activity %>%
    group_by(interval) %>%
    reframe(steps)
max(group_interval)
```

```
## [1] 2355
```

```r
which.max(group_interval$steps)
```

```
## [1] 4026
```

We see that the maximum number of steps is 2355 that belong to row 4633.

# Code to describe and show a strategy for imputing missing data

To calculate the total number of missing values in the dataset, we used the below command.


```r
missing <- sum(is.na(activity$steps))
missing
```

```
## [1] 2304
```

We see that 2304 observations are missing value.

Now, we use mean values to replace them with missing values.


```r
ImputedData <- activity
for (i in 1:nrow(ImputedData)) {
    if (is.na(ImputedData$steps)[i]) {
        # Find the index value for when the interval matches the average
        ndx <- which(ImputedData$interval[i] ==  
        activity$interval)
        # Assign the value to replace the NA
        ImputedData$steps[i] <- group_mean[ndx,]$Mean
    }
}
```

# Histogram of the total number of steps taken each day after missing values are imputedS

Again, we used ggplot function to make a histogram of the total number of steps taken each day. 


```r
group_sum <- na.omit(ImputedData) %>%
    group_by(date) %>%
    summarise_at(vars(steps),
                 list(Sum_Frequency = sum))
group_sum
```

```
## # A tibble: 61 × 2
##    date       Sum_Frequency
##    <date>             <dbl>
##  1 2012-10-01         1981.
##  2 2012-10-02          126 
##  3 2012-10-03        11352 
##  4 2012-10-04        12116 
##  5 2012-10-05        13294 
##  6 2012-10-06        15420 
##  7 2012-10-07        11015 
##  8 2012-10-08         1981.
##  9 2012-10-09        12811 
## 10 2012-10-10         9900 
## # ℹ 51 more rows
```

```r
ggplot(group_sum, aes(x=Sum_Frequency)) +   
    geom_histogram(color="black", fill="white") + xlab("Total  
    number of steps taken each day")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

Now, we Calculate and report the mean and median total number of steps taken per day. We see that the NA values are replaced with mean and median values which could give us a idea about missing values. 


```r
group_mean <- na.omit(ImputedData) %>%
    group_by(date) %>%
    summarise_at(vars(steps),
                 list(Mean = mean))
group_mean
```

```
## # A tibble: 61 × 2
##    date         Mean
##    <date>      <dbl>
##  1 2012-10-01 37.4  
##  2 2012-10-02  0.438
##  3 2012-10-03 39.4  
##  4 2012-10-04 42.1  
##  5 2012-10-05 46.2  
##  6 2012-10-06 53.5  
##  7 2012-10-07 38.2  
##  8 2012-10-08 37.4  
##  9 2012-10-09 44.5  
## 10 2012-10-10 34.4  
## # ℹ 51 more rows
```

```r
group_median <- na.omit(ImputedData) %>%
    group_by(date) %>%
    summarise_at(vars(steps),
                 list(Median = median))
group_median
```

```
## # A tibble: 61 × 2
##    date       Median
##    <date>      <dbl>
##  1 2012-10-01   37.4
##  2 2012-10-02    0  
##  3 2012-10-03    0  
##  4 2012-10-04    0  
##  5 2012-10-05    0  
##  6 2012-10-06    0  
##  7 2012-10-07    0  
##  8 2012-10-08   37.4
##  9 2012-10-09    0  
## 10 2012-10-10    0  
## # ℹ 51 more rows
```

We used the **weekdays** function to extract  the days of the weeks and the create the new variable to define weekdays and weekends.


```r
ImputedData$day <- weekdays(ImputedData$date)
ImputedData$daytype <- "weekday"
ImputedData$daytype[ImputedData$day %in% c("Saturday", "Sunday")] <- "weekend"
dayaverage <- na.omit(ImputedData) %>%
    group_by(daytype, interval) %>%
    summarize(AverageSteps=mean(steps))
```

```
## `summarise()` has grouped output by 'daytype'. You can override using the
## `.groups` argument.
```

Now, we can make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
qplot(interval, AverageSteps, data=dayaverage,
      type="l",geom="line",xlab="Interval",
      ylab="Number of Steps (Average)",
      main="Average steps taken Weekends vs. Weekdays",
      facets =daytype ~ .)
```

```
## Warning: `qplot()` was deprecated in ggplot2 3.4.0.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

```
## Warning in geom_line(type = "l"): Ignoring unknown parameters: `type`
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
