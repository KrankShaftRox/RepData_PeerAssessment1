---
title: "Reproducible Research Project 1"
author: "Ajay Menon"
date: "28/05/2020"
output:
  pdf_document: default
  html_document: default
 keep_md:true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Assignment Process

The following details are provided in this repository for the course "Reproducible Research":

1. Code for reading in the dataset and/or processing the data
2. Histogram of the total number of steps taken each day
3. Mean and median number of steps taken each day
4. Time series plot of the average number of steps taken
5. The 5-minute interval that, on average, contains the maximum number of steps
6. Code to describe and show a strategy for imputing missing data
7. Histogram of the total number of steps taken each day after missing values are imputed
8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

### Step 1
### Code for reading in the dataset and/or processing the data

The main aim is to read the data i, using read.csv() command, and then formatting the data so that it is in a suitable form for analysis. To store the data for further use, we create a variable outdat. It is important to check where one's working directory is prior to this step. We will also load all required packages into the system.
```{r, echo = TRUE}
outdat <- read.csv("activity.csv", header = TRUE)
```

Using this command, we have loaded in the required dataset from the downloadable package. And now, we will load the packages.
```{r}
library(dplyr)
library(ggplot2)
library(lubridate)
```

Now, we observe certain basic parameters of the data, and the classes of each column in the data frame.
```{r}
dim(outdat)
head(outdat)
names(outdat)
sapply(outdat, class) #To observe the classes of each column of the data frame.
```

Now, we will format the column with dates,  so that it becomes of class "date". To perform this, we use the package "lubridate".
```{r}
outdat$date <- ymd(outdat$date)
class(outdat$date) #You will notice that the output is now changed to the date class.
```

### Step 2
### Histogram of the total number of steps taken each day

The next step is to analyze the steps taken per day, and then plot it in a histogram. For this purpose, we will use tapply to find the number of steps, when the steps is categorized on the basis of the date. Then, we will combine the data into a data frame using cbind().
```{r}
TotalSteps <- tapply(outdat$steps, outdat$date, sum, na.rm = TRUE)
Date <- unique(outdat$date)
df <- data.frame()
df <- as.data.frame(cbind(TotalSteps, Date))
```

We will next plot a histogram using the gplot2 package that has the ggplot()function. This plot will be saved to a png format file, labelled "plot1.png".
```{r}
png("plot1.png")
ggplot(df, aes(x = as.numeric(Date), y = TotalSteps)) + geom_bar(stat = "identity") + ylab("Total Steps") + xlab("Date") + ggtitle("Total Steps by date")
dev.off()
ggplot(df, aes(x = as.numeric(Date), y = TotalSteps)) + geom_bar(stat = "identity") + ylab("Total Steps") + xlab("Date") + ggtitle("Total Steps by date")
```


![plot1](F:\Projects_R/plot1.png)

### Step 3
### Mean and median number of steps taken each day

The next step is to determine the mean and median number of steps per day. To accomplish this, we will create a data frame with 3 columns: (i) Date (ii) Mean steps (iii) Median steps.
```{r}
MeanSteps <- TotalSteps/(nrow(outdat)/length(unique(outdat$date)))
MedianSteps <- tapply(outdat$steps, outdat$date, median, na.rm = TRUE)
MeanMedSteps <- as.data.frame(cbind(Date, MeanSteps))
MeanMedSteps <- as.data.frame(cbind(MeanMedSteps, MedianSteps))
```

Now, we will check if the data is properly given, by obtaining the first 6 values of the data frame.
```{r}
head(MeanMedSteps)
```

We have now obtained the mean and median values for the outdat data frame.

### Step 4
### Time series plot of the average number of steps taken

Now, the next step is to construct a plot of the average steps taken every day. For this, we will use the ggplot() function with a geom_bar() geometric specifier.

```{r}
png("plot2.png")
ggplot(MeanMedSteps, aes(x = Date, y = MeanSteps)) + geom_bar(stat = "identity") + ylab("Average Steps") + xlab("Date") + ggtitle("Time Series of Average Steps by date")
dev.off()
ggplot(MeanMedSteps, aes(x = Date, y = MeanSteps)) + geom_bar(stat = "identity") + ylab("Average Steps") + xlab("Date") + ggtitle("Time Series of Average Steps by date")
```

Thus, we have obtained a time series plot of the average steps taken every day.

### Step 5
### The 5-minute interval that, on average, contains the maximum number of steps

Now, the next step is to find the average interval on which the maximum steps have been covered. To find this, we will use tapply() to determine the mean steps covered in each interval. Then we find the unique number of such intervals, and then obtain the interval with the maximum steps.

```{r}
IntSteps <- tapply(outdat$steps, outdat$interval, mean, na.rm = TRUE)
Ints <- unique(outdat$interval)
maxInts <- as.data.frame(cbind(Ints, IntSteps))
png("plot3.png")
ggplot(maxInts, aes(x = Ints, y = IntSteps)) + geom_line() + ylab("Average Steps") + xlab("Interval") + ggtitle("Time Series of Average Steps by interval")
dev.off()
maxInts <- arrange(maxInts, desc(IntSteps))
maxInts[1, 1]
ggplot(maxInts, aes(x = Ints, y = IntSteps)) + geom_line() + ylab("Average Steps") + xlab("Interval") + ggtitle("Time Series of Average Steps by interval")
```

We can observe the output is the interval with the highest average number of steps, for all the dates given in the data. We also obtain a time series curve of the avergae number of steps covered for every interval, over the course of every day.

### Step 6
### Code to describe and show a strategy for imputing missing data

The simplest method to impute missing values in the dataset is by replacing all the NA values with the mean or average of that particular interval and day. For this process, we will compute the average of 2 quantities- the average steps per day, and the average steps per interval.
```{r}
outdat2 <- outdat
outdat2$IntervalAverage <- rep(IntSteps, times = 61)
outdat2$DailyAverage <- rep(MeanSteps, each = 288)
outdat2$ReplAvg <- outdat2$IntervalAverage + outdat2$DailyAverage
outdat2$steps <- ifelse(is.na(outdat2$steps), outdat2$ReplAvg, outdat2$steps)
```

Thus, using this strategy, we have successfully replaced every NA with the corresponding average of the interval and daily mean. This is very accurate, as it represents both an interval and daily mean value.

### Step 7
### Histogram of the total number of steps taken each day after missing values are imputed

Now, having corrected and imputed missing values into the dataset, we will obtain a plot of the total number of steps taken per day, while taking into account the imputations. For this process, we will create a new data frame on which to plot the values.
```{r}
df2 <- as.data.frame(cbind(Date, tapply(outdat2$steps, outdat2$date, sum)))
names(df2) <- c("Date", "TotalSteps2")
png("plot4.png")
ggplot(df2, aes(x = as.numeric(Date), y = TotalSteps2)) + geom_bar(stat = "identity") + ylab("Total Steps") + xlab("Date") + ggtitle("Total Steps after Imputation by date")
dev.off()
ggplot(df2, aes(x = as.numeric(Date), y = TotalSteps2)) + geom_bar(stat = "identity") + ylab("Total Steps") + xlab("Date") + ggtitle("Total Steps after Imputation by date")
```

Thus, we obtain the required graph that depicts how the value has changed with the missing value corrections. The new dataset is outdat2 that has the same dataset, but with all missing values filled in.

We also need to calculate the mean and median steps for this new corrected dataset. This is accomplished as shown below.
```{r}
MeanSteps2 <- df2$TotalSteps2/(nrow(outdat2)/length(unique(outdat2$date)))
MedianSteps2 <- tapply(outdat2$steps, outdat2$date, median, na.rm = TRUE)
MeanMedSteps2 <- as.data.frame(cbind(Date, MeanSteps2))
MeanMedSteps2 <- as.data.frame(cbind(MeanMedSteps2, MedianSteps2))
```

Now, we will check if the data is properly given, by obtaining the first 6 values of the data frame.
```{r}
head(MeanMedSteps2)
```

We have now obtained the mean and median values for the outdat2 corrected data frame.

### Step 8
### Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

Now, the final step is to compare the average steps taken on a regular weekday (spanning Monday to Friday) and a weekend (spanning Saturday to Sunday). For this, we will have to classify the dates as either weekdays or weekends using date commands.
```{r}
outdat2$IntervalAverage <- NULL
outdat2$DailyAverage <- NULL
outdat2$ReplAvg <- NULL
outdat2$DayFactor <- ifelse(weekdays(outdat2$date) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "Weekday", "Weekend")
outdat3 <- filter(outdat2, outdat2$DayFactor == "Weekday")
df3 <- as.data.frame(cbind(unique(outdat3$interval), tapply(outdat3$steps, outdat3$interval, mean)))
df3$DayFactor <- rep("Weekday", times = length(unique(outdat3$interval)))
names(df3) <- c("Interval", "Average", "DayFactor")
outdat4 <- filter(outdat2, outdat2$DayFactor == "Weekend")
df4 <- as.data.frame(cbind(unique(outdat4$interval), tapply(outdat4$steps, outdat4$interval, mean)))
df4$DayFactor <- rep("Weekend", times = length(unique(outdat4$interval)))
names(df4) <- c("Interval", "Average", "DayFactor")
df5 <- as.data.frame(rbind(df3, df4))
png("plot5.png")
ggplot(df5, aes(x = Interval, y = Average)) + geom_line() + ylab("Average Steps") + xlab("Interval") + ggtitle("Total Steps on Weekday and Weekends") + facet_grid(DayFactor ~ ., labeller = label_both)
dev.off()
ggplot(df5, aes(x = Interval, y = Average)) + geom_line() + ylab("Average Steps") + xlab("Interval") + ggtitle("Total Steps on Weekday and Weekends") + facet_grid(DayFactor ~ ., labeller = label_both)
```

And now, we have obtained the panel plot that compares the average steps between weekdays and weekends.

### Conclusion

Using these various graphs and plots, we have obtained a complete understanding of the data given by various organizations such as FitBit, Nike etc. This document has been typed in RMarkdown (saved with extension .Rmd) and converted to an html file (saved with file extension .htm or .html). This serves as completion of the first Course Project on the Coursera website, for the course Reproducible Research conducted by the John Hopkins University. I hope this document has been concise and provided the reader with the necessary understanding required to document this project. This is original work, so I would appreciate if there were no attempts at either copying or plagiarism. Cheers!
