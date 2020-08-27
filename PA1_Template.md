`{r setup, include=FALSE} knitr::opts_chunk$set(echo = TRUE)`

Introduction
------------

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a Fitbit, Nike
Fuelband, or Jawbone Up. These type of devices are part of the
“quantified self” movement – a group of enthusiasts who take
measurements about themselves regularly to improve their health, to find
patterns in their behavior, or because they are tech geeks. But these
data remain under-utilized both because the raw data are hard to obtain
and there is a lack of statistical methods and software for processing
and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012 and
include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data \[52K\] The variables included in this
dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are
coded as NA) date: The date on which the measurement was taken in
YYYY-MM-DD format interval: Identifier for the 5-minute interval in
which measurement was taken The dataset is stored in a
comma-separated-value (CSV) file and there are a total of 17,568
observations in this dataset.

Part 1: Code for reading in the dataset and/or processing the data
------------------------------------------------------------------

``` {r}
linkdt<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
comp<-getwd()
download.file(linkdt,destfile = file.path(comp,"projet51.zip"))
unzip(zipfile="projet51.zip")
dt<-read.csv(file.path(comp,"activity.csv"))
```

Part 2: Histogram of the total number of steps taken each day
-------------------------------------------------------------

If you do not understand the difference between a histogram and a
barplot, research the difference between them. Make a histogram of the
total number of steps taken each day

``` {r}
dtq2<-as.data.frame(as.table((tapply(dt$steps,dt$date,sum)),ncol=2))
colnames(dtq2)<-c("Date","Steps")
hist(dtq2$Steps,breaks = nrow(dtq2))
```

Part 3: Mean and median number of steps taken each day
------------------------------------------------------

``` {r}
meanST<-rep(NA,nrow(dtq2))
for (i in 1:nrow(dtq2)){meanST[i]<-dtq2$Steps[i]/sum(dt$date==dtq2$Date[i])}
medianST<-rep(NA,nrow(dtq2))
for (i in 1:nrow(dtq2)){medianST[i]<-median(subset(dt,dt$date==dtq2$Date[i])$steps)}
```

Part 4: Time series plot of the average number of steps taken
-------------------------------------------------------------

``` {r}
timesr<-strptime(dtq2$Date,"%Y-%m-%d")
plot(timesr,meanST,type = "l")
```

Part 5: The 5-minute interval that, on average, contains the maximum number of steps
------------------------------------------------------------------------------------

``` {r}
maxstep<-max(dt$steps,na.rm=T)
check<-dt$steps==maxstep
for (i in 1:length(check)){
  if (is.na(check[i])==T){
    check[i]<-F
    }
}
maxinterval<-dt[check,]$interval
```

Part 6: Code to describe and show a strategy for imputing missing data
----------------------------------------------------------------------

Because the missing data is always missing day by day and there is a lot
of data in a day, I choose 0

``` {r}
sum(is.na(dt$steps))
missingvalue<-dt[is.na(dt$steps),]

for (i in 1:nrow(dt)){
  if (is.na(dt$steps[i])==T){
    dt$steps[i]<-0
  }
}
```

Part 7: Histogram of the total number of steps taken each day after missing values are imputed
----------------------------------------------------------------------------------------------

``` {r}
dtq7<-as.data.frame(as.table((tapply(dt$steps,dt$date,sum)),ncol=2))
colnames(dtq7)<-c("Date","Steps")
hist(dtq7$Steps,breaks = 30)
```

Part 8 :
--------

Create a new factor variable in the dataset with two levels – “weekday”
and “weekend” indicating whether a given date is a weekday or weekend
day.

``` {r}
dt$date<-strptime(dt$date,"%Y-%m-%d")
dt$wk<-weekdays(dt$date)
dt$rs<-NA
for (i in 1:nrow(dt)){
  if (dt$wk[i] %in% c("Monday","Tuesday","Wednesday","Thursday","Friday")){
    dt$rs[i]<-"Weekday"
  } else {
    dt$rs[i]<-"Weekend"
  }
}
library(ggplot2)
ggplot(dt,aes(x=as.factor(interval),y=steps/5,fill =as.factor(rs)))+geom_line()+facet_grid(.~rs)+labs(x="Interval 5 minutes",y="Average Steps",title = "Average Steps by Interval 5 minutes")
```
