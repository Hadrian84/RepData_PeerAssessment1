---
title: "Data Science: Reproducible Research: Peer assignment 1"
output: html_document
---

## Introduction
In this assignment we make use of a personal activity monitoring device which collects data at 5 minute intervals throughout the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and includes the number of steps taken in 5 minute intervals each day.

### Loading and preprocessing the data
Before we read in the dataset to RStudio, we first check whether the .csv file exists in our working directory. If not, we have to download the data file first from the internet and unzip the file.

```r
setwd("C:/Users/u0058947/Documents/")

if (!file.exists("activity.csv")) {
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",destfile = "C://Users/u0058947/Documents/repdata-data-activity.zip")

unzip(zipfile = "repdata-data-activity.zip",overwrite = TRUE)
}
```

The dataset can subsequently be loaded into RStudio with the **read.csv()** command. We will call the dataset *repdata*. Including other arguments such as *header* or *sep* are not necessary as the default settings suffice.


```r
repdata = read.csv("activity.csv")
```

We can have a quick look at the data with the **summary()** command. As the name of the command suggests, it provides a quick summary of all the variables within the dataset.


```r
summary(repdata)
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

We can conclude so far that the dataset has 2 numeric variables (steps and time) and 1 factor variable (date). Furthermore, the variable steps seems to have some missing values (NA: 2304).

In all other perspectives, the dataset seems suitable for our subsequent analyses.

### What is the mean total number of steps taken per day?

To calculate the total number of steps taken per day, we can resort to the **tapply()** command. This quickly enables to construct a table with the sum of all steps taken per day. This data will be saved in the table *dayTotal*.


```r
dayTotal = tapply(repdata$steps,repdata$date,sum)
```

The distribution of the total number of steps taken for each day can be displayed with the **hist()** command. The produced histogram shows that daily total number of steps is approximately normally distributed.  


```r
hist(dayTotal, 
     main="Distribution of daily personal activity",
     xlab="Daily total number of steps")
```

![plot of chunk hist1](figure/hist1-1.png) 

Finally, we can calculate the mean daily total number of steps taken...


```r
round(mean(dayTotal,na.rm=TRUE),digit=0)
```

```
## [1] 10766
```

... as well as the median of the total number of steps taken per day.


```r
round(median(dayTotal,na.rm=TRUE),digit=0)
```

```
## [1] 10765
```

As the values of the mean and median are almost on top of each other, we can conclude from  this that the values are normally distributed.

### What is the average daily activity pattern?
To visualize the average daily activity pattern, we will use the **tapply()** command to construct a table (*dayPattern*) with the average steps taken per interval across all days. Afterwards we use this data to make a line plot.


```r
dayPattern = tapply(repdata$steps,repdata$interval,mean, na.rm=TRUE)

plot(levels(as.factor(repdata$interval)),dayPattern,
     type="l",xlab="Interval",ylab="Number of steps taken",
     main="Average day pattern")
```

![plot of chunk line plot](figure/line plot-1.png) 

In this line plot, we clearly see that activity peaks at a particular interval. Namely, during this interval, approx. 200 steps were noted. The code below allows to find the interval of interest (and the actual number of steps taken).


```r
dayPattern[max(dayPattern)]
```

```
##     1705 
## 56.30189
```

### Imputing missing values
Earlier we have found that there were quite some NA values in the dataset using the **summary()** command. Alternatively, we could directly search for number of cells that contain NA values with the **is.na()** command.


```r
sum(is.na(repdata))
```

```
## [1] 2304
```

```r
prop = round(sum(is.na(repdata))/nrow(repdata)*100,digit=2)
```

To put it into another perspective, approximately 13.11 % of the dataset contains NA values.

A way to remedy this caveat is to replace NA values by either (1) average values of other intervals for that day or (2) average values of identical intervals of other days. The first option is not possible, as NA values are not during sporadic intervals within a day, but are across the entire day.

To evaluate this, we constructed a new dataset where every row is a single day (columns are different intervals; *dayRow*). The **is.na()** command evaluates which cells contain NA values. Afterwards, the **rowMeans()** command shows what proportion of that particular day contains NA values. To summarize, we constructed a table which shows that 53 days are 100% NA-free, and 8 days 100%  contaminated with NA values. There are no days that contain some NA values.


```r
dayRow = as.data.frame(matrix(numeric(17568),nrow=61))
for (i in 1:61) dayRow[i,] = repdata[repdata$date==levels(repdata$date)[i],1]

NAvalues = is.na(dayRow)
table(rowMeans(NAvalues))
```

```
## 
##  0  1 
## 53  8
```

We will use our previously constructed dataset (*dayRow*) to replace NA values by average values of identical intervals of other days. These average values were already calculated and inserted in a table we have constructed earlier (*dayPattern*). Additionally, we know which days contain NA values.


```r
which(rowMeans(NAvalues)==1)
```

```
## [1]  1  8 32 35 40 41 45 61
```

```r
w = which(rowMeans(NAvalues)==1)
for (i in 1:length(w)) dayRow[w[i],] = dayPattern
```

Now, we can recreate a new dataset *repdata_new*, which is equal to the original dataset but with the missing data filled in.


```r
dayRow=as.matrix(dayRow)
dayRow=t(dayRow)
dayRow=as.data.frame(dayRow)
dayRow=stack(dayRow)
repdata_new=repdata
repdata_new[,1]=round(dayRow$values,digit=2)
summary(repdata_new)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 27.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##                   (Other)   :15840
```

Now with this new dataset, we can also construct a histogram to visualize the distribution of the total number of steps taken for each day. We can now compare the two histograms. 


```r
dayTotal_new = tapply(repdata_new$steps,repdata_new$date,sum)

par(mfrow=c(1,2))
hist(dayTotal, 
     main="Histogram 1\nwith NA values",
     xlab="Daily total number of steps",ylim=c(0,35))
abline(v=median(dayTotal,na.rm=TRUE),col="red")
abline(v=mean(dayTotal,na.rm=TRUE),col="blue")
hist(dayTotal_new, 
     main="Histogram 2\nwith imputed NA values",
     xlab="Daily total number of steps",ylim=c(0,35))
abline(v=median(dayTotal_new),col="red")
abline(v=mean(dayTotal_new),col="blue")
legend("topright",legend = c("median","mean"),lty=1,col = c("red","blue"))
```

![plot of chunk hist2](figure/hist2-1.png) 

When examining the median and mean of both histograms, no significant changes can be detected. In both histograms, median and mean values are on top of each other, indicative for a normal distribution...


```r
sd1 = round(sd(dayTotal,na.rm=TRUE),digit=0)
sd2 = round(sd(dayTotal_new),digit=0)
```

... Furthermore, the distribution in the second histogram has a smaller spread than the first histogram (sd histogram 1: 4269; sd histogram 2: 3974). This is logical as the average population values replacing the NA values inflate the current values. 

### Are there differences in activity patterns between weekdays and weekends?

```r
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

To be able to answer this question, we first create a new factor variable with two levels, namely "weekday" and "weekend".


```r
repdata_new$weekdays = weekdays(as.Date(repdata$date))
repdata_new$weekdays = factor(repdata_new$weekdays)
levels(repdata_new$weekdays) = list(weekday=c("Monday","Tuesday","Wednesday","Thursday","Friday"),
                                    weekend=c("Saturday","Sunday"))
```

Subsequently, we make two seperate datasets containing either "weekday" data or "weekend" data.


```r
weekday_ss = repdata_new[repdata_new$weekdays=="weekday",]
weekend_ss = repdata_new[repdata_new$weekdays=="weekend",]
```

Using a similar principle as earlier, we use the **tapply()** command to make a table containing the average daily pattern for either "weekday" or "weekend". We will visualize the data in two seperate time series subplots. These contain data over every 5-minute interval of either an average "weekday" or "weekend".


```r
weekTotal = tapply(weekday_ss$steps,weekday_ss$interval,mean)
weekendTotal = tapply(weekend_ss$steps,weekend_ss$interval,mean)

par(mfrow=c(2,1))
plot(weekTotal, 
     main="weekday",xlab="",ylab="Number of steps",
     type="l",ylim=c(0,250))
plot(weekendTotal, 
     main="weekend",xlab="Interval",ylab="Number of steps",
     type="l",ylim=c(0,250))
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png) 

The daily activity pattern for either "weekday" or "weekend" have a similar pattern, though weekdays are generally more active than weekends around the 100th interval.
