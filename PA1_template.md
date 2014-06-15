
Assignement One Rmd
==========================
Part 1  Loading and preprocessing the data
------------------------------------------
#### Question 1: Load the data 

```r
#loading data
data <- read.csv("activity.csv")
data$date <- as.Date(data$date)
```
#### Question 2: Process/transform the data (if necessary) into a format suitable for your analysis

```r
#get rid of na data for the drist part
dataNoNa <- na.omit(data)
```
______________________________________________________________________
Part 2 What is mean total number of steps taken per day?
--------------------------------------------------------
#### Question 1: Make a histogram of the total number of steps taken each day

```r
numOfSteps <- tapply(dataNoNa$steps, dataNoNa$date, sum)
hist(numOfSteps)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

#### Question 2: Calculate and report the mean and median total number of steps taken per day 

```r
mn <- mean(tapply(dataNoNa$steps, dataNoNa$date, sum))
md <- median(tapply(dataNoNa$steps, dataNoNa$date, sum))
```

The mean number of steps is  1.0766 &times; 10<sup>4</sup> per day  
The median number of steps is 10765 per day  
  
_______________________________________________________________
    
Part 3 What is the average daily activity pattern?
--------------------------------------------------
#### Question 1: Make a time series plot  

```r
plot(tapply(dataNoNa$steps, dataNoNa$date, mean), type="l")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

#### Qustion 2: Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
mostSteps <- dataNoNa[dataNoNa$steps == max(dataNoNa$steps),,]
```

The most steps in any one interval is 615 with 806 steps on 2012-11-27.  
_______________________________

Part 4 Imputing missing values
-------------------------------------------------------
#### Question 1: Calculate and report the total number of missing values in the dataset

```r
naCount <- dim(data)[1] - dim(dataNoNa)[1]
```
There are 2304 missing values in the source data.  

#### Question 2: Devise a strategy for filling in all of the missing values in the dataset. 
I decided to go with the average value across the entire dataset rounded down.  

#### Question 3: Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
#First i need to caculate the daily step averages
stepDayAve <- round(ave(dataNoNa$steps)[1])
```
this value is 37.  

```r
#Now I have to merge these values into the orginal dataset replacing the NA values
t <- replace (data$steps, is.na(data$steps), stepDayAve)

#create a copy of the orginal dataset and make the changes to the step counts.
data2 <- data
data2$steps <- t
```


#### Question 4: Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

```r
#plot the histagram
numberOfSteps <- tapply(data2$steps, data2$date, sum)
hist(numberOfSteps)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 



```r
mean(tapply(data2$steps, data2$date, sum))
```

```
## [1] 10752
```

```r
median(tapply(data2$steps, data2$date, sum))
```

```
## [1] 10656
```

The numbers above are slight lower the then nubmers found in Part one of this assignment. There are more points to be used when caculating the averages which is why the numbers are slightly lower.  


___________________________________________
Part 5 Are there differences in activity patterns between weekdays and weekends?
------------------------------------------
#### Question 1: Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
library("data.table")
myfun <- function(x)
{
  temp <- weekdays(x)
  ifelse( temp %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
    list("weekday"),list("weekend"))
}

#add the name of the week to the data table.
finalTable <- data.table(data)
finalTable[,c("dayOfWeek") := myfun(finalTable$date)]
```

```
##        steps       date interval dayOfWeek
##     1:    NA 2012-10-01        0   weekday
##     2:    NA 2012-10-01        5   weekday
##     3:    NA 2012-10-01       10   weekday
##     4:    NA 2012-10-01       15   weekday
##     5:    NA 2012-10-01       20   weekday
##    ---                                    
## 17564:    NA 2012-11-30     2335   weekday
## 17565:    NA 2012-11-30     2340   weekday
## 17566:    NA 2012-11-30     2345   weekday
## 17567:    NA 2012-11-30     2350   weekday
## 17568:    NA 2012-11-30     2355   weekday
```

#### Question 2: Make a panel plot containing a time series plot
-----------------------------------------------------------------

```r
library("lattice")
filter <- c("weekday", "weekend")
xyplot(finalTable$steps ~ finalTable$interval| filter ,layout(c(2,1)), type="l")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 
