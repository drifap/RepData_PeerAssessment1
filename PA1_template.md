# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

**_Load the data_**

```r
# load data with unz to unzip - stringAsFactors False to make date conversion easier
data <- read.csv(unz(description="activity.zip",filename="activity.csv"),stringsAsFactors=F)
```

**_Process/transform the data (if necessary) into a format suitable for your analysis_**

```r
# date into Date format
data$date <- as.Date(data$date,format="%Y-%m-%d")

# create new dataset without NA 
data_NA <- data[which(data$steps !="NA"),]
```



## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset. *Use data_NA* 

**_Make a histogram of the total number of steps taken each day_**

```r
# prepair data for plotting using ddply
require(plyr)
day_steps <- ddply(data_NA,.(date),
                   summarize,
                   steps=sum(steps))
```


```r
# create plot
hist(day_steps$steps,
     xlab="steps per day",
     col="blue",
     main="Total number of steps taken per day")
```

![plot of chunk hist](figure/hist.png) 


**_Calculate and report the mean and median total number of steps taken per day_**

```r
summary(day_steps$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8840   10800   10800   13300   21200
```


## What is the average daily activity pattern?

**_Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)_**

```r
# prepair data for plot using ddply
require(plyr)
step_int <- ddply(data_NA,.(interval),
                   summarize,
                   steps=mean(steps))
```


```r
# create plot
plot(x=step_int$interval,y=step_int$steps,
     type="l",
     xlab="5 minute interval",
     ylab="Average steps",
     main="Average daily activity pattern")
```

![plot of chunk int plot](figure/int plot.png) 


**_Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?_**

```r
# Find maximum interval 
step_int[step_int$steps==max(step_int$steps),]
```

```
##     interval steps
## 104      835 206.2
```

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

**_Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)_**

```r
sum(is.na(data)) # only missing values in steps variable
```

```
## [1] 2304
```


**_Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. Create a new dataset that is equal to the original dataset but with the missing data filled in._**


```r
# create new data set 
data_NA_filled <- data

# fill in missing values with 0
data_NA_filled[is.na(data_NA_filled)] <- 0

# check if any missing values
sum(is.na(data_NA_filled))
```

```
## [1] 0
```


**_Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?_**


```r
# prepair data for plot using plyr
require(plyr)
day_steps_2 <- ddply(data_NA_filled,.(date),
                   summarize,
                   steps=sum(steps))

# create plot
hist(day_steps_2$steps,
     xlab="steps per day",
     col="blue",
     main="Total number of steps taken per day - NA filled in")
```

![plot of chunk plot missing](figure/plot missing.png) 

```r
# summary for data without missing values 
summary(day_steps$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8840   10800   10800   13300   21200
```

```r
# summary for data with missing values imputed at 0
summary(day_steps_2$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6780   10400    9350   12800   21200
```

 1. **Do these values differ from the estimates from the first part of the assignment?** 
    * Yes they differ from the estimate in the first part of the assignment
    
 2. **What is the impact of imputing missing data on the estimates of the total daily number of steps?**
    * By imputing the missing data we have created minimum values that will skew the data. It changes the min, 1 st Qu, Median, Mean, 3rd Qu. 


## Are there differences in activity patterns between weekdays and weekends?
Use the dataset with the filled-in missing values for this part.

**_Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day._**

```r
# creating weekday
weekday <- weekdays(data_NA_filled$date,abbreviate=T)

# create new factor variable from weekday
data_NA_filled$week <- ifelse(weekday%in%c("lau.","sun."),"weekend","weekday")
data_NA_filled$week <- factor(data_NA_filled$week)
```

**_Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)._**

```r
# prepair data for plot - using ddply
require(plyr)
step_int_week <- ddply(data_NA_filled,.(interval,week),
                   summarize,
                   steps=mean(steps))

# plotting data
require(lattice)
xyplot(step_int_week$steps~step_int_week$interval|step_int_week$week,
       type="l",
       layout=c(1,2),
       xlab="Interval",
       ylab="Number of steps")
```

![plot of chunk weekdays plot](figure/weekdays plot.png) 
