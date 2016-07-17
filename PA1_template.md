# Reproducible Research: Peer Assessment 1




## Loading and preprocessing the data


#### Show any code that is needed to

1. Load the data (i.e. <math xmlns="http://www.w3.org/1998/Math/MathML">
  <mstyle mathcolor="red">
    <mtext mathvariant="monospace">read.csv()</mtext>
  </mstyle>
</math>)
2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
# Change the current working directory to the directory this script is  
thisdir<-getSrcDirectory(function(x) {x}) #Anonymous function to get the source directory. The anonymous function is dummy.

setwd(file.path(thisdir))


data_file<-"activity.csv"

#Check if the data file is present, else unzip it

if (! file.exists(data_file))
    unzip ("activity.zip")

# Read data from the file

data_with_NA<-read.csv(data_file)
data_wo_NA<-data_with_NA[!is.na(data_with_NA$steps),]
```


## What is mean total number of steps taken per day?

#### For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
3. Calculate and report the mean and median of the total number of steps taken per day


```r
total_steps_per_day<-aggregate(data_wo_NA$steps,list(data_wo_NA$date),sum)

colnames(total_steps_per_day)<-c("dates","steps")


library(ggplot2)

ggplot(data=total_steps_per_day, aes(x=steps,fill=..count..)) +
  geom_histogram(binwidth=1000) +
  theme_bw()+
  scale_fill_gradient(low = "#F9C6F5", high = "#FA1259")+
  guides(fill=FALSE)+     
  ggtitle("Total number of steps taken each Day") +
  labs(x="Number of Steps per Day", y="Frequency")
```

![](figure/unnamed-chunk-2-1.png)<!-- -->

```r
steps_mean   <- mean(total_steps_per_day$steps)
steps_median <- median(total_steps_per_day$steps)
cat("Mean of number of steps taken per day:",steps_mean)
```

```
## Mean of number of steps taken per day: 10766.19
```

```r
cat("Median of number of steps taken per day:",steps_median)
```

```
## Median of number of steps taken per day: 10765
```
## What is the average daily activity pattern?

1. Make a time series plot (i.e. <math xmlns="http://www.w3.org/1998/Math/MathML">
  <mstyle mathcolor="red">
    <mtext mathvariant="monospace">type="l"
  </mstyle>
</math>) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
daily_average <- aggregate(steps~interval, data=data_with_NA, FUN=mean, na.rm=TRUE)

ggplot(data=daily_average, aes(x=interval, y=steps)) +
    theme_bw()+
    geom_line(aes(colour = steps),show.legend = FALSE)+
    scale_colour_gradient(low = "#F9C6F5", high = "#FA1259")+
    guides(colour = guide_legend(show = FALSE))+
    labs(x="Interval", y="Steps")+
    ggtitle("Average Daily Activity Pattern")
```

![](figure/unnamed-chunk-3-1.png)<!-- -->

```r
max_interval <- daily_average[which.max(daily_average$steps),]
cat("Max interval is:",max_interval$interval,", with a mean of",max_interval$steps,"steps.")
```

```
## Max interval is: 835 , with a mean of 206.1698 steps.
```

    
## Imputing missing values

#### Note that there are a number of days/intervals where there are missing values (coded as <math xmlns="http://www.w3.org/1998/Math/MathML"> <mstyle mathcolor="red"> <mtext mathvariant="monospace">NA</mtext> </mstyle></math>). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with <math xmlns="http://www.w3.org/1998/Math/MathML">
  <mstyle mathcolor="red">
    <mtext mathvariant="monospace">NA</mtext>
  </mstyle>
</math>s)
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
total_missing_values<-sum(is.na(data_with_NA$steps))
cat("Total number of missing values:",total_missing_values)
```

```
## Total number of missing values: 2304
```

```r
interval_steps <- aggregate(steps ~ interval, data_with_NA , FUN = mean)

data_with_NA_replaced<-data_with_NA
for (i in 1:nrow(data_with_NA)){
    na_flag <- data_with_NA$steps[i]
    if(is.na(na_flag)){
        for(j in 1:nrow(interval_steps)){
            if(data_with_NA$interval[i] == interval_steps$interval[j]){
                data_with_NA_replaced$steps[i] = interval_steps$steps[j]
                break
            }
        }
    }
}

total_steps_per_day_NA_replaced<-aggregate(data_with_NA_replaced$steps,list(data_with_NA_replaced$date),sum)
colnames(total_steps_per_day_NA_replaced)<-c("dates","steps")

ggplot(data=total_steps_per_day_NA_replaced, aes(x=steps,fill=..count..)) +
  geom_histogram(binwidth=1000) +
  theme_bw()+
  scale_fill_gradient(low = "#9AEAFA", high = "#0B5AB4")+
  guides(fill=FALSE)+   
  ggtitle("Total number of steps taken each Day") +
  labs(x="Number of Steps per Day", y="Frequency")
```

![](figure/unnamed-chunk-4-1.png)<!-- -->

```r
mean_with_NA_replaced   <- mean(total_steps_per_day_NA_replaced$steps)
median_with_NA_replaced <- median(total_steps_per_day_NA_replaced$steps)
cat("Mean of number of steps taken per day:",mean_with_NA_replaced)
```

```
## Mean of number of steps taken per day: 10766.19
```

```r
cat("Median of number of steps taken per day:",median_with_NA_replaced)
```

```
## Median of number of steps taken per day: 10766.19
```

```r
ggplot(data=daily_average, aes(x=interval, y=steps)) +
    theme_bw()+
    geom_line(aes(colour = steps),show.legend = FALSE)+
    scale_colour_gradient(low = "#9AEAFA", high = "#0B5AB4")+
    guides(colour = guide_legend(show = FALSE)) +
    labs(x="Interval", y="Steps")+
    ggtitle("Average Daily Activity Pattern")
```

![](figure/unnamed-chunk-4-2.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?

#### For this part the <math xmlns="http://www.w3.org/1998/Math/MathML"><mstyle mathcolor="red"><mtext mathvariant="monospace">weekdays()</mstyle></math> function may be of some help here. Use the dataset with the filled-in missing values for this part.
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. <math xmlns="http://www.w3.org/1998/Math/MathML">
  <mstyle mathcolor="red">
    <mtext mathvariant="monospace">type="l"
  </mstyle>
</math>) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.



```r
day_of_week<-weekdays(as.Date(data_with_NA_replaced$date, "%Y-%m-%d"))


for (i in 1:length(day_of_week)) {
    if ((day_of_week[i] == "Saturday") | (day_of_week[i] == "Sunday")) 
        day_of_week[i] = "weekend" else day_of_week[i] = "weekday"
}

data_with_NA_replaced$day_type <- as.factor(day_of_week)


daytype_interval_steps <- aggregate(
    data=data_with_NA_replaced,
    steps ~ day_type + interval,
    FUN=mean
)

library("lattice")

xyplot(
    type="l",
    data=daytype_interval_steps,
    steps ~ interval | day_type,
    xlab="Interval",
    ylab="Number of steps",
    layout=c(1,2)
)
```

![](figure/unnamed-chunk-5-1.png)<!-- -->
