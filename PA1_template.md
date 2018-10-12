# Reproducible Research - Course project 1 ####

#### Loading and pre-processing the data ######
The first part of this assignment focuses on the subject of reading the data in to R! In the following I have replaced the true path on my local computer, with the phrase: "MyDataLocation"  
  
###### Unzipping and reading the data ######

```{r unzip, echo=TRUE}
setwd("C:/Projekter/Data Science specialization/Reproducible research/Week2/Wk2_Project")
RawData <- read.csv("activity.csv", header = TRUE, sep = ",", dec = ".")
RawData$date <- as.Date(RawData$date)
```
#### What is mean total number of steps taken per day? ####

```{r mean_total, echo=TRUE}
Processed_Mean <- tapply(RawData$steps, RawData$date, sum, na.rm = TRUE)
Total_steps_taken <- print(sum(Processed_Mean))
```

The total number of steps taken per day is equal to `r Total_steps_taken` steps  
 
A histogram of the data looks like this:

```{r Histogram, echo=TRUE}
hist(Processed_Mean, col = "darkgreen", xlab = "Total steps", ylab = "Frequency", maintainer = "Total number of steps taken per day")
mean_steps <- mean(Processed_Mean)
median_steps <- median(Processed_Mean)
```
The mean number of steps taken is `r mean_steps` while the median equals `r median_steps`

#### What is the average daily activity pattern  

The following plot shows the number of steps taken per 5-minute interval:

```{r timeseries plot, echo=TRUE}
Processed_data2 <- tapply(RawData$steps, RawData$interval, mean, na.rm = TRUE)
plot(unique(RawData$interval), Processed_data2, type = "l", xlab = "Intervals", ylab = "Steps taken", main = "Steps taken per interval - Average")
Processed_data3 <- data.frame(cbind(Processed_data2, unique(RawData$interval)))
Find <- which.max(Processed_data3$Processed_data2)
max <- Processed_data3[Find, 2]
```

The 5-minute interval with the most steps taken is the interval occuring after: `r max` minutes!

#### Imputing missing values ####  

Here is a histogram of the totalt number of steps taken each day, after correcting for missing values! 

```{r missing values, echo = TRUE}
Num_na <- sum(is.na(RawData$steps))
Find_nas <- which(is.na(RawData$steps))
num_na <- length(Find_nas)

Avg_steps <- tapply(RawData$steps, RawData$date, mean, na.rm = TRUE)
na <- mean(Avg_steps, na.rm = TRUE)
for (i in 1:num_na) {
    RawData[Find_nas[i],1] <- na
}
Processed_data4 <- tapply(RawData$steps, RawData$date, sum, na.rm = TRUE)
hist(Processed_data4, col = "blue", xlab="Steps", ylab="Frequency", main="Total steps")
mean2 <- mean(Processed_data4)
median2 <- median(Processed_data4)
```
The total number of missing values in the dataset equals: `r Num_na` 

After the missing values have been replaced by the average number of steps taken, the total mean is now `r mean2` steps, and the median is `r median2`
 

#### Are there differences in activity patterns between weekdays and weekends? ####

the following variables will be created in order to later make a plots of the data according to weekdays and weekends
```{r split_days, echo=TRUE}
Week_days <- c("mandag", "tirsdag", "onsdag", "torsdag", "fredag")
RawData$weekDay <- factor((weekdays(RawData$date) %in% Week_days), levels=c(FALSE, TRUE), labels = c("weekend", "weekday"))
Processed_weekday <- subset(RawData, as.character(RawData$weekDay) == "weekday")
Processed_weekend <- subset(RawData, as.character(RawData$weekDay) == "weekend")
Weekday_mean_steps <- tapply(Processed_weekday$steps, Processed_weekday$interval, mean, na.rm = TRUE)
Weekend_mean_steps <- tapply(Processed_weekend$steps, Processed_weekend$interval, mean, na.rm = TRUE)
Weekday_intervals <- unique(Processed_weekday$interval)
Weekend_intervals <- unique(Processed_weekend$interval)
Combo_Weekday <- data.frame(cbind(Weekday_mean_steps, Weekday_intervals))
Combo_Weekend <- data.frame(cbind(Weekend_mean_steps, Weekend_intervals))
par(mfrow = c(2,1), mar=c(4,4,2,1))
plot(Combo_Weekday$Weekday_intervals, Combo_Weekday$Weekday_mean_steps, type = "l", xlab="intervals", ylab="Steps on average", main = "Weekdays")
plot(Combo_Weekend$Weekend_interval, Combo_Weekend$Weekend_mean_steps, type = "l", xlab="intervals", ylab="Steps on average", main = "Weekends")
```




