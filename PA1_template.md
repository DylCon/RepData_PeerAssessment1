Download libraries used in the following analyses:

    library(dplyr)
    library(ggplot2)

Loading and preprocessing the data
----------------------------------

The following analysis entails performing diagnostic statistics and
graphs based on an individual's monitoring device data. The data
provides step activity for 5 minute intervals in a period from October
to November 2012.

Start by downloading the data (Note: first set working directory to
location with file):

    ActTable <- read.csv("activity.csv",stringsAsFactors = FALSE)
    str(ActTable)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

What is mean total number of steps taken per day?
-------------------------------------------------

The first exercise will help understand average steps per day by
calculating the mean and median and looking at some simple graphs.

    AT_Group_Date <- ActTable %>% 
                group_by(date) %>% 
                summarise(TotalSteps = sum(steps))
    AT_Group_Date$date <- as.Date(AT_Group_Date$date)

    hist(AT_Group_Date$TotalSteps, breaks = 20, xlab = "Daily Steps", 
         main = "Histogram of Daily Steps")

![](PA1_template_files/figure-markdown_strict/CodeBlock2-1.png)

    mean(AT_Group_Date$TotalSteps, na.rm = TRUE)

    ## [1] 10766.19

    median(AT_Group_Date$TotalSteps, na.rm = TRUE)

    ## [1] 10765

Without any adjustments to the data, we observe a mean and median of
10766.19 and 10765 respectively. The daily activity histogram also
indicates a considerable spike in the 10,000 to 11,000 bucket and

What is the average daily activity pattern?
-------------------------------------------

Next we will observe how the different 5 minute intervals compare to
each other across the data set.

    AT_Group_Interval <- ActTable %>% 
                      group_by(interval) %>% 
                      summarise(Average_Int_Steps = mean(steps, na.rm = TRUE))
    ggplot(AT_Group_Interval,  aes(x = interval, y = Average_Int_Steps)) + geom_line() + ylab("Total Steps In Interval") + xlab("5 Minute Interval")

![](PA1_template_files/figure-markdown_strict/CodeBlock3-1.png)

    AT_Group_Interval[AT_Group_Interval$Average_Int_Steps == 
                      max(AT_Group_Interval$Average_Int_Steps,na.rm = TRUE),]

    ## Source: local data frame [1 x 2]
    ## 
    ##   interval Average_Int_Steps
    ##      (int)             (dbl)
    ## 1      835          206.1698

The interval with the highest mean steps across the data is interval
835, averaging 206.17 steps.

Imputing missing values
-----------------------

There are a number of days/intervals where there are missing values
which are coded as NA in the data. Instead of treating these as zeros
for our statistics, we will use the interval means calculated above as
placeholders to see if the missing data is significantly impacting the
statistics.

    sum(is.na(ActTable$steps))

    ## [1] 2304

    ActTable2 <- left_join(x = ActTable, y = AT_Group_Interval, by = "interval")
    head(ActTable2)

    ##   steps       date interval Average_Int_Steps
    ## 1    NA 2012-10-01        0         1.7169811
    ## 2    NA 2012-10-01        5         0.3396226
    ## 3    NA 2012-10-01       10         0.1320755
    ## 4    NA 2012-10-01       15         0.1509434
    ## 5    NA 2012-10-01       20         0.0754717
    ## 6    NA 2012-10-01       25         2.0943396

    ActTable2$steps_clean <- ActTable2$Average_Int_Steps
    for(i in 1:nrow(ActTable2)){
      if(is.na(ActTable2$steps[i]) == FALSE){
      ActTable2$steps_clean[i] <- ActTable2$steps[i]
      }
    }

    AT_Group_Date2 <- ActTable2 %>% 
      group_by(date) %>% 
      summarise(TotalSteps = sum(steps_clean))
    AT_Group_Date2$date <- as.Date(AT_Group_Date2$date)

    hist(AT_Group_Date2$TotalSteps, breaks = 20, xlab = "Daily Steps", 
         main = "Histogram of Daily Steps")

![](PA1_template_files/figure-markdown_strict/CodeBlock4-1.png)

    mean(AT_Group_Date2$TotalSteps, na.rm = TRUE)

    ## [1] 10766.19

    median(AT_Group_Date2$TotalSteps, na.rm = TRUE)

    ## [1] 10766.19

Imputing the missing 2,355 values using interval means does not appear
to significantly impact the statistics. The histogram has a very similar
shape but a more pronounced spike in the 10,000 to 11,000 bucket.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

Finally, we consider interval activity on weekdays versus weekend to see
if this variable has a significant impact on daily activity as we may
expect.

    ActTable2$WeekendFlag <- factor(rep("Weekday",nrow(ActTable2)),levels = 
                                      c("Weekday", "Weekend"))
    ActTable2$WeekendFlag[weekdays(as.Date(ActTable2$date)) %in% c("Saturday", "Sunday")] <- "Weekend"

    AT_Group_Interval2 <- ActTable2 %>% 
      group_by(interval,WeekendFlag) %>% 
      summarise(Average_Int_Steps = mean(steps, na.rm = TRUE))

    ggplot(AT_Group_Interval2,  aes(x = interval, y = Average_Int_Steps)) + 
      geom_line() + facet_grid(WeekendFlag ~.)+ ylab("Total Steps In Interval") + xlab("5 Minute Interval")

![](PA1_template_files/figure-markdown_strict/CodeBlock5-1.png)

It appears that during the weekend, activities tend to start later,
there is a lower morning peak and activity throughout the middle of the
day is consistently higher.
