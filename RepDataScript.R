library(dplyr)
library(lattice)
dev.off()
###=========================== Loading and preprocessing the data
path <- rstudioapi::getActiveDocumentContext()$path
pathzip <- gsub("RepDataScript.R","activity.zip",path)
pathcsv <- gsub(pathzip,"activity.csv",pathzip)
datatoanalyze <- read.csv(unz(pathzip, pathcsv), header = TRUE)

#Step1:Preprocess the data:-
datatoanalyze$date <- as.Date(datatoanalyze$date)
str(datatoanalyze)
#Step2:Basic structure of the data:-
names(datatoanalyze)
str(datatoanalyze)
range(datatoanalyze$interval)





###=========================== Plot the histogram of Total number of steps taken each day:-
histogramdata <- group_by(datatoanalyze, date)
histogramdata <- summarise_at(histogramdata, .vars = c("steps"),sum)
hist(histogramdata$steps, xlab = "Total Steps per day", col = "yellow", main  ="Total steps per day", ylim = c(0,40))
rug(histogramdata$steps)
meanValue <- round(mean(histogramdata$steps, na.rm = TRUE), digits = 0)
medianValue <- median(histogramdata$steps, na.rm = TRUE)





###=========================== What is the average daily activity pattern?
activitypattern <- group_by(datatoanalyze,interval)
activitypattern <- summarize_at(activitypattern, .vars = c("steps"),mean, na.rm = TRUE)
with(activitypattern, plot(interval,steps, type = "l", col = "black", ylab = "Average Steps",main = "Average Daily Activity Pattern"))


maximumSteps <- max(activitypattern$steps)
intervalwithMaximumSteps <- activitypattern[grep(maximumSteps,activitypattern$steps),"interval"]
abline(v = intervalwithMaximumSteps, col = "red")
legend("topright",legend = c(paste("Maximum:",intervalwithMaximumSteps)),col = c("red"),lty = 1)




###=========================== Imputing missing values

#Step1: separate dataframe into: dataframe with NA steps;  dataframe with Non-NA steps
onlyNonNAData <- datatoanalyze[!is.na(datatoanalyze$steps),]
onlyNAData <- datatoanalyze[is.na(datatoanalyze$steps),]

#Step2: get intervals inside the dataframe of NA steps
listtotraverse <- unique(onlyNAData$interval)

#Step 3:
#Impute the missing values by filling in  with (means for the 5-minute intervals)
#Do this by:
#Go through the listtotraverse, by interval, adding in the (means for the 5-minute intervals)
#Add the newly imputed values into a newdataframeimputed

for(val in listtotraverse)
{
  onlyNAData[onlyNAData$interval == val,"steps"] <- activitypattern[activitypattern$interval == val,"steps"]
}
#Combine imputed onlyNAData frame and onlyNonNAData frame to create the newly imputed dataset
newimputedDataframe <- rbind(onlyNonNAData,onlyNAData)





###==================Histogram of the total number of steps taken each day after missing values are imputed

# Plot: The histogram with imputed data
histogramdata1 <- group_by(newimputedDataframe, date)
histogramdata1 <- summarise_at(histogramdata1, .vars = c("steps"),sum)
hist(histogramdata1$steps, xlab = "Total Steps per day", col = "yellow", main  ="Total steps per day", ylim = c(0,40))
rug(histogramdata1$steps)
meanValue <- round(mean(histogramdata1$steps, na.rm = TRUE), digits = 0)
medianValue <- median(histogramdata1$steps, na.rm = TRUE)

#Plot: Histogram of Non-imputed , imputed data side by side. (1 row and 2 columns) layout
par(mfrow = c(1,2))
histogramdata <- group_by(datatoanalyze, date)
histogramdata <- summarise_at(histogramdata, .vars = c("steps"),sum)
hist(histogramdata$steps, xlab = "Total Steps per day", col = "yellow", main  ="(before imputation)", ylim = c(0,40))
rug(histogramdata$steps)
meanValue <- round(mean(histogramdata$steps, na.rm = TRUE), digits = 0)
medianValue <- round(median(histogramdata$steps, na.rm = TRUE),digits = 0)
legend("topright",legend = c(paste("Mean:",meanValue),paste("Median:",medianValue)),bty = "n")

histogramdata1 <- group_by(newimputedDataframe, date)
histogramdata1 <- summarise_at(histogramdata1, .vars = c("steps"),sum)
hist(histogramdata1$steps, xlab = "Total Steps per day", col = "yellow", main  ="(after imputation)", ylim = c(0,40))
rug(histogramdata1$steps)
meanValue <- round(mean(histogramdata1$steps, na.rm = TRUE), digits = 0)
medianValue <- round(median(histogramdata1$steps, na.rm = TRUE),digits = 0)
legend("topright",legend = c(paste("Mean:",meanValue),paste("Median:",medianValue)),bty = "n")

dev.off() # reset the display device

###========================== Are there differences in activity patterns between weekdays and weekends?
newimputedDataframe$date <- as.Date(newimputedDataframe$date)

#Display the days in terms of: Mon,Tue,Wed....., and list out their breakups in a table
daysname <- (weekdays(newimputedDataframe$date,abbreviate = TRUE))
table(daysname)

#Replace : (Mon- Fri, with: Weekday), (Sat-Sun, with the name: Weekend)
daysname <- gsub("(Sat|Sun)","Weekend",daysname)
daysname <- gsub("(Mon|Tue|Wed|Thu|Fri)","Weekday",daysname)
table(daysname)

#Add a new column called day_type to our dataframe:
newimputedDataframe$day_type <- as.factor(daysname)

#Group data by day_type(Weekend/Weekday), then group by Interval
newimputedDataframe <- group_by(newimputedDataframe,day_type)
newimputedDataframe <- group_by(newimputedDataframe,interval,add=TRUE)

#Summarize the activty pattern
activitypattern1 <- summarize_at(newimputedDataframe, .vars = c("steps"),mean, na.rm = TRUE)

#Create the panel plot, for activity pattern for; Weekend/ Weekday
xyplot(steps~interval|day_type, data = activitypattern1, layout= c(1,2), type = "l")

with(activitypattern1, plot(interval,steps, type = "l", col = "black", ylab = "Average Steps",main = "Average Daily Activity Pattern"))




