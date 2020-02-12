library(dplyr)

## Loading and preprocessing the data
path <- rstudioapi::getActiveDocumentContext()$path
pathzip <- gsub("RepDataScript.R","activity.zip",path)
pathcsv <- gsub(pathzip,"activity.csv",pathzip)
datatoanalyze <- read.csv(unz(pathzip, pathcsv), header = TRUE)

###Preprocess the data:-
datatoanalyze$date <- as.Date(datatoanalyze$date)
str(datatoanalyze)

### Basic structure of the data:-
names(datatoanalyze)
str(datatoanalyze)
range(datatoanalyze$interval)


### Plot the histogram of Total number of steps taken each day:-
histogramdata <- group_by(datatoanalyze, date)
histogramdata <- summarise_at(histogramdata, .vars = c("steps"),sum)
hist(histogramdata$steps, xlab = "Total Steps per day", col = "yellow", main  ="Total steps per day", ylim = c(0,40))
rug(histogramdata$steps)
meanValue <- round(mean(histogramdata$steps, na.rm = TRUE), digits = 0)
medianValue <- median(histogramdata$steps, na.rm = TRUE)


## What is the average daily activity pattern?
activitypattern <- group_by(datatoanalyze,interval)
activitypattern <- summarize_at(activitypattern, .vars = c("steps"),mean, na.rm = TRUE)
with(activitypattern, plot(interval,steps, type = "l", col = "black", ylab = "Average Steps",main = "Average Daily Activity Pattern"))


maximumSteps <- max(activitypattern$steps)
intervalwithMaximumSteps <- activitypattern[grep(maximumSteps,activitypattern$steps),"interval"]
abline(v = intervalwithMaximumSteps, col = "red")
legend("topright",legend = c(paste("Maximum:",intervalwithMaximumSteps)),col = c("red"),lty = 1)

## Imputing missing values

#separate dataframe into: dataframe with NA steps;  dataframe with Non-NA steps
onlyNonNAData <- datatoanalyze[!is.na(datatoanalyze$steps),]
onlyNAData <- datatoanalyze[is.na(datatoanalyze$steps),]

#get intervals inside the dataframe of NA steps
listtotraverse <- unique(onlyNAData$interval)

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


##Histogram of the total number of steps taken each day after missing values are imputed
histogramdata1 <- group_by(newimputedDataframe, date)
histogramdata1 <- summarise_at(histogramdata1, .vars = c("steps"),sum)
hist(histogramdata1$steps, xlab = "Total Steps per day", col = "yellow", main  ="Total steps per day", ylim = c(0,40))
rug(histogramdata1$steps)
meanValue <- round(mean(histogramdata1$steps, na.rm = TRUE), digits = 0)
medianValue <- median(histogramdata1$steps, na.rm = TRUE)

par(mfrow = c(1,2), xpd = TRUE)
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
legend("topright",legend = c(paste("Mean:",meanValue),paste("Median:",medianValue)), bty = "n")

par(mfrow = c(1,1))



