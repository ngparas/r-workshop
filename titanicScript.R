#install.packages("ggplot2")

# Import Data -------------------------------------------------------------

#read the data from the csv file
df.train = read.csv("train.csv",header=TRUE)
df.test = read.csv("test.csv",header=TRUE)

# Data Exploration --------------------------------------------------------

#Preview the data - first 6 rows
head(df.train)

#How complete is this data?
#Discuss Data-Types, Logical Operations, Loops

#First, we find the number of missing data points in each column
#However, to do this we need to understand data types
  #R has several major data types:
    #integer   : Whole numbers like 0, 1, 2, 3...
    #numeric   : Decimal numbers, like float or double in C
    #character : Text, like "Hello World"
    #factor    : Categorical data with discrete 'levels'
#create a new Data Frame
  #A data frame is used for storing data tables
  #Each column is a list/vector of data
missing.data <- data.frame(Count = 0, Feature = names(df.train))

#We want to find the number of missing entries per feature
#So we'll loop through each column
#Note that each data type has to be handled differently
for (ii in 1:ncol(df.train)) {
  if (is.numeric(df.train[,ii])==TRUE) {
    missing.data$Count[ii] = sum(is.na(df.train[,ii]))
  } else {
    missing.data$Count[ii] = sum(df.train[,ii] == "")
  }
}

#Visualize the missing data
#compare native barplot to ggplot
barplot(missing.data$Count, names.arg=missing.data$Feature)
library(ggplot2)
ggplot(missing.data, aes(x = Feature, y = Count)) +
  geom_bar(stat = "identity") +
  geom_text(aes(title = 'Missing Data',label = sprintf("%.2f%%", Count/nrow(df.train) * 100)), vjust = -.5)
detach(package:ggplot2)

#Examine the Data we do have, what are the distributions of the columns?
barplot(table(df.train$Survived),
        names.arg = c("Perished", "Survived"),
        main="Survived (passenger fate)", col="black")
barplot(table(df.train$Pclass), 
        names.arg = c("first", "second", "third"),
        main="Pclass (passenger traveling class)", col="firebrick")
barplot(table(df.train$Sex), main="Sex (gender)", col="darkviolet")

#There are multiple ways to visualize each of these things
hist(df.train$Age, main="Age", xlab = NULL, col="brown")
plot(density(df.train$Age, na.rm = TRUE))
summary(df.train$Age)

#Are they normal? Is there a bias? etc.
#The range and distribution of these features will be important 
barplot(table(df.train$SibSp), main="SibSp (siblings + spouse aboard)", 
        col="darkblue")
barplot(table(df.train$Parch), main="Parch (parents + kids aboard)", 
        col="gray50")
hist(df.train$Fare, main="Fare (fee paid for ticket[s])", xlab = NULL, 
     col="darkgreen")
barplot(table(df.train$Embarked), 
        names.arg = c("","Cherbourg", "Queenstown", "Southampton"),
        main="Embarked (port of embarkation)", col="sienna")

#We can visualize more than 1 feature at a time
#Colored barplot
counts <- table(df.train$Survived, df.train$Sex)
#Counts is a 2x2 matrix - observe the coloring in the following barplot
barplot(counts, 
        xlab = "Gender", 
        ylab = "Number of People", 
        main = "survived and deceased between male and female")

#R has a number of other plotting packages that allow for things like 
#install.packages("vcd")
library(vcd)
mosaicplot(df.train$Sex ~ df.train$Survived, 
           main="Passenger Fate by Gender", shade=FALSE, color=TRUE, 
           xlab="Sex", ylab="Survived")

#More statistical plot
boxplot(df.train$Age ~ df.train$Survived, 
        main="Passenger Fate by Age",
        xlab="Survived", ylab="Age")

# Handling Missing Data ---------------------------------------------------

#You may recall that Embarked, Cabin, and Age all had missing values
summary(df.train$Embarked) # Two values are ""
summary(df.train$Cabin) # 687 are "", 49 are "(other)"
summary(df.train$Age) # 177 are NA

#We'll ignore Cabin for now, since 77% is missing

#Lets replace the two missing values for Embarked with the most common, S
df.train$Embarked[which(df.train$Embarked=='')] <- 'S'
summary(df.train$Embarked)
df.train$Embarked=droplevels(df.train$Embarked)
summary(df.train$Embarked)
