### DATA R-Workshop 1
### Nick Paras | Rohan Agarwal
### Based on Kaggle Titanic Tutorials:
# github.com/wehrley/wehrley.github.io/blob/master/SOUPTONUTS.md
# trevorstephens.com/post/72916401642/titanic-getting-started-with-r
# https://statsguys.wordpress.com/2014/01/03/first-post/
# https://statsguys.wordpress.com/2014/01/11/data-analytics-for-beginners-pt-2/
# https://statsguys.wordpress.com/2014/01/10/data-analytics-for-beginners-pt-3/

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
df.train$Name = as.character(df.train$Name)
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

#Now what to do about age?
#we could delete all rows with missing values
#we could model them
#we could pick a modeling algorithm thats robust to missing data
#we could force all missing values to the mean
#we could impute the missing values using the other features

#Lets take a look at the data again
head(df.train)
#Do any of these seem like they'll be helpful to determine ages?

#As it turns out, titles like Mr, Mrs, Master, Ms may be helpful
#How could we possibly know this?! It comes down to knowledge of the
#problem, outside research, etc. We'll come back to this next time
#in more depth when we discuss Feature Engineering

#So, those titles occur in the Name column, and each one has 
#the same format: "Last, Title. First Middle"
#lets define a function to extract the titles
getTitle <- function(name){
  #Split strings
  Title = strsplit(name,'[,.]')[[1]][2]
  Title = sub(' ','',Title)
  return(Title)
}

#now lets get the titles for all the passengers
#How might we apply this to every name?
  #we could use a for loop, although R has some better ways
  #sapply simply applies a function to every row, which is perfect
df.train$Title = sapply(df.train$Name,getTitle)
#What data type would be appropriate for this column?
df.train$Title = as.factor(df.train$Title)
#So what titles did we end up with?
levels(df.train$Title)

#What's missing?
table(df.train$Title,is.na(df.train$Age))

##FOR DISCUSSION
#What do you notice about these? Why might it be dangerous to 
#calculate ages based on title?
  #Infrequent titles, titles that only appear in test set, etc.
    #We'll come back to this later, just be aware

#Lets say we want to fill these missing values with the mean
#of the ages of the others with that title
#define a new function to fill the missing ages
getAges <- function(dataFrame){
  #first we need to find the factors with missing ages
  titlesMissingAge = levels(droplevels(dataFrame$Title[which(is.na(dataFrame$Age)==TRUE)]))
  #now we want to store the average age of each title in those that are missing
  for (title in titlesMissingAge) {
    dataFrame$Age[which((dataFrame$Title==title)&(is.na(dataFrame$Age)==TRUE))]=mean(dataFrame$Age[which((dataFrame$Title==title)&(is.na(dataFrame$Age)==FALSE))])
  }
  return(dataFrame)
}

#finally, lets get the missing ages
df.train = getAges(df.train)

# Intro to Simple Feature Engineering -------------------------------------

#We still have 17 titles, which is far too many to be useful
#since so many have only a couple occurrences
#so we can aggregate some titles for simplicity:

#make titles character for simplicity
df.train$Title = as.character(df.train$Title)
#Replace infrequent titles
df.train$Title[df.train$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
df.train$Title[df.train$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
df.train$Title[df.train$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
#Make Title a factor again
df.train$Title = as.factor(df.train$Title)

# Fitting Your First Model ------------------------------------------------

#Now we've done a substantial amount of data cleanup, can we
#Please finally start modeling?!

#There are a number of other factors to consider, but for the sake of
#Illustrating what we've managed to do today, lets go ahead

#Logistic Regression
#Perhaps conceptually the simplest classification algorithm
#This version will output the probability of an observation being one
#of the two classes or not

#first we make a partition of the data


