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
    #character : Like strings
missing.data <- data.frame(Count = 0, Feature = names(df.train))
for (ii in 1:ncol(df.train)) {
  if (is.numeric(df.train[,ii])==TRUE) {
    missing.data$Count[ii] = sum(is.na(df.train[,ii]))
  } else {
    missing.data$Count[ii] = sum(df.train[,ii] == "")
  }
}


#compare native barplot to ggplot
barplot(missing.data$Count, names.arg=missing.data$Feature)

library(ggplot2)
ggplot(missing.data, aes(x = Feature, y = Count)) +
  geom_bar(stat = "identity") +
  geom_text(aes(title = 'Missing Data',label = sprintf("%.2f%%", Count/nrow(df.train) * 100)), vjust = -.5)
detach(package:ggplot2)

#Examine the Data we do have
barplot(table(df.train$Survived),
        names.arg = c("Perished", "Survived"),
        main="Survived (passenger fate)", col="black")
barplot(table(df.train$Pclass), 
        names.arg = c("first", "second", "third"),
        main="Pclass (passenger traveling class)", col="firebrick")
barplot(table(df.train$Sex), main="Sex (gender)", col="darkviolet")
hist(df.train$Age, main="Age", xlab = NULL, col="brown")
barplot(table(df.train$SibSp), main="SibSp (siblings + spouse aboard)", 
        col="darkblue")
barplot(table(df.train$Parch), main="Parch (parents + kids aboard)", 
        col="gray50")
hist(df.train$Fare, main="Fare (fee paid for ticket[s])", xlab = NULL, 
     col="darkgreen")
barplot(table(df.train$Embarked), 
        names.arg = c("","Cherbourg", "Queenstown", "Southampton"),
        main="Embarked (port of embarkation)", col="sienna")

#Colored barplot
counts <- table(df.train$Survived, df.train$Sex)
#Counts is a 2x2 matrix - observe the coloring in the following barplot
barplot(counts, 
        xlab = "Gender", 
        ylab = "Number of People", 
        main = "survived and deceased between male and female")