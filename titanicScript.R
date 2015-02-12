install.packages("ggplot2")
library(ggplot2)
train = read.csv("train.csv",header=TRUE)

#How complete is this data?
#First, we find the number of missing data points in each column
missing.data <- data.frame(Count = 0, Feature = names(train))
for (ii in 1:ncol(train)) {
  if (is.numeric(train[,ii])==TRUE) {
    missing.data$Count[ii] = sum(is.na(train[,ii]))
  } else {
    missing.data$Count[ii] = sum(train[,ii] == "")
  }
}

ggplot(missing.data, aes(x = Feature, y = Count)) +
  geom_bar(stat = "identity") +
  geom_text(aes(title = 'Missing Data',label = sprintf("%.2f%%", Count/nrow(train) * 100)), vjust = -.5)

