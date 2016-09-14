#FINAL SCRIPT
#Script for Visualization Using GGPLOT
#Also some feature engineering
#This script, is greatly adopted and editted from Code Dave Langer tutorial. -  Copyright 2016 Dave Langer


#Set the directory
#You can also check current/default directory by this command : getwd()
#setwd("D:/directoty/")


#Load raw data from csv files (fron the above drectory) intto train and test respecively
#Header = TRUE since csv files had titles/headers
#Make sure both dataset are on the directory set above..
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# Add a "Survived" variable to the test set to allow for combining data sets
test.survived <- data.frame(survived = rep("None", nrow(test)), test[,])

# Combine data sets (Train and Test)
data.combined <- rbind(train, test.survived)

#View the data schema.
str(data.combined)

#R work well with factor, variable... Change survived & pclass to factor.
data.combined$survived <- as.factor(data.combined$survived)
data.combined$pclass <- as.factor(data.combined$pclass)



# Load up ggplot2 package to use for visualizations
library(ggplot2)


# Hypothesis - Rich folks survived at a higer rate
train$pclass <- as.factor(train$pclass)
ggplot(train, aes(x = pclass, fill = factor(survived))) +
  geom_bar() +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived") 


#Distribution of Survival By Sex
ggplot(train, aes(x = sex, fill = factor(survived))) +
  geom_bar() +
  ggtitle("Distribution of Survival By Sex") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived") 


#We would like to explore using title in the name  variable.
#Hypothesis - Women and Children are more likely to survive.
#stringr is Fast string manipulation package.
library(stringr)


# Create a function extract title from Name variable
#So we could explore its link with survival rate.
extractTitle <- function(name) {
  name <- as.character(name)
  
  if (length(grep("Miss.", name)) > 0) {
    return ("Miss.")
  } else if (length(grep("Master.", name)) > 0) {
    return ("Master.")
  } else if (length(grep("Mrs.", name)) > 0) {
    return ("Mrs.")
  } else if (length(grep("Mr.", name)) > 0) {
    return ("Mr.")
  } else {
    return ("Other")
  }
}



#We add Title variable to data.combined, using the following for loop.
titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i,"name"]))
}
data.combined$title <- as.factor(titles)

#We plot survival rate by Titles and Class
#In the data.combined set, the first 891 instances are of the train data set.
ggplot(data.combined[1:891,], aes(x = title, fill = survived)) +
  stat_count(width = 0.5) +
  facet_wrap(~pclass) + 
  ggtitle("Pclass") +
  xlab("Titles") +
  ylab("Total Count") +
  labs(fill = "Survived")



#Distribution of Survival By Title
ggplot(data.combined[1:891,], aes(x = title, fill = survived)) +
  stat_count(width = 0.5) + 
  ggtitle("Distribution of Survival By Title") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")


# What's the distribution of females to males across train & test?
table(data.combined$sex)


# Visualize the 3-way relationship of sex, pclass, and survival,
ggplot(data.combined[1:891,], aes(x = sex, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass) + 
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")


# Just to be thorough, take a look at survival rates broken out by sex, pclass, and age
ggplot(data.combined[1:891,], aes(x = age, fill = survived)) +
  facet_wrap(~sex + pclass) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count")



# Validate that "Master." is a good proxy for male children
boys <- data.combined[which(data.combined$title == "Master."),]
summary(boys$age)


# We know that "Miss." is more complicated, let's examine further
misses <- data.combined[which(data.combined$title == "Miss."),]
summary(misses$age)


# Move on to the sibsp variable, summarize the variable
summary(data.combined$sibsp)


# Can we treat as a factor?
length(unique(data.combined$sibsp))
data.combined$sibsp <- as.factor(data.combined$sibsp)



# We believe title is predictive. Visualize survival reates by sibsp, pclass, and title
ggplot(data.combined[1:891,], aes(x = sibsp, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


# Treat the parch vaiable as a factor and visualize
data.combined$parch <- as.factor(data.combined$parch)
ggplot(data.combined[1:891,], aes(x = parch, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("ParCh") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


# Let's try some feature engineering. What about creating a family size feature?
temp.sibsp <- c(train$sibsp, test$sibsp)
temp.parch <- c(train$parch, test$parch)
data.combined$family.size <- as.factor(temp.sibsp + temp.parch + 1)


# Visualize it to see if it is predictive
ggplot(data.combined[1:891,], aes(x = family.size, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


# Take a look at the ticket variable
str(data.combined$ticket)


# Based on the huge number of levels ticket really isn't a factor variable it is a string. 
# Convert it and display first 20
data.combined$ticket <- as.character(data.combined$ticket)
data.combined$ticket[1:20]


# There's no immediately apparent structure in the data, let's see if we can find some.
# We'll start with taking a look at just the first char for each
ticket.first.char <- ifelse(data.combined$ticket == "", " ", substr(data.combined$ticket, 1, 1))
unique(ticket.first.char)


# OK, we can make a factor for analysis purposes and visualize
data.combined$ticket.first.char <- as.factor(ticket.first.char)

# First, a high-level plot of the data
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = survived)) +
  geom_bar() +
  ggtitle("Rate of Survival by Ticket's First Character") +
  xlab("Ticket's First Character") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")

# Ticket seems like it might be predictive, drill down a bit
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass) + 
  ggtitle("Survival By Ticket First Charater and PClass") +
  xlab("Ticket First Character and Class") +
  ylab("Total Count") +
  ylim(0,150) +
  labs(fill = "Survived")

# Lastly, see if we get a pattern when using combination of pclass & title
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,200) +
  labs(fill = "Survived")




# Next up - the fares Titanic passengers paid
summary(data.combined$fare)
length(unique(data.combined$fare))


# Can't make fare a factor, treat as numeric & visualize with histogram
ggplot(data.combined, aes(x = fare)) +
  stat_count(width = 5) +
  ggtitle("Combined Fare Distribution") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,70)


# Let's check to see if fare has predictive power
ggplot(data.combined[1:891,], aes(x = fare, fill = survived)) +
  stat_count(width = 5) +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("fare") +
  ylab("Total Count") +
  ylim(0,50) + 
  labs(fill = "Survived")




# Analysis of the cabin variable
str(data.combined$cabin)


# Cabin really isn't a factor, make a string and the display first 100
data.combined$cabin <- as.character(data.combined$cabin)
data.combined$cabin[1:100]


# Replace empty cabins with a "U"
data.combined[which(data.combined$cabin == ""), "cabin"] <- "U"
data.combined$cabin[1:100]


# Take a look at just the first char as a factor
cabin.first.char <- as.factor(substr(data.combined$cabin, 1, 1))
str(cabin.first.char)
levels(cabin.first.char)


# Add to combined data set and plot 
data.combined$cabin.first.char <- cabin.first.char

# High level plot
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = survived)) +
  geom_bar() +
  ggtitle("Survivability by cabin.first.char") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,750) +
  labs(fill = "Survived")

# Could have some predictive power, drill in
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass) +
  ggtitle("Survival Rate by Cabin 1st Char and Class") +
  xlab("Cabin first Char") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")

# Does this feature improve upon pclass + title?
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")


# Does survivability depend on where you got onboard the Titanic?
str(data.combined$embarked)
levels(data.combined$embarked)


# Plot data for analysis
ggplot(data.combined[1:891,], aes(x = embarked, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass) +
  ggtitle("Survival on Embarked and Pclass") +
  xlab("embarked") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

