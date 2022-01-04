# Melissa Hunfalvay. Last Revised: 6-22-2021
# Data 630
# AIDS Clinical Trials Group Study 320 Data (actg320.dat)
#Assignment 2
# Professor Firdu

# 1. Introduction

#Loading the data in R and preview it

# Set working directory and read the data
setwd("/Users/melissahunfalvay/Documents/HUN/My Professional Development/Machine Learning Data 630/Assignments/Assignment 2") 
# display the file names in the current working directory
dir()

#Use the read.csv command to load the data into RStudio.  
actg320<-read.csv(file="actg320.csv", header=TRUE, sep=",", as.is = FALSE)

# Preview the data in spreadsheet View to ensure the read in looks accurate
View(actg320)


# Change  variable names so they are more descriptive 
names(actg320)[names(actg320) == "tx"] <- "Treatment"
names(actg320)[names(actg320) == "txgrp"] <- "Treatment_grp"
names(actg320)[names(actg320) == "strat2"] <- "CD4_Stratum"
names(actg320)[names(actg320) == "raceth"] <- "Race"
names(actg320)[names(actg320) == "ivdrug"] <- "IV_drug"
names(actg320)[names(actg320) == "cd4"] <- "CD4_base"

# Run the command to preview the first 10 data rows.
head(actg320, 10)


#Load packages 
# Load ggplot2 for higher level visualization graphics
library("ggplot2")
library("verification")

#Section 2

#Analysis-Model Development


# Perform exploratory analysis

# a) Provide basic description of the data
str(actg320)

# b) Descriptive Statistics
# Run the summary command to display the descriptive statistics for all variables 
summary (actg320)

# Check for collinearity
cor(actg320)


# Descriptive statistics for integer variables transformed to factors

# censor
#transform this variable to a factor
actg320$censor<-as.factor(actg320$censor)  #Transform int to factor
#actg320$censor<-factor(actg320$censor, levels = 0:1, labels = c("Otherwise", "AIDS or Death")) # Relabel levels for easy understanding
# show first 100 raw variables
head(actg320$censor, 100)
# Count the number of values within each category
table(actg320$censor)
# Percentage
table(actg320$censor)/length(actg320$censor)

# censor_d
#transform this variable to a factor
actg320$censor_d=as.factor(actg320$censor_d)
actg320$censor_d<-factor(actg320$censor_d, levels = 0:1, labels = c("Otherwise", "Death")) # Relabel levels for easy understanding
# show first 100 raw variables
head(actg320$censor_d, 100)
# Count the number of values within each category
table(actg320$censor_d)
# Percentage
table(actg320$censor_d)/length(actg320$censor_d)

# Treatment
#transform this variable to a factor
actg320$Treatment<-as.factor(actg320$Treatment)  #Transform int to factor
actg320$Treatment<-factor(actg320$Treatment, levels = 0:1, labels = c("Control", "Treatment wIDV")) # Relabel levels for easy understanding
# show first 100 raw variables
head(actg320$Treatment, 100)
# Count the number of values within each category
table(actg320$Treatment)
# Percentage
table(actg320$Treatment)/length(actg320$Treatment)

# Treatment_grp
#transform this variable to a factor
actg320$Treatment_grp<-as.factor(actg320$Treatment_grp)  #Transform int to factor
actg320$Treatment_grp<-factor(actg320$Treatment_grp, levels = 1:4, labels = c("ZDV+3TC", "ZDV+3TC+IDV", "d4T+3TC", "d4T+3TC+IDV")) # Relabel levels for easy understanding
# show first 100 raw variables
head(actg320$Treatment_grp, 100)
# Count the number of values within each category
table(actg320$Treatment_grp)
# Percentage
table(actg320$Treatment_grp)/length(actg320$Treatment_grp)

# CD4_Stratum
#transform this variable to a factor
actg320$CD4_Stratum<-as.factor(actg320$CD4_Stratum)  #Transform int to factor
actg320$CD4_Stratum<-factor(actg320$CD4_Stratum, levels = 0:1, labels = c("<=50", ">50")) # Relabel levels for easy understanding
# show first 100 raw variables
head(actg320$CD4_Stratum, 100)
# Count the number of values within each category
table(actg320$CD4_Stratum)
# Percentage
table(actg320$CD4_Stratum)/length(actg320$CD4_Stratum)

# sex
#transform this variable to a factor
actg320$sex<-as.factor(actg320$sex)  #Transform int to factor
actg320$sex<-factor(actg320$sex, levels = 1:2, labels = c("Male", "Female")) # Relabel levels for easy understanding
# show first 100 raw variables
head(actg320$sex, 100)
# Count the number of values within each category
table(actg320$sex)
# Percentage
table(actg320$sex)/length(actg320$sex)

# Race
#transform this variable to a factor
actg320$Race<-as.factor(actg320$Race)  #Transform int to factor
actg320$Race<-factor(actg320$Race, levels = 1:6, labels = c("White", "Black", "Hispanic", "Asian/PI", "Indian/Alaskan", "Other/Unknown")) # Relabel levels for easy understanding
# show first 100 raw variables
head(actg320$Race, 100)
# Count the number of values within each category
table(actg320$Race)
# Percentage
table(actg320$Race)/length(actg320$Race)

# IV_drug
#transform this variable to a factor
#actg320$IV_drug<-as.factor(actg320$IV_drug)  #Transform int to factor
#actg320$IV_drug<-factor(actg320$IV_drug, levels = 1:3, labels = c("Never", "Current", "Previous")) # Relabel levels for easy understanding
# show first 100 raw variables
head(actg320$IV_drug, 100)
# Count the number of values within each category
table(actg320$IV_drug)
# Percentage
table(actg320$IV_drug)/length(actg320$IV_drug)

# hemophil
#transform this variable to a factor
actg320$hemophil<-as.factor(actg320$hemophil)  #Transform int to factor
actg320$hemophil<-factor(actg320$hemophil, levels = 0:1, labels = c("No", "Yes")) # Relabel levels for easy understanding
# show first 100 raw variables
head(actg320$hemophil, 100)
# Count the number of values within each category
table(actg320$hemophil)
# Percentage
table(actg320$hemophil)/length(actg320$hemophil)


# karnof 
#transform this variable to a factor
actg320$karnof<-as.factor(actg320$karnof)  #Transform int to factor
#actg320$karnof<-factor(actg320$karnof, levels = 1:4, labels = c("70", "80", "90", "100")) # Relabel levels for easy understanding
# show first 100 raw variables
head(actg320$karnof, 100)
# Count the number of values within each category
table(actg320$karnof)
# Percentage
table(actg320$karnof)/length(actg320$karnof)

# Descriptive statistics, Standard deviation and mode calculations for numeric variables

# time 
# Descriptive Statistics
summary (actg320$time)
# Standard deviation  
sd(actg320$time)
# Mode 
Data_time <- actg320
names(sort(-table(Data_time$time)))[1]

# time_d 
# Descriptive Statistics
summary (actg320$time_d)
# Standard deviation  
sd(actg320$time_d)
# Mode 
Data_time_d <- actg320
names(sort(-table(Data_time_d$time_d)))[1]

# CD4_base 
# Descriptive Statistics
summary (actg320$CD4_base)
# Standard deviation  
sd(actg320$CD4_base)
# Mode 
Data_CD4_base <- actg320
names(sort(-table(Data_CD4_base$CD4_base)))[1]

# priorzdv
# Descriptive Statistics
summary (actg320$priorzdv)
# Standard deviation  
sd(actg320$priorzdv)
# Mode 
Data_priorzdv <- actg320
names(sort(-table(Data_priorzdv$priorzdv)))[1]

# age
# Descriptive Statistics
summary (actg320$age)
# Standard deviation 
sd(actg320$age)
# Mode  
Data_age <- actg320
names(sort(-table(Data_age$age)))[1]

str(actg320)

# Remove variables
str(actg320) #Before
actg320$id<-NULL # removed because does not add value 
actg320$Treatment = NULL # remove because of collinearlity with Treatment_grp 
actg320$censor_d = NULL # remove because of collinearlity with censor
actg320$time_d = NULL # remove because of collinearlity with time
actg320$CD4_base = NULL # remove because of collinearlity with CD4_stratum
str(actg320) # After

# check for missing values
apply(actg320, MARGIN =2, anyNA) #no missing variables found

# c) Visualize the distribution of the data 

# Visualizations for factor variables

# censor
# Simple Pie Chart
pie(table(actg320$censor))
# Bar plot
barplot(table(actg320$censor))

# Treatment_grp
# Simple Pie Chart
pie(table(actg320$Treatment_grp))
# Bar plot
barplot(table(actg320$Treatment_grp))

# CD4_Stratum
# Simple Pie Chart
pie(table(actg320$CD4_Stratum))
# Bar plot
barplot(table(actg320$CD4_Stratum))

# sex
# Simple Pie Chart
pie(table(actg320$sex))
# Bar plot
barplot(table(actg320$sex))

# Race
# Simple Pie Chart
pie(table(actg320$Race))
# Bar plot
barplot(table(actg320$Race))

# IV_drug
# Simple Pie Chart
pie(table(actg320$IV_drug))
# Bar plot
barplot(table(actg320$IV_drug))

# hemophil
# Simple Pie Chart
pie(table(actg320$hemophil))
# Bar plot
barplot(table(actg320$hemophil))

# karnof
# Simple Pie Chart
pie(table(actg320$karnof))
# Bar plot
barplot(table(actg320$karnof))


# Visualizations for numeric variables

# By variable: time, type = int
#histogram
hist(actg320$time, main="Time to AIDS diagnosis or death", xlab="Days", ylab = "Frequency") # Left Skewed
#box plot
boxplot(actg320$time, main="Time to AIDS diagnosis or death", xlab="Days", ylab = "Frequency") # Outliers? Yes

# By variable: priorzdv, type = num
#histogram
hist(actg320$priorzdv, main="Months of Prior ZDV Use", xlab="ZDV", ylab = "Months") # Right Skewed
#box plot
boxplot(actg320$priorzdv, main="Months of Prior ZDV Use", xlab="ZDV", ylab = "Months") # Outliers? Yes

# By variable: age, type = int
#histogram
hist(actg320$age, main="Age", xlab="Years", ylab = "Frequency") # Normally distributed
#box plot
boxplot(actg320$age, main="Age", xlab="Years", ylab = "Frequency") # Outliers? Yes

#Preprocessing


#a) remove rows 

# IV_drug remove current drug use
table(actg320$IV_drug) # before
actg320=actg320[-which(actg320$IV_drug==2),]
actg320$IV_drug<-as.factor(actg320$IV_drug)
# Count the number of values within each category
table(actg320$IV_drug) # after

str(actg320)
summary (actg320)

# b) remove outliers from numeric variables

#time variable

# before
boxplot(actg320$time, main="Time", xlab="Time", ylab = "Frequency") #boxplot
summary(actg320$time) # descriptives


# Find the outliers using the quantile() function to find the 25th and the 75th percentile of the dataset, and the IQR() function 
Q <- quantile(actg320$time, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(actg320$time)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range

#Eliminate the outliers
eliminated<- subset(eliminated, time > (Q[1] - 1.5*iqr) & time < (Q[2]+1.5*iqr))

# after
boxplot(eliminated$time, main="Time", xlab="Time", ylab = "Frequency") #boxplot
summary(eliminated$time) # descriptives

#priorzdv variable

# before
boxplot(actg320$priorzdv, main="Months of Prior ZDV Use", xlab="ZDV", ylab = "Months") #boxplot
summary(actg320$priorzdv) # descriptives


# Find the outliers using the quantile() function to find the 25th and the 75th percentile of the dataset, and the IQR() function 
Q <- quantile(actg320$priorzdv, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(actg320$priorzdv)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range

#Eliminate the outliers
eliminated<- subset(eliminated, priorzdv > (Q[1] - 1.5*iqr) & priorzdv < (Q[2]+1.5*iqr))

# after
boxplot(eliminated$priorzdv, main="Months of Prior ZDV Use", xlab="ZDV", ylab = "Months") #boxplot
summary(eliminated$priorzdv) # descriptives

#age variable

# before
boxplot(actg320$age, main="Age", xlab="Age", ylab = "Frequency") #boxplot
summary(actg320$age) # descriptives


# Find the outliers using the quantile() function to find the 25th and the 75th percentile of the dataset, and the IQR() function 
Q <- quantile(actg320$age, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(actg320$age)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range

#Eliminate the outliers
eliminated<- subset(eliminated, age > (Q[1] - 1.5*iqr) & age < (Q[2]+1.5*iqr))

# after
boxplot(eliminated$age, main="Age", xlab="Age", ylab = "Frequency") #boxplot
summary(eliminated$age) # descriptives

# C) remove variables with collinearity 
# Placed earlier in the script

# Model Generation - Logistic Regression

#make sure that the result is reproducible
set.seed(1234)
#split the data into a training and test set
ind <- sample(2, nrow(eliminated), replace = TRUE, prob = c(0.7, 0.3))
train.data <- eliminated [ind == 1, ]
test.data <- eliminated [ind == 2, ]
str(train.data)
str(test.data)

#build the model and store in a variable model
# change
model<-glm(CD4_Stratum~., family=binomial, data=train.data)
#output the coeficients and Residual Deviance
print(model)
#output the coefficient, p value, and standard error for each independent variable and intercept
summary(model)
#output the coefficients and an intercept
exp(coef(model))

#first 10 estimated values
model$fitted.values[1:10]
#confusion matrix for the training set; need to round the estimated values
table(round(model$fitted.values), train.data$CD4_Stratum)
table(round(predict(model, train.data, type="response")), train.data$CD4_Stratum)

#display the first 10 estimated values for the test data
predict (model, test.data, type="response")[1:10]
#store the estimated values in a variable mypredictions; need to round the values
mypredictions<-round(predict (model, test.data, type="response"))
#confusion matrix for the test data
table (mypredictions, test.data$CD4_Stratum)
plot(mypredictions, test.data$CD4_Stratum)

#Confusion matrix for the test data
table (mypredictions, test.data$CD4_Stratum, dnn=c("predicted", "actual"))
# Classification accuracy for test data
predict(model, test.data, type="response")==test.data$CD4_Stratum
label = test.data$CD4_Stratum
levels(label) = c(0,1)
mean(round(predict (model, test.data, type="response"))== label)
# Misclassification error for test data
mean(round(predict (model, test.data, type="response"))!= label)

# Create ROC curve
#install.packages("ROCR")
library(ROCR)
pred <- prediction(mypredictions, test.data$CD4_Stratum)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE, text.adj = c(-0.2,1.7), lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")                  #45 degrees line

#Calculate AUC
auc.tmp <- performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc

#plot the residuals
plot(predict(model),residuals(model), col=c("blue"))
lines(lowess(predict(model),residuals(model)), col=c("black"), lwd=2)
abline(h=0, col="grey")

#Effect plots
#install.packages("effects")
#install.packages("statmod")
library(statmod)
library(effects)
plot(allEffects(model))

#minimal adequate model
summary(step(model))

# Model Generation - Naive Bayesian

library ("e1071")  #Load the package in memory each time you need to use it.
library ("arules") #Load the arules library to use discretization

# Preprocessing

# The Na?ve Bayes method requires all variables in the dataset to be discrete, or factor. 

#Discretize priorzdv
summary(eliminated$priorzdv) #before
eliminated$priorzdv<-discretize(eliminated$priorzdv, method = "frequency", breaks = 6)
summary(eliminated$priorzdv) #after

#make sure that the result is reproducible
set.seed(1234)

#split the data into a training and test set
ind <- sample(2, nrow(eliminated), replace = TRUE, prob = c(0.7, 0.3))
train.data <- eliminated[ind == 1, ]
test.data <- eliminated[ind == 2, ]
str(train.data)
str(test.data)

#build the model and store in a variable model
model<-naiveBayes(CD4_Stratum~., train.data)
#output the model
print(model)

#confusion matrix for the training set; need to round the estimated values
table(predict(model, train.data), train.data$CD4_Stratum)
#confusion matrix for the test data
table(predict(model, test.data), test.data$CD4_Stratum)

#mosaic plot
mosaicplot(table(predict(model, test.data), test.data$CD4_Stratum), shade=TRUE, main="Predicted vs. Actual PEP")

# Plot the ROC Curve
library(ROCR)                                           #Load ROCR to memory
mypredictions<-predict(model, test.data, type="class")
ROCRpred <- prediction(as.numeric(mypredictions)-1, test.data$CD4_Stratum)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.2), lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")                  #45 degrees line
