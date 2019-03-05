##script for Chapter 1 of Zumel and Mount

#importing libraries that might be useful
library(tidyverse)
library(rpart)
#set working directory
setwd("D:/lab_misc/data_science_study_group/data_science_study_group/Statlog")


load('GCDData.RData')

head(d)
View(d)
str(d)
?rpart
?rpart.control

#we would need to undertand Recursive Partitioning and Regression Tree models to construct this wisely
#need to make decisions about control, method
model <- rpart(Good.Loan ~
  Duration.in.month + 
  Installment.rate.in.percentage.of.disposable.income + 
  Credit.amount +
  Other.installment.plans,
  data=d,
  control=rpart.control(maxdepth=4),
  method="class")

model
#we should discuss what lines in the model output mean

resultframe <- data.frame(Good.Loan=creditdata$Good.Loan, pred=predict(model, type="class"))
resultframe
#we've created a new dataframe that contains 2 columns--the Good.Loan column from the input "d" dataset, 
#and our prediction 

rtab <- table(resultframe)
rtab
#rows are actual loan status, columns are predictions, diagonals are correct predictions

sum(diag(rtab))/sum(rtab) #overall model accuracy
sum(rtab[1,1])/sum(rtab[,1]) #precision; 76% of applicants predicted as bad really did default
sum(rtab[1,1])/sum(rtab[1,]) #recall; model found 14$ of defaulting loands
sum(rtab[2,1])/sum(rtab[2,]) # false positive rate; 2% of good applicants were mistakenly identified as bad

#relation between disposable income and loan outcome
#testing rule that loand >15% of disposable income will default
#looking for unexplainable variance in our output--how much of variation can't be explained by input variables
#population 1:
tab1
#loans as pct of disposable income do a good job of explaining loan quality
# all loans <15% were good; all but 6 >15% were bad

sum(diag(tab1))/sum(tab1) #94% accurate

#population 2:
tab2
#bad job of explaining loan quality
sum(diag(tab2))/sum(tab2) #66% accurate
#for population 2, we'll need additional input variables to meet our goal of building a model >70% accurate
