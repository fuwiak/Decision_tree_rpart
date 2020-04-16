
#before running code
#install.packages("rpart.plot")
# install.packages("caret")
# install.packages('e1071', dependencies=TRUE)
# install.packages("corrplot")
# install.packages("varhandle")

#put here your dir path
setwd("~/Desktop/marta174")



#decision tree rpart
library("rpart")
library("rpart.plot")
library(rpart)
library(dplyr)
library(caret)
library(varhandle)
#library(tidyverse)


#Import the data

df <- read.csv("my_churn.csv")
col_nam <-colnames(df)


#dataframe statistics
summary(df)


#drop selected columns
df <- subset(df, select = -c(X...customerID))



#convert datatypes and clean the dataset
#df.na
df<-na.omit(df)

#df$Churn<-as.numeric(df$Churn)

#feature selection using random forest
library(randomForest)
fit_rf = randomForest(Churn~., data=df, ntree=100, proximity=T)
# Create an importance based on mean decreasing gini
importance(fit_rf) #select columns with lowest gini
#df <- subset(df, select = c(name1, name2, ...)) #name1--> first selected column, name2--> second etc,




# Create train/test set
create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1:total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

data_train <- create_train_test(df, 0.6, train = TRUE)
data_test <- create_train_test(df, 0.6, train = FALSE)



#zbudowanie drzew... itp
#decision tree visualisation, using rprart and ctree, fit --> rpart, fit2---> ctree

fit<-rpart(Churn~., data=data_train, method = "class",minbucket=50) #prosze tutaj zmienic liczbe minbucket by ustalic optymalna liczbe lisci
prp(fit) # pierwszy sposob rysowania drzeewa

rpart.plot::rpart.plot(fit) # drugi sposob rysowania drzewa

fit2<-partykit::ctree(Churn~., data=data_train) # trzeci sposob rysowania drzewa

prp(fit2)

#variable importance(decision tree) - 
#zbadanie zaleznosci pomiedzy zmienna churn a pozostalymi istotnymi zmiennymi


varImp(fit)



#Performance Evaluation

pred<-predict(object = fit,  
        newdata = data_test,   
        type = "class")

#confusion matrix
#library(caret)
confusionMatrix(data = pred,       
                reference = data_test$Churn)



