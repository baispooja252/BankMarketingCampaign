setwd("~/Desktop/predictive/finalproject") 

# Libraray
library(Hmisc)
library(dplyr)
library(arm)
library(Hmisc)
library(dplyr)
library(arm)
library(randomForest)
library(gbm)


# Read File
ds <- read.csv("bank.csv", header = T)
View(ds)

#Know datatype 
str(ds)
sapply(ds,mode)

#
#
#
#
#
#
# Visual Representation of data
# Bar graph representation of clients saying yer/no with respect to previos data set 
library(ggplot2)
ggplot(ds %>% group_by(previous, deposit) %>%tally(),
       aes(previous, n, fill = deposit)) +
  geom_col() +
  theme_bw()

# Corelation Matrix 
install.packages("corrplot")
library(corrplot)

ds_cor <- select_if(ds, is.numeric) %>% cor()
corrplot(ds_cor, method = "number")

# Stacked Bargraph representing relation between deposit and campaign w.r.t. marital status
ggplot(ds, aes(fill=marital, y=campaign, x=deposit)) + 
  geom_bar(position="stack", stat="identity")

# Stacked Bargraph representing relation between deposit and balance w.r.t. marital status
ggplot(ds, aes(fill=marital, y=balance, x=deposit)) + 
  geom_bar(position="stack", stat="identity")


#Convert categorical data into numerical data
ds1 <- ds %>% mutate_if(is.factor, as.numeric)

#Convert "deposit" data column into binary type i.e. 1s and 0s
for(i in 1:nrow(ds1))
{
  if(ds1$deposit[i] == 2)
  {
    ds1$is_deposit[i]=1
  }
  else
  {  
    ds1$is_deposit[i]=0
  }
}

View(ds1)



#Train 80% of ds and test 20% of data
set.seed(123)
train_index = sample(1:nrow(ds1),0.8*nrow(ds1), replace = T)

d_train <- ds1[train_index, ]
d_test <- ds1[-train_index, ]

train <- d_train[-17]  #remove deposit column
test <- d_test[-c(17,18)] #remove deposit and is_deposit(output) column

View(test)




#Logistic Regression 
#bayesglm is used so that algorithm can converge well 
glm_model=bayesglm(is_deposit~.,data = train, family= binomial(link= "logit"))
summary(glm_model)

pred <- predict(glm_model,test,type = "response")
View(pred)

#Confusion matrix 
table_mat <- table(d_test$is_deposit, pred > 0.5)
table_mat

#Accuracy of the GLM model
accuracy_Test_glm <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test_glm



###################
# Random Forest

rf <- randomForest(is_deposit~.,data = train)
rf
p_rf <- predict(rf, test , type = 'response')
p_rf

#Confusion matrix for Random Forest
cm_rf <- table(d_test$is_deposit, p_rf > 0.5)
cm_rf

#Accuracy of Random Forest

accuracy_rf <- sum(diag(cm_rf)) / sum(cm_rf)
accuracy_rf


#------------------------------
#Gradient Boost

gbm <- gbm(is_deposit~., data = train, distribution = "gaussian")
gbm
p_gbm <- predict(gbm, test , type = 'response', n.trees = gbm$n.trees)
p_gbm

#Confusion matrix for Gradient Boost
cm_gbm <- table(d_test$is_deposit, p_gbm > 0.5)
cm_gbm

#Accuracy of Gradient Boost

accuracy_gbm <- sum(diag(cm_gbm)) / sum(cm_gbm)
accuracy_gbm

#---------------------------------------------
install.packages("e1071")
library(e1071)
install.packages("neuralnet")
library(neuralnet)

#Naive Bayes
naive_bayes <- naiveBayes(is_deposit~.,data = train)
nb=(predict(naive_bayes,newdata=test,type="raw"))

#Confusion Matrix for Naive Bayes
cm_nb <- table(d_test$is_deposit, nb[,2] > 0.5)
cm_nb

#Accuracy of Naive Bayes
accuracy_nb <- sum(diag(cm_nb)) / sum(cm_nb)
accuracy_nb

#Neural Networks
nn=neuralnet(is_deposit~.,data=train,hidden = 3,act.fct = "logistic",stepmax = 1e6)
p_nn= predict(nn, test)




#Confusion Matrix for Neural Networks
cm_nn <- table(d_test$is_deposit, p_nn > 0.5)
cm_nn

#Accuracy of Neural Networks
accuracy_nn <- sum(diag(cm_nn)) / sum(cm_nn)
accuracy_nn


