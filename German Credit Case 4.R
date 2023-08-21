#############Start of the Code ###############

german_credit = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")

colnames(german_credit) = c("chk_acct", "duration", "credit_his", "purpose", "amount", "saving_acct", "present_emp", "installment_rate", "sex", "other_debtor", "present_resid", "property", "age", "other_install", "housing", "n_credits", "job", "n_people", "telephone", "foreign", "response")

# orginal response coding 1= good, 2 = bad we need 0 = good, 1 = bad

german_credit$response = german_credit$response - 1

#############End of the Code ###############
str(german_credit)
#Load libraries
library(rpart)
library(rpart.plot)
library(dplyr)
#split data into training and testing
#index <- sample(nrow(german_credit),nrow(german_credit)*.80)
german_train <- german_credit[index,]
german_test <- german_credit[-index,]

#Fit Classification tree
credit_rpart <- rpart(response ~., data = german_train, method = "class", parms = list(loss=matrix(c(0,5,1,0), nrow = 2)))
prp(credit_rpart)
#asymmetric
credit_rpart0 <- rpart(response ~., data = german_train, method = "class" )

#predict and run misclassification table
#INSAMPLE
pred_credit.train <- predict(credit_rpart, german_train, type = "class")
table(german_train$response, (pred_credit.train > 0.5)*1, dnn=c("True", "Predicted"))

#OUT SAMPLE PREDICT AND MISCLASSIFICATION TABLE
pred_credit.test <- predict(credit_rpart,german_test, type = "class")
table(german_test$response, pred_credit.test, dnn = c("True","Predicted"))

#Cost Function
cost <- function(r,phat){
  weight1 <- 5
  weight0 <- 1
  pcut <- weight0/(weight1 + weight0)
  c1 <- (r==1)&(phat<pcut)
  c0 <- (r==0)&(phat>pcut)
  return(mean(weight1*c1+weight0*c0))
}

#insample cost
cost(german_train$response, predict(credit_rpart, german_train, type = "prob"))

#Outsample cost
cost(german_test$response, predict(credit_rpart,german_test, type = "prob"))
