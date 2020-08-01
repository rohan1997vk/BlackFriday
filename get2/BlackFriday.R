library(dplyr)
library(mltools)
library(data.table)

#Loading Data
setwd("C:/Users/rohan/OneDrive/Desktop/AnalyticsVidya-BlackFriday")
train <- read.csv("new_train3.csv")
test <- read.csv("test.csv")
submission <- read.csv("sample_submission_AN0dlrC.csv")
City <- one_hot(as.data.table(train$City_Category))
f_train <- data.frame(City = City, 
                      Gender = train$Gender, 
                      Age = train$Age, 
                      Occupation = train$Occupation,
                      City_years = train$Stay_In_Current_City_Years,
                      Marital_Status = train$Marital_Status,
                      PROD_1 = train$Product_Category_1,
                      PROD_2 = train$PROD_2,
                      PROD_3 = train$PROD_3,
                      Purchase = train$Purchase) 
#Multiple Regression
regressor_mr <- lm(Purchase ~., data = f_train)
#XGBOOST
r_1 <- read.csv("r_1.csv")
s_1 <- read.csv("s_1.csv")

submission <- read.csv("sample_submission_AN0dlrC.csv")
r_1<- as.matrix(r_1, rownames.force=NA)
s_1<- as.matrix(s_1, rownames.force=NA)
train_1 <- as(r_1, "sparseMatrix")
test_2 <- as(s_1, "sparseMatrix")
# Never forget to exclude objective variable in 'data option'
train_Data <- xgboost(data = train_1, label = train_1[,"Purchase"],nrounds=10)
y_pred = predict(train_Data, newdata = test_2)

regressor = randomForest(x = r_1[,-c(1,2)],
                         y = r_1$Purchase,
                         ntree = 50)

train$Gender <- factor(train$Gender, levels = c("F","M"), labels = c(0,1))

Age <- one_hot(as.data.table(train$Age))
City <- one_hot(as.data.table(train$City_Category))
Stay_In_Current_City_Years <- one_hot(as.data.table(train$Stay_In_Current_City_Years))
Marital_Status <- one_hot(as.data.table(train$Marital_Status))
train$Gender <- as.numeric(train$Gender)
train$Product_ID <- as.numeric(train$Product_ID)
t1 <- data.frame(Age = Age,
                 City = City,
                 Stay = Stay_In_Current_City_Years,
                 MS = Marital_Status,
                 User_ID = train$User_ID,
                 Product_ID = train$Product_ID,
                 Gender = train$Gender,
                 Occupation = train$Occupation,
                 Product_Category_1 = train$Product_Category_1,
                 Product_Category_2 = train$Product_Category_2,
                 Product_Category_3 = train$Product_Category_3,
                 Purchase = train$Purchase)

amelia_try <- amelia(t1)
r_5 <- amelia_try$imputations[[5]]
r_4 <- amelia_try$imputations[[4]]
r_3 <- amelia_try$imputations[[3]]
r_2 <- amelia_try$imputations[[2]]
r_1 <- amelia_try$imputations[[1]]


test$Gender <- factor(test$Gender, levels = c("F","M"), labels = c(0,1))

Age_test <- one_hot(as.data.table(test$Age))
City_test <- one_hot(as.data.table(test$City_Category))
Stay_In_Current_City_Years_test <- one_hot(as.data.table(test$Stay_In_Current_City_Years))
Marital_Status_test <- one_hot(as.data.table(test$Marital_Status))
test$Gender <- as.numeric(test$Gender)
test$Product_ID <- as.numeric(test$Product_ID)
t2 <- data.frame(Age = Age_test,
                 City = City_test,
                 Stay = Stay_In_Current_City_Years_test,
                 MS = Marital_Status_test,
                 User_ID = test$User_ID,
                 Product_ID = test$Product_ID,
                 Gender = test$Gender,
                 Occupation = test$Occupation,
                 Product_Category_1 = test$Product_Category_1,
                 Product_Category_2 = test$Product_Category_2,
                 Product_Category_3 = test$Product_Category_3
)

amelia_try1 <- amelia(t2)
rt_5 <- amelia_try$imputations[[5]]
rt_4 <- amelia_try$imputations[[4]]
rt_3 <- amelia_try$imputations[[3]]
rt_2 <- amelia_try$imputations[[2]]
rt_1 <- amelia_try$imputations[[1]]


R1_REG <- lm(Purchase ~., data = r_1)
y_pred_r1 <- predict(R1_REG, newdata = t2)
submission$Purchase <- y_pred_r1

amelia_try <- amelia(t2)
r_5 <- amelia_try$imputations[[5]]
r_4 <- amelia_try$imputations[[4]]
r_3 <- amelia_try$imputations[[3]]
r_2 <- amelia_try$imputations[[2]]
r_1 <- amelia_try$imputations[[1]]

