library(dplyr)
library(ROSE)
library(usdata)
library(rpart)
library(caret)
library(nnet)
df1_state <- read.csv("C://Users/kkonar2/OneDrive - University of Illinois at Chicago/Documents/Fall 2021/Data Mining/HW3/State.csv")
df2_mort <-read.csv("C://Users/kkonar2/OneDrive - University of Illinois at Chicago/Documents/Fall 2021/Data Mining/HW3/MortgageDefaulters.csv")

df1_state$State[df1_state$State=="Districto Columbia"] <- "District of Columbia"
df1_state$State[df1_state$State=="New Maxico"] <- "New Mexico"
df1_state$State <- state2abbr(df1_state$State)

df4 <- merge(df2_mort, df1_state)
df4 = select(df4,-12)
df4 <- df4 %>%
  filter(LoanValuetoAppraised!="#DIV/0!")

indx <- sample(2, nrow(df4), replace = TRUE, prob = c(0.75,0.25))
train_data <- df4[indx == 1,]
test_data <- df4[indx == 2 ,]


TrgA<-ovun.sample(OUTCOME~.,data= df4,method="under",N=1333)
TrgA<-data.frame(TrgA$data)
indx <- sample(2, nrow(TrgA), replace = TRUE, prob = c(0.75,0.25))
train_data_A <- TrgA[indx == 1,]
test_data_A <- TrgA[indx == 2 ,]


TrgB<-ovun.sample(OUTCOME~.,data= df4,method="under",N=4000)
TrgB<-data.frame(TrgB$data)
indx <- sample(2, nrow(TrgA), replace = TRUE, prob = c(0.75,0.25))
train_data_B <- TrgB[indx == 1,]
test_data_B <- TrgB[indx == 2 ,]

#Decision tree Model
#Train_data_A
tree_modelA <- rpart(OUTCOME~ ., data = train_data_A)
tree_pred_class_trainA <- predict(tree_modelA, train_data_A, type = "class")
mean(train_data_A$OUTCOME != tree_pred_class_trainA)

#Test_data_A
id <- which(!(test_data_A$State %in% levels(train_data_A$State)))
test_data_A$State[id] <- NA
tree_pred_class_testA <- predict(tree_modelA, test_data_A, type = "class")
mean(test_data_A$OUTCOME != tree_pred_class_trainA)
confusionMatrix(as.factor(tree_pred_class_testA), as.factor(test_data_A$OUTCOME))


#Train_data_B
tree_modelB <- rpart(OUTCOME~ ., data = train_data_B)
tree_pred_class_trainB <- predict(tree_modelB, train_data_B, type = "class")
mean(train_data_B$OUTCOME != tree_pred_class_trainB)


#Test_data_B
id <- which(!(test_data_B$State %in% levels(train_data_B$State)))
test_data_B$State[id] <- NA
id_median <- which(!(test_data_B$median.income.... %in% levels(train_data_B$median.income....)))
test_data_B$median.income....[id_median] <- NA
tree_pred_class_testB <- predict(tree_modelB, test_data_B, type = "class")
mean(test_data_B$OUTCOME != tree_pred_class_trainB)
confusionMatrix(as.factor(tree_pred_class_testB), as.factor(test_data_B$OUTCOME))


#Train_data
tree_modelD <- rpart(OUTCOME~ ., data = train_data)
tree_pred_class_trainD <- predict(tree_modelD, train_data, type = "class")
mean(train_data$OUTCOME != tree_pred_class_trainD)
id <- which(!(test_data$LoanValuetoAppraised %in% levels(train_data$LoanValuetoAppraised)))
test_data$LoanValuetoAppraised[id] <- NA


#Test_data
tree_full_pred_class <- predict(tree_modelD, test_data, type = "class")
mean(test_data$OUTCOME != tree_full_pred_class)
confusionMatrix(as.factor(tree_full_pred_class), as.factor(test_data$OUTCOME))

#Logisitic regression model
#train_data_A
logitModel_A <- glm(as.factor(OUTCOME) ~ ., data = train_data_A, family = "binomial")
Pred_A <- predict(logitModel_A, newdata = train_data_A, type = "response")
Class_train_A <- ifelse(Pred_A >= 0.5, "non-default", "default")
mean(train_data_A$OUTCOME==Class_train_A)
cutoff=1/2
prob.logitModel_A <- predict(logitModel_A, type = "response")
predicted.logitModel_A <- prob.logitModel_A > cutoff
predicted.logitModel_A_CM<- as.numeric(predicted.logitModel_A)#confusion matrix
conf_traindata_A<-table(train_data_A$OUTCOME, predicted.logitModel_A_CM, dnn = c("Actual", "Predicted"))
null.model <- glm(as.factor(OUTCOME) ~ 1, family = binomial, train_data_A)
logit_modelA.step.aic<-step(null.model, scope=list(lower=null.model, upper=logitModel_A), k = 2, direction="forward")

#for stepwise model --
AICprob.logit_model_A <- predict(logit_modelA.step.aic, type = "response")
predicted.logitModel_A1 <- as.numeric(AICprob.logit_model_A > cutoff)#confusion matrix
table(train_data_A$OUTCOME, predicted.logitModel_A1, dnn = c("Actual", "Predicted"))


#test_data_A
id <- which(!(test_data_A$State %in% levels(train_data_A$State)))
test_data_A$State[id] <- NA
Pred_test_A <- predict(logitModel_A, newdata = test_data_A, type = "response")
Class_test_A <- ifelse(Pred_test_A >= 0.5, "non-default", "default")
mean(test_data_A$OUTCOME == Class_test_A)
conf_testdata_A<-table(test_data_A$OUTCOME, predicted.logitModel_A_CM, dnn = c("Actual", "Predicted"))

#train_data_B
logitModel_B <- glm(as.factor(OUTCOME) ~ ., data = train_data_B, family = "binomial")
Pred_B <- predict(logitModel_B, newdata = train_data_B, type = "response")
Class_train_B <- ifelse(Pred_B >= 0.5, "non-default", "default")
mean(train_data_B$OUTCOME == Class_train_B)

#test_data_B
id <- which(!(test_data_B$State %in% levels(train_data_B$State)))
test_data_B$State[id] <- NA
Pred_test_B <- predict(logitModel_B, newdata = test_data_B, type = "response")
Class_test_B <- ifelse(Pred_test_B >= 0.5, "non-default", "default")
mean(test_data_B$OUTCOME == Class_test_B)

#train_data
logitModel_D <- glm(as.factor(OUTCOME) ~ ., data = train_data, family = "binomial")
Pred_D <- predict(logitModel_D, newdata = train_data, type = "response")
Class_train <- ifelse(Pred_D >= 0.5, "non-default", "default")
mean(train_data$OUTCOME == Class_train)

#test_data
Pred_test_D <- predict(logitModel_D, newdata = test_data, type = "response")
Class_test <- ifelse(Pred_B >= 0.5, "non-default", "default")
mean(test_data$OUTCOME == Class_test)

#Neural network
#Train_data
X_train <- scale(train[, c(1:2)]) 
y_train <- train$y 
dim(y_train) <- c(length(y_train), 1) # add extra dimension to vector 
X_test <- scale(test[, c(1:2)])
y_test <- test$y 
dim(y_test) <- c(length(y_test), 1)
nn <- nnet(OUTCOME~.,data=train_data, linout=T, size=10, decay=0.01)
