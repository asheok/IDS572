#===================================================================================#
#Logisitic regression model
#train_data_A(TrgA)
logitModel_A <- glm(as.factor(OUTCOME) ~ ., data = train_data_A, family = "binomial")
Pred_A <- predict(logitModel_A, newdata = train_data_A, type = "response")
Class_train_A <- ifelse(Pred_A >= 0.5, "non-default", "default")
mean(train_data_A$OUTCOME!=Class_train_A)
cutoff=2/3
prob.logitModel_A <- predict(logitModel_A, type = "response")
predicted.logitModel_A <- prob.logitModel_A > cutoff
predicted.logitModel_A_CM<- as.numeric(predicted.logitModel_A)#confusion matrix
table(train_data_A$OUTCOME, predicted.logitModel_A_CM, dnn = c("Actual", "Predicted"))
null.modelA <- glm(as.factor(OUTCOME) ~ 1, family = binomial, train_data_A)
logit_modelA.step.aic<-step(null.modelA, scope=list(lower=null.modelA, upper=logitModel_A), k = 2, direction="both")

#for stepwise model --
AICprob.logit_model_A <- predict(logit_modelA.step.aic, type = "response")
predicted.logitModel_A1 <- as.numeric(AICprob.logit_model_A > cutoff)#confusion matrix
table(train_data_A$OUTCOME, Class_train_A, dnn = c("Actual", "Predicted"))
mean(as.factor(test_data$OUTCOME) != as.factor(Class_train_A))

#train_data_B(TrgB)
logitModel_B <- glm(as.factor(OUTCOME) ~ ., data = train_data_B, family = "binomial")
Pred_B <- predict(logitModel_B, newdata = train_data_B, type = "response")
Class_train_B <- ifelse(Pred_B >= 0.5, "non-default", "default")
mean(train_data_B$OUTCOME!=Class_train_B)
cutoff= 2/3
prob.logitModel_B <- predict(logitModel_B, type = "response")
predicted.logitModel_B <- prob.logitModel_B > cutoff
predicted.logitModel_B_CM<- as.numeric(predicted.logitModel_B)#confusion matrix
table(train_data_B$OUTCOME, predicted.logitModel_B_CM, dnn = c("Actual", "Predicted"))
null.modelB <- glm(as.factor(OUTCOME) ~ 1, family = binomial, train_data_B)
logit_modelB.step.aic<-step(null.modelB, scope=list(lower=null.modelB, upper=logitModel_B), k = 2, direction="both")

#for stepwise model --
AICprob.logit_model_B <- predict(logit_modelB.step.aic, type = "response")
predicted.logitModel_B1 <- as.numeric(AICprob.logit_model_B > cutoff)#confusion matrix
table(train_data_B$OUTCOME, Class_train_B, dnn = c("Actual", "Predicted"))
mean(as.factor(test_data$OUTCOME) != as.factor(Class_train_B))

#train_data
logitModel <- glm(as.factor(OUTCOME) ~ ., data = train_data, family = "binomial")
Pred <- predict(logitModel, newdata = train_data, type = "response")
Class_train <- ifelse(Pred >= 0.5, "non-default", "default")
mean(train_data$OUTCOME!=Class_train)
mean(test_data$OUTCOME!=Class_train)
cutoff= 2/3
prob.logitModel <- predict(logitModel, type = "response")
predicted.logitModel <- prob.logitModel > cutoff
predicted.logitModel_CM<- as.numeric(predicted.logitModel)#confusion matrix
table(train_data$OUTCOME, predicted.logitModel_CM, dnn = c("Actual", "Predicted"))
null.model <- glm(as.factor(OUTCOME) ~ 1, family = binomial, train_data)
logit_model.step.aic<-step(null.model, scope=list(lower=null.model, upper=logitModel), k = 2, direction="both")

#for stepwise model --
AICprob.logit_model <- predict(logit_model.step.aic, type = "response")
predicted.logitModel_1 <- as.numeric(AICprob.logit_model > cutoff)#confusion matrix
table(train_data$OUTCOME, Class_train, dnn = c("Actual", "Predicted"))
mean(as.factor(test_data$OUTCOME) != as.factor(Class_train))

#====================================================================================#
#Neural network
#train_data_A(TrgA)
set.seed(123)
nn_A <- nnet(as.factor(OUTCOME)~ ., data=train_data_A, subset = indx,
             size=2, decay=0.01, maxit=3)
nn.predsA<- predict(nn_A,test_data, type = "class")
plot.nnet(nn_A)
mean((nn.predsA-test_data$OUTCOME)^2) 
scoreA<-nn.predsA
pred.nnA <- prediction(scoreA, test_data$OUTCOME) 

#train_data_B(TrgB)
nn_B <- nnet(as.factor(OUTCOME) ~ ., data=train_data_B, subset = indx,
             size=2, decay=0.01, maxit=3)
nn.predsB<- predict(nn_B,test_data,type = "class")
plot.nnet(nn_B)
mean((nn.predsA-test_data$OUTCOME)^2) 


#train_data
nn <- nnet(as.factor(OUTCOME) ~ ., data=train_data, subset = indx,
             size=2, decay=0.01, maxit=3)
nn.pred<- predict(nn,test_data,type = "class")
plot.nnet(nn)
mean((nn.pred-test_data$OUTCOME)^2) 

  
#====================================================================================#


