library(foreign)
library(dplyr)
library(randomForest)
data1 <- read.arff("C:/Users/jsonav2/OneDrive - University of Illinois at Chicago/IDS 572/Training Dataset.arff")
data.frame(data1)
View(data1)
sum(duplicated(data1))
data2 <- unique(data1)
dim(data2)
#--------------------------------------------------------------------
library(caret)
library(dplyr)
data2$having_IP_Address<-as.factor(data2$having_IP_Address)
data2$URL_Length<-as.factor(data2$URL_Length)
data2$Shortining_Service<-as.factor(data2$Shortining_Service)
data2$having_At_Symbol<-as.factor(data2$having_At_Symbol)
data2$double_slash_redirecting<-as.factor(data2$double_slash_redirecting)
data2$Prefix_Suffix<-as.factor(data2$Prefix_Suffix)
data2$having_Sub_Domain<-as.factor(data2$having_Sub_Domain)
data2$SSLfinal_State<-as.factor(data2$SSLfinal_State)
data2$Domain_registeration_length<-as.factor(data2$Domain_registeration_length)
data2$Favicon<-as.factor(data2$Favicon)
data2$port<-as.factor(data2$port)
data2$HTTPS_token<-as.factor(data2$HTTPS_token)
data2$Request_URL<-as.factor(data2$Request_URL)
data2$URL_of_Anchor<-as.factor(data2$URL_of_Anchor)
data2$Links_in_tags<-as.factor(data2$Links_in_tags)
data2$SFH<-as.factor(data2$SFH)
data2$Submitting_to_email<-as.factor(data2$Submitting_to_email)
data2$Abnormal_URL<-as.factor(data2$Abnormal_URL)
data2$Redirect<-as.factor(data2$Redirect)
data2$on_mouseover<-as.factor(data2$on_mouseover)
data2$RightClick<-as.factor(data2$RightClick)
data2$popUpWidnow<-as.factor(data2$popUpWidnow)
data2$Iframe<-as.factor(data2$Iframe)
data2$age_of_domain<-as.factor(data2$age_of_domain)
data2$DNSRecord<-as.factor(data2$DNSRecord)
data2$web_traffic<-as.factor(data2$web_traffic)
data2$Page_Rank<-as.factor(data2$Page_Rank)
data2$Google_Index<-as.factor(data2$Google_Index)
data2$Links_pointing_to_page<-as.factor(data2$Links_pointing_to_page)
data2$Statistical_report<-as.factor(data2$Statistical_report)
data2$Result<-as.factor(data2$Result)


dummy <- dummyVars(Result~ ., data=data2)
dummy
newdata <- data.frame(predict(dummy, newdata = data2))
head(newdata)

Result=data2$Result
alldata<- cbind(newdata,Result)
View(alldata)


#-----------------------------------
Data <- data2[sample(nrow(data2)), ]


k <- 10
nmethod <- 3
folds <- cut(seq(1,nrow(data2)),breaks=k,labels=FALSE) 
model.err <- matrix(-1,k,nmethod,dimnames=list(paste0("Fold", 1:k), c("DT","RF","NB")))


library(rpart)
for(i in 1:k)
{ 
  testindexes <- which(folds==i, arr.ind=TRUE) 
  test <- alldata[testindexes, ] 
  train <- alldata[-testindexes, ] 
  
  
  decisionTree <- rpart(Result ~ ., data = train , method = "class")
  DTpredicted_prob <- predict(decisionTree, newdata = test, type = "prob") # to find the probabilites
  DTpredicted_class <- predict(decisionTree, newdata = test, type = "class")# to find the class
  model.err[i,"DT"] <- mean(test$Result != DTpredicted_class) # to find the error
  
  #--------------------------------------------------
  #random forest
  indx<- sample(2,nrow(myData_2),replace=TRUE,prob=c(0.8,0.2))
  train = myData_2[indx==1,]
  test = myData_2[indx==2,]
  
  rf = randomForest(as.factor(Result)~., data=train,ntree=100,norm.votes=TRUE,replace=TRUE,
                    mtry=sqrt(ncol(train)-1),proximity=TRUE,importance=TRUE,
                    type='classification')
  rfpredict_test<- predict(rf,test,type='class')
  model.err[i,"RF"] <- mean(test$Result != rfpredict_test)
  
  #-----------------------------------------------------------------
  #naive_bayes
  library(e1071)
  naive_e1071 <- naiveBayes(Result ~ ., data = train)
  
  nb_predict <- predict(naive_e1071, newdata = test, type = "raw")
  preded <- predict(naive_e1071, newdata = test)
  table(preded, test$Result)
  model.err[i,"NB"] <- mean(test$Result != preded)
  
}


# -----------------------
myData_2 = alldata
summary(myData_2$Result)

indx<- sample(2,nrow(myData_2),replace=TRUE,prob=c(0.8,0.2))
train = myData_2[indx==1,]
test = myData_2[indx==2,]

rf = randomForest(as.factor(Result)~., data=train,ntree=100,norm.votes=TRUE,replace=TRUE,
                  mtry=sqrt(ncol(train)-1),proximity=TRUE,importance=TRUE,
                  type='classification')


library(caret)

print(rf)
rf$proximity
rf$importance
rf$votes[,2]
rf$predicted
confusionMatrix(rf$predicted,train$Result)
mean(rf$err.rate[,1])


predict_train<- predict(rf,train,type='class')
predict_test<- predict(rf,test,type='class')
confusionMatrix(predict_train,train$Result)
confusionMatrix(predict_test,test$Result)
#--------------------------------------------------------------------


mean(model.err)




# -----------------------
myData_2 = data2
summary(myData_2$Result)x
train = sample(4000,1561)
rf.model = randomForest(Result~., data=myData_2,subset=train,mtry=3,ntree=100,importance=TRUE)
print(rf.model)
predict.randomForest(rf.model)
mean(rf.model$err.rate[,1])
rf.pred = predict(rf.model,myData_2[-train,],type="class")
importance(rf.model)
table(rf.pred,myData_2[-train,]$Result)
mean(rf.pred == myData_2[-train,]$Result)
#In random forests, there is no need for cross-validation, 
#or a separate test set to get an unbiased estimate of the test set error. 
#It is estimated internally, during the run

# -------------------------------------------------------------
#rf with CV

library(caret)
set.seed(100)
inTrain <- createDataPartition(y = data2$Result, p = .7, list = FALSE)
iris.train <- data2[inTrain, ]
iris.test <- data2[- inTrain, ]
fit.control <- caret::trainControl(method = "cv", number = 10)

rf.fit <- caret::train(Result ~ .,
                       data = iris.train,
                       method = "rf",
                       trControl = fit.control)

# ----------------------------------------------------
#naive_bayes with 10k fold
library(naivebayes)
set.seed(100)
trctrl <- trainControl(method = "cv", number = 10, savePredictions=TRUE)
nb.fit <- train(Result ~ .,
                       data = alldata,
                       method = "naive_bayes",
                       trControl = trctrl)
print(nb.fit)
confusionMatrix(nb.fit)

#----------------------------------------------------------

# naive_bayes teacher's method
library(rsample)
set.seed(123)
split <- initial_split(data2, prop = .7, strata = NULL)
nb_train <- training(split)
nb_test  <- testing(split)



