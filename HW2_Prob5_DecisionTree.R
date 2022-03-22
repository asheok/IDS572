library(foreign)
install.packages("devtools")
devtools::install_github("devanshagr/CrossValidation")
library(caret)
library(rpart.plot)
phis <- read.arff("C:/Users/kkonar2/OneDrive - University of Illinois at Chicago/Documents/Online/OneDrive - University of Illinois at Chicago/Training Dataset.arff")
indx <- sample(2, nrow(phis), replace = T, prob = c(0.7,0.3))
train <- phis[indx == 1,]
test <- phis[indx == 2,]

tree_model <- rpart(Result~., data=train)
rpart.plot(tree_model)
print(tree_model)


tree_pred_prod <- predict(tree_model, test, type = "prob")
tree_pred_class <- predict(tree_model, test, type = "class")
test$Result == tree_pred_class #to find the accuracy of the model
mean(test$Result == tree_pred_class)

tree_pred_class_train <- predict(tree_model,train, type = "class")
mean(train$Result != tree_pred_class_train) #!= for error rate

CrossValidation::cross_validate(train,tree_model, 10, 0.7)
