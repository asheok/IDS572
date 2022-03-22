library(dplyr)
library(ROSE)
library(usdata)
library(rpart)
df1_state <- read.csv("C://Users/kkonar2/OneDrive - University of Illinois at Chicago/Documents/Fall 2021/Data Mining/HW3/State.csv")
df2_mort <-read.csv("C://Users/kkonar2/OneDrive - University of Illinois at Chicago/Documents/Fall 2021/Data Mining/HW3/MortgageDefaulters.csv")

df1_state$State[df1_state$State=="Districto Columbia"] <- "District of Columbia"
df1_state$State[df1_state$State=="New Maxico"] <- "New Mexico"
df1_state$State <- state2abbr(df1_state$State)

df3 <- merge(df2_mort, df1_state)
df4 <- merge(df2_mort, df1_state)
df4 = select(df4,-12)
df4 <- df4 %>%
  filter(LoanValuetoAppraised!="#DIV/0!")
df4 <- df4$LoanValuetoAppraised*1.0


df3$age45 <- ifelse(df3$Bo_Age >= 45, "old" , "young")
df3 %>% count(age45,Status)

df3$age45 <- ifelse(df3$Bo_Age >= 35, "old" , "young")
df3 %>% count(age45,Status)



df3$age45 <- ifelse(df3$Bo_Age >= 25, "old" , "young")
df3 %>% count(age45,Status)



df3$newratio <- ifelse(df3$DTI.Ratio <= 0.2 & df3$DTI.Ratio >=0.0, "+ve", "-ve")
df3%>% count(newratio,Status)




df3$score <- ifelse(df3$Credit_score >= 700, "GE_700" , "LE_700")
df3 %>% count(score,Status)
df3 %>% count(score,newratio,Status)



df3$score <- ifelse(df3$Credit_score >= 750, "GE_750" , "LE_750")
df3 %>% count(score,Status)
df3 %>% count(score,newratio,Status)



df3$score <- ifelse(df3$Credit_score >= 650, "GE_650" , "LE_650")
df3 %>% count(score,Status)
df3 %>% count(score,newratio,Status)



df3$score <- ifelse(df3$Credit_score >= 600, "GE_600" , "LE_600")
df3 %>% count(score,Status)
df3 %>% count(score,newratio,Status)




df3 %>% count(First_home,Status)



df3 %>% count(UPB.Appraisal,Status)




grp1<-df3 %>%
  group_by(is.na(df4$LoanValuetoAppraised))%>%
  summarise(n=n())
grp1




indx <- sample(2, nrow(df4), replace = TRUE, prob = c(0.75,0.25))
train_data <- df4[indx == 1,]
test_data <- df4[indx == 2 ,]


df4$OUTCOME <- as.factor(df4$OUTCOME)
TrgA<-ovun.sample(OUTCOME~.,data= df4,method="under",N=1333)
summary(TrgA$data$OUTCOME)
TrgA<-data.frame(TrgA$data)
summary(TrgA)

TrgB<-ovun.sample(OUTCOME~.,data= df4,method="under",N=4000)
summary(TrgB$data)
TrgB<-data.frame(TrgB$data)
summary(TrgA)

tree_modelA <- rpart(OUTCOME~ ., data = TrgA)
tree_pred_class_trainA <- predict(tree_modelA, TrgA, type = "class")
mean(TrgA$OUTCOME != tree_pred_class_trainA)

tree_modelB <- rpart(OUTCOME~ ., data = TrgB)
tree_pred_class_trainB <- predict(tree_modelB, TrgB, type = "class")
mean(TrgB$OUTCOME != tree_pred_class_trainB)

tree_modelD <- rpart(OUTCOME~ ., data = train_data)
tree_pred_class_trainD <- predict(tree_modelD, train_data, type = "class")
mean(train_data$OUTCOME != tree_pred_class_trainD)

tree_full_pred_class <- predict(tree_modelD, test_data, type = "class")
mean(test_data$OUTCOME != tree_full_pred_class)

test_data$LoanValuetoAppraised <- test_data$LoanValuetoAppraised[,drop=TRUE]
install.packages("mlr")
mergeSmallFactorLevels(task,cols = LoanValuetoAppraised)

