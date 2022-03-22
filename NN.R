#Neural Net 
#Using TrgA 
#Experimenting with different values for size and decay 
#Size 20, decay 0.1 
nnA <- nnet(MyFormula, data = TrgA, linout=T, size=20, decay=0.1, maxit = 5000) 
nnApred <- predict(nnA, test) 
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r') 
plot.nnet(nnA) 
MSEA <- mean((nnApred-test$OUTCOME)^2) 
MSEA 
#1.020048e-06 
#Size 15, decay 0.1 
nnA <- nnet(MyFormula, data = TrgA, linout=T, size=15, decay=0.1, maxit = 5000) 
nnApred <- predict(nnA, test) source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r') 
plot.nnet(nnA) 
MSEA <- mean((nnApred-test$OUTCOME)^2) 
MSEA 
#1.05313e-06 
#Size 10, decay 0.1 
nnA <- nnet(MyFormula, data = TrgA, linout=T, size=10, decay=0.1, maxit = 5000) 
nnApred <- predict(nnA, test) source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r') 
plot.nnet(nnA) 
MSEA <- mean((nnApred-test$OUTCOME)^2) 
MSEA 
#1.128021e-06 #Size 7, decay 0.1 
nnA <- nnet(MyFormula, data = TrgA, linout=T, size=7, decay=0.1, maxit=1000)
nnApred <- predict(nnA, test) 
nnApred source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r') 
plot.nnet(nnA) MSEA <- mean((nnApred-test$OUTCOME)^2) 
MSEA #1.334786e-06 #Size 25, decay 0.1 
nnA <- nnet(MyFormula, data = TrgA, linout=T, size=25, decay=0.1, maxit=5000) 
nnApred <- predict(nnA, test) source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r') 
plot.nnet(nnA) MSEA <- mean((nnApred-test$OUTCOME)^2) 
MSEA #1.007765e-06 #Size 25, decay 0.2 
nnA <- nnet(MyFormula, data = TrgA, linout=T, size=25, decay=0.2, maxit = 5000) 
nnApred <- predict(nnA, test) 
nnApred source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r') 
plot.nnet(nnA) 
MSEA <- mean((nnApred-test$OUTCOME)^2) 
MSEA #3.989004e-06 
#Hence the final values for decay and size used are 
nnA <- nnet(MyFormula, data = TrgA, linout=T, size=, decay=, maxit = 5000) 
nnApred <- predict(nnA, test) 
nnApred source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r') 
plot.nnet(nnA) 
MSEA <- mean((nnApred-test$OUTCOME)^2) 
MSEA 
scorennA <- nnApred prednnA <- prediction(scorennA, test$OUTCOME) # Gain Chart 
perfnnA <- performance(prednnA, "tpr", "rpp") 
plot(perfnnA) # ROC Curve 
perfnnA <- performance(prednnA, "tpr", "fpr") 
plot(perfnnA) # PR Curve 
perfnnA <- performance(prednnA, "ppv" , "tpr") 
plot (perfnnA) 
aucnnA <- performance(prednnA, "auc") 
aucnnA <- unlist(slot(aucnnA, "y.values")) 
aucnnA #Lift Chart 
perfnnA <- performance(prednnA, "lift", "rpp") 
plot (perfnnA, main="Lift Curve") #Using TrgB #Size 20, decay 0.1 
nnB <- nnet(MyFormula, data = TrgB, linout=T, size=20, decay=0.1, maxit = 5000) 
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r') 
plot.nnet(nnB) nnBpred <- predict(nnB, test) MSEB <- mean((nnBpred-test$OUTCOME)^2) 
MSEB #2.106851e-07 #Size 15, decay 0.1 
nnB <- nnet(MyFormula, data = TrgB, linout=T, size=15, decay=0.1, maxit=1000) 
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r') 
plot.nnet(nnB) 
nnBpred <- predict(nnB, test) 
MSEB <- mean((nnBpred-test$OUTCOME)^2) 
MSEB #1.826512e-07 #Size 10, decay 0.1 
nnB <- nnet(MyFormula, data = TrgB, linout=T, size=10, decay=0.1, maxit=1000) 
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r') 
plot.nnet(nnB) nnBpred <- predict(nnB, test) 
MSEB <- mean((nnBpred-test$OUTCOME)^2) 
MSEB #1.854081e-07 #Size 7, decay 0.1 
nnB <- nnet(MyFormula, data = TrgB, linout=T, size=7, decay=0.1, maxit=1000) 
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r') 
plot.nnet(nnB) 
nnBpred <- predict(nnB, test) 
MSEB <- mean((nnBpred-test$OUTCOME)^2) 
MSEB #2.019017e-07 #Size 5, decay 0.1 
nnB <- nnet(MyFormula, data = TrgB, linout=T, size=5, decay=0.1, maxit=1000) 
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r') 
plot.nnet(nnB) 
nnBpred <- predict(nnB, test) 
MSEB <- mean((nnBpred-test$OUTCOME)^2) 
MSEB #2.227385e-07 #Hence, the final values for size and decay used are 
nnB <- nnet(MyFormula, data = TrgB, linout=T, size=, decay=, maxit = 5000) 
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r') 
plot.nnet(nnB) 
nnBpred <- predict(nnB, test) 
MSEB <- mean((nnBpred-test$OUTCOME)^2) MSEB 
#Performance Charts 
scorennB <- nnBpred 
prednnB <- prediction(scorennB, test$OUTCOME) 
# Gain Chart 
perfnnB <- performance(prednnB, "tpr", "rpp") 
plot(perfnnB) # ROC Curve 
perfnnB <- performance(prednnB, "tpr", "fpr") 
plot(perfnnB) # PR Curve 
perfnnB <- performance(prednnB, "ppv" , "tpr") 
plot (perfnnB) 
aucnnB <- performance(prednnB, "auc") 
aucnnB <- unlist(slot(aucnnB, "y.values")) aucnnB 
#Lift Chart 
perfnnB <- performance(prednnB, "lift", "rpp") 
plot (perfnnB, main="Lift Curve")
