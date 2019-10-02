# Metrics to evaluate a model for survival data 
# Author: Loic Verlingue

#######
# C index you have seen in CindexPlot.R

#######
# survivalROC
library(survivalROC)

# you need :
 # model = a trained model
 # X = A matrix used for training and validation testing  
 # Train and Val = vectors of exemples used for training and validation
 # time and status = the 2 component of your survival data
 

Pred_val<-predict(model,as.array(as.matrix(X[Val,])) )

ROCvalue<-survivalROC(Stime = time[Val], status = status[Val], marker = Pred_val,
                          predict.time = quantile(time,0.9), method = "KM")

#and plot
plot(ROCvalue$FP, ROCvalue$TP, type="l", xlim=c(0,1), ylim=c(0,1),
           xlab="FP",
           ylab="TP",main="Validation survival ROC\n at 90% survival")
abline(0,1)
      
#######
#if you model is non linear, we can think of the linear Cox-model p-value as a metrics of the reliability of your predictions.


