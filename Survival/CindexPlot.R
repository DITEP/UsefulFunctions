# a script to compute survival C index and plot it simply
# author: Loic Verlingue
 
 library(survival)
 library(pec)
 
 # you need :
 # model = a trained model
 # X = A matrix used for training and validation testing  
 # Train and Val = vectors of exemples used for training and validation
 # time and status = the 2 component of your survival data
 

Pred_val<-predict(model,as.array(as.matrix(X[Val,])) )

plot(Pred_val,time[Val],pch=ifelse(status[Val]==0,2,1),main="Val")

# Classical C index with the survival package 
Cindex<-survConcordance(Surv(time[Val],status[Val])~Pred_val)

# Weighted C index with the pec package
 Y <- cbind(X,Surv(time,status))
IPCW <- cindex(as.matrix(Pred_val), cens.model="marginal",
                 formula = Surv(time,status)~.,data = Y[Val,])
  legend("topleft",legend = c(paste("C-index =", 1-round(Cindex$concordance,3), 
                                    "; se =", round(Cindex$std.err,3)),
                              paste("IPCW =",round(as.numeric(IPCW$AppCindex),3))),cex=0.5)
  
