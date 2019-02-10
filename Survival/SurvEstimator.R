# Author Loic Verlingue
# first publication on github oct 10th 2018
# open source

# this function can be used to compute :
# The survival function S(t): plots and ProbTable
# A matrix of survival classes by time intervals: ProbMat

SurvEstimator<-function(Interval=30, time=time, status=status, PDF=F){
  
  L=seq(min(time),max(time),length.out = (Interval)-1)
  L=c(1,L) # add a first interval
  ProbTable<-data.frame(TimeInt=L)
  
  # cumulative number of events to calculate the nb of subject at risk remaining in each intervals
  Nevent<-sapply(seq(Interval),function(l){
    subjectRisk<-time>=L[l]
    Nevent<-as.numeric(table(subjectRisk)["TRUE"])
    return(Nevent)  
  })
  
  ProbTable[,"NatRisk"]<-Nevent
  
  # nb of subject censored
  Ncensored<-sapply(seq(Interval),function(l){
    MAX<-ifelse(is.na(L[l+1]),L[l],L[l+1])
    
    Int<-time<MAX &  time>=L[l]
    Ncensored<-ifelse(any(status[Int]==0), as.numeric(table(status[Int]==0)["TRUE"]), 0)
    ProbTable[l,"Ncensored"]<<-Ncensored
    Ndeath<-ifelse(any(status[Int]==1), as.numeric(table(status[Int]==1)["TRUE"]), 0)
    ProbTable[l,"Ndeath"]<<-Ndeath
    
    return(Ncensored)  
  })
  ProbTable[,"Ncensored"]<-Ncensored
  
  # probability of survival relative to intervals
  ProbTable[1,"Prob"]<-1
  for(l in seq(Interval)[-1]){
    Prob<-ProbTable$Prob[l-1]*( (ProbTable$NatRisk[l]-ProbTable$Ndeath[l]) / ProbTable$NatRisk[l] )
    ProbTable[l,"Prob"]<-Prob
  }
  
  if(PDF){
     plot(ProbTable$Prob, type = 'l', main="Kaplan Meier curve", 
         xlab="Descrete time intervals", ylab="Probability of survival", ylim=c(0,1))
  }
  
  # survival function
  St<-ProbTable$Prob
  
  #discrete probability function
  Ft<-St[-length(St)]-St[-1]
  Ft=c(0,Ft)
  
  if(PDF){
    plot(Ft, main="Discrete probability function", xlab="Descrete time intervals")
   }
  
  #discrete hazard rate = conditional failure probability
  Ht=Ft/St
  
  if(PDF){
    plot(Ht,main="Ht")
  }
  # check relations are ok!
  #plot(St,cumprod(1-Ht)) ##############
  #plot(Ft,Ht*cumprod(1-Ht)) #############
 
  ############# survival matrix
 
  IntPat<-sapply(seq(length(time)),function(i){  
    Proba<-vector()
    time_interval<-max(which(ProbTable$TimeInt<=time[i]))
    
    for( l in seq(Interval)){
      # uncensored
      Pu<-ifelse(time_interval>=l,1,0)
      
      # censored
      Pc<-ifelse(time_interval>=l,1,St[l])
      Proba<-c(Proba,ifelse(status[i]==0,Pc,Pu))
    }
    return(Proba)  
  })
  if(PDF){
    image(t(IntPat))
  }
  return(list(ProbMat=IntPat,ProbTable=ProbTable))
}
