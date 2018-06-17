##########
# Creating a new variable from an existing one
##########

# info:
# can be useful in regression and svm but less in classification trees
# balancing summarization vs informamtion loss
# only built in the trainig set but can be applyed in test # careful because can impact efficacy in test set
# check by plotting/tables

# script
# ex from Wage data
library(ISLR);data(Wage)

#do the partition with package caret, or manually 
inTrain<-createDataPartition(y=Wage$wage,p=0.7,list=F)
training<-Wage[inTrain,]
testing<-Wage[-inTrain,]

# load splines to create polynomial easely
library(splines)
bsBasis<-bs(training$age,df=3) # create a polynomial (3rd degree) variable

# eg of modeling with linear model 
lm1<-lm(wage~bsBasis,data=training)
plot(training$age,training$wage)
points(training$age,predict(lm1,newdata=training),col=2,pch=19,cex=0.5)

# do it on the test set
predict(bsBasis,age=testing$age)

###############
# other actions that can be performed to transform or check variables
# with caret package
###############

# qualitative/factor into dummy variables
table(training$jobclass)
dummies<-dummyVars(wage~jobclass,data=training)
head(predict(dummies,newdata=training))

# near zero variance variable
nsv<-nearZeroVar(training,saveMetrics = T) # are bad variables
nsv # discard the less meaningfull variables

# ....

