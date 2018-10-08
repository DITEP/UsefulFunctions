# this script is an example of Cox model penalized with Lasso procedure 
# M is your matrix of variables
# Y is your table of survival data, same order as M rows
#Train is your random selection of patients in the training cohort, usually 70%

#the cross validation procedure alows you to determine the best value of lambda, the penalization parameter:
cv.fit <- cv.glmnet(M[Train,],Surv(Y$Surv[Train,1],Y$Surv[Train,2]), family="cox", alpha=1)
log(cv.fit$lambda)
plot(cv.fit)

#you can use previous coefficients from cross validation
COEF<-coef(cv.fit,s="lambda.min")

# or you can use best lambda to run a new regression
fit<-glmnet(M[Train,],Surv(Y$Surv[Train,1],Y$Surv[Train,2]), family = "cox", alpha = 1, lambda=cv.fit$lambda.min)
plot(fit, label=T)

# can have coeeficients from
coefficients <- coef(fit, s = cv.fit$lambda.min)
active_coefficients <- coefficients[,1] != 0
coefficients[active_coefficients,]

# compute stats
x_selected <- M[,active_coefficients]
summary(coxph(Surv(Y$Surv[,1],Y$Surv[,2])~x_selected[,1]))

#and do survival plots for eg
