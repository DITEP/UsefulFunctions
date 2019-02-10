######################################################
# few commands to run random forest with survival data
library(randomForestSRC)

# data format
# DATA: a data frame or matrice with covariables and outcome values in survival format (OS,STATUS)

# run the rf
# the ~. means that you use all remainig data of your DATA matrice to compute the model (except OS and STATUS for sure)
# check further hyperparmaters that you can tune in the information: https://cran.r-project.org/web/packages/randomForestSRC/randomForestSRC.pdf

Fitrf<- rfsrc(Surv(OS,STATUS) ~ ., data = DATA, 
               ntree = 5000, importance = T)

# check relative importance of the varibales
vimp(Fitrf)

# check error
Fitrf$err.rate[Fitrf$ntree]

# compute stats with fonctions from the caret package (quite long to install if you haven't done yert), but you can also do it manually
precision <- posPredValue(Fitrf$class.oob, Fitrf$yvar)
recall <- sensitivity(Fitrf$class.oob, Fitrf$yvar)
F1 <- (2 * precision * recall) / (precision + recall)
