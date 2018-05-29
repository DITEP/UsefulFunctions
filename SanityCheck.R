# List of generic functions for sanity check

#######
# quick look at nb of NAs or missing values in selected colnames rom your DATA file: a df or matrix

ColNames<-c("X","Y")

NAs<-sapply(ColNames,function(x){
  table(is.na(DATA[,x])|DATA[,x]=="")
})

print(NAs)
