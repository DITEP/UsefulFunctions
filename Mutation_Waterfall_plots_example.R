###
# example with type of table like Cholangiocarinoma one, here called CCclinic
# figure in https://www.ejcancer.com/article/S0959-8049(17)31353-9/fulltext


# Take the alteration and technic

TotalTechnic <- rbind(
  cbind(c(as.vector(CCclinic$SEQUENCAGEResultats1), as.vector(CCclinic$SEQUENCAGEResultats2), 
          as.vector(CCclinic$SEQUENCAGEResultats3),  as.vector(CCclinic$SEQUENCAGEResultats4)),1),
  cbind(c(as.vector(CCclinic$CGHamplif1), as.vector(CCclinic$CGHamplif2), as.vector(CCclinic$CGHamplif3)),2),
  cbind(c(as.vector(CCclinic$CGHdel1), as.vector(CCclinic$CGHdel2)), 3),
  cbind(c(as.vector(CCclinic$Transloc1), as.vector(CCclinic$Transloc2)), 4)
  #  cbind(as.vector(CCclinic$IHC_FISH), 5)
)
TotalTechnic <- as.data.frame(TotalTechnic)
table(is.na(TotalTechnic$V1))
TotalTechnic <- TotalTechnic[!is.na(TotalTechnic$V1),]
str(TotalTechnic)
TotalTechnic$V2 <- as.numeric(TotalTechnic$V2)

# 
temp <- unique(TotalTechnic$V1)
temp

MATCC <- data.frame(row.names = temp)

for (i in c(1:length(CCclinic$NIP))){
  MUT <- c(as.vector(CCclinic$SEQUENCAGEResultats1[i]), as.vector(CCclinic$SEQUENCAGEResultats2[i]), 
           as.vector(CCclinic$SEQUENCAGEResultats3[i]),  as.vector(CCclinic$SEQUENCAGEResultats4[i]))
  MUT <- MUT[!is.na(MUT)]
  MATCC[MUT, i] <- 1

  AMP <- c(as.vector(CCclinic$CGHamplif1[i]), as.vector(CCclinic$CGHamplif2[i]), as.vector(CCclinic$CGHamplif3[i]))
  AMP <- AMP[!is.na(AMP)]
  MATCC[AMP, i] <- 2
  
  DEL <- as.vector(CCclinic$CGHdel1[i])
  
  DEL <- DEL[!is.na(DEL)]
  MATCC[DEL, i] <- 3
  
  TRANSLOC <- c(as.vector(CCclinic$Transloc1[i]), as.vector(CCclinic$Transloc2[i]))
  TRANSLOC <- TRANSLOC[!is.na(TRANSLOC)]
  MATCC[TRANSLOC, i] <- 4
    
  MATCC[is.na(MATCC[,i]),i] <- 0

  if(unique(AMP[1])!="" & length(unique(grep(AMP[1], MUT, value=T)))>0){ MATCC[unique(grep(AMP[1], MUT, value=T)), i] <- 7}
  if(unique(AMP[2])!="" & length(unique(grep(AMP[2], MUT, value=T)))>0){ MATCC[unique(grep(AMP[2], MUT, value=T)), i] <- 7}
  if(unique(DEL[1])!="" & length(unique(grep(DEL[1], MUT, value=T)))>0){ MATCC[unique(grep(DEL[1], MUT, value=T)), i] <- 7}
  #if(unique(DEL[2])!="" & length(unique(grep(DEL[2], MUT, value=T)))>0){ MATCC[unique(grep(DEL[2], MUT, value=T)), i] <- 7}
}

colnames(MATCC) <- paste(CCclinic$NIP,CCclinic$DateRCP)
MATCC <- MATCC[-c(grep("NC", rownames(MATCC)), grep("RIEN", rownames(MATCC))),]
MATCC <- MATCC[-3,]

#calculate the number of patients with an alteration per gene
for (i in c(1:length(rownames(MATCC)))){
  MATCC$SUMALTERATION[i] <- as.numeric(table(MATCC[i,]!=0)["TRUE"])
}
i
MATCC <- MATCC[with(MATCC, order(MATCC$SUMALTERATION, decreasing = F)),] # order by gene alteration frequency

# doing barplots
COL <-numeric() 
for (i in c(1:length(MATCC[,1]))){
  ALTER <- unique(as.numeric(MATCC[i, c(1:(length(MATCC[1,])-1))]))
  if (length(ALTER[ALTER>0])==1) {COL[i] <- ALTER[ALTER>0]} else {(COL[i]=7)} 
}
COL

PERCET <- MATCC$SUMALTERATION/length(MATCC[1,])
names(PERCET) <- rownames(MATCC)
rev(PERCET)

pdf("longtail.pdf")
barplot(rev(PERCET*100), las=2, col=rev(COL), cex.names = 0.7,space = 0,
        main = "The molecular pattern of biliary tract cancers", ylab="Alterations' frequency (% of samples)")
legend('topright',col=c(1:4, 7), legend = c("Mutation", "Amplification", "Deletion", "Translocation",
                                        "Multiple alterations"), lty=1, lwd=4)
dev.off()

# order by patients

for (i in c(1:length(rownames(MATCC)))){
  MATCC <- MATCC[,with(MATCC, order(MATCC[i,], decreasing = T))]
}

for (i in c(1:length(rownames(MATCC)))){
  MATCC$SUMALTERATION[i] <- table(MATCC[i,]!=0)["TRUE"]
}
MATCC$SUMALTERATION[is.na(MATCC$SUMALTERATION)] <- 0
MATCC <- MATCC[with(MATCC, order(MATCC$SUMALTERATION, decreasing = F)),] # order by gene alteration frequency
SUMALTERATION <- MATCC$SUMALTERATION 
MATCC$SUMALTERATION <- NULL

pdf("TotalAlteration.pdf")

par(mar= c(5, 3, 4, 4.4) )
plot(1,1, xlim=c(1,length(colnames(MATCC))), ylim=c(1,length(rownames(MATCC))), type='n', axes=F,
     xlab="Patients", ylab="", main="Alterations in biliary tract cancers")
     #main="Alterations in biliary tract cancers per sample")

for (i in c(1:length(rownames(MATCC)))){
  for(y in c(1:length(colnames(MATCC)))){
    rect(xleft = y-0.4 , ybottom = i-0.4, border = 0,
         xright = y+0.4, ytop =i+0.4, col=MATCC[i,y])
  }
}

mtext(rownames(MATCC), side = 2, at = c(1:length(rownames(MATCC))), las=2, cex=0.6)
legend('topright', fill=c(1:4, 7),legend = c("Mutation", "Amplification", "Deletion", "Translocation",
                                                 "Multiple alterations"), cex=0.8,  bty= 'n' )
par(new=T)
par(mar= c(4.7, 30.5, 3.9, 0.6) )
COL <-numeric() 
for (i in c(1:length(MATCC[,1]))){
  ALTER <- unique(as.numeric(MATCC[i, c(1:(length(MATCC[1,])-1))]))
  if (length(ALTER[ALTER>0])==1) {COL[i] <- ALTER[ALTER>0]} else {(COL[i]=7)} 
}
COL
barplot(SUMALTERATION/length(MATCC[1,])*100, col=COL,space = 0, horiz = T, names.arg = "")

par(new=T)
par(mar= c(0, 3, 4, 3.9) )
plot(1,1,type = "n",xlim = c(0,ncol(MATCC)),ylim = c(0,10),xlab = "",ylab = "",axes = F)
TTT<-vector()
j=0
#gsub("\\..*","", colnames(MATCC))
for(i in  colnames(MATCC)){
  j=j+1
  TTT[j]<-ifelse(CCclinic$C1J1[paste(CCclinic$NIP,CCclinic$DateRCP) %in%i]=="","orange","purple")
  rect(xleft = j-1.5,xright = j-0.5,ybottom = 1,ytop = 1.2,col = adjustcolor(TTT[j],0.8))
}

par(mar= c(4, 2, 2, 2) )
plot(1,1)
legend('topright', fill=c("purple","orange"),legend = c("Targeted treatment","Not orientated"), cex=0.8,  bty= 'n')

dev.off()

ncol(MATCC)
rownames(MATCC)
MATCC["TP53",]
TTT<-ifelse(TTT=="orange","NT","T")
DUPL<-duplicated(gsub("\\..*","", colnames(MATCC)))
table(TTT[!DUPL],as.numeric(MATCC["TP53",!DUPL]))
fisher.test(table(TTT[!DUPL],as.numeric(MATCC["TP53",!DUPL])))
table(TTT[!DUPL],ifelse(as.numeric(MATCC["KRAS",!DUPL])==0,0,1))
fisher.test(table(TTT[!DUPL],ifelse(as.numeric(MATCC["KRAS",!DUPL])==0,0,1)) )
fisher.test(table(TTT[!DUPL],ifelse(as.numeric(MATCC["FGFR2",!DUPL])==0,0,1)) )


### number of mutations per patient
par(mar= c(5, 3, 4, 4) )
SUMALTERATION <- vector()
for (i in c(1:length(colnames(MATCC)))){
  SUMALTERATION[i] <- as.numeric(table(MATCC[,i]!=0)["TRUE"])
}
barplot(SUMALTERATION, names.arg = colnames(MATCC))
median(SUMALTERATION, na.rm = T)
