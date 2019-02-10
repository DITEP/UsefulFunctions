#### plot nice kaplan meyer curves

library(survival)
library(survcomp)

pdf("you dir")

par(mar=c(8,2,4,2)+0.1)

# compute survival 
patients$SurvPFS<-Surv(patients$PFS, patients$Status)

# test stats for difference of your groups
VALUES <- summary(coxph(patients$SurvPFS~patients$group)) # suppose we have 2 groups

# plot the curves and legend the stats
km.coxph.plot(formula.s=patients$SurvPFS~patients$ClusterTF, mark.time=TRUE, 
              data.s=patients,x.label="Time from inclusion (days)",
              y.label="Probability of PFS", main.title= "By groups", 
              .col=c(1,2), .lty=c(1), show.n.risk=2, n.risk.step=200, n.risk.cex=0.8, o.text = "", # n.risk.step = design the nb of patient
              verbose=F, frame=F) #xlim=c(0,1000),
legend("topright", lty=1, col = c(2,1,0,0), legend = c("Group 1", "Gourp 2", paste("HR =", round(VALUES$coefficients[2],3), 
                                                      "CI95[", round(VALUES$conf.int[3],3), "-", round(VALUES$conf.int[4],3),"]" ), 
                                                       paste("Logrank p =", round(VALUES$sctest[3],4) )))

dev.off()
