#this code is part of the Short_large_fires project by Dr. R. Chelsea Nagy
#this is for the sensitivity analysis

all_fires<- read.csv("data/merged/all_fires.csv")


######################
#for sensitivity analysis; try also 95% and 80%
lrg_fires95=NULL
for (i in tt3) {
  subby<-all_fires[all_fires$NA_L3CODE==i,]
  ninetyfive<-subset(subby, FIRE_SIZE >= quantile(FIRE_SIZE, 0.95))
  lrg_fires95<-rbind(lrg_fires95,data.frame(ninetyfive[,]))
}
#nobs=96739

lrg_fires80=NULL
for (i in tt3) {
  subby<-all_fires[all_fires$NA_L3CODE==i,]
  eighty<-subset(subby, FIRE_SIZE >= quantile(FIRE_SIZE, 0.80))
  lrg_fires80<-rbind(lrg_fires80,data.frame(eighty[,]))
}
#nobs=392937


######################
#are there fires with missing causes in all_fires?
check95<-summaryBy(clean_id~STAT_CAUSE_DESCR,data=lrg_fires95,FUN=c(length))
check95
#yes

#remove these
keep95<-lrg_fires95[which(lrg_fires95$STAT_CAUSE_DESCR!="Missing/Undefined"),]
checkall2<-summaryBy(clean_id~STAT_CAUSE_DESCR,data=keep95,FUN=c(length))
checkall2

keep80<-lrg_fires80[which(lrg_fires80$STAT_CAUSE_DESCR!="Missing/Undefined"),]
checkall3<-summaryBy(clean_id~STAT_CAUSE_DESCR,data=keep80,FUN=c(length))
checkall3
#ok


######################

#summary stats
sum80<-summaryBy(data=keep80, FIRE_SIZE_ha~NA_L3CODE+IGNITION, FUN=c(length))

#reshape
w80<-reshape(sum80,timevar="IGNITION",idvar="NA_L3CODE",v.names="FIRE_SIZE_ha.length",direction="wide")
head(w80)

colnames(w80) <- c("NA_L3CODE", "hnobs", "lnobs")
#for figures in manuscript???

w80$totfires<-w80$hnobs+w80$lnobs
w80$perh20<-w80$hnobs/w80$totfires*100
write.table(w80, "results/perh_large20.csv", sep=",", row.names=FALSE, append=FALSE)



#summary stats
sum95<-summaryBy(data=keep95, FIRE_SIZE_ha~NA_L3CODE+IGNITION, FUN=c(length))

#reshape
w95<-reshape(sum95,timevar="IGNITION",idvar="NA_L3CODE",v.names="FIRE_SIZE_ha.length",direction="wide")
head(w95)

colnames(w95) <- c("NA_L3CODE", "hnobs", "lnobs")

w95$totfires<-w95$hnobs+w95$lnobs
w95$perh5<-w95$hnobs/w95$totfires*100
write.table(w95, "results/perh_large5.csv", sep=",", row.names=FALSE, append=FALSE)

