#this code is part of the Short_large_fires project by Dr. R. Chelsea Nagy

library(ggplot2)
library(doBy)
library(dplyr)

#bring in Short data with fm, ws, biomass extracted
#note, this has large fires only (top 10%)
lrg_fires<- read.csv("data/merged/lrg_fires.csv")

#remove the fires with undefined/missing cause
keep<-lrg_fires[which(lrg_fires$STAT_CAUSE_DESCR!="Missing/Undefined"),]
head(keep)


######################################################
#biomass
#summarize biomass by ecoregion
biostats<-summaryBy(NBCD_countrywide_biomass_mosaic~NA_L3CODE,data=keep,FUN=c(mean),na.rm=TRUE)

#join table of fire size and biomass by ecoregion
#bring in fire size data by ecoregion (made in script 6, line 260)
fireha<-as.data.frame(read.csv("results/firehasum_ecn_top_ten_Short_update.csv"))
dfbio<-left_join(biostats,fireha,by="NA_L3CODE")

head(dfbio)
colnames(dfbio) <- c("NA_L3CODE", "bio.mean","NA_L3NAME","nobs", "ha.mean","ha.sd","ha.median","ha.min", "ha.max", "NA_L1NAME")

#remove unused columns
dfbio<- subset(dfbio, select=-c(nobs,ha.sd,ha.median,ha.min,ha.max))

#bring in Level I ecoregion info
key<-unique(keep[c("NA_L3CODE","NA_L1CODE")])
key
dfbio1<-left_join(dfbio,key,by=c('NA_L3CODE'))
head(dfbio1)
dfbio1$L1<-as.character(dfbio1$NA_L1CODE)

#code to make Fig 7c
p16 <- ggplot(data = dfbio1, aes(y = log(ha.mean), x = log(bio.mean),color=L1)) + 
  geom_point() +
  ylab('log (fire size (ha))') +
  xlab('log (biomass (g/m2))')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,8)+
  ylim(0,12)+
  #scale_colour_gradient(name="% human fires", low = "blue", high = "red", guide = "colourbar")+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.text=element_text(size=18),legend.title=element_text(size=18))+
  geom_smooth(method='lm', formula=y~x, se=FALSE)
p16


#7 and 11 are positive; all others are negative
#7 Marine West Coast Forest
#11 Mediterranean California

l5<-dfbio1[which(dfbio1$L1=="5"),]
l6<-dfbio1[which(dfbio1$L1=="6"),]
l7<-dfbio1[which(dfbio1$L1=="7"),]
l8<-dfbio1[which(dfbio1$L1=="8"),]
l9<-dfbio1[which(dfbio1$L1=="9"),]
l10<-dfbio1[which(dfbio1$L1=="10"),]
l11<-dfbio1[which(dfbio1$L1=="11"),]
l12<-dfbio1[which(dfbio1$L1=="12"),]
l13<-dfbio1[which(dfbio1$L1=="13"),]
l15<-dfbio1[which(dfbio1$L1=="15"),]

keyzzz1<-unique(keep[c("NA_L1CODE","NA_L1NAME")])
keyzzz1

lm5<-lm(log(ha.mean)~log(bio.mean), data=l5)
summary(lm5)
#p=0.15

lm6<-lm(log(ha.mean)~log(bio.mean), data=l6)
summary(lm6)
#p=0.94

lm7<-lm(log(ha.mean)~log(bio.mean), data=l7)
summary(lm7)
#p=0.82

lm8<-lm(log(ha.mean)~log(bio.mean), data=l8)
summary(lm8)
#p=0.82

lm9<-lm(log(ha.mean)~log(bio.mean), data=l9)
summary(lm9)
#p=0.02
#this is sig related (-)

lm10<-lm(log(ha.mean)~log(bio.mean), data=l10)
summary(lm10)
#p=0.306

lm11<-lm(log(ha.mean)~log(bio.mean), data=l11)
summary(lm11)
#p=0.1751

lm12<-lm(log(ha.mean)~log(bio.mean), data=l12)
summary(lm12)
#p=NA

lm13<-lm(log(ha.mean)~log(bio.mean), data=l13)
summary(lm13)
#p=NA

lm15<-lm(log(ha.mean)~log(bio.mean), data=l15)
summary(lm15)
#p=NA


lmall<-lm(log(ha.mean)~log(bio.mean), data=dfbio1)
summary(lmall)
#p=1.08e-06
#y=-0.57x+9.1689


#split ecoregions into east and west US
region<-as.data.frame(read.csv("data/bounds/ecoregion/east_west/arc_map_regions.csv"))

#join region
dfbio1r<-left_join(dfbio1,region,by=c('NA_L3CODE'))

#split into east and west
eastbio<-dfbio1r[which(dfbio1r$region=="east"),]
westbio<-dfbio1r[which(dfbio1r$region=="west"),]

lmrege<-lm(log(ha.mean)~log(bio.mean), data=eastbio)
summary(lmrege)
#p=0.00425; (-)
lmregw<-lm(log(ha.mean)~log(bio.mean), data=westbio)
summary(lmregw)
#p=0.109



######################################################
#biophysical setting (bps)
#summarize number of fires by bps

#bring in all fires
all_fires<- read.csv("data/merged/all_fires.csv")
checkall<-summaryBy(clean_id~STAT_CAUSE_DESCR,data=all_fires,FUN=c(length))
checkall

keepall<-all_fires[which(all_fires$STAT_CAUSE_DESCR!="Missing/Undefined"),]
checkall2<-summaryBy(clean_id~STAT_CAUSE_DESCR,data=keepall,FUN=c(length))
checkall2

checkall2b<-summaryBy(clean_id~IGNITION,data=keepall,FUN=c(length))
checkall2b
#1,424,630 human
#274,205 lightning

#stats for all fires with Missing/Undefined removed
bpsstats<-summaryBy(clean_id~GROUPVEG+IGNITION,data=keepall,FUN=c(length))
head(bpsstats)

#this is Table 2a
wbpsall<-reshape(bpsstats,timevar="IGNITION",idvar="GROUPVEG",v.names="clean_id.length",direction="wide")
head(wbpsall)
colnames(wbpsall) <- c("GROUPVEG", "hnobsall", "lnobsall")
wbpsall$ratio<-wbpsall$hnobsall/wbpsall$lnobsall
write.table(wbpsall, "results/bps_stats_all.csv", sep=",", row.names=FALSE, append=FALSE)

#this is Table 2b
bpslrgstats<-summaryBy(clean_id~GROUPVEG+IGNITION,data=keep,FUN=c(length))
wbpslrg<-reshape(bpslrgstats,timevar="IGNITION",idvar="GROUPVEG",v.names="clean_id.length",direction="wide")
head(wbpslrg)
colnames(wbpslrg) <- c("GROUPVEG", "hnobslrg", "lnobslrg")
wbpslrg$ratio<-wbpslrg$hnobslrg/wbpslrg$lnobslrg
write.table(wbpslrg, "results/bps_stats_lrg.csv", sep=",", row.names=FALSE, append=FALSE)


checkall2c<-summaryBy(clean_id~IGNITION,data=keep,FUN=c(length))
checkall2c
#142,276 human
#32,946 lightning

142276/32946
#4.318
