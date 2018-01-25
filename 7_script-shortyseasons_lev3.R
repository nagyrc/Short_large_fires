#this code is part of the Short_large_fires project by Dr. R. Chelsea Nagy
#FIRE SEASONALITY with Level 3 ecoregions

library(doBy)
library(gdata)
library(ggplot2)
library(tidyr)
library(dplyr)
library(tibble)

#updated Short data and joined with ecoregion data

#this has the geometry field removed
lrg_fires<- read.csv("data/merged/lrg_fires.csv")
keep<-lrg_fires[which(lrg_fires$STAT_CAUSE_DESCR!="Missing/Undefined"),]

#make new column of ecoregion number to easily index
keep$ecn<-as.character(gsub("\\.","",keep$NA_L3CODE))
keep$ecn<-as.numeric(keep$ecn)
head(keep)

#update FIRE_SIZE_ha here
keep$FIRE_SIZE_ha<-keep$FIRE_SIZE_m2*0.0001
summary(keep$FIRE_SIZE_ha)


tt33<-unique(keep$ecn)

r3 <- summaryBy(clean_id~DISCOVERY_DOY+IGNITION, data=keep, FUN=length)

# fire season of large fires, for all ecoregions
wide <- reshape(r3,v.names="clean_id.length", idvar="IGNITION", timevar="DISCOVERY_DOY", direction="wide") #change number here 
wide[is.na(wide)] <- 0

w <- wide[,2:length(wide)]
colnames(wide) <- c("ig", substr(colnames(w), 11, 13))
colnames(wide) <- c("ig", substr(colnames(w), 11, 13))

r2m <- as.matrix(wide[,2:length(wide)])
rownames(r2m) <- wide[,1]
colnames(r2m)<-c(1:366)

#plot
barplot(r2m, beside=T, horiz=F, legend=T, col=c("red", "blue"), 
        xlab = "Day of year in Julian Day", ylab = "Number of Fires", 
        border=c("red","blue"), cex.names=1.25, cex.axis=1.25, cex.lab=1.25,ylim=c(0,1200))

#check to make sure correct # obs
yyy<-rowSums(r2m[,])
head(yyy)
142276+32946
#175222


################################################
#median day of year for large human and lightning fires
r4 <- summaryBy(DISCOVERY_DOY~IGNITION, data=keep, FUN=median)
r4
#median doy for human fires = 118, median doy for lightning fires = 205


r4.1<-summaryBy(DISCOVERY_DOY~IGNITION+NA_L3CODE, data=keep, FUN=median)
r4.1
wdoy<-reshape(r4.1,timevar="IGNITION",idvar="NA_L3CODE",v.names="DISCOVERY_DOY.median",direction="wide")
head(wdoy)
wdoy$diff<-wdoy$DISCOVERY_DOY.median.Human-wdoy$DISCOVERY_DOY.median.Lightning
#this gives the difference in the median day of year for human and lightning by ecoregion

#output table; this is Figure S5
write.table(wdoy, "results/meddoyhl.csv", sep=",", row.names=FALSE, append=FALSE)
################################################


#stats about what is large human fire season vs. large lightning fire season
hub<-keep[which(keep$IGNITION=="Human"),]
#142276
lub<-keep[which(keep$IGNITION=="Lightning"),]
#32946

twofive<-quantile(hub$DISCOVERY_DOY, 0.25)
twofive
#75
sevenfive<-quantile(hub$DISCOVERY_DOY, 0.75)
sevenfive
#221

twofive<-quantile(lub$DISCOVERY_DOY, 0.25)
twofive
#178
sevenfive<-quantile(lub$DISCOVERY_DOY, 0.75)
sevenfive
#226


j2.5<-quantile(lub$DISCOVERY_DOY, 0.025)
j2.5
#103
j97.5<-quantile(lub$DISCOVERY_DOY, 0.975)
j97.5
#267

#number of large human-caused fires outside of lightning fire season
out<-hub[ which(hub$DISCOVERY_DOY>267| hub$DISCOVERY_DOY < 103), ]
out
#80896; need to report this in the manuscript

#number of human-caused large fires outside of lightning fire season by ecoregion
r66 <- summaryBy(clean_id~NA_L3CODE, data=out, FUN=length)
r66

#output table. This is Figure S6
write.table(r66, "results/non_light_season.csv", sep=",", row.names=FALSE, append=FALSE)


#########################################
#split into east and west US; Figure 3a; Figure 3b
region<-as.data.frame(read.csv("data/bounds/ecoregion/east_west/arc_map_regions.csv"))

outputrr<-left_join(keep,region,by="NA_L3CODE")

rr<-summaryBy(clean_id~IGNITION+region, data=outputrr, FUN=length)
rr
#human east = 96837 or 92.23%
#lightning east = 8157 or 7.77%
#human west = 45439 or 64.70%
#lightning west = 24789 or 35.30%

#east total = 104994
96837+8157

(96837/104994)*100

(8157/104994)*100

#west total = 70228
45439+24789

(45439/70228)*100

(24789/70228)*100

r5 <- summaryBy(clean_id~DISCOVERY_DOY+IGNITION+region, data=outputrr, FUN=length)
r5
#tt<-as.numeric(unique(r3$ecn))

head(r5)

r5east <- subset(r5, r5$region == "east")
r6west <- subset(r5, r5$region == "west")


#east
# fire season, large fires, all ecoregions, Figure 3a in manuscript
r5east <- subset(r5east, select = -c(region) )

wide5 <- reshape(r5east,v.names="clean_id.length", idvar="IGNITION", timevar="DISCOVERY_DOY", direction="wide") #change number here 
wide5[is.na(wide5)] <- 0

w5 <- wide5[,2:length(wide5)]

colnames(wide5) <- c("ig", substr(colnames(w5), 11, 13))
colnames(wide5) <- c("ig", substr(colnames(w5), 11, 13))

r5m <- as.matrix(wide5[,2:length(wide5)])
rownames(r5m) <- wide5[,1]
colnames(r5m)<-c(1:366)

#Figure 3a
barplot(r5m, beside=T, horiz=F, legend=T, col=c("red", "blue"), 
        xlab = "Day of year in Julian Day", ylab = "Number of Fires", main= "East",
        border=c("red","blue"), cex.names=1.25, cex.axis=1.25, cex.lab=1.25,ylim=c(0,1200))



#west
r6west <- subset(r6west, select = -c(region) )

wide6 <- reshape(r6west,v.names="clean_id.length", idvar="IGNITION", timevar="DISCOVERY_DOY", direction="wide") #change number here 
wide6[is.na(wide6)] <- 0

w6 <- wide6[,2:length(wide6)]

colnames(wide6) <- c("ig", substr(colnames(w6), 11, 13))
colnames(wide6) <- c("ig", substr(colnames(w6), 11, 13))

r6m <- as.matrix(wide6[,2:length(wide6)])
rownames(r6m) <- wide6[,1]
colnames(r6m)<-c(1:366)

#Figure 3b
barplot(r6m, beside=T, horiz=F, legend=T, col=c("red", "blue"), 
        xlab = "Day of year in Julian Day", ylab = "Number of Fires", main="West",
        border=c("red","blue"), cex.names=1.25, cex.axis=1.25, cex.lab=1.25,ylim=c(0,1200))



##########





######
#seasonal correlations of large fires with fires of all sizes for east and west (by region)
#number of fires by Julian day of year
#human ignitions only

#large fires only
#join region to large fires
lrg_fires_rr<-left_join(keep,region,by="NA_L3CODE")

#subset the human caused fires only
lrghrr<-lrg_fires_rr[lrg_fires_rr$IGNITION=="Human",]

r8 <- summaryBy(clean_id~DISCOVERY_DOY+region, data=lrghrr, FUN=length)
head(r8)

#fires of all sizes
#join region to all fires
all_fires_rr<-left_join(all_fires,region,by="NA_L3CODE")

#exclude those with unknown cause
all_fires_known<-all_fires_rr[which(all_fires_rr$STAT_CAUSE_DESCR!="Missing/Undefined"),]
#1698835

#subset the human caused fires only
allhrr<-all_fires_known[all_fires_known$IGNITION=="Human",]

r9 <- summaryBy(clean_id~DISCOVERY_DOY+region, data=allhrr, FUN=length)
head(r9)

#join together
lll<-merge(r8,r9, by=c("DISCOVERY_DOY","region"))
head(lll)
lll

names(lll)[1] <-"doy"
names(lll)[3]<-"large"
names(lll)[4]<-"all"

tempw<-subset(lll, lll$region == "west")
cor(tempw$large,tempw$all)
#r=0.9366
summary(lm(large~all, data=tempw))
#p<2e-16

tempe<-subset(lll, lll$region == "east")
cor(tempe$large,tempe$all)
#r=0.9891
summary(lm(large~all, data=tempe))
#p<2e-16
#these numbers are reported in the manuscript








#seasonal correlations at the ecoregion scale
#number of fires by Julian day of year
#human ignitions only
r11 <- summaryBy(clean_id~DISCOVERY_DOY+NA_L3CODE, data=lrghrr, FUN=length)

r12 <- summaryBy(clean_id~DISCOVERY_DOY+NA_L3CODE, data=allhrr, FUN=length)

qqq<-merge(r11,r12, by=c("DISCOVERY_DOY","NA_L3CODE"))
head(qqq)


names(qqq)[1] <-"doy"
names(qqq)[3]<-"large"
names(qqq)[4]<-"all"


cor(qqq$large,qqq$all)
#r=0.9407
summary(lm(large~all, data=qqq))
#p<2e-16


ttq<-unique(qqq$NA_L3CODE)


outpute<-NULL
for (i in ttq) {
  subbye<-qqq[qqq$NA_L3CODE==i,]
  corre<-cor(subbye$large,subbye$all)
  outpute<-rbind(outpute,corre)
}

cop<-outpute

head(cop)

dfe<-as.data.frame(cop)

names(dfe)[1] <- "corr"

ttqv<-as.vector(ttq)
dfe$NA_L3CODE<-ttqv

row.names(df)<-NULL

head(dfe)

write.table(dfe, "results/seasonal_corr_human_only.csv", sep=",", row.names=FALSE, append=FALSE)















