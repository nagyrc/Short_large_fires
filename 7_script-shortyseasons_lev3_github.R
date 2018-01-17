#this code is part of the Short_large_fires project by Dr. R. Chelsea Nagy
#FIRE SEASONALITY with Level 3 ecoregions...Level 1 ecoregions in separate R script (script-shortyseasons061516.R)
#use updated Short data from preprocessing

library(doBy)
library(gdata)
library(ggplot2)
library(tidyr)
library(dplyr)
library(tibble)

#updated Short data and joined with ecoregion data
#slim<-as.data.frame(read.csv("data/merged/updatedShort_w_eco_slim3.csv"))

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


#check to make sure no water ecoregions
#subz <- subset(keep, keep$ecn != 000)
#subz <- subset(keep, keep$ecn != 0)
#subz <- subset(keep, is.na(keep$ecn) == FALSE)
#no water, undefined, keep using keep
#can remove this

tt33<-unique(keep$ecn)

#top 10% largest fires only
#################################
#to understand what a large fire is in each ecoregion
#extract top 10% of largest fires from each ecoregion
#then put back into a dataframe that has all columns
#outputy=NULL
#for (i in tt3) {
  #subby<-subz[subz$ecn==i,]
  #ninety<-subset(subby, ha >= quantile(ha, 0.90))
  #outputy<-rbind(outputy,data.frame(ninety[,]))
#}

#output table of large fires
#write.table(outputy, "/Users/rana7082/Dropbox/ecoregions/derived/Short_large10.csv", sep=",", row.names=FALSE)


################################
#top 10% largest fires; summary statistics; reported in Table S1 in manuscript
#create dataframe of number of fires, mean, sd of fire size by ecoregion (summary columns of the 10% largest fires)
head(keep)
output=NULL

#this is not correct; keep has already been subset to the top 10% fires
for (i in tt33) {
  subby<-keep[keep$ecn==i,]
  ninety<-subset(subby, FIRE_SIZE_ha >= quantile(FIRE_SIZE_ha, 0.9))
  outty<- c(i,length(ninety$FIRE_SIZE_ha),mean(ninety$FIRE_SIZE_ha),sd(ninety$FIRE_SIZE_ha),median(ninety$FIRE_SIZE_ha),sum(ninety$FIRE_SIZE_ha))
  output<-rbind(output,outty)
}
head(output)
###

colnames(output) <- c("ecn", "nobs", "mean","sd","median","sum")
row.names(output)<-NULL

#add key to this dataframe
tt9<-unique(keep[c("ecn", "NA_L3CODE")])
fff<-merge(output,tt9,by="ecn")
head(fff)

#output table; used to make Table S1
write.table(fff, "results/firehasum_ecn_top_ten_Short_update.csv", sep=",", row.names=FALSE, append=FALSE)








#####################################
#calculate % human by ecoregion for just top 10% largest fires
#use this to make Figure 1 

#summary statistics on each subset (human and lightning)
#human subset of large fires only
head(keep)
outputh=NULL

for (i in tt33) {
  subh<-keep[keep$IGNITION=="Human",]
  subbyh<-subh[subh$ecn==i,]
  ninetyh<-subset(subbyh, FIRE_SIZE_ha >= quantile(FIRE_SIZE_ha, 0.9))
  outtyh<- c(i,length(ninetyh$FIRE_SIZE_ha),mean(ninetyh$FIRE_SIZE_ha),sd(ninetyh$FIRE_SIZE_ha),median(ninetyh$FIRE_SIZE_ha),sum(ninetyh$FIRE_SIZE_ha))
  outputh<-rbind(outputh,outtyh)
}

colnames(outputh) <- c("ecn", "hnobs", "hmean","hsd","hmedian","hsum")
row.names(outputh)<-NULL
head(outputh)

summary(hmean)

#lightning subset of large fires only
outputl=NULL

for (i in tt33) {
  subl<-keep[keep$IGNITION=="Lightning",]
  subbyl<-subl[subl$ecn==i,]
  ninetyl<-subset(subbyl, FIRE_SIZE_ha >= quantile(FIRE_SIZE_ha, 0.9))
  outtyl<- c(i,length(ninetyl$FIRE_SIZE_ha),mean(ninetyl$FIRE_SIZE_ha),sd(ninetyl$FIRE_SIZE_ha),median(ninetyl$FIRE_SIZE_ha),sum(ninetyl$FIRE_SIZE_ha))
  outputl<-rbind(outputl,outtyl)
}

colnames(outputl) <- c("ecn", "lnobs", "lmean","lsd","lmedian","lsum")
row.names(outputl)<-NULL
head(outputl)


#merge two dataframes together
output2<-merge(outputh,outputl,by="ecn")

#calculate the total number of fires of all ignitions
output2$totfires<-output2$hnobs+output2$lnobs
output2

#calculate the percent of human ignitions by ecoregion
output2$perh<-output2$hnobs/output2$totfires*100

#put NA_L3CODE back in to join easily in Arc
#tt9<-unique(subz[c("ecn", "NA_L3CODE")])
jjj<-left_join(output2,tt9,by="ecn")
jjj
#output table; used to make Figure 1
write.table(jjj, "results/firehasum_ecn_top_ten_Short_update_hl.csv", sep=",", row.names=FALSE, append=FALSE)

#what is the range of percent human started fires by ecoregion?
summary(jjj$perh)
#min=12.50
#max=100.00


################################################
# fire season, large fires, all ecoregions
#cannot use 'output' from above because this one keeps different columns (summary columns)... this one also takes longer to run
#I believe that output3 is the same as outputy...try using outputy
#output3=NULL

#for(i in tt3) {
  #subby<-subz[subz$ecn==i,]
  #ninety<-subset(subby, ha >= quantile(ha, 0.9))
  #outty<-ninety[,]
  #output3<-rbind(output3,outty)
#}

#do I need these 6 lines of code below?
#eco.legend <- data.frame(ec = unique(outputy$NA_L3CODE), ed = unique(outputy$NA_L3NAME))
#eco.legend$ec2<-as.character(eco.legend$ec)
#eco.legend$ec3<-gsub("\\.", "", (eco.legend$ec2))
#eco.legend$ecn<-as.numeric(eco.legend$ec3)
#eco.legend <- subset(eco.legend, eco.legend$ecn != 000 & is.na(eco.legend$ecn) == FALSE)
#eco.legend <- eco.legend[order(eco.legend$ecn),]

head(keep)
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
#this gives the median day of year for human and lightning by ecoregion

#output table
write.table(r4.1, "results/meddoyhl.csv", sep=",", row.names=FALSE, append=FALSE)
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

#output table
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















