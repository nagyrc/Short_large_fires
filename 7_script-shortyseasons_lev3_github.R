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
outputhrr<-outputrr[outputrr$ig=="human",]

#large human-caused fires only
r8 <- summaryBy(OBJECTID~DISCOVERY1+region, data=outputhrr, FUN=nobs)
head(r8)

#fires of all sizes
subzrr<-left_join(subz,region,by="NA_L3CODE")
head(subzrr)

#subset for human-caused fires of all sizes only
subzhrr<-subzrr[subzrr$ig=="human",]

r9 <- summaryBy(OBJECTID~DISCOVERY1+region, data=subzhrr, FUN=nobs)
head(r9)

#join together
lll<-merge(r8,r9, by=c("DISCOVERY1","region"))
head(lll)
lll

names(lll)[1] <- "doy"
names(lll)[3]<-"large"
names(lll)[4]<-"all"

tempw<-subset(lll, lll$region == "west")
cor(tempw$large,tempw$all)
#r=0.9309
summary(lm(large~all, data=tempw))
#p<2e-16

tempe<-subset(lll, lll$region == "east")
cor(tempe$large,tempe$all)
#r=0.988
summary(lm(large~all, data=tempe))
#p<2e-16
#these numbers are reported in the manuscript








#seasonal correlations at the ecoregion scale
#number of fires by Julian day of year
#human ignitions only
outputh<-outputy[outputy$ig=="human",]

r11 <- summaryBy(OBJECTID~DISCOVERY1+NA_L3CODE, data=outputh, FUN=nobs)

subzh<-subz[subz$ig=="human",]

r12 <- summaryBy(OBJECTID~DISCOVERY1+NA_L3CODE, data=subzh, FUN=nobs)

qqq<-merge(r11,r12, by=c("DISCOVERY1","NA_L3CODE"))
head(qqq)


names(qqq)[1] <- "doy"
names(qqq)[3]<-"large"
names(qqq)[4]<-"all"


cor(qqq$large,qqq$all)
#r=0.940
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

write.table(dfe, "/Users/rana7082/Dropbox/ecoregions/derived/seasonal_corr_human_only.csv", sep=",", row.names=FALSE)















####################################
#large fire data exploration

#what is max fire size of large fires?
summary(outputy$ha)
#max=225900 ha
#use this as max for x axis and determining bin width

#look at histogram of fire sizes across all ecns
ggplot(data=outputy, aes(outputy$ha)) + 
  geom_histogram(binwidth=1000) 
#many relatively small 'large' fires


#######################
#individual ecoregion histograms of fire size by human and lightning ignitions
#not shown in manuscript
hpy <- list()

for (i in tt3) {
  subzz<-outputy[outputy$ecn==i,]
  
  hpy[[i]] <- ggplot(subzz, aes(x=ha)) + 
    geom_histogram(binwidth=100) + 
    facet_wrap(~ig)
  
}

#example ecoregion
hpy[821]








#######################################################
#seasonal correlations of large fires with fires of all sizes for east and west (by region)
#number of fires by Julian day of year
#fires of all causes
#not reported in the manuscript

#large fires only
r3 <- summaryBy(OBJECTID~DISCOVERY1+region, data=outputrr, FUN=nobs)
head(r3)

#fires of all sizes
subzrr<-left_join(subz,region,by="NA_L3CODE")
head(subzrr)

r4 <- summaryBy(OBJECTID~DISCOVERY1+region, data=subzrr, FUN=nobs)
head(r4)

#join the two dataframes
lll<-merge(r3,r4, by=c("DISCOVERY1","region"))
head(lll)
lll

names(lll)[1] <- "doy"
names(lll)[3]<-"large"
names(lll)[4]<-"all"

tempw<-subset(lll, lll$region == "west")
cor(tempw$large,tempw$all)
#r=0.978
summary(lm(large~all, data=tempw))
#p<2e-16

tempe<-subset(lll, lll$region == "east")
cor(tempe$large,tempe$all)
#r=0.988
summary(lm(large~all, data=tempe))
#p<2e-16


##############################
#####
#seasonal correlations at the ecoregion scale
head(output)

r3 <- summaryBy(OBJECTID~DISCOVERY1+NA_L3CODE, data=output, FUN=nobs)
head(r3)

r4 <- summaryBy(OBJECTID~DISCOVERY1+NA_L3CODE, data=subz, FUN=nobs)
head(r4)

lll<-merge(r3,r4, by=c("DISCOVERY1","NA_L3CODE"))
head(lll)


names(lll)[1] <- "doy"
names(lll)[3]<-"large"
names(lll)[4]<-"all"


cor(lll$large,lll$all)
#r=0.937
summary(lm(large~all, data=lll))
#p<2e-16


tt3<-unique(lll$NA_L3CODE)
tt3

outpute<-NULL
for (i in tt3) {
  subbye<-lll[lll$NA_L3CODE==i,]
  corre<-cor(subbye$large,subbye$all)
  outpute<-rbind(outpute,corre)
}

cop<-outpute

head(cop)

df<-as.data.frame(cop)

names(df)[1] <- "corr"

tt3v<-as.vector(tt3)
df$NA_L3CODE<-tt3v

row.names(df)<-NULL


head(df)

write.table(df, "C:/Users/rnagy/Dropbox/ecoregions/derived/seasonal_corr.csv", sep=",", row.names=FALSE)







#######################################################
#####
#not used in manuscript; below is for fires of all sizes, not just the largest fires
str(slim)
head(slim)

vec1<-unique(slim$NA_L3CODE)
vec2<-unique(slim$NA_L3NAME)

vec1

#############################
#BY ECOREGION


#############
#histograms with fire frequency by day of year, all fires
eco.legend <- data.frame(ec = unique(slim$NA_L3CODE), ed = unique(slim$NA_L3NAME))
head(eco.legend)
str(eco.legend)

#change the format of the legend in a not so elegant fashion
eco.legend$ec2<-as.character(eco.legend$ec)
eco.legend$ec3<-gsub("\\.", "", (eco.legend$ec2))
eco.legend$ecn<-as.numeric(eco.legend$ec3)
eco.legend <- subset(eco.legend, eco.legend$ecn != 000 & is.na(eco.legend$ecn) == FALSE)
eco.legend <- eco.legend[order(eco.legend$ecn),]

head(eco.legend)
unique(eco.legend$ecn)
###################

head(slim)

r2 <- summaryBy(OBJECTID~DISCOVERY1+ig+ecn, data=slim, FUN=length)
r2 <- subset(r2, r2$ecn != 000)
r2 <- subset(r2, is.na(r2$ecn) == FALSE)


head(r2)
unique(r2$ecn)
table(r2$DISCOVERY1)

#plots of number of fires (human and lightning) by Julian day per ecoregion
tt<-as.numeric(unique(r2$ecn))

for (i in tt) {
  
  wide <- reshape(r2[r2$ecn == i,], v.names="OBJECTID.length", idvar="ig", timevar="DISCOVERY1", direction="wide") #change number here 
  wide[is.na(wide)] <- 0
  
  head(wide)
  
  w <- wide[,3:length(wide)]
  
  colnames(w) <- c(substr(colnames(w), 17,19))
  rownames(w)<-c ("human","lightning")
  
  wm<-as.matrix(w)
  
  head(wm)
  
  blank<-matrix(0,2,366)
  rownames(blank)<-c("human","lightning")
  colnames(blank)<-c(1:366)
  
  head(blank)
  
  h <- match(rownames(wm), rownames(blank))
  j <- match(colnames(wm), colnames(blank))
  
  blank[h,j] <- wm
  
  head(blank)
  
  eco.desc <- eco.legend[eco.legend$ecn == i, c("ed")] #change number here
  
  barplot(blank, beside=T, horiz=F, legend=T, col=c("red", "blue"), 
          main = eco.desc, xlab = "Day of year in Julian Day", ylab = "Number of Fires", 
          border=c("red","blue"), xlim=c(1,1300))
}

########
#example ecoregion plots
tt2<-c(6215,1112)  
for (i in tt2) {
  
  wide <- reshape(r2[r2$ecn == i,], v.names="oid.nobs", idvar="ig", timevar="ddoy", direction="wide") #change number here 
  wide[is.na(wide)] <- 0
  
  w <- wide[,3:length(wide)]
  colnames(wide) <- c("ig", "ecn", substr(colnames(w), 10, 12))
  
  r2m <- as.matrix(wide[,3:length(wide)])
  rownames(r2m) <- wide[,1]
  
  head(r2m)
  
  eco.desc <- eco.legend[eco.legend$ecn == i, c("ed")] #change number here
  
  barplot(r2m, beside=T, horiz=F, legend=T, col=c("red", "blue"), 
          main = eco.desc, xlab = "Day of year in Julian Day", ylab = "Number of Fires", 
          border=c("red","blue"), cex.names=1.25, cex.axis=1.25, cex.lab=1.25)
}

#######
#END (unless needed on same scale or log scale, then change ec to ecn in below)


#######################################################

#plots by indv. ecoregions of fire size binned in categories for human vs. lightning fires

###
#to make a key of ecoregion names and numbers
tt7<-unique(subz[c("ecn", "NA_L3NAME")])

#write table to interpret the ecn numbers
write.table(tt7, "/Users/rana7082/Desktop/ecn_Lev3_key.csv", sep=",", row.names=FALSE)
###

###
#to make plots
tt3<-unique(subz$ecn)

hp <- list()

for (i in tt3) {
  subzz<-subz[subz$ecn==i,]
  
  hp[[i]] <- ggplot(subzz, aes(x=ha)) + 
    geom_histogram(binwidth=100) + 
    facet_wrap(~ig)
  
}

#print one example
hp[821]


#############################

#summary stats of number of fires, mean fire size, median fire size, total burned area for each ecoregion
r3ccc <- summaryBy(ha~ecn, data=subz, FUN=c(length,mean,sd,median,sum))
r3ccc$ha.se<-r3ccc$ha.sd/sqrt(r3ccc$ha.length)

#make into a dataframe
r3cccb<-as.data.frame(r3ccc)
head(r3cccb)

#order dataframe by mean fire size (ha)
r3d<-r3cccb[order(r3cccb$ha.mean),]

#output table
write.table(r3d, "/Users/rana7082/Dropbox/ecoregions/derived/fireha_ecn.csv", sep=",", row.names=FALSE)

#order dataframe by sum of burned area (ha)
r3e<-r3cccb[order(r3cccb$ha.sum),]

#output table
write.table(r3e, "/Users/rana7082/Dropbox/ecoregions/derived/firehasum_ecn.csv", sep=",", row.names=FALSE)


#add in code to dataframe for easy joining in Arc
tt9<-unique(subz[c("ecn", "NA_L3CODE")])
hhh<-left_join(r3d,tt9,by="ecn")

#output table
write.table(hhh, "/Users/rana7082/Dropbox/ecoregions/derived/fireha_ecn.csv", sep=",", row.names=FALSE)

#################################
#summary stats of number of fires, mean fire size, median fire size, total burned area by ecoregion and ignition source
r4 <- summaryBy(ha~ig+ecn, data=subz, FUN=c(length,mean,sd,median,sum))
r4$ha.se<-r4$ha.sd/sqrt(r4$ha.length)

#make into dataframe
r4b<-as.data.frame(r4)

#output table
write.table(r4b, "/Users/rana7082/Desktop/fireha_hvsl_ecn.csv", sep=",", row.names=FALSE)

#order by mean fire size
r5<-r4b[order(r4b$ha.mean),]

#order by sum of burned area
r5bb<-r4b[order(r4b$ha.sum),]

#transpose table into wide format
r5wide<-r5[,c("ig","ecn","ha.mean")]
r5wide<-spread(r5wide,ig,ha.mean)
head(r5wide)

#scatterplot of mean fire size of human vs. lightning by ecoregion
plot(r5wide$human,r5wide$lightning)
#within an ecoregion, as the human fires get bigger, lightning fires tend to get bigger


r5widebb<-r5bb[,c("ig","ecn","ha.sum")]
r5widebb<-spread(r5widebb,ig,ha.sum)
head(r5widebb)

#scatterplot of sum of burned area of human vs. lightning by ecoregion
plot(r5widebb$human,r5widebb$lightning)

#convert to dataframe
r5widebbdf<-as.data.frame(r5widebb)

#output table
write.table(r5widebbdf, "/Users/rana7082/Desktop/firehasum_hvsl_ecn_wide.csv", sep=",", row.names=FALSE)

#######
#number of human vs. lightning caused fires by ecoregion of fires of all sizes

r6 <- summaryBy(OBJECTID~ig+ecn, data=subz, FUN=length)

#transform to wide
r6wide<-spread(r6,ig,OBJECTID.length)

#calculate the total number of fires (all ignitions)
r6wide$tot<-r6wide$human+r6wide$lightning

#calculate the percent of human started fires
r6wide$perh<-r6wide$human/r6wide$tot*100

#order by decreasing percent human fires
r7<-r6wide[order(r6wide$perh),]
#range across ecn's is from 16-99% human

#output table
write.table(r7, "/Users/rana7082/Dropbox/ecoregions/derived/perh_ecn_Short_update.csv", sep=",", row.names=FALSE)


##############################################