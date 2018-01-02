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

setwd("/Users/rana7082/Dropbox/ecoregions/derived/")
setwd("C:/Users/rnagy/Dropbox/ecoregions/derived/")
setwd("/Users/rana7082-su/Dropbox/ecoregions/derived/")

#get updated Short data from: update_Short_dataset_102716.R
slimtab <- read.csv("updatedShort_w_eco_slim3.csv")

slim<-as.data.frame(slimtab)

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

slim$ecn<-as.character(gsub("\\.","",slim$NA_L3CODE))
slim$ecn<-as.numeric(slim$ecn)

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

#subset out water fires
subz <- subset(slim, slim$ecn != 000)
subz <- subset(slim, slim$ecn != 0)
subz <- subset(subz, is.na(subz$ecn) == FALSE)

head(subz)
tail(subz)

tt3<-unique(subz$ecn)

is.numeric (tt3)

###
#to make a key of ecoregion names and numbers
tt7<-unique(subz[c("ecn", "NA_L3NAME")])

#write table to interpret the ecn numbers
write.table(tt7, "/Users/rana7082/Desktop/ecn_Lev3_key.csv", sep=",", row.names=FALSE)
###

###
#to make plots
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



#top 10% largest fires only
#################################
#to understand what a large fire is in each ecoregion
#extract top 10% of largest fires from each ecoregion
#then put back into a dataframe that has all columns
outputy=NULL
for (i in tt3) {
  subby<-subz[subz$ecn==i,]
  ninety<-subset(subby, ha >= quantile(ha, 0.90))
  outputy<-rbind(outputy,data.frame(ninety[,]))
}

#output table of large fires
write.table(outputy, "/Users/rana7082/Dropbox/ecoregions/derived/Short_large10.csv", sep=",", row.names=FALSE)


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
hpy <- list()

for (i in tt3) {
  subzz<-outputy[outputy$ecn==i,]
  
  hpy[[i]] <- ggplot(subzz, aes(x=ha)) + 
    geom_histogram(binwidth=100) + 
    facet_wrap(~ig)
  
}

#example ecoregion
hpy[821]





################################
#top 10% largest fires; summary statistics
#create dataframe of number of fires, mean, sd of fire size by ecoregion (summary columns of the 10% largest fires)
output=NULL

for (i in tt3) {
  subby<-subz[subz$ecn==i,]
  ninety<-subset(subby, ha >= quantile(ha, 0.9))
  outty<- c(i,length(ninety$ha),mean(ninety$ha),sd(ninety$ha),median(ninety$ha),sum(ninety$ha))
  output<-rbind(output,outty)
}

###

colnames(output) <- c("ecn", "nobs", "mean","sd","median","sum")
row.names(output)<-NULL
output

#add key to this dataframe
fff<-merge(output,tt9,by="ecn")
head(fff)

#output table; used to make Table S1
write.table(fff, "/Users/rana7082/Dropbox/ecoregions/derived/firehasum_ecn_top_ten_Short_update.csv", sep=",", row.names=FALSE)









#calculate % human by ecoregion for just top 10% largest fires
#use this to make Figure 1 

#run on each subset (human and lightning)
#human subset of large fires only
outputh=NULL

for (i in tt3) {
  subh<-subz[subz$ig!="lightning",]
  subbyh<-subh[subh$ecn==i,]
  ninetyh<-subset(subbyh, ha >= quantile(ha, 0.9))
  outtyh<- c(i,length(ninetyh$ha),mean(ninetyh$ha),sd(ninetyh$ha),median (ninetyh$ha),sum(ninetyh$ha))
  outputh<-rbind(outputh,outtyh)
}

colnames(outputh) <- c("ecn", "hnobs", "hmean","hsd","hmedian","hsum")
row.names(outputh)<-NULL

#lightning subset of large fires only
outputl=NULL

for (i in tt3) {
  subl<-subz[subz$ig=="lightning",]
  subbyl<-subl[subl$ecn==i,]
  ninetyl<-subset(subbyl, ha >= quantile(ha, 0.9))
  outtyl<- c(i,length(ninetyl$ha),mean(ninetyl$ha),sd(ninetyl$ha),median(ninetyl$ha),sum(ninetyl$ha))
  outputl<-rbind(outputl,outtyl)
}

colnames(outputl) <- c("ecn", "lnobs", "lmean","lsd","lmedian","lsum")
row.names(outputl)<-NULL


#merge two dataframes together
output2<-merge(outputh,outputl,by="ecn")

#calculate the total number of fires of all ignitions
output2$totfires<-output2$hnobs+output2$lnobs

#calculate the percent of human ignitions by ecoregion
output2$perh<-output2$hnobs/output2$totfires*100

#put back in NA_L3CODE to join easily in Arc
tt9<-unique(subz[c("ecn", "NA_L3CODE")])
jjj<-left_join(output2,tt9,by="ecn")

#output table; used to make Figure 1
write.table(jjj, "/Users/rana7082/Dropbox/ecoregions/derived/firehasum_ecn_top_ten_Short_update_hl.csv", sep=",", row.names=FALSE)

#what is range of percent human started fires by ecoregion?
summary(jjj$perh)
#min=15.76, 6.2.15, Idaho Batholith
#max=99.58, 11.1.2, Central CA Valley


################################################
# fire season, large fires, all ecoregions
#cannot use 'output' from above because this one keeps different columns (summary columns)... this one also takes longer to run
output3=NULL

for(i in tt3) {
  subby<-subz[subz$ecn==i,]
  ninety<-subset(subby, ha >= quantile(ha, 0.9))
  outty<-ninety[,]
  output3<-rbind(output3,outty)
}

eco.legend <- data.frame(ec = unique(output3$NA_L3CODE), ed = unique(output3$NA_L3NAME))
eco.legend$ec2<-as.character(eco.legend$ec)
eco.legend$ec3<-gsub("\\.", "", (eco.legend$ec2))
eco.legend$ecn<-as.numeric(eco.legend$ec3)
eco.legend <- subset(eco.legend, eco.legend$ecn != 000 & is.na(eco.legend$ecn) == FALSE)
eco.legend <- eco.legend[order(eco.legend$ecn),]

r3 <- summaryBy(OBJECTID~DISCOVERY1+ig, data=output3, FUN=length)

# fire season, large fires, all ecoregions
wide <- reshape(r3,v.names="OBJECTID.length", idvar="ig", timevar="DISCOVERY1", direction="wide") #change number here 
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
#this looks pretty funky, but is fine when you zoom in 

#check to make sure correct # obs
yyy<-rowSums(r2m[,])
head(yyy)
130346+30641
#160987, but should be 160608
################################################
#median day of year for large human and lightning fires
r4 <- summaryBy(DISCOVERY1~ig, data=output3, FUN=median)
r4
#median doy for human fires = 118, median doy for lightning fires = 204


r4.1<-summaryBy(DISCOVERY1~ig+NA_L3CODE, data=output3, FUN=median)
r4.1
#this gives the median day of year for human and lightning by ecoregion

#output table
write.table(r4.1, "C:/Users/rnagy/Dropbox/ecoregions/derived/meddoyhl.csv", sep=",", row.names=FALSE)
################################################


#stats about what is human fire vs. lightning large fire season
hsub<-subset(output3,output3$ig=="human")
lsub<-subset(output3,output3$ig=="lightning")

twofive<-quantile(hsub$DISCOVERY1, 0.25)
twofive
#76
sevenfive<-quantile(hsub$DISCOVERY1, 0.75)
#222

twofive<-quantile(lsub$DISCOVERY1, 0.25)
#177
sevenfive<-quantile(lsub$DISCOVERY1, 0.75)
#226


j2.5<-quantile(lsub$DISCOVERY1, 0.025)
#104
j97.5<-quantile(lsub$DISCOVERY1, 0.975)
#267

#number of large human-caused fires outside of lightning fire season
out<-hsub[ which(hsub$DISCOVERY1>267| hsub$DISCOVERY1 < 104), ]
#74,704; this is reported in manuscript

#number of human-caused large fires outside of lightning fire season by ecoregion
r66 <- summaryBy(OBJECTID~NA_L3CODE, data=out, FUN=nobs)
r66

#output table
write.table(r66, "C:/Users/rnagy/Dropbox/ecoregions/derived/non_light_season.csv", sep=",", row.names=FALSE)


#########################################
#split into east and west US; Figure 3a; Figure 3b
setwd("/Users/rana7082/Dropbox/ecoregions/derived/")
setwd("C:/Users/rnagy/Dropbox/ecoregions/derived/")
setwd("/Users/rana7082-su/Dropbox/ecoregions/derived/")
regiontab<-read.csv("arc_map_regions.csv")
region<-as.data.frame(regiontab)

head(region)
head(output3)

str(region)
str(output3)

outputrr<-left_join(output3,region,by="NA_L3CODE")
head(outputrr)

rr<-summaryBy(OBJECTID~ig+region, data=outputrr, FUN=nobs)
rr
#human east = 88114 or 92.08%
#lightning east = 7574 or 7.92%
#human west = 41854 or 64.47%
#lightning west = 23066 or 35.53%

#east total = 95688
88114+7574
(88114/95688)*100

(7574/95688)*100

#west total = 64920
41854+23066

(41854/64920)*100

(23066/64920)*100

r3 <- summaryBy(OBJECTID~DISCOVERY1+ig+region, data=outputrr, FUN=nobs)
r3
#tt<-as.numeric(unique(r3$ecn))

head(r3)

r3east <- subset(r3, r3$region == "east")
r3west <- subset(r3, r3$region == "west")


#east
# fire season, large fires, all ecoregions, Figure 3a in manuscript
head(r3east)

r3east <- subset(r3east, select = -c(region) )

wide <- reshape(r3east,v.names="OBJECTID.nobs", idvar="ig", timevar="DISCOVERY1", direction="wide") #change number here 
wide[is.na(wide)] <- 0

head(wide)

w <- wide[,2:length(wide)]
head(w)
colnames(wide) <- c("ig", substr(colnames(w), 11, 13))
colnames(wide) <- c("ig", substr(colnames(w), 11, 13))

r2m <- as.matrix(wide[,2:length(wide)])
rownames(r2m) <- wide[,1]
colnames(r2m)<-c(1:366)

head(r2m)

barplot(r2m, beside=T, horiz=F, legend=T, col=c("red", "blue"), 
        xlab = "Day of year in Julian Day", ylab = "Number of Fires", main= "East",
        border=c("red","blue"), cex.names=1.25, cex.axis=1.25, cex.lab=1.25,ylim=c(0,1200))



#west
r3west <- subset(r3west, select = -c(region) )

wide <- reshape(r3west,v.names="OBJECTID.nobs", idvar="ig", timevar="DISCOVERY1", direction="wide") #change number here 
wide[is.na(wide)] <- 0

head(wide)

w <- wide[,2:length(wide)]
head(w)
colnames(wide) <- c("ig", substr(colnames(w), 11, 13))
colnames(wide) <- c("ig", substr(colnames(w), 11, 13))

r2m <- as.matrix(wide[,2:length(wide)])
rownames(r2m) <- wide[,1]
colnames(r2m)<-c(1:366)

head(r2m)

barplot(r2m, beside=T, horiz=F, legend=T, col=c("red", "blue"), 
        xlab = "Day of year in Julian Day", ylab = "Number of Fires", main="West",
        border=c("red","blue"), cex.names=1.25, cex.axis=1.25, cex.lab=1.25,ylim=c(0,1200))



##########
#seasonal correlations of large fires with fires of all sizes for east and west (by region); number of fires by Julian day of year
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




######
#seasonal correlations for east and west (by region); human ignitions only
#subset only large human-caused fires
outputh<-outputrr[outputrr$ig=="human",]

#large human-caused fires only
r3 <- summaryBy(OBJECTID~DISCOVERY1+region, data=outputh, FUN=nobs)
head(r3)

#fires of all sizes
subzrr<-left_join(subz,region,by="NA_L3CODE")
head(subzrr)

#subset for human-caused fires of all sizes only
subzh<-subzrr[subzrr$ig=="human",]

r4 <- summaryBy(OBJECTID~DISCOVERY1+region, data=subzh, FUN=nobs)
head(r4)

#join together
lll<-merge(r3,r4, by=c("DISCOVERY1","region"))
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




#seasonal correlations at the ecoregion scale- human ignitions only


head(output)

outputh<-output[output$ig=="human",]

r3 <- summaryBy(OBJECTID~DISCOVERY1+NA_L3CODE, data=outputh, FUN=nobs)
head(r3)

subzh<-subz[subz$ig=="human",]

r4 <- summaryBy(OBJECTID~DISCOVERY1+NA_L3CODE, data=subzh, FUN=nobs)
head(r4)

lll<-merge(r3,r4, by=c("DISCOVERY1","NA_L3CODE"))
head(lll)


names(lll)[1] <- "doy"
names(lll)[3]<-"large"
names(lll)[4]<-"all"


cor(lll$large,lll$all)
#r=0.940
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

write.table(df, "/Users/rana7082/Dropbox/ecoregions/derived/seasonal_corr_human_only.csv", sep=",", row.names=FALSE)

