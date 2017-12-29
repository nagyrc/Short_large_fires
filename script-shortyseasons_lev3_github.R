#this code is part of the Short_large_fires project by Dr. R. Chelsea Nagy
#FIRE SEASONALITY with Level 3 ecoregions...Level 1 ecoregions in separate R script (script-shortyseasons061516.R)
#use updated Short data from preprocessing

library(doBy)
library(gdata)
library(ggplot2)
library(tidyr)

#updated Short data and joined with ecoregion data

setwd("/Users/rana7082/Dropbox/ecoregions/derived/")
setwd("C:/Users/rnagy/Dropbox/ecoregions/derived/")

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

#summary stats
r3ccc <- summaryBy(ha~ecn, data=subz, FUN=c(length,mean,sd,median,sum))
r3ccc$ha.se<-r3ccc$ha.sd/sqrt(r3ccc$ha.length)
#r3ccc$ha.mean2sd<-r3ccc$ha.mean+(2*r3ccc$ha.sd)
r3ccc

r3cccb<-as.data.frame(r3ccc)
head(r3cccb)

#order by mean ha
r3d<-r3cccb[order(r3cccb$ha.mean),]
write.table(r3d, "/Users/rana7082/Dropbox/ecoregions/derived/fireha_ecn.csv", sep=",", row.names=FALSE)

#order by sum ha
r3e<-r3cccb[order(r3cccb$ha.sum),]
write.table(r3e, "/Users/rana7082/Dropbox/ecoregions/derived/firehasum_ecn.csv", sep=",", row.names=FALSE)

tt9<-unique(subz[c("ecn", "NA_L3CODE")])

#add in code for easy joining in Arc
library(dplyr)
hhh<-left_join(r3d,tt9,by="ecn")

head(hhh)
write.table(hhh, "/Users/rana7082/Dropbox/ecoregions/derived/fireha_ecn.csv", sep=",", row.names=FALSE)

#################################
#fire size by cause across all fires
r4 <- summaryBy(ha~ig+ecn, data=subz, FUN=c(length,mean,sd,median,sum))
r4$ha.se<-r4$ha.sd/sqrt(r4$ha.length)
#r4$ha.mean2sd<-r4$ha.mean+(2*r4$ha.sd)
r4

r4b<-as.data.frame(r4)
head(r4b)
write.table(r4b, "/Users/rana7082/Desktop/fireha_hvsl_ecn.csv", sep=",", row.names=FALSE)

#order by mean fire size
r5<-r4b[order(r4b$ha.mean),]
r5
head(r5)
#biggest are lightning fires
#ask Bethany for help here

r5bb<-r4b[order(r4b$ha.sum),]
r5bb


r5wide<-r5[,c("ig","ecn","ha.mean")]
r5wide<-spread(r5wide,ig,ha.mean)
head(r5wide)

plot(r5wide$human,r5wide$lightning)
#within an ecoregion, as the human fires get bigger, lightning fires tend to get bigger


r5widebb<-r5bb[,c("ig","ecn","ha.sum")]
r5widebb<-spread(r5widebb,ig,ha.sum)
head(r5widebb)

plot(r5widebb$human,r5widebb$lightning)

r5widebbdf<-as.data.frame(r5widebb)
write.table(r5widebbdf, "/Users/rana7082/Desktop/firehasum_hvsl_ecn_wide.csv", sep=",", row.names=FALSE)

#######
#%human across all fires
#will use this to make updated version of Figure 1. updated 11/10/16
#join to ecoregion layer to display % human
head(subz)
r6 <- summaryBy(OBJECTID~ig+ecn, data=subz, FUN=length)
#Error in nobs.default(x, ...) : no 'nobs' method is available
r6

r6wide<-spread(r6,ig,OBJECTID.length)
head(r6wide)

r6wide$tot<-r6wide$human+r6wide$lightning
r6wide$perh<-r6wide$human/r6wide$tot*100

head(r6wide)

r7<-r6wide[order(r6wide$perh),]
r7

write.table(r7, "/Users/rana7082/Dropbox/ecoregions/derived/perh_ecn_Short_update.csv", sep=",", row.names=FALSE)

#range across ecn's is from 16-99% human
##############################################



#top 10% largest fires only
#################################
#to understand what a large fire is in each ecoregion
#extract top 10% of largest fires from each ecoregion


#then put back into a dataframe that has all columns
#can change this to 0.95 to get the 5% largest fires per ecn
outputy=NULL
for (i in tt3) {
  subby<-subz[subz$ecn==i,]
  ninety<-subset(subby, ha >= quantile(ha, 0.90))
  outputy<-rbind(outputy,data.frame(ninety[,]))
}

write.table(outputy, "/Users/rana7082/Dropbox/ecoregions/derived/Short_large10.csv", sep=",", row.names=FALSE)


head(outputy)
tail(outputy)
summary(outputy$ha)
#max=225900 ha
#use this as max for x axis and determining bin width

#now can use this dataframe for further analyses



#look at histogram across all ecns
library(ggplot2)
ggplot(data=outputy, aes(outputy$ha)) + 
  geom_histogram(binwidth=1000) 

#######################
#need to split out by ignitions? if so, see code below
barplot(r2m, beside=T, horiz=F, legend=T, col=c("red", "blue"), 
        main = eco.desc, xlab = "Day of year in Julian Day", ylab = "Number of Fires", 
        border=c("red","blue"), cex.names=1.25, cex.axis=1.25, cex.lab=1.25)


#######################
#look at ind. ecn histograms to make sure they look reasonable
hpy <- list()

for (i in tt3) {
  subzz<-outputy[outputy$ecn==i,]
  
  hpy[[i]] <- ggplot(subzz, aes(x=ha)) + 
    geom_histogram(binwidth=100) + 
    facet_wrap(~ig)
  
}

#test by printing one
hpy[821]
#I think it's working- compare these to the whole dataset, seems to have shifted



#create dataframe of nobs, mean, sd of fire size from each ecoregion

head(subz)

output=NULL

for (i in tt3) {
  subby<-subz[subz$ecn==i,]
  ninety<-subset(subby, ha >= quantile(ha, 0.9))
  outty<- c(i,length(ninety$ha),mean(ninety$ha),sd(ninety$ha),median(ninety$ha),sum(ninety$ha))
  output<-rbind(output,outty)
}

head(output)

colnames(output) <- c("ecn", "nobs", "mean","sd","median","sum")
row.names(output)<-NULL
output

tt9<-unique(subz[c("ecn", "NA_L3CODE")])
library(dplyr)
head(tt9)
fff<-merge(output,tt9,by="ecn")
head(fff)

write.table(fff, "/Users/rana7082/Dropbox/ecoregions/derived/firehasum_ecn_top_ten_Short_update.csv", sep=",", row.names=FALSE)
#updated 11/3/16 with new Short data; updated 11/21/16 with medians
#use this to remake Figure 2



#calculate % human by ecn for just top 10% largest fires
#use this to remake Figure 2 or to make alternative version of Figure 3
#this isn't working with updated Short data.
#need subz$ig info in here. no ig info in output
head(output)


#run on each subset (human and lightning)
outputh=NULL

for (i in tt3) {
  subh<-subz[subz$ig!="lightning",]
  subbyh<-subh[subh$ecn==i,]
  ninetyh<-subset(subbyh, ha >= quantile(ha, 0.9))
  outtyh<- c(i,length(ninetyh$ha),mean(ninetyh$ha),sd(ninetyh$ha),median (ninetyh$ha),sum(ninetyh$ha))
  outputh<-rbind(outputh,outtyh)
}

head(outputh)
colnames(outputh) <- c("ecn", "hnobs", "hmean","hsd","hmedian","hsum")
row.names(outputh)<-NULL

outputl=NULL

for (i in tt3) {
  subl<-subz[subz$ig=="lightning",]
  subbyl<-subl[subl$ecn==i,]
  ninetyl<-subset(subbyl, ha >= quantile(ha, 0.9))
  outtyl<- c(i,length(ninetyl$ha),mean(ninetyl$ha),sd(ninetyl$ha),median(ninetyl$ha),sum(ninetyl$ha))
  outputl<-rbind(outputl,outtyl)
}

head(outputl)
colnames(outputl) <- c("ecn", "lnobs", "lmean","lsd","lmedian","lsum")
row.names(outputl)<-NULL

outputl

#merge two dataframes together
output2<-merge(outputh,outputl,by="ecn")
head(output2)
output2

output2$totfires<-output2$hnobs+output2$lnobs
output2$perh<-output2$hnobs/output2$totfires*100


#put back in NA_L3CODE to join easily in Arc
tt9<-unique(subz[c("ecn", "NA_L3CODE")])
library(dplyr)
jjj<-left_join(output2,tt9,by="ecn")
head(jjj)
write.table(jjj, "/Users/rana7082/Dropbox/ecoregions/derived/firehasum_ecn_top_ten_Short_update_hl.csv", sep=",", row.names=FALSE)

jjj
summary(jjj$perh)
#min=15.76, 6.2.15, Idaho Batholith
#max=99.58, 11.1.2, Central CA Valley


#do not need next 20 lines below???
###
library(doBy)
r6b <- summaryBy(OBJECTID~ig+ecn, data=output, FUN=nobs)
r6b

library(tidyr)
r6wideb<-spread(r6b,ig,oid.nobs)
head(r6wideb)

#replace NA's here with zeros
r6wideb[is.na(r6wideb)]<-0

r6wideb$tot<-r6wideb$human+r6wideb$lightning
r6wideb$perh<-r6wideb$human/r6wideb$tot*100

head(r6wideb)

r7b<-r6wideb[order(r6wideb$perh),]
r7b

write.table(r7b, "/Users/rana7082/Desktop/perh_ecn_top_ten.csv", sep=",", row.names=FALSE)




##############################
#fire season for top 10% of fires only by ecoregion
#histograms with fire frequency by day of year, top 10% only
output=NULL

for (i in tt3) {
  subby<-slim[slim$ecn==i,]
  ninety<-subset(subby, ha >= quantile(ha, 0.9))
  outty<-ninety[,]
  output<-rbind(output,outty)
}

head(output)

eco.legend <- data.frame(ec = unique(output$NA_L3CODE), ed = unique(output$NA_L3NAME))
eco.legend$ec2<-as.character(eco.legend$ec)
eco.legend$ec3<-gsub("\\.", "", (eco.legend$ec2))
eco.legend$ecn<-as.numeric(eco.legend$ec3)

eco.legend

eco.legend <- subset(eco.legend, eco.legend$ecn != 000 & is.na(eco.legend$ecn) == FALSE)
eco.legend <- eco.legend[order(eco.legend$ecn),]

###################
rr<-summaryBy(OBJECTID~ecn,data=output,FUN=nobs)
rr

summary(rr$OBJECTID.nobs)
#min=31, max=23140
sum(rr$OBJECTID.nobs<100)



rr2<-summaryBy(OBJECTID~ecn+ig,data=output,FUN=nobs)
rr2

84*2

#
r2 <- summaryBy(OBJECTID~DISCOVERY1+ig+ecn, data=output, FUN=nobs)
r2 <- subset(r2, r2$ecn != "000")
r2 <- subset(r2, is.na(r2$ecn) == FALSE)

head(r2)

tt<-as.numeric(unique(r2$ecn))

head(r2)

# plots by indv. ecoregions
for (i in tt) {
  
  wide <- reshape(r2[r2$ecn == i,], v.names="OBJECTID.nobs", idvar="ig", timevar="DISCOVERY1", direction="wide") #change number here 
  wide[is.na(wide)] <- 0
  
  w <- wide[,3:length(wide)]
  
  colnames(wide) <- c("ig", "ecn", substr(colnames(w), 15, 17))
  
  r2m <- as.matrix(wide[,3:length(wide)])
  rownames(r2m) <- wide[,1]
  
  head(r2m)
  
  eco.desc <- eco.legend[eco.legend$ecn == i, c("ed")] #change number here
  
  barplot(r2m, beside=T, horiz=F, legend=T, col=c("red", "blue"), 
          main = eco.desc, xlab = "Day of year in Julian Day", ylab = "Number of Fires", 
          border=c("red","blue"), cex.names=1.25, cex.axis=1.25, cex.lab=1.25)
}

#attempt to standardize x axis by filling in new matrix
#no idea why I have to set the x lim to 1000 to get the whole record to display
#this is part of figure 4 (figure 4b (Central California Valley), figure 4c (Idaho Batholith))
for (i in tt) {
  
  wide <- reshape(r2[r2$ecn == i,], v.names="OBJECTID.nobs", idvar="ig", timevar="DISCOVERY1", direction="wide") #change number here 
  wide[is.na(wide)] <- 0
  
  w <- wide[,3:length(wide)]
  
  colnames(wide) <- c("ig", "ecn", substr(colnames(w), 15, 17))
  
  blank<-matrix(0,2,366)
  rownames(blank)<-c("human","lightning")
  colnames(blank)<-c(1:366)
  
  r2m <- as.matrix(wide[,3:length(wide)], replace=blank)
  rownames(r2m) <- wide[,1]
  
  h <- match(rownames(r2m), rownames(blank))
  j <- match(colnames(r2m), colnames(blank))
  
  blank[h,j] <- r2m
  
  eco.desc <- eco.legend[eco.legend$ecn == i, c("ed")] #change number here
  
  barplot(blank, beside=T, horiz=F, legend=T, col=c("red", "blue"), 
          main = eco.desc, xlab = "Day of year in Julian Day", ylab = "Number of Fires", 
          border=c("red","blue"), xlim=c(1,1300))
}



head(r2m)
head(blank)
#ggplot(d,aes(x=x,y=x))+geom_bar(stat="identity",fill="lightblue",colour="gray")+xlim(c(0,100))

eco.desc




head(output)
r3 <- summaryBy(OBJECTID~DISCOVERY1+ig, data=output, FUN=nobs)
r3
#tt<-as.numeric(unique(r3$ecn))


# fire season, large fires, all ecoregions
wide <- reshape(r3,v.names="OBJECTID.nobs", idvar="ig", timevar="DISCOVERY1", direction="wide") #change number here 
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
        xlab = "Day of year in Julian Day", ylab = "Number of Fires", 
        border=c("red","blue"), cex.names=1.25, cex.axis=1.25, cex.lab=1.25,ylim=c(0,1200))

#check to make sure correct # obs
yyy<-rowSums(r2m[,])
head(yyy)
129968+30640
#160608

r4 <- summaryBy(DISCOVERY1~ig, data=output, FUN=median)
r4
#median doy for human fires = 118, median doy for lightning fires = 204

r4.1<-summaryBy(DISCOVERY1~ig+NA_L3CODE, data=output, FUN=median)
r4.1

write.table(r4.1, "C:/Users/rnagy/Dropbox/ecoregions/derived/meddoyhl.csv", sep=",", row.names=FALSE)


hsub<-subset(output,output$ig=="human")
lsub<-subset(output,output$ig=="lightning")

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

out<-hsub[ which(hsub$DISCOVERY1>267| hsub$DISCOVERY1 < 104), ]
#74,704

head(out)

r66 <- summaryBy(OBJECTID~NA_L3CODE, data=out, FUN=nobs)
r66


write.table(r66, "C:/Users/rnagy/Dropbox/ecoregions/derived/non_light_season.csv", sep=",", row.names=FALSE)



#split into east and west US; figure 4a; figure 4b
setwd("/Users/rana7082/Dropbox/ecoregions/derived/")
setwd("C:/Users/rnagy/Dropbox/ecoregions/derived/")
regiontab<-read.csv("arc_map_regions.csv")
region<-as.data.frame(regiontab)

head(region)
head(output)

library(dplyr)
outputrr<-left_join(output,region,by="NA_L3CODE")
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
# fire season, large fires, all ecoregions, Figure 4a in manuscript
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
#seasonal correlations for east and west (by region)
setwd("C:/Users/rnagy/Dropbox/ecoregions/derived/")
regiontab<-read.csv("arc_map_regions.csv")
region<-as.data.frame(regiontab)

head(region)
head(output)

library(dplyr)
outputrr<-left_join(output,region,by="NA_L3CODE")
head(outputrr)

r3 <- summaryBy(OBJECTID~DISCOVERY1+region, data=outputrr, FUN=nobs)
head(r3)

subzrr<-left_join(subz,region,by="NA_L3CODE")
head(subzrr)

r4 <- summaryBy(OBJECTID~DISCOVERY1+region, data=subzrr, FUN=nobs)
head(r4)

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
setwd("C:/Users/rnagy/Dropbox/ecoregions/derived/")
regiontab<-read.csv("arc_map_regions.csv")
region<-as.data.frame(regiontab)

head(region)
head(output)

library(dplyr)
outputrr<-left_join(output,region,by="NA_L3CODE")
head(outputrr)

outputh<-outputrr[outputrr$ig=="human",]

r3 <- summaryBy(OBJECTID~DISCOVERY1+region, data=outputh, FUN=nobs)
head(r3)


subzrr<-left_join(subz,region,by="NA_L3CODE")
head(subzrr)

subzh<-subzrr[subzrr$ig=="human",]

r4 <- summaryBy(OBJECTID~DISCOVERY1+region, data=subzh, FUN=nobs)
head(r4)

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

