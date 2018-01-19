#this code is part of the Short_large_fires project by Dr. R. Chelsea Nagy

library(sp)
library(rgdal)
library(dplyr)
library(doBy)
library(reshape)
library(ggplot2)
library(foreign)
library(stats)
library(plotrix)
library(stringr)

#bring in Short data with fm and ws extracted
#note, this has large fires only (top 10%)
lrg_fires<- read.csv("data/merged/lrg_fires.csv")
head(lrg_fires)

#the format of NA_L3CODE is formatted incorrectly, so remove that field
#read$NA_L3CODE<-NULL

#bring in something else that can make key from
#readz<- read.csv("data/fire/Short_large10.csv")

#create the key
#key<-unique(readz[c("NA_L3CODE","NA_L3NAME")])

#joinz<-left_join(read,key,by=c('NA_L3NAME'))
#head(joinz)
#this now has a correct field for NA_L3CODE

#tt3<-unique(joinz$NA_L3CODE)
#tt3<-unique(joinz$NA_L3CODE)

#subset just large human or just large lightning fires
summary(lrg_fires$STAT_CAUSE_DESCR)
15414/190636*100
#8.08%
#report this in manuscript

keep<-lrg_fires[which(lrg_fires$STAT_CAUSE_DESCR!="Missing/Undefined"),]
190636-15414
#175222

#correct ha
keep$FIRE_SIZE_ha<-keep$FIRE_SIZE_m2*0.0001
namescheck<-unique(keep$NA_L3NAME)
namescheck

keep$NA_L3NAME <- as.character(keep$NA_L3NAME)
str(keep)
keep$NA_L3NAME <- mapply(gsub, pattern = "Chihuahuan Deserts",
                        replacement = "Chihuahuan Desert", keep$NA_L3NAME)
sum(keep$NA_L3NAME == "Chihuahuan Deserts")
#0
sum(keep$NA_L3NAME == "Chihuahuan Desert")
#529

sum(keep$NA_L3CODE == "10.2.4")
#529

hub<-keep[which(keep$STAT_CAUSE_DESCR!="Lightning"),]
#142276
lub<-keep[which(keep$STAT_CAUSE_DESCR=="Lightning"),]
#32946

#split ecoregions into east and west US
region<-as.data.frame(read.csv("data/bounds/ecoregion/east_west/arc_map_regions.csv"))

#for individual fires
lrg_fires_rr<-left_join(keep,region,by="NA_L3CODE")

eastlrg<-lrg_fires_rr[which(lrg_fires_rr$region=="east"),]
westlrg<-lrg_fires_rr[which(lrg_fires_rr$region=="west"),]

#bring in fire size data by ecoregion (made below, line 236)
fireha<-as.data.frame(read.csv("results/firehasum_ecn_top_ten_Short_update.csv"))
#fireha$ecn<-as.numeric(gsub("[.]","",fireha$NA_L3CODE))
head(fireha)
##########################################################
#manuscript figures
head(hub)
#break fuel moisture and wind speed into discrete bins
#fm vs wind speed of large human-caused fires; figure 5a in manuscript
p <- ggplot(hub, aes(fm, Wind, fill=cut(..count.., c(0,2,5,50,500,5000,20000))))+
  geom_bin2d(bins = 20)+
  scale_fill_manual("count", values = c("gray90","gray70", "gray50", "red","red2","red4"))+
  xlim(0,30)+
  ylim(0,9)+
  xlab('100-hr Fuel Moisture (%)') +
  ylab('Mean Wind Speed (m s-1)')+
  ggtitle("Large Human-caused Fires")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.text=element_text(size=12),legend.title=element_text(size=12))+
  theme(plot.title = element_text(size=20))
p

#fm vs wind speed of large lightning-caused fires; figure 5b in manuscript
p <- ggplot(lub, aes(fm, Wind, fill=cut(..count.., c(0,2,5,50,500,5000,20000))))+
  geom_bin2d(bins = 20)+
  scale_fill_manual("count", values = c("gray90","gray70", "gray50", "dodgerblue","dodgerblue2","dodgerblue4"))+
  xlim(0,30)+
  ylim(0,9)+
  xlab('100-hr Fuel Moisture (%)') +
  ylab('Mean Wind Speed (m s-1)')+
  ggtitle("Large Lightning-caused Fires")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.text=element_text(size=12),legend.title=element_text(size=12))+
  theme(plot.title = element_text(size=20))
p



###
#box and whisker plot for each variable (fuel moisture or wind speed by ignition type)
#fuel moisture; figure 5c
ggplot(keep, aes(x = IGNITION, y = fm,fill=IGNITION)) +
  geom_boxplot()+
  scale_fill_manual(values = c("red", "blue"))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  labs(y="100-hr Fuel Moisture",x="Ignition")+
  coord_flip()

#wind speed; figure 5d
ggplot(keep, aes(x = IGNITION, y = Wind,fill=IGNITION)) +
  geom_boxplot()+
  scale_fill_manual(values = c("red", "blue"))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  labs(y="Wind Speed",x="Ignition")



###
#try a discrete bin figure for each ecoregion
#human-caused large fires; this is Fig. S7a

p <- ggplot(hub, aes(fm, Wind, fill=cut(..count.., c(0,2,5,50,500,5000))))+
  geom_bin2d(bins = 20)+
  scale_fill_manual("count", values = c("gray90","gray70", "gray50", "red","red4"))+
  xlim(0,30)+
  ylim(0,9)+
  xlab('100-hr Fuel Moisture (%)') +
  ylab('Mean Wind Speed (m/s)')+
  ggtitle("Large Human-caused Fires")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  #theme(axis.text=element_text(size=12),axis.title=element_text(size=16),legend.text=element_text(size=12),legend.title=element_text(size=12))+
  #theme(plot.title = element_text(size=16))+
  facet_wrap(~NA_L3CODE)+
  #theme(panel.margin.y = unit(-1.8, "lines"))+
  theme(strip.background = element_blank())
  #theme(panel.margin.x = unit(0.2, "lines"))
p


#lightning-caused large fires; this is Fig. S7b
p <- ggplot(lub, aes(fm, Wind, fill=cut(..count.., c(0,2,5,50,500,5000))))+
  geom_bin2d(bins = 20)+
  scale_fill_manual("count", values = c("gray90","gray70", "gray50", "dodgerblue","dodgerblue4"))+
  xlim(0,30)+
  ylim(0,9)+
  xlab('100-hr Fuel Moisture (%)') +
  ylab('Mean Wind Speed (m/s)')+
  ggtitle("Large Lightning-caused Fires")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  #theme(axis.text=element_text(size=12),axis.title=element_text(size=16),legend.text=element_text(size=12),legend.title=element_text(size=12))+
  #theme(plot.title = element_text(size=16))+
  facet_wrap(~NA_L3CODE)+
  #theme(panel.margin.y = unit(-0.7, "lines"))+
  theme(strip.background = element_blank())
  #theme(panel.margin.x = unit(0.2, "lines"))
p



###
#prep for Fig. 7
#calculate mean fuel moisture vs. mean fire size by ecoregion
tt11<-summaryBy(fm~NA_L3CODE,data=keep,FUN=c(mean),na.rm=TRUE)
tt12<-summaryBy(Wind~NA_L3CODE,data=keep,FUN=c(mean),na.rm=TRUE)
head(tt11)
#join summary tables
dfa <- merge(tt11,tt12,by=c("NA_L3CODE"))
head(dfa)
#output for later use
write.table(dfa, "results/fm_ws_monthly_ecn.csv", sep=",", row.names=FALSE, append=FALSE)

#join table of fire size and fuel moisture, wind speed by ecoregion
dft<-left_join(dfa,fireha,by="NA_L3CODE")

head(dft)
colnames(dft) <- c("NA_L3CODE", "fm.mean","Wind.mean","NA_L3NAME","nobs", "ha.mean","ha.sd","ha.median","ha.sum","ha.max","NA_L1NAME")

#join tables and subset
dftt<-left_join(dft,region,by="NA_L3CODE")
east<-dftt[which(dftt$region=="east"),]
west<-dftt[which(dftt$region=="west"),]

#fuel moisture vs. fire size for east and west; Fig. 7a
p16 <- ggplot(data = dftt, aes(y = log(ha.mean), x = fm.mean,color=region)) + 
  geom_point() +
  ylab('Log (fire size (ha))') +
  xlab('100-hr fuel moisture (%)')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,20)+
  ylim(0,10)+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.text=element_text(size=18),legend.title=element_text(size=18))+
  scale_color_manual(values=c("east"="black","west"="dark gray"))+
  geom_smooth(method='lm', formula=y~x, se=FALSE)
p16


#wind speed vs. fire size for east and west; Fig. 7b
p16 <- ggplot(data = dftt, aes(y = log(ha.mean), x = Wind.mean,color=region)) + 
  geom_point() +
  ylab('Log (fire size (ha))') +
  xlab('wind speed (m/s)')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,6)+
  ylim(0,10)+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.text=element_text(size=18),legend.title=element_text(size=18))+
  scale_color_manual(values=c("east"="black","west"="dark gray"))
p16


###
#Figure 2b with lightning fires in front
ggplot(keep,aes(x=log(FIRE_SIZE_ha)+1)) + 
  geom_histogram(data=subset(keep,IGNITION == 'Human'),fill = "red",alpha=0.7)+
  geom_histogram(data=subset(keep,IGNITION == 'Lightning'),fill = "blue",alpha=0.7)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.text=element_text(size=12),legend.title=element_text(size=12))+
  theme(plot.title = element_text(size=20))+
  xlab('Log (fire size (ha))+1') +
  ylab('Number of large fires')



##########################################################
#summary stats, regression, and t-tests

#summary statistics for large fires (mean, median, min, and max fire size) by ecoregion
sum1<-summaryBy(data=keep, FIRE_SIZE_ha~NA_L3NAME, FUN=c(length,mean, sd, median,min, max))
head(sum1)
#for Table S1 in manuscript

#need level 1 name too
key11<-unique(keep[c("NA_L3NAME","NA_L1NAME","NA_L3CODE")])
key11
sum11<-left_join(sum1,key11,by=c('NA_L3NAME'))
head(sum11)
write.table(sum11, "results/firehasum_ecn_top_ten_Short_update.csv", sep=",", row.names=FALSE, append=FALSE)

#summary of mean fire size by ignition and ecoregion
#sum2<-summaryBy(data=keep, FIRE_SIZE_ha~IGNITION+NA_L3NAME, FUN=c(mean))

#tranform this to wide, then do paired t=test
#w<-reshape(sum2,timevar="ig",idvar="NA_L3NAME",v.names="ha.mean",direction="wide")

sum2a<-summaryBy(data=keep, FIRE_SIZE_ha~NA_L3CODE+IGNITION, FUN=c(length))
sum2a

sum2b<-summaryBy(data=keep, FIRE_SIZE_ha~NA_L3CODE+IGNITION, FUN=c(mean),na.rm=TRUE)
sum2b

sum2c<-summaryBy(data=keep, FIRE_SIZE_ha~NA_L3CODE+IGNITION, FUN=c(median))
sum2c

wa<-reshape(sum2a,timevar="IGNITION",idvar="NA_L3CODE",v.names="FIRE_SIZE_ha.length",direction="wide")
wb<-reshape(sum2b,timevar="IGNITION",idvar="NA_L3CODE",v.names="FIRE_SIZE_ha.mean",direction="wide")
wc<-reshape(sum2c,timevar="IGNITION",idvar="NA_L3CODE",v.names="FIRE_SIZE_ha.median",direction="wide")


output2<-merge(wa,wb,by="NA_L3CODE")
output2

output2<-merge(output2,wc,by="NA_L3CODE")
colnames(output2) <- c("NA_L3CODE", "hnobs", "lnobs","hameanh","hameanl","hamedh","hamedl")
#for figures in manuscript???

output2$totfires<-output2$hnobs+output2$lnobs
output2$perh<-output2$hnobs/output2$totfires*100
zzz<-left_join(output2,tt9,by="NA_L3CODE")
head(zzz)
#to make Figure 1
write.table(zzz, "results/firestats_ecn_top_ten_Short_update_hl.csv", sep=",", row.names=FALSE, append=FALSE)


#t-test of mean size of large human fires vs. large lightning fires
t.test(zzz$hameanh,zzz$hameanl,paired=TRUE)
#p=0.0013; lightning fires are sig larger than human fires by ecoregion

summary(zzz$hameanh)
#mean=286.486 ha
summary(zzz$hameanl)
#mean=504.877 ha




#summary stats of large fire summed burned area by ignition and ecoregion
#used to make Figure S4 with ecoregion area
sum66<-summaryBy(data=keep, FIRE_SIZE_ha~IGNITION+NA_L3CODE, FUN=c(sum))
w66<-reshape(sum66,timevar="IGNITION",idvar="NA_L3CODE",v.names="FIRE_SIZE_ha.sum",direction="wide")

#don't use the EcoArea_km2 field
#keyea<-unique(keep[c("NA_L3CODE","EcoArea_km2")])

#w66ea<-left_join(w66,keyea,by="NA_L3CODE")
#head(w66ea)

#w66ea$barat<-w66ea$FIRE_SIZE_ha.sum.Human/w66ea$EcoArea_km2
write.table(w66, "results/burned_area_hl.csv", sep=",", row.names=FALSE, append=FALSE)


###
#there are some NAs in wind and fuel moisture, have to exclude those from calculations
#stats for fuel moisture
head(hub)
mean(hub$fm, na.rm=TRUE)
#14.541
median(hub$fm, na.rm=TRUE)
#15.249
mean(lub$fm, na.rm=TRUE)
#11.437
median(lub$fm, na.rm=TRUE)
#10.726
std.error(hub$fm, na.rm=TRUE)
#0.008
std.error(lub$fm, na.rm=TRUE)
#0.021

#stats for wind speed
summary(hub$Wind)
mean(hub$Wind, na.rm=TRUE)
#4.003
median(hub$Wind, na.rm=TRUE)
#4.040
mean(lub$Wind, na.rm=TRUE)
#3.439
median(lub$Wind, na.rm=TRUE)
#3.323

std.error(hub$Wind, na.rm=TRUE)
#0.00192
std.error(lub$Wind, na.rm=TRUE)
#0.00343


#stats for standard deviation of wind speed
#not reported in manuscript
#mean(hub$stdwind_m)
#1.479
#median(hub$stdwind_m)
#1.514
#mean(lub$stdwind_m)
#1.1658
#median(lub$stdwind_m)
#1.12958

#std.error(hub$stdwind_m)
#0.000996
#std.error(lub$stdwind_m)
#0.00172


#t-test of mean fuel moisture data by ignition type
t.test(keep$fm~keep$IGNITION,var.equal = TRUE)
#p=<2.2e-16

#t-test of mean wind speed data by ignition type
t.test(keep$Wind~keep$IGNITION,var.equal = TRUE)
#p=<2.2e-16

#t-test of mean wind speed standard deviation by ignition type
#t.test(keep$stdwind_m~keep$ig,var.equal = TRUE)
#p=<2.2e-16



#summary data of fuel moisture by ignition type and ecoregion
tti1<-summaryBy(data=keep, fm~IGNITION+NA_L3CODE)

#make a dataframe of this
dfi1<-as.data.frame(tti1)

#reshape dataframe
wi1 <- reshape(dfi1, timevar = "IGNITION", idvar = "NA_L3CODE", v.names = "fm.mean", direction = "wide")
head(wi1)
#calculate the difference in fuel moisture in human vs. lightning fires by ecoregion
wi1$fmdiffmon<-wi1$fm.mean.Human-wi1$fm.mean.Lightning

#order
word<-wi1[with(wi1, order(-fmdiffmon)), ]

#t-test of the difference fuel moisture in human vs. lightning fires
t.test(wi1$fm.mean.Human,wi1$fm.mean.Lightning,paired=TRUE)
#p=4.25e-07


###
#regression

#fire size vs. fuel moisture
lm1<-lm(log(ha.mean)~fm.mean, data=dft)
summary(lm1)
#p=3.28e-11
#y=-0.47177x+12.8953
#is significantly (-) related

#fire size vs. wind speed
lm2<-lm(log(ha.mean)~Wind.mean, data=dft)
summary(lm2)
#p=3.73e-05
#y=-1.8437x+14.0533
#is significantly (-) related

#fire size vs. fuel moisture; eastern ecoregions
lm3<-lm(log(ha.mean)~fm.mean, data=east)
summary(lm3)
#p=0.487; 

#fire size vs. fuel moisture; western ecoregions
lm4<-lm(log(ha.mean)~fm.mean, data=west)
summary(lm4)
#p=0.00546; (-)

#fire size vs. wind speed; eastern ecoregions
lm5<-lm(log(ha.mean)~Wind.mean, data=east)
summary(lm5)
#p=0.913

#fire size vs. wind speed; western ecoregions
lm6<-lm(log(ha.mean)~Wind.mean, data=west)
summary(lm6)
#p=0.6035


###
#to calculate the difference in mean fuel moisture: human vs. lightning; used to make Figure 6a
r22 <- summaryBy(fm~IGNITION+NA_L3CODE, data=keep, FUN=mean, na.rm=TRUE)
wfm<-cast(r22, NA_L3CODE ~ IGNITION, value = 'fm.mean')
head(wfm)
wfm$diff_fm10<-wfm$Human-wfm$Lightning
#note, where positive human > lightning

keyzzz<-unique(keep[c("NA_L3CODE","NA_L3NAME")])
keyzzz
wfmzzz<-left_join(wfm,keyzzz,by=c('NA_L3CODE'))
#head(joinz)

write.table(wfmzzz, "results/diff_fm_NA_L3CODE_10_monthly.csv", sep=",", row.names=FALSE, append=FALSE)
#this data was added to shapefile in Arc


#to calculate the difference in mean wind speed: human vs. lightning; used to make Figure 6b
r44 <- summaryBy(Wind~IGNITION+NA_L3CODE, data=keep, FUN=mean, na.rm=TRUE)
wws<-cast(r44, NA_L3CODE ~ IGNITION, value = 'Wind.mean')
wws$diff_windspeed<-wws$Human-wws$Lightning

wwszzz<-left_join(wws,keyzzz,by=c('NA_L3CODE'))
#head(joinz)

write.table(wwszzz, "results/diff_wind_NA_L3CODE_10_monthly.csv", sep=",", row.names=FALSE, append=FALSE)
#this data was added to shapefile in Arc


#to calculate the difference in the standard deviation of wind speed: human vs. lightning
#standard deviation of wind speed; not reported in manuscript
#r4 <- summaryBy(stdwind_m~ig+NA_L3CODE, data=keep, FUN=mean)
#wwssd<-cast(r4, NA_L3CODE ~ ig, value = 'stdwind_m.mean')
#wwssd$diff_sdwindspeed<-w$human-w$lightning
#write.table(wwssd, "/Users/rana7082/Dropbox/ecoregions/derived/diff_sdwind_NA_L3CODE_10_monthly.csv", sep=",", row.names=FALSE, append=FALSE)


###
#summary of fire size by ecoregion and ignition
#sum66<-as.data.frame(summaryBy(FIRE_SIZE_ha~NA_L3CODE+IGNITION,data=keep, FUN=mean))
#wid<-cast(sum66, NA_L3CODE ~ IGNITION, value = 'FIRE_SIZE_ha.mean')
#write.table(wid, "results/fireha_10_hl.csv", sep=",", row.names=FALSE, append=FALSE)

#summary of number fire events by ecoregion and ignition
#sum77<-as.data.frame(summaryBy(clean_id~NA_L3CODE+IGNITION,data=keep,FUN=length))
#wider<-cast(sum77, NA_L3CODE ~ IGNITION, value = 'clean_id.length')
#head(wider)
#write.table(wider, "results/nobs_10_hl.csv", sep=",", row.names=FALSE, append=FALSE)

#summary of fire size by ecoregion only
#uuu<-as.data.frame(summaryBy(ha~NA_L3CODE,data=keep, FUN=mean))
#write.table(uuu, "/Users/rana7082/Dropbox/ecoregions/derived/fireha_10.csv", sep=",", row.names=FALSE, append=FALSE)


###
#stats with median
#tti2<-summaryBy(data=keep, ha~ig+NA_L3CODE, FUN=median)
#df2<-as.data.frame(tti2)
#w2 <- reshape(df2, timevar = "ig", idvar = "NA_L3CODE", v.names = "ha.median", direction = "wide")
#write.table(w2, "C:/Users/rnagy/Dropbox/ecoregions/derived/firehamed_10hl.csv", sep=",", row.names=FALSE, append=FALSE)






