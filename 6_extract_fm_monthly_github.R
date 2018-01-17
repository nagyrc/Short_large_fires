#this code is part of the Short_large_fires project by Dr. R. Chelsea Nagy
#read Bethany's csv of Short data (large fires only) + average monthly fuel moisture from 1992-2013 by fire event

library(sp)
library(rgdal)
library(dplyr)
library(doBy)
library(reshape)
library(ggplot2)
library(foreign)
library(stats)
library(plotrix)

#bring in Short data with fm and ws extracted
#note, this has large fires only (top 10%)
lrg_fires<- read.csv("data/merged/lrg_fires.csv")
head(read)

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

#bring in fire size data by ecoregion (made in Script 7)
fireha<-as.data.frame(read.csv("results/firehasum_ecn_top_ten_Short_update.csv"))
#fireha$ecn<-as.numeric(gsub("[.]","",fireha$NA_L3CODE))

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
tt11<-summaryBy(fm~NA_L3CODE,data=keep,FUN=c(mean))
tt12<-summaryBy(Wind~NA_L3CODE,data=keep,FUN=c(mean))

#join summary tables
dfa <- merge(tt11,tt12,by=c("NA_L3CODE"))
head(dfa)
#output for later use
write.table(dfa, "results/fm_ws_monthly_ecn.csv", sep=",", row.names=FALSE, append=FALSE)

#join table of fire size and fuel moisture, wind speed by ecoregion
dft<-left_join(dfa,fireha,by="NA_L3CODE")

head(dft)
colnames(dft) <- c("NA_L3CODE", "fm.mean","Wind.mean","ecn","nobs", "ha.mean","ha.sd","ha.median","ha.sum")

#join tables and subset
dftt<-left_join(dft,region,by="NA_L3CODE")
east<-dftt[which(dftt$region=="east"),]
west<-dftt[which(dftt$region=="west"),]

#fuel moisture vs. fire size for east and west; Fig. 7a
p16 <- ggplot(data = dftt, aes(y = log(ha.mean), x = fm.mean,color=region)) + 
  geom_point() +
  ylab('log (Fire Size (ha))') +
  xlab('100-hr Fuel Moisture (%)')+
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
  ylab('log (Fire Size (ha))') +
  xlab('Wind Speed (m/s)')+
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
  xlab('log (Fire Size (ha))+1') +
  ylab('Number of Large Fires')



##########################################################
#summary stats, regression, and t-tests

#stopped here
#summary statistics for large fires (mean, median, min, and max fire size) by ecoregion
sum1<-summaryBy(data=keep, FIRE_SIZE_ha~NA_L3NAME, FUN=c(length,mean, sd, median,min, max))

#for Table S1 in manuscript
write.table(sum1, "results/firehasum_ecn_top_ten_Short_update.csv", sep=",", row.names=FALSE, append=FALSE)

#summary of mean fire size by ignition and ecoregion
#sum2<-summaryBy(data=keep, FIRE_SIZE_ha~IGNITION+NA_L3NAME, FUN=c(mean))

#tranform this to wide, then do paired t=test
#w<-reshape(sum2,timevar="ig",idvar="NA_L3NAME",v.names="ha.mean",direction="wide")

sum2a<-summaryBy(data=keep, FIRE_SIZE_ha~NA_L3CODE+IGNITION, FUN=c(length))
sum2a

sum2b<-summaryBy(data=keep, FIRE_SIZE_ha~NA_L3CODE+IGNITION, FUN=c(mean))
sum2b

wa<-reshape(sum2a,timevar="IGNITION",idvar="NA_L3CODE",v.names="FIRE_SIZE_ha.length",direction="wide")
wb<-reshape(sum2b,timevar="IGNITION",idvar="NA_L3CODE",v.names="FIRE_SIZE_ha.mean",direction="wide")

output2<-merge(wa,wb,by="NA_L3CODE")
output2

colnames(output2) <- c("NA_L3CODE", "hnobs", "lnobs","hhamean","lhamean")
#for figures in manuscript???

output2$totfires<-output2$hnobs+output2$lnobs
output2$perh<-output2$hnobs/output2$totfires*100
zzz<-left_join(output2,tt9,by="NA_L3CODE")
head(zzz)
#to make Figure 1
write.table(zzz, "results/firestats_ecn_top_ten_Short_update_hl.csv", sep=",", row.names=FALSE, append=FALSE)


#t-test of mean size of large human fires vs. large lightning fires
t.test(zzz$hhamean,zzz$lhamean,paired=TRUE)
#p=0.0013; lightning fires are sig larger than human fires by ecoregion

summary(zzz$hhamean)
#mean=286.486 ha
summary(zzz$lhamean)
#mean=504.877 ha




#summary stats of large fire summed burned area by ignition and ecoregion
#used to make Fig. S4 with ecoregion area
sum66<-summaryBy(data=keep, FIRE_SIZE_ha~IGNITION+NA_L3CODE, FUN=c(sum))
w66<-reshape(sum66,timevar="IGNITION",idvar="NA_L3CODE",v.names="FIRE_SIZE_ha.sum",direction="wide")
write.table(w66, "results/burned_area_hl.csv", sep=",", row.names=FALSE, append=FALSE)



###
#stats for fuel moisture
head(hub)
mean(hub$fm)
#14.56
median(hub$fm)
#15.21
mean(lub$fm)
#11.45
median(lub$fm)
#10.76
std.error(hub$fm)
#0.008
std.error(lub$fm)
#0.022

#stopped here Wed afternoon...these means are not working.
#stats for wind speed
mean(hub$Wind)
#3.96
median(hub$Wind)
#3.999
mean(lub$Wind)
#3.418
median(lub$Wind)
#3.306

std.error(hub$Wind)
#0.00198
std.error(lub$Wind)
#0.003566


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
t.test(keep$fm100_m~keep$ig,var.equal = TRUE)
#p=<2.2e-16

#t-test of mean wind speed data by ignition type
t.test(keep$mnwind_m~keep$ig,var.equal = TRUE)
#p=<2.2e-16

#t-test of mean wind speed standard deviation by ignition type
t.test(keep$stdwind_m~keep$ig,var.equal = TRUE)
#p=<2.2e-16



#summary data of fuel moisture by ignition type and ecoregion
tti1<-summaryBy(data=keep, fm100_m~ig+NA_L3CODE)

#make a dataframe of this
df1<-as.data.frame(tti1)

#reshape dataframe
w <- reshape(df1, timevar = "ig", idvar = "NA_L3CODE", v.names = "fm100_m.mean", direction = "wide")

#calculate the difference in fuel moisture in human vs. lightning fires by ecoregion
w$fmdiffmon<-w$fm100_m.mean.human-w$fm100_m.mean.lightning

#order
word<-w[with(w, order(-fmdiffmon)), ]

#t-test of the difference fuel moisture in human vs. lightning fires
t.test(w$fm100_m.mean.human,w$fm100_m.mean.lightning,paired=TRUE)
#p=7.039e-06


###
#regression

#fire size vs. fuel moisture
lm1<-lm(log(ha.mean)~fm100_m.mean, data=dft)
summary(lm1)
#p=3.22e-09
#y=-0.355x+9.4499
#is significantly (-) related

#fire size vs. wind speed
lm1<-lm(log(ha.mean)~mnwind_m.mean, data=dft)
summary(lm1)
#p=9.99e-05
#y=-1.465x+10.4702
#is significantly (-) related

#fire size vs. fuel moisture; eastern ecoregions
lm1<-lm(log(ha.mean)~fm100_m.mean, data=east)
summary(lm1)
#p=0.05; (+)

#fire size vs. fuel moisture; western ecoregions
lm2<-lm(log(ha.mean)~fm100_m.mean, data=west)
summary(lm2)
#p=0.0005; (-)

#fire size vs. wind speed; eastern ecoregions
lm3<-lm(log(ha.mean)~mnwind_m.mean, data=east)
summary(lm3)
#p=0.17

#fire size vs. wind speed; western ecoregions
lm4<-lm(log(ha.mean)~mnwind_m.mean, data=west)
summary(lm4)
#p=0.93


###
#to calculate the difference in mean fuel moisture: human vs. lightning; used to make Figure 6a
r2 <- summaryBy(fm100_m~ig+NA_L3CODE, data=keep, FUN=mean)
wfm<-cast(r2, NA_L3CODE ~ ig, value = 'fm100_m.mean')
wfm$diff_fm10<-w$human-w$lightning
#note, where positive human > lightning
write.table(wfm, "/Users/rana7082/Dropbox/ecoregions/derived/diff_fm_NA_L3CODE_10_monthly.csv", sep=",", row.names=FALSE, append=FALSE)
#this data was added to shapefile in Arc


#to calculate the difference in mean wind speed: human vs. lightning; used to make Figure 6b
r3 <- summaryBy(mnwind_m~ig+NA_L3CODE, data=keep, FUN=mean)
wws<-cast(r3, NA_L3CODE ~ ig, value = 'mnwind_m.mean')
wws$diff_windspeed<-w$human-w$lightning
write.table(wws, "/Users/rana7082/Dropbox/ecoregions/derived/diff_wind_NA_L3CODE_10_monthly.csv", sep=",", row.names=FALSE, append=FALSE)
#this data was added to shapefile in Arc


#to calculate the difference in the standard deviation of wind speed: human vs. lightning
#standard deviation of wind speed; not reported in manuscript
r4 <- summaryBy(stdwind_m~ig+NA_L3CODE, data=keep, FUN=mean)
wwssd<-cast(r4, NA_L3CODE ~ ig, value = 'stdwind_m.mean')
wwssd$diff_sdwindspeed<-w$human-w$lightning
write.table(wwssd, "/Users/rana7082/Dropbox/ecoregions/derived/diff_sdwind_NA_L3CODE_10_monthly.csv", sep=",", row.names=FALSE, append=FALSE)


###
#summary of fire size by ecoregion and ignition
lll<-as.data.frame(summaryBy(ha~NA_L3CODE+ig,data=keep, FUN=mean))
wid<-cast(lll, NA_L3CODE ~ ig, value = 'ha.mean')
write.table(wid, "/Users/rana7082/Dropbox/ecoregions/derived/fireha_10_hl.csv", sep=",", row.names=FALSE, append=FALSE)

#summary of number fire events by ecoregion and ignition
hhh<-as.data.frame(summaryBy(FOD_ID~NA_L3CODE+ig,data=keep,FUN=length))
wider<-cast(hhh, NA_L3CODE ~ ig, value = 'FOD_ID.length')
head(wider)
write.table(wider, "/Users/rana7082/Dropbox/ecoregions/derived/nobs_10_hl.csv", sep=",", row.names=FALSE, append=FALSE)

#summary of fire size by ecoregion only
uuu<-as.data.frame(summaryBy(ha~NA_L3CODE,data=keep, FUN=mean))
write.table(uuu, "/Users/rana7082/Dropbox/ecoregions/derived/fireha_10.csv", sep=",", row.names=FALSE, append=FALSE)


###
#stats with median
tti2<-summaryBy(data=keep, ha~ig+NA_L3CODE, FUN=median)
df2<-as.data.frame(tti2)
w2 <- reshape(df2, timevar = "ig", idvar = "NA_L3CODE", v.names = "ha.median", direction = "wide")
write.table(w2, "C:/Users/rnagy/Dropbox/ecoregions/derived/firehamed_10hl.csv", sep=",", row.names=FALSE, append=FALSE)








##########################################################
#plotting the data for visualization only
#not presented in manuscript

#plot the conditions where large human and large lightning fires exist
#xy scatterplots of wind speed vs. fm for large fires
#human only
p16 <- ggplot() + 
  geom_point(data = hub, color='red', aes(x = fm100_m, y = mnwind_m)) +
  xlab('100 hr fuel moisture') +
  ylab('wind speed')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,30)+
  ylim(0,9)+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.text=element_text(size=18),legend.title=element_text(size=18))
p16

#lightning only
p16 <- ggplot() + 
  geom_point(data = lub, color='blue', aes(x = fm100_m, y = mnwind_m)) +
  xlab('100 hr fuel moisture') +
  ylab('wind speed')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,30)+
  ylim(0,9)+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.text=element_text(size=18),legend.title=element_text(size=18))
p16

#combined human and lightning
p16 <- ggplot() + 
  geom_point(data = hub, alpha=0.5, color='red',aes(x = fm100_m, y = mnwind_m)) +
  geom_point(data = lub, alpha=0.5, color='blue',aes(x = fm100_m, y = mnwind_m)) +
  xlab('100 hr fuel moisture') +
  ylab('wind speed')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,30)+
  ylim(0,9)+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.text=element_text(size=18),legend.title=element_text(size=18))
p16



###
#fuel moisture vs. standard deviation of wind speed, human fires
p <- ggplot(hub, aes(fm100_m, stdwind_m, fill=cut(..count.., c(0,2,5,50,500,5000,20000))))+
  geom_bin2d(bins = 20)+
  scale_fill_manual("count", values = c("gray90","gray70", "gray50", "red","red2","red4"))+
  xlim(0,30)+
  ylim(0,5)+
  xlab('100 hr fuel moisture (%)') +
  ylab('std wind speed (m/s)')+
  ggtitle("large human-caused fires")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.text=element_text(size=12),legend.title=element_text(size=12))+
  theme(plot.title = element_text(size=20))
p


#fuel moisture vs. standard deviation of wind speed, lightning fires
p <- ggplot(lub, aes(fm100_m, stdwind_m, fill=cut(..count.., c(0,2,5,50,500,5000,20000))))+
  geom_bin2d(bins = 20)+
  scale_fill_manual("count", values = c("gray90","gray70", "gray50", "dodgerblue","dodgerblue2","dodgerblue4"))+
  xlim(0,30)+
  ylim(0,5)+
  xlab('100 hr fuel moisture (%)') +
  ylab('std wind speed (m/s)')+
  ggtitle("large lightning-caused fires")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.text=element_text(size=12),legend.title=element_text(size=12))+
  theme(plot.title = element_text(size=20))
p


#wind speed vs. wind variability
p <- ggplot(hub, aes(mnwind_m, stdwind_m, fill=cut(..count.., c(0,2,5,50,500,5000,20000))))+
  geom_bin2d(bins = 20)+
  scale_fill_manual("count", values = c("gray90","gray70", "gray50", "red","red2","red4"))+
  xlim(0,9)+
  ylim(0,5)+
  xlab('mean wind speed ()') +
  ylab('std wind speed ()')+
  ggtitle("large human-caused fires")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.text=element_text(size=12),legend.title=element_text(size=12))+
  theme(plot.title = element_text(size=20))
p


p <- ggplot(lub, aes(mnwind_m, stdwind_m, fill=cut(..count.., c(0,2,5,50,500,5000,20000))))+
  geom_bin2d(bins = 20)+
  scale_fill_manual("count", values = c("gray90","gray70", "gray50", "dodgerblue","dodgerblue2","dodgerblue4"))+
  xlim(0,9)+
  ylim(0,5)+
  xlab('mean wind speed ()') +
  ylab('std wind speed ()')+
  ggtitle("large lightning-caused fires")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.text=element_text(size=12),legend.title=element_text(size=12))+
  theme(plot.title = element_text(size=20))
p


###
#regression of fire size vs. fuel moisture
p16 <- ggplot(data = dft, aes(y = log(ha.mean), x = fm100_m.mean)) + 
  geom_point() +
  ylab('log (fire size (ha))') +
  xlab('100-hr fuel moisture (%)')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,20)+
  ylim(0,10)+
  #scale_colour_gradient(name="% human fires", low = "blue", high = "red", guide = "colourbar")+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.text=element_text(size=18),legend.title=element_text(size=18))+
  geom_smooth(method='lm', formula=y~x, se=FALSE)
p16


#regression of fire size vs. wind speed
p16 <- ggplot(data = dft, aes(y = log(ha.mean), x = mnwind_m.mean)) + 
  geom_point() +
  ylab(' log(fire size (ha))') +
  xlab('wind speed (m/s)')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,6)+
  ylim(0,10)+
  #scale_colour_gradient(name="% human fires", low = "blue", high = "red", guide = "colourbar")+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.text=element_text(size=18),legend.title=element_text(size=18))+
  geom_smooth(method='lm', formula=y~x, se=FALSE)
p16


###
#testy...I'm not sure that this works
p16<-ggplot()+
  geom_point(data=keep, aes(x=fm100_m,y=mnwind_m,color=ig),alpha=0.05)+
  xlab('100 hr fuel moisture (%)') +
  ylab('wind speed (m/s)')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,30)+
  ylim(0,9)+
  #theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.text=element_text(size=18),legend.title=element_text(size=18))+
  ggtitle("ecoregion conditions of large fires")+
  theme(plot.title = element_text(size=12))+
  scale_color_manual(values=c("red","blue"))+
  facet_wrap(~NA_L3CODE)+
  theme(strip.background = element_blank())
  #theme(panel.spacing = unit(0, "lines"))
p16


####
#histograms of fire size vs. ignition
ggplot(keep,aes(log(x=ha)+1))+
  geom_histogram()+
  facet_grid(~ig)+
  theme_bw()


ggplot(aes(log(x=ha)+1))+
  geom_histogram(data=hub)+
  geom_histogram(data=lub)+
  theme_bw()


###
#regression with ind. fires rather than ecoregion totals
#fire size vs. fuel moisture
p16 <- ggplot(data = dftt, aes(y = log(ha), x = fm100_m,color=region)) + 
  geom_point() +
  ylab('log (fire size (ha))') +
  xlab('100-hr fuel moisture (%)')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,40)+
  ylim(0,20)+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.text=element_text(size=18),legend.title=element_text(size=18))+
  scale_color_manual(values=c("east"="black","west"="dark gray"))+
  geom_smooth(method='lm', formula=y~x, se=FALSE)
p16


#fire size vs. wind speed
p16 <- ggplot(data = dftt, aes(y = log(ha), x = mnwind_m,color=region)) + 
  geom_point() +
  ylab('log (fire size (ha))') +
  xlab('wind speed (m/s)')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,12)+
  ylim(0,20)+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.text=element_text(size=18),legend.title=element_text(size=18))+
  scale_color_manual(values=c("east"="black","west"="dark gray"))
geom_smooth(method='lm', formula=y~x, se=FALSE)
p16


