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
#note, this has large fires only
read<- read.csv("data/merged/Short_large10_FM_Wind.csv")

#the format of NA_L3CODE is formatted incorrectly, so remove that field
read$NA_L3CODE<-NULL
head(read)

#bring in something else that can make key from
readz<- read.csv("data/fire/Short_large10.csv")

key<-unique(readz[c("NA_L3CODE","NA_L3NAME")])

joinz<-left_join(read,key,by=c('NA_L3NAME'))
head(joinz)
#this now has a correct field for NA_L3CODE

tt3<-unique(joinz$NA_L3CODE)

#subset just large human or just large lightning fires
hub<-joinz[which(joinz$STAT_CAU_1!="Lightning"),]
lub<-joinz[which(joinz$STAT_CAU_1=="Lightning"),]


#split into east and west US
region<-as.data.frame(read.csv("data/bounds/ecoregion/east_west/arc_map_regions.csv"))

#for ecoregions
dftt<-left_join(dft,region,by="NA_L3CODE")

east<-dftt[which(dftt$region=="east"),]
west<-dftt[which(dftt$region=="west"),]

#for individual fires
dfttall<-left_join(joinz,region,by="NA_L3CODE")

east<-dfttall[which(dfttall$region=="east"),]
west<-dfttall[which(dfttall$region=="west"),]

#bring in fire size data by ecoregion (made in Script XXXXX)
fireha<-as.data.frame(read.csv("data/fire/fireha_10.csv"))
fireha$ecn<-as.numeric(gsub("[.]","",fireha$NA_L3CODE))

##########################################################
#manuscript figures
#box and whisker plot for each variable (fuel moisture or wind speed by ignition type)
#fuel moisture; figure 5c
ggplot(joinz, aes(x = ig, y = fm100_m,fill=ig)) +
  geom_boxplot()+
  scale_fill_manual(values = c("red", "blue"))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  labs(y="100 hr fuel moisture",x="ignition")+
  coord_flip()

#wind speed; figure 5d
ggplot(joinz, aes(x = ig, y = mnwind_m,fill=ig)) +
  geom_boxplot()+
  scale_fill_manual(values = c("red", "blue"))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  labs(y="wind speed",x="ignition")



###
#break fuel moisture and wind speed into discrete bins
#fm vs wind speed of large human-caused fires; figure 5a in manuscript
p <- ggplot(hub, aes(fm100_m, mnwind_m, fill=cut(..count.., c(0,2,5,50,500,5000,20000))))+
  geom_bin2d(bins = 20)+
  scale_fill_manual("count", values = c("gray90","gray70", "gray50", "red","red2","red4"))+
  xlim(0,30)+
  ylim(0,9)+
  xlab('100 hr fuel moisture (%)') +
  ylab('mean wind speed (m s-1)')+
  ggtitle("large human-caused fires")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.text=element_text(size=12),legend.title=element_text(size=12))+
  theme(plot.title = element_text(size=20))
p

#fm vs wind speed of large lightning-caused fires; figure 5b in manuscript
p <- ggplot(lub, aes(fm100_m, mnwind_m, fill=cut(..count.., c(0,2,5,50,500,5000,20000))))+
  geom_bin2d(bins = 20)+
  scale_fill_manual("count", values = c("gray90","gray70", "gray50", "dodgerblue","dodgerblue2","dodgerblue4"))+
  xlim(0,30)+
  ylim(0,9)+
  xlab('100 hr fuel moisture (%)') +
  ylab('mean wind speed (m s-1)')+
  ggtitle("large lightning-caused fires")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.text=element_text(size=12),legend.title=element_text(size=12))+
  theme(plot.title = element_text(size=20))
p

###
#try a discrete bin figure for each ecoregion
#human-caused large fires; this is Fig. S7a

p <- ggplot(hub2, aes(fm100_m, mnwind_m, fill=cut(..count.., c(0,2,5,50,500,5000))))+
  geom_bin2d(bins = 20)+
  scale_fill_manual("count", values = c("gray90","gray70", "gray50", "red","red4"))+
  xlim(0,30)+
  ylim(0,9)+
  xlab('100 hr fuel moisture (%)') +
  ylab('mean wind speed (m/s)')+
  ggtitle("large human-caused fires")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  #theme(axis.text=element_text(size=12),axis.title=element_text(size=16),legend.text=element_text(size=12),legend.title=element_text(size=12))+
  #theme(plot.title = element_text(size=16))+
  facet_wrap(~NA_L3CODE)+
  theme(panel.margin.y = unit(-1.8, "lines"))+
  theme(strip.background = element_blank())+
  theme(panel.margin.x = unit(0.2, "lines"))
p


#lightning-caused large fires; this is Fig. S7b
p <- ggplot(lub, aes(fm100_m, mnwind_m, fill=cut(..count.., c(0,2,5,50,500,5000))))+
  geom_bin2d(bins = 20)+
  scale_fill_manual("count", values = c("gray90","gray70", "gray50", "dodgerblue","dodgerblue4"))+
  xlim(0,30)+
  ylim(0,9)+
  xlab('100 hr fuel moisture (%)') +
  ylab('mean wind speed (m/s)')+
  ggtitle("large lightning-caused fires")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  #theme(axis.text=element_text(size=12),axis.title=element_text(size=16),legend.text=element_text(size=12),legend.title=element_text(size=12))+
  #theme(plot.title = element_text(size=16))+
  facet_wrap(~NA_L3CODE)+
  theme(panel.margin.y = unit(-0.7, "lines"))+
  theme(strip.background = element_blank())+
  theme(panel.margin.x = unit(0.2, "lines"))
p


###
#fuel moisture vs. fire size for east and west; Fig. 7a
p16 <- ggplot(data = dftt, aes(y = log(ha.mean), x = fm100_m.mean,color=region)) + 
  geom_point() +
  ylab('log (fire size (ha))') +
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
p16 <- ggplot(data = dftt, aes(y = log(ha.mean), x = mnwind_m.mean,color=region)) + 
  geom_point() +
  ylab('log (fire size (ha))') +
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
ggplot(joinz,aes(x=log(ha)+1)) + 
  geom_histogram(data=subset(joinz,ig == 'human'),fill = "red",alpha=0.7)+
  geom_histogram(data=subset(joinz,ig == 'lightning'),fill = "blue",alpha=0.7)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.text=element_text(size=12),legend.title=element_text(size=12))+
  theme(plot.title = element_text(size=20))+
  xlab('log (fire size (ha))+1') +
  ylab('number of large fires')



##########################################################
#summary stats, regression, and t-tests

#summary statistics for large fires (mean, median, min, and max fire size) by ecoregion
sum1<-summaryBy(data=read, ha~NA_L3NAME, FUN=c(length,mean, median,min, max))

#for Table S1
write.table(sum1, "C:/Users/rnagy/Dropbox/ecoregions/derived/fireha_ecn_stats.csv", sep=",", row.names=FALSE, append=FALSE)

#summary of mean fire size by ignition and ecoregion
sum2<-summaryBy(data=read, ha~ig+NA_L3NAME, FUN=c(mean))

#tranform this to wide, then do paired t=test
w<-reshape(sum2,timevar="ig",idvar="NA_L3NAME",v.names="ha.mean",direction="wide")

#t-test of mean size of large human fires vs. large lightning fires
t.test(w$ha.mean.human,w$ha.mean.lightning,paired=TRUE)
#p=0.002; lightning fires are sig larger than human fires by ecoregion

summary(w$ha.mean.human)
#mean=285.300 ha
summary(w$ha.mean.lightning)
#mean=488.400 ha

#how many large fires of lightning and human igition?
summary(read$ig)
#human=129966, light=30640
129966+30640
#160606 total large fires



#summary stats of large fire summed burned area by ignition and ecoregion
sum66<-summaryBy(data=joinz, ha~ig+NA_L3CODE, FUN=c(sum))

w<-reshape(sum66,timevar="ig",idvar="NA_L3CODE",v.names="ha.sum",direction="wide")

write.table(w, "C:/Users/rnagy/Dropbox/ecoregions/derived/burned_area_hl.csv", sep=",", row.names=FALSE, append=FALSE)



###
mean(hub$fm100_m)
#14.56
median(hub$fm100_m)
#15.21
mean(lub$fm100_m)
#11.45
median(lub$fm100_m)
#10.76
std.error(hub$fm100_m)
#0.008
std.error(lub$fm100_m)
#0.022

mean(hub$mnwind_m)
#3.96
median(hub$mnwind_m)
#3.999
mean(lub$mnwind_m)
#3.418
median(lub$mnwind_m)
#3.306

std.error(hub$mnwind_m)
#0.00198
std.error(lub$mnwind_m)
#0.003566




mean(hub$stdwind_m)
#1.479
median(hub$stdwind_m)
#1.514
mean(lub$stdwind_m)
#1.1658
median(lub$stdwind_m)
#1.12958

std.error(hub$stdwind_m)
#0.000996
std.error(lub$stdwind_m)
#0.00172


#t-test of fuel moisture data by ignition type
t.test(joinz$fm100_m~joinz$ig,var.equal = TRUE)
#p=<2.2e-16

#t-test of wind speed data by ignition type
t.test(joinz$mnwind_m~joinz$ig,var.equal = TRUE)
#p=<2.2e-16

#t-test of wind speed standard deviation by ignition type
t.test(joinz$stdwind_m~joinz$ig,var.equal = TRUE)
#p=<2.2e-16

#summary data of fuel moisture by ignition type and ecoregion
tti1<-summaryBy(data=joinz, fm100_m~ig+NA_L3CODE)

#make a dataframe of this
df1<-as.data.frame(tti1)

#reshape dataframe
w <- reshape(df1, timevar = "ig", idvar = "NA_L3CODE", v.names = "fm100_m.mean", direction = "wide")

#calculate the difference in fuel moisture in human vs. lightning fires by ecoregion
w$fmdiffmon<-w$fm100_m.mean.human-w$fm100_m.mean.lightning

word<-w[with(w, order(-fmdiffmon)), ]


#t-test of the difference fuel moisture in human vs. lightning fires
t.test(w$fm100_m.mean.human,w$fm100_m.mean.lightning,paired=TRUE)
#p=7.039e-06


###
#regression
#calculate mean fuel moisture vs. mean fire size by ecoregion
tt1<-summaryBy(fm100_m~NA_L3CODE,data=joinz,FUN=c(mean))
tt2<-summaryBy(mnwind_m~NA_L3CODE,data=joinz,FUN=c(mean))

#join summary tables
dfa <- merge(tt1,tt2,by=c("NA_L3CODE"))

#output for later use
write.table(dfa, "C:/Users/rnagy/Dropbox/ecoregions/derived/fm_ws_monthly_ecn.csv", sep=",", row.names=FALSE, append=FALSE)


#join table of fire size and fuel moisture, wind speed by ecoregion
dft<-left_join(dfa,fireha,by="NA_L3CODE")


lm1<-lm(log(ha.mean)~fm100_m.mean, data=dft)
summary(lm1)
#p=3.22e-09
#y=-0.355x+9.4499
#is significantly (-) related

lm1<-lm(log(ha.mean)~mnwind_m.mean, data=dft)
summary(lm1)
#p=9.99e-05
#y=-1.465x+10.4702
#is significantly (-) related

lm1<-lm(log(ha.mean)~fm100_m.mean, data=east)
summary(lm1)
#p=0.05; (+)

lm2<-lm(log(ha.mean)~fm100_m.mean, data=west)
summary(lm2)
#p=0.0005; (-)


lm3<-lm(log(ha.mean)~mnwind_m.mean, data=east)
summary(lm3)
#p=0.17
lm4<-lm(log(ha.mean)~mnwind_m.mean, data=west)
summary(lm4)
#p=0.93


#fuel moisture, large fires only
r2 <- summaryBy(fm100_m~ig+NA_L3CODE, data=joinz, FUN=mean)

w<-cast(r2, NA_L3CODE ~ ig, value = 'fm100_m.mean')

w$diff_fm10<-w$human-w$lightning
#where positive human > lightning

#export table to join in Arc
write.table(w, "/Users/rana7082/Dropbox/ecoregions/derived/diff_fm_NA_L3CODE_10_monthly.csv", sep=",", row.names=FALSE, append=FALSE)


#mean wind speed, large fires only
r3 <- summaryBy(mnwind_m~ig+NA_L3CODE, data=joinz, FUN=mean)

w<-cast(r3, NA_L3CODE ~ ig, value = 'mnwind_m.mean')

w$diff_windspeed<-w$human-w$lightning

write.table(w, "/Users/rana7082/Dropbox/ecoregions/derived/diff_wind_NA_L3CODE_10_monthly.csv", sep=",", row.names=FALSE, append=FALSE)



#std wind speed, large fires only
r4 <- summaryBy(stdwind_m~ig+NA_L3CODE, data=joinz, FUN=mean)

w<-cast(r4, NA_L3CODE ~ ig, value = 'stdwind_m.mean')

w$diff_sdwindspeed<-w$human-w$lightning

write.table(w, "/Users/rana7082/Dropbox/ecoregions/derived/diff_sdwind_NA_L3CODE_10_monthly.csv", sep=",", row.names=FALSE, append=FALSE)



lll<-as.data.frame(summaryBy(ha~NA_L3CODE+ig,data=joinz, FUN=mean))

wid<-cast(lll, NA_L3CODE ~ ig, value = 'ha.mean')

write.table(wid, "/Users/rana7082/Dropbox/ecoregions/derived/fireha_10_hl.csv", sep=",", row.names=FALSE, append=FALSE)


hhh<-as.data.frame(summaryBy(FOD_ID~NA_L3CODE+ig,data=joinz,FUN=length))

wider<-cast(hhh, NA_L3CODE ~ ig, value = 'FOD_ID.length')

write.table(wider, "/Users/rana7082/Dropbox/ecoregions/derived/nobs_10_hl.csv", sep=",", row.names=FALSE, append=FALSE)



uuu<-as.data.frame(summaryBy(ha~NA_L3CODE,data=joinz, FUN=mean))

write.table(uuu, "/Users/rana7082/Dropbox/ecoregions/derived/fireha_10.csv", sep=",", row.names=FALSE, append=FALSE)



#stats with median
tti2<-summaryBy(data=joinz, ha~ig+NA_L3CODE, FUN=median)

df2<-as.data.frame(tti2)

#w <- reshape(l.sort, timevar = "subj", idvar = c("id", "female", "race", "ses", "schtyp", "prog"), direction = "wide")
w2 <- reshape(df2, timevar = "ig", idvar = "NA_L3CODE", v.names = "ha.median", direction = "wide")

write.table(w2, "C:/Users/rnagy/Dropbox/ecoregions/derived/firehamed_10hl.csv", sep=",", row.names=FALSE, append=FALSE)


tti3<-summaryBy(data=joinz, ha~NA_L3CODE, FUN=median)
df3<-as.data.frame(tti3)

#for Table S1
write.table(df3, "C:/Users/rnagy/Dropbox/ecoregions/derived/firehamed_10.csv", sep=",", row.names=FALSE, append=FALSE)










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
  geom_point(data=joinz, aes(x=fm100_m,y=mnwind_m,color=ig),alpha=0.05)+
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
ggplot(joinz,aes(log(x=ha)+1))+
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


