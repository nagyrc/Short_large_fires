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
read<- read.csv("C:/Users/rnagy/Dropbox/ecoregions/data/Short_large10_FM_Wind/Short_large10_FM_Wind.csv")
read<- read.csv("/Users/rana7082/Dropbox/ecoregions/data/Short_large10_FM_Wind/Short_large10_FM_Wind.csv")
#note, this has large fires only
head(read)

readsub <- subset(read, read$STAT_CAU_1 != "Missing/Undefined")
#nothing removed
#readsub$ig<-ifelse(readsub$STAT_CAU_1=="Lightning","lightning","human")

sum1<-summaryBy(data=read, ha~NA_L3NAME, FUN=c(length,mean, median,min, max))
head(sum1)
write.table(sum1, "C:/Users/rnagy/Dropbox/ecoregions/derived/fireha_ecn_stats.csv", sep=",", row.names=FALSE, append=FALSE)


sum2<-summaryBy(data=read, ha~ig+NA_L3NAME, FUN=c(mean))
head(sum2)
#tranform this to wide, then do paired t=test
#w <- reshape(df1, timevar = "ig", idvar = "NA_L3CODE", v.names = "fm100_m.mean", direction = "wide")
#t.test(w$fm100_m.mean.human,w$fm100_m.mean.lightning,paired=TRUE)
w<-reshape(sum2,timevar="ig",idvar="NA_L3NAME",v.names="ha.mean",direction="wide")
head(w)
t.test(w$ha.mean.human,w$ha.mean.lightning,paired=TRUE)
#p=0.002; lightning fires are sig larger than human fires by ecoregion
summary(w$ha.mean.human)
#mean=285.300
summary(w$ha.mean.lightning)
#mean=488.400

head(readsub)
readsubdf<-as.data.frame(readsub)
summary(readsub$ig)
#human=129966, light=30640


#format of NA_L3CODE is all messed up
#remove that field
readsubdf$NA_L3CODE<-NULL
head(readsubdf)

#bring in something else that can make key from
readz<- read.csv("C:/Users/rnagy/Dropbox/ecoregions/derived/Short_large10.csv")
readz<- read.csv("/Users/rana7082/Dropbox/ecoregions/derived/Short_large10.csv")
#unique(df[c("yad", "per")])

key<-unique(readz[c("NA_L3CODE","NA_L3NAME")])
key

joinz<-left_join(readsubdf,key,by=c('NA_L3NAME'))
head(joinz)

subecbp<- subset(joinz, joinz$NA_L3CODE == "8.2.4")
subecbp

subhelp<- subset(joinz, joinz$NA_L3CODE == "8.2.2")
subhelp

#129883+30635=160,518 (old dataset)
#160,606 new dataset



sum66<-summaryBy(data=joinz, ha~ig+NA_L3CODE, FUN=c(sum))
sum66

w<-reshape(sum66,timevar="ig",idvar="NA_L3CODE",v.names="ha.sum",direction="wide")
w

write.table(w, "C:/Users/rnagy/Dropbox/ecoregions/derived/burned_area_hl.csv", sep=",", row.names=FALSE, append=FALSE)

##########
#for large fires only

head(joinz$ha)
tt3<-unique(joinz$NA_L3CODE)


#plot these conditions where large human and large lightning fires exist
hub<-joinz[which(joinz$STAT_CAU_1!="Lightning"),]
lub<-joinz[which(joinz$STAT_CAU_1=="Lightning"),]

#hub nobs= 129,966
#lub nobs= 30,640

129966+30640
#160,606

csv2<- read.csv("/Users/rana7082/Dropbox/ecoregions/derived/Short_large10.csv")
#160,608 obs

subecbp2<- subset(csv2, csv2$NA_L3CODE == "8.2.4")
subecbp2
#31 obs, 0 lightning

subhelp2<- subset(csv2, csv2$NA_L3CODE == "8.2.2")
subhelp2
#35 obs, 0 lightning

head(hub)

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


#combined
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





#try binning...yay, this worked!
#geom_bin2d(mapping = NULL, data = NULL, stat = "bin2d", position = "identity", ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)
#two -9999 values in hub$fm_1000hr
hub2<-subset(hub, fm100_m>=0)





###
#box and whisker plot for each variable or bagplot for both
head(joinz)

library(ggplot2)
#both, but this does not show variation in y axis
ggplot(joinz, aes(x = fm100_m, y=mnwind_m, fill = ig)) +
  geom_boxplot() +
  scale_fill_manual(values = c("red", "blue"))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  labs(x="100 hr fuel moisture",y="wind speed") 

#fm only; figure 5c
ggplot(joinz, aes(x = ig, y = fm100_m,fill=ig)) +
  geom_boxplot()+
  scale_fill_manual(values = c("red", "blue"))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  labs(y="100 hr fuel moisture",x="ignition")+
  coord_flip()

#ws only; figure 5d
ggplot(joinz, aes(x = ig, y = mnwind_m,fill=ig)) +
  geom_boxplot()+
  scale_fill_manual(values = c("red", "blue"))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  labs(y="wind speed",x="ignition")

#bagplot
library(aplpack)
bagplot(joinz$fm100_m,joinz$mnwind_m, xlab="fuel moisture", ylab="wind speed")


###
#######################################
#stats
mean(hub2$fm100_m)
#14.56
median(hub2$fm100_m)
#15.21
mean(lub$fm100_m)
#11.45
median(lub$fm100_m)
#10.76
std.error(hub2$fm100_m)
#0.008
std.error(lub$fm100_m)
#0.022

mean(hub2$mnwind_m)
#3.96
median(hub2$mnwind_m)
#3.999
mean(lub$mnwind_m)
#3.418
median(lub$mnwind_m)
#3.306

std.error(hub2$mnwind_m)
#0.00198
std.error(lub$mnwind_m)
#0.003566



head(hub2)
mean(hub2$stdwind_m)
#1.479
median(hub2$stdwind_m)
#1.514
mean(lub$stdwind_m)
#1.1658
median(lub$stdwind_m)
#1.12958

std.error(hub2$stdwind_m)
#0.000996
std.error(lub$stdwind_m)
#0.00172


#t-test on joinz data by ig
#t.test(y~x)
t.test(joinz$fm100_m~joinz$ig,var.equal = TRUE)
#p=<2.2e-16

t.test(joinz$mnwind_m~joinz$ig,var.equal = TRUE)
#p=<2.2e-16

t.test(joinz$stdwind_m~joinz$ig,var.equal = TRUE)
#p=<2.2e-16

tti1<-summaryBy(data=joinz, fm100_m~ig+NA_L3CODE)
tti1

df1<-as.data.frame(tti1)
head(df1)

#w <- reshape(l.sort, timevar = "subj", idvar = c("id", "female", "race", "ses", "schtyp", "prog"), direction = "wide")
w <- reshape(df1, timevar = "ig", idvar = "NA_L3CODE", v.names = "fm100_m.mean", direction = "wide")
head(w)

w$fmdiffmon<-w$fm100_m.mean.human-w$fm100_m.mean.lightning

word<-w[with(w, order(-fmdiffmon)), ]
word


t.test(w$fm100_m.mean.human,w$fm100_m.mean.lightning,paired=TRUE)
#p=7.039e-06

###
#plots
p16<-ggplot()+
  geom_bin2d(bins=20,data=hub2, aes(x=fm100_m,y=mnwind_m))+
  xlab('100 hr fuel moisture (%)') +
  ylab('wind speed ()')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,30)+
  ylim(0,9)+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.text=element_text(size=18),legend.title=element_text(size=18))+
  ggtitle("large human-caused fires")+
  theme(plot.title = element_text(size=20))+
  scale_fill_continuous(low="gray90", high="red", limits=c(0,20000),breaks=seq(0, 20000, by=4000))
p16

p16<-ggplot()+
  geom_bin2d(bins=20,data=lub, aes(x=fm100_m,y=mnwind_m))+
  xlab('100 hr fuel moisture (%)') +
  ylab('wind speed ()')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,30)+
  ylim(0,9)+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.text=element_text(size=18),legend.title=element_text(size=18))+
  ggtitle("large lightning-caused fires")+
  theme(plot.title = element_text(size=20))+
  scale_fill_continuous(low="gray90", high="blue",limits=c(0,20000), breaks=seq(0, 20000, by=4000))
p16



###
#try discrete binning
#p <- ggplot(df, aes(x, y, fill=cut(..count.., c(0,6,8,9,Inf))))
#p <- p + stat_bin2d(bins = 20)
#p + scale_fill_hue("count")

hub2<-subset(hub, fm100_m>=0)
summary(hub2$fm100_m)
summary(hub$mnwind_m)

#rrr<-quantile(hub2$fm_1000hr,probs = seq(0, 1, 0.20))
#rrr



#fm vs wind speed; figure 5a
p <- ggplot(hub2, aes(fm100_m, mnwind_m, fill=cut(..count.., c(0,2,5,50,500,5000,20000))))+
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

#figure 5b
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




#try a discrete bin fig for each ecoregion????
#human

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


#lightning
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



#fm vs. wind variability
head(hub2)
p <- ggplot(hub2, aes(fm100_m, stdwind_m, fill=cut(..count.., c(0,2,5,50,500,5000,20000))))+
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
head(hub2)
p <- ggplot(hub2, aes(mnwind_m, stdwind_m, fill=cut(..count.., c(0,2,5,50,500,5000,20000))))+
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





####################
#regression

head(joinz)


#mean fuel moisture vs. mean fire size by ecoregion
tt1<-summaryBy(fm100_m~NA_L3CODE,data=joinz,FUN=c(mean))
tt2<-summaryBy(mnwind_m~NA_L3CODE,data=joinz,FUN=c(mean))
head(tt1)

dfa <- merge(tt1,tt2,by=c("NA_L3CODE"))
head(dfa)
write.table(dfa, "C:/Users/rnagy/Dropbox/ecoregions/derived/fm_ws_monthly_ecn.csv", sep=",", row.names=FALSE, append=FALSE)


setwd("/Users/rana7082/Dropbox/ecoregions/derived/")
setwd("C:/Users/rnagy/Dropbox/ecoregions/derived/")
firehatab<-read.csv("fireha_10.csv")
fireha<-as.data.frame(firehatab)
head(fireha)

fireha$ecn<-as.numeric(gsub("[.]","",fireha$NA_L3CODE))

library(dplyr)
dft<-left_join(dfa,fireha,by="NA_L3CODE")
head(dft)

#fire size vs. fuel moisture
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

lm1<-lm(log(ha.mean)~fm100_m.mean, data=dft)
summary(lm1)
#p=3.22e-09
#y=-0.355x+9.4499
#is significantly (-) related


#fire size vs. wind speed
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

lm1<-lm(log(ha.mean)~mnwind_m.mean, data=dft)
summary(lm1)
#p=9.99e-05
#y=-1.465x+10.4702
#is significantly (-) related

head(dft)
####################
#split into east and west US
setwd("/Users/rana7082/Dropbox/ecoregions/derived/")
setwd("C:/Users/rnagy/Dropbox/ecoregions/derived/")
regiontab<-read.csv("arc_map_regions.csv")
region<-as.data.frame(regiontab)

library(dplyr)
dftt<-left_join(dft,region,by="NA_L3CODE")
head(dftt)

east<-dftt[which(dftt$region=="east"),]
west<-dftt[which(dftt$region=="west"),]

head(east)

#cs<-c(east="green",west="black")

#plot both subsets on one plot
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

lm1<-lm(log(ha.mean)~fm100_m.mean, data=east)
summary(lm1)
#p=0.05; (+)
lm2<-lm(log(ha.mean)~fm100_m.mean, data=west)
summary(lm2)
#p=0.0005; (-)

head(dftt)
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
#geom_smooth(method='lm', formula=y~x, se=FALSE)
p16

lm3<-lm(log(ha.mean)~mnwind_m.mean, data=east)
summary(lm3)
#p=0.17
lm4<-lm(log(ha.mean)~mnwind_m.mean, data=west)
summary(lm4)
#p=0.93
###################



#fuel moisture, large fires only
r2 <- summaryBy(fm100_m~ig+NA_L3CODE, data=joinz, FUN=mean)
r2

#cast(df, year ~ group, mean, value = 'income')
w<-cast(r2, NA_L3CODE ~ ig, value = 'fm100_m.mean')
w

w$diff_fm10<-w$human-w$lightning
#where positive human > lightning

#export table to join in Arc

write.table(w, "/Users/rana7082/Dropbox/ecoregions/derived/diff_fm_NA_L3CODE_10_monthly.csv", sep=",", row.names=FALSE, append=FALSE)
write.table(w, "C:/Users/rnagy/Dropbox/ecoregions/derived/diff_fm_NA_L3CODE_10_monthly.csv", sep=",", row.names=FALSE, append=FALSE)


head(joinz)
#mean wind speed, large fires only
r3 <- summaryBy(mnwind_m~ig+NA_L3CODE, data=joinz, FUN=mean)
r3

#cast(df, year ~ group, mean, value = 'income')
w<-cast(r3, NA_L3CODE ~ ig, value = 'mnwind_m.mean')
w

w$diff_windspeed<-w$human-w$lightning

write.table(w, "/Users/rana7082/Dropbox/ecoregions/derived/diff_wind_NA_L3CODE_10_monthly.csv", sep=",", row.names=FALSE, append=FALSE)
write.table(w, "C:/Users/rnagy/Dropbox/ecoregions/derived/diff_wind_NA_L3CODE_10_monthly.csv", sep=",", row.names=FALSE, append=FALSE)



#std wind speed, large fires only
r4 <- summaryBy(stdwind_m~ig+NA_L3CODE, data=joinz, FUN=mean)
r4

#cast(df, year ~ group, mean, value = 'income')
w<-cast(r4, NA_L3CODE ~ ig, value = 'stdwind_m.mean')
w

w$diff_sdwindspeed<-w$human-w$lightning

write.table(w, "/Users/rana7082/Dropbox/ecoregions/derived/diff_sdwind_NA_L3CODE_10_monthly.csv", sep=",", row.names=FALSE, append=FALSE)
write.table(w, "C:/Users/rnagy/Dropbox/ecoregions/derived/diff_sdwind_NA_L3CODE_10_monthly.csv", sep=",", row.names=FALSE, append=FALSE)



###########
head(joinz)
#testy
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
  theme(strip.background = element_blank())+
  theme(panel.margin = unit(0, "lines"))
p16

lll<-as.data.frame(summaryBy(ha~NA_L3CODE+ig,data=joinz, FUN=mean))
wid<-cast(lll, NA_L3CODE ~ ig, value = 'ha.mean')
wid

write.table(wid, "/Users/rana7082/Dropbox/ecoregions/derived/fireha_10_hl.csv", sep=",", row.names=FALSE, append=FALSE)


hhh<-as.data.frame(summaryBy(FOD_ID~NA_L3CODE+ig,data=joinz,FUN=length))
wider<-cast(hhh, NA_L3CODE ~ ig, value = 'FOD_ID.length')
wider

write.table(wider, "/Users/rana7082/Dropbox/ecoregions/derived/nobs_10_hl.csv", sep=",", row.names=FALSE, append=FALSE)



uuu<-as.data.frame(summaryBy(ha~NA_L3CODE,data=joinz, FUN=mean))
uuu

write.table(uuu, "/Users/rana7082/Dropbox/ecoregions/derived/fireha_10.csv", sep=",", row.names=FALSE, append=FALSE)







#stats with median
head(joinz)
tti2<-summaryBy(data=joinz, ha~ig+NA_L3CODE, FUN=median)
tti2


df2<-as.data.frame(tti2)
head(df2)

#w <- reshape(l.sort, timevar = "subj", idvar = c("id", "female", "race", "ses", "schtyp", "prog"), direction = "wide")
w2 <- reshape(df2, timevar = "ig", idvar = "NA_L3CODE", v.names = "ha.median", direction = "wide")
head(w2)


#export for Figure S2b
write.table(w2, "C:/Users/rnagy/Dropbox/ecoregions/derived/firehamed_10hl.csv", sep=",", row.names=FALSE, append=FALSE)



tti3<-summaryBy(data=joinz, ha~NA_L3CODE, FUN=median)
df3<-as.data.frame(tti3)
head(df3)

#for Table S2a
write.table(df3, "C:/Users/rnagy/Dropbox/ecoregions/derived/firehamed_10.csv", sep=",", row.names=FALSE, append=FALSE)




######
#histograms of fire size vs. ignition
head(joinz)
library(ggplot2)

summary(joinz$ha)
#min=0.40 ha

log(1.4)
log(0.4)
(log(0.4))+1

ggplot(joinz,aes(log(x=ha)+1))+
  geom_histogram()+
  facet_grid(~ig)+
  theme_bw()

hub <- subset(joinz, joinz$ig == "human")
lub <- subset(joinz, joinz$ig == "lightning")

ggplot(aes(log(x=ha)+1))+
  geom_histogram(data=hub)+
  geom_histogram(data=lub)+
  theme_bw()

(log(100))+1
#5.605

(log(500))+1
#7.2146

(log(1000))+1
#7.9077


head(joinz)

#figure 2c
ggplot(joinz,aes(x=log(ha)+1)) + 
  geom_histogram(data=subset(joinz,ig == 'human'),fill = "red",alpha=0.3)+
  geom_histogram(data=subset(joinz,ig == 'lightning'),fill = "blue",alpha=0.3)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.text=element_text(size=12),legend.title=element_text(size=12))+
  theme(plot.title = element_text(size=20))+
  xlab('log (fire size (ha))+1') +
  ylab('number of large fires')

ggplot(joinz,aes(x=log(ha)+1)) + 
  geom_histogram(data=subset(joinz,ig == 'lightning'),fill = "blue",alpha=0.7)+
  geom_histogram(data=subset(joinz,ig == 'human'),fill = "red",alpha=0.7)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.text=element_text(size=12),legend.title=element_text(size=12))+
  theme(plot.title = element_text(size=20))+
  xlab('log (fire size (ha))+1') +
  ylab('number of large fires')




#regression with ind. fires rather than ecoregion totals

library(dplyr)
dftt<-left_join(joinz,region,by="NA_L3CODE")
head(dftt)

east<-dftt[which(dftt$region=="east"),]
west<-dftt[which(dftt$region=="west"),]


#fire size vs. fm
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

lm1<-lm(log(ha)~fm100_m, data=east)
summary(lm1)
#p<2e-16; (+)
lm2<-lm(log(ha)~fm100_m, data=west)
summary(lm2)
#p<2e-16; (-)


#fire size vs. ws
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

lm3<-lm(log(ha)~mnwind_m, data=east)
summary(lm3)
#p<2e-16
lm4<-lm(log(ha)~mnwind_m, data=west)
summary(lm4)
#p<2e-16


head(dftt)
head(read)
