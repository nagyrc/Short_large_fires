#this code is part of the Short_large_fires project by Dr. R. Chelsea Nagy

library(ggplot2)
library(doBy)
library(dplyr)

#bring in Short data with fm, ws, biomass extracted
#note, this has large fires only (top 10%)
lrg_fires<- read.csv("data/merged/lrg_fires.csv")

keep<-lrg_fires[which(lrg_fires$STAT_CAUSE_DESCR!="Missing/Undefined"),]
head(keep)

#summarize biomass by ecoregion
biostats<-summaryBy(NBCD_countrywide_biomass_mosaic~NA_L3CODE,data=keep,FUN=c(mean),na.rm=TRUE)

#join table of fire size and biomass by ecoregion
#bring in fire size data by ecoregion (made in script 6, line 236)
fireha<-as.data.frame(read.csv("results/firehasum_ecn_top_ten_Short_update.csv"))
dfbio<-left_join(biostats,fireha,by="NA_L3CODE")

head(dfbio)
colnames(dfbio) <- c("NA_L3CODE", "bio.mean","ecn","nobs", "ha.mean","ha.sd","ha.median","ha.sum")

#remove unused columns
dfbio<- subset(dfbio, select=-c(nobs,ha.sd,ha.median,ha.sum))

#bring in Level I info
#levels <- read.csv("C:/Users/rnagy/Dropbox/ecoregions/data/NA_CEC_Eco_Level3/NA_CEC_Eco_Level3.csv")
#head(levels)
key<-unique(keep[c("NA_L3CODE","NA_L1CODE")])
key
dfbio1<-left_join(dfbio,key,by=c('NA_L3CODE'))
head(dfbio1)
dfbio1$L1<-as.character(dfbio1$NA_L1CODE)

#code to make Fig 7c
p16 <- ggplot(data = dfbio1, aes(y = log(ha.mean), x = log(bio.mean),color=L1)) + 
  geom_point() +
  ylab('log fire size (ha)') +
  xlab('log biomass')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,8)+
  ylim(0,10)+
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

lm5<-lm(log(ha.mean)~log(bio.mean), data=l5)
summary(lm5)
#p=0.14

lm6<-lm(log(ha.mean)~log(bio.mean), data=l6)
summary(lm6)
#p=0.52

lm7<-lm(log(ha.mean)~log(bio.mean), data=l7)
summary(lm7)
#p=0.64

lm8<-lm(log(ha.mean)~log(bio.mean), data=l8)
summary(lm8)
#p=0.297

lm9<-lm(log(ha.mean)~log(bio.mean), data=l9)
summary(lm9)
#p=0.463

lm10<-lm(log(ha.mean)~log(bio.mean), data=l10)
summary(lm10)
#p=0.99

lm11<-lm(log(ha.mean)~log(bio.mean), data=l11)
summary(lm11)
#p=0.328

lm12<-lm(log(ha.mean)~log(bio.mean), data=l12)
summary(lm12)
#p=NA

lm13<-lm(log(ha.mean)~log(bio.mean), data=l13)
summary(lm13)
#p=NA

lm15<-lm(log(ha.mean)~log(bio.mean), data=l15)
summary(lm15)
#p=NA


lm1<-lm(log(ha.mean)~log(bio.mean), data=dftb)
summary(lm1)
#p=5.09e-06
#y=-0.57x+7.38


#split ecoregions into east and west US
region<-as.data.frame(read.csv("data/bounds/ecoregion/east_west/arc_map_regions.csv"))

#join region
dfbio1r<-left_join(dfbio1,region,by=c('NA_L3CODE'))

#split into east and west
eastbio<-dfbio1r[which(dfbio1r$region=="east"),]
westbio<-dfbio1r[which(dfbio1r$region=="west"),]

lm1<-lm(log(ha.mean)~log(bio.mean), data=eastbio)
summary(lm1)
#p=0.00124; (-)
lm2<-lm(log(ha.mean)~log(bio.mean), data=westbio)
summary(lm2)
#p=0.11