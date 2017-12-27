#this code is part of the Short_large_fires project by Dr. R. Chelsea Nagy
#skip to line 214 to re-open and plot
#need to update masterord.csv with updated Short data
#updated 11/3/16

#need to update the perh.csv file (do this in script-shortyseasons_lev3_101916.R, then come back to line 191 here)
#updated 11/10/16

library("ncdf4")
library(raster)
library(rgdal)
library(sp)
library(GISTools)
library(ggplot2)
library(doBy)
library(plyr)


#################################
#bring in fm, light, and fire data
setwd("/Users/rana7082/Dropbox/ecoregions/derived/")
setwd("C:/Users/rnagy/Dropbox/ecoregions/derived/")

masterordtab <- read.csv("masterord_update.csv")
head(masterordtab)

masterord<-as.data.frame(masterordtab)
head(masterord)

#copy
keeps<-masterord



#################################
#bring in centroids that have linked ecoregions
#this file below was made in Arc using a join (right click on layer) of the reprojected fishnet-centroids-conus.shp with the level 3 ecoregion shapefile
fcent_ecn <- readOGR(dsn="C:/Users/rnagy/Documents/CU_Boulder/US_fire/fcent_ecn.shp", 
                     layer="fcent_ecn")

fcent_ecn <- readOGR(dsn="/Users/rana7082/Dropbox/ecoregions/derived/fcent_ecn.shp", 
                     layer="fcent_ecn")

head(fcent_ecn)


#pull data
xx<-fcent_ecn@data

head(xx)

xxdf<-as.data.frame(xx)
head(xxdf)


#####################################


#add ecn to dataframe by matching X50k_ID
keeps$ecocode = xxdf[match(keeps$X50k_ID, xxdf$X50k_ID),"NA_L3CODE"] 
keeps$econame = xxdf[match(keeps$X50k_ID, xxdf$X50k_ID),"NA_L3NAME"] 
head(keeps)
#yay, this worked!

#bind back to masterord
masterord$ecocode = keeps[match(masterord$X50k_ID, keeps$X50k_ID),"ecocode"]
masterord$econame = keeps[match(masterord$X50k_ID, keeps$X50k_ID),"econame"] 

head(masterord)
#now plot


########################################################################################################################
####################
#export table for re-opening and plotting

write.table(masterord, "/Users/rana7082/Dropbox/ecoregions/masterord_update_w_ecn.csv", sep=",", row.names=FALSE)

#to re-open for plotting 
setwd("/Users/rana7082/Dropbox/ecoregions/")
setwd("C:/Users/rnagy/Dropbox/ecoregions/")

masterordtab <- read.csv("masterord_update_w_ecn.csv")
head(masterordtab)

masterord<-as.data.frame(masterordtab)


#####################################################
#add lat long to dataframe
lattysp <- readOGR(dsn="/Users/rana7082/Dropbox/CHEATFIRE (1)/data/study area/fishnets/fishnet_50km/fishnet-centroids-conus3_wgs.shp", 
                   layer="fishnet-centroids-conus3_wgs")
crs(lattysp)
plot(lattysp)

#extent(newData) <- c(-124.792995453, -67.0465202332, 25.0422458649, 49.4157581329) #from metadata in Arc, when you open .nc file

xxx<-lattysp@data

xxxdf<-as.data.frame(xxx)
head(xxxdf)

#rename column in xxxdf (for some reason, name doesn't match)
colnames(xxxdf)[5] <- "X50k_ID"

#add lat long to dataframe by matching X50k_ID
masterord$lat = xxxdf[match(masterord$X50k_ID, xxxdf$X50k_ID),"lat"] 
masterord$long = xxxdf[match(masterord$X50k_ID, xxxdf$X50k_ID),"lon"] 

head(masterord)

####################
write.table(masterord, "/Users/rana7082/Dropbox/ecoregions/masterord_update_w_ecn_ll.csv", sep=",", row.names=FALSE)

#to re-open for plotting
setwd("/Users/rana7082/Dropbox/ecoregions/")

masterordtab <- read.csv("masterord_update_w_ecn_ll.csv")
head(masterordtab)

masterord<-as.data.frame(masterordtab)
head(masterord)
######################################################

#bring in biomass data
setwd("/Users/rana7082/Dropbox/CHEATFIRE (1)/data/veg/WHRC_biomass/")
bio<- readGDAL("/Users/rana7082/Dropbox/CHEATFIRE (1)/data/veg/WHRC_biomass/NBCD_countrywide_biomass_mosaic.tif")

summary(bio)
#projection
#+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0

#convert to raster
bioras<-raster(bio)

#get prjection of other layers
fcent <- readOGR(dsn="/Users/nagyrc/Dropbox/CHEATFIRE (1)/data/study area/fishnets/fishnet_50km/fishnet-centroids-conus.shp", 
                 layer="fishnet-centroids-conus")

proj<-projection(fcent)

#reproject
bio_proj <- projectRaster(bioras, crs=proj) 

#biomass data is 240 m; aggregate to 50 km
setwd("/Users/rana7082/Dropbox/ecoregions/derived/")
bio_agg<-aggregate(bio_proj, fact=208, fun=mean, na.rm=TRUE, filename="bio_agg.tif", overwrite=TRUE)


####
#aggregation factor
50/4
#note, 12.5 factor for fm data

50/0.24
#208.3333 factor for bio data
####

#extract data from centroids of raster
biorex <- extract(bio_agg, fcent, method='simple', sp=TRUE) #, nl=12) #will extract across layers in a raster brick
str(biorex)
summary(biorex)

biorexdf<-as.data.frame(biorex)

subby<-biorexdf[,c("X50k_ID","bio_agg")]

#merge biorex$bio_agg with masterord
head(subby)
head(masterord)

masterord2<-merge(subby,masterord, by=c("X50k_ID"))
#total <- merge(data frameA,data frameB,by=c("ID","Country"))
head(masterord2)
tail(masterord2)

######################
#export and re-open data with biomass
write.table(masterord2, "/Users/rana7082/Dropbox/ecoregions/derived/masterord2.csv", sep=",", row.names=FALSE)

setwd("/Users/rana7082/Dropbox/ecoregions/derived/")
setwd("C:/Users/rnagy/Dropbox/ecoregions/derived/")
masterordtab2 <- read.csv("masterord2.csv")
masterord2<-as.data.frame(masterordtab2)

head(masterord2)
#these are 22 year averages of fm and lightning by month

#add ecn to masterord2
masterord2$ecn<-as.character(gsub("\\.","",masterord2$ecocode))
masterord2$ecn<-as.numeric(masterord2$ecn)

#bring in and join perh
#need to update with new Short data
#updated 11/10/16
setwd("/Users/rana7082/Dropbox/ecoregions/derived/")
setwd("C:/Users/rnagy/Dropbox/ecoregions/derived/")
perhtab<-read.csv("perh_ecn_Short_update.csv")
perh<-as.data.frame(perhtab)
head(perh)

#join by ecn
masterord3<-merge(masterord2,perh, by="ecn",all=TRUE)
head(masterord3)
tail(masterord3)
#has NAs

#remove NAs
masterord4<-subset(masterord3,!(is.na(masterord3["ecn"])))
head(masterord4)


write.table(masterord4, "/Users/rana7082/Dropbox/ecoregions/derived/masterord4.csv", sep=",", row.names=FALSE)

setwd("/Users/rana7082/Dropbox/ecoregions/derived/")
setwd("C:/Users/rnagy/Dropbox/ecoregions/derived/")
masterordtab4 <- read.csv("masterord4.csv")
masterord4<-as.data.frame(masterordtab4)

head(masterord4)

###






###
#######################
tt1<-summaryBy(fm1000~ecn,data=masterord4,FUN=c(mean, sd, length))
tt2<-summaryBy(light.x~ecn,data=masterord4,FUN=c(mean, sd, length))
tt3<-summaryBy(bio_agg~ecn,data=masterord4,FUN=c(mean,sd,length,sum))
write.table(tt3, "C:/Users/rnagy/Dropbox/ecoregions/derived/bio_ecn.csv", sep=",", row.names=FALSE, append=FALSE)


tt4<-summaryBy(fhuman_avg.x~ecn,data=masterord4,FUN=c(mean,sd,length,sum))
tt5<-summaryBy(flight_avg.x~ecn,data=masterord4,FUN=c(mean,sd,length,sum))
tt6<-summaryBy(perh~ecn,data=masterord4,FUN=c(mean,length))
tt7<-summaryBy(tot~ecn,data=masterord4,FUN=c(mean,length))
tt1


dfa <- merge(tt1,tt2,by=c("ecn"))
dfb <- merge(dfa,tt3,by=c("ecn"))
dfc <- merge(dfb,tt4,by=c("ecn"))
dfd <- merge(dfc,tt5,by=c("ecn"))
dfe <- merge(dfd,tt6,by=c("ecn"))
dff <- merge(dfe,tt7,by=c("ecn"))

head(dff)


#bring in other variables to plot here

#ha10_hlrat
setwd("/Users/rana7082/Dropbox/ecoregions/derived/")
flubtab<-read.csv("firehasum_ecn_top_ten_Short_update_hl.csv")
flub<-as.data.frame(flubtab)
head(flub)

flub$ha10_hlrat<-flub$hmean/flub$lmean
flubs<-flub[,c(1,15)]
head(flubs)

library(dplyr)
dfg<-left_join(dff,flubs,by="ecn")
head(dfg)



###
head(dfg)
dfg$log_light<-log(dff$light.x.mean)+1

#fm vs. lightning scaled by biomass and perh
p16 <- ggplot() + 
  geom_point(data = dfg, aes(x = fm1000.mean, y = log_light, color=perh.mean, size=bio_agg.mean)) +
  xlab('1000 hr fuel moisture') +
  ylab('log lightning')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,30)+
  ylim(0,10)+
  scale_colour_gradient(name="% human fires", low = "blue", high = "red", guide = "colourbar")
p16

#lightning values here are about 2x what they were for the Shorty ms...what happened to lightning data when I updated to Short 1992-2013???
###




setwd("/Users/rana7082/Dropbox/ecoregions/derived/")
setwd("C:/Users/rnagy/Dropbox/ecoregions/derived/")
firehatab<-read.csv("fireha_10.csv")
fireha<-as.data.frame(firehatab)
head(fireha)

fireha$ecn<-as.numeric(gsub("[.]","",fireha$NA_L3CODE))

library(dplyr)
dft<-left_join(dff,fireha,by="ecn")
head(dft)


#regression of biomass vs. fire size
p16 <- ggplot(data = dft, aes(y = log(ha.mean), x = log(bio_agg.mean))) + 
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

lm1<-lm(log(ha.mean)~log(bio_agg.mean), data=dft)
summary(lm1)
#p=5.09e-06
#y=-0.57x+7.38


#break into regressions for east and west US here

setwd("/Users/rana7082/Dropbox/ecoregions/derived/")
setwd("C:/Users/rnagy/Dropbox/ecoregions/derived/")
regiontab<-read.csv("arc_map_regions.csv")
region<-as.data.frame(regiontab)

library(dplyr)
dftt<-left_join(dft,region,by="NA_L3CODE")
head(dftt)



p16 <- ggplot(data = dftt, aes(y = log(ha.mean), x = log(bio_agg.mean),color=region)) + 
  geom_point() +
  ylab('log (fire size (ha))') +
  xlab('log (biomass (g/m2))')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,8)+
  ylim(0,10)+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.text=element_text(size=18),legend.title=element_text(size=18))+
  scale_color_manual(values=c("east"="black","west"="dark gray"))+
  geom_smooth(method='lm', formula=y~x, se=FALSE)
p16

east<-dftt[which(dftt$region=="east"),]
west<-dftt[which(dftt$region=="west"),]

lm1<-lm(log(ha.mean)~log(bio_agg.mean), data=east)
summary(lm1)
#p=0.00124; (-) east is sig
lm2<-lm(log(ha.mean)~log(bio_agg.mean), data=west)
summary(lm2)
#p=0.11; west not sig




#plot by level I ecoregions here
levels <- read.csv("C:/Users/rnagy/Dropbox/ecoregions/data/NA_CEC_Eco_Level3/NA_CEC_Eco_Level3.csv")

head(levels)

key<-unique(levels[c("NA_L3CODE","NA_L1CODE")])
key

dftb<-left_join(dft,key,by=c('NA_L3CODE'))
head(dftb)

dftb$L1<-as.character(dftb$NA_L1CODE)
str(dftb)


p16 <- ggplot(data = dftb, aes(y = log(ha.mean), x = log(bio_agg.mean),color=L1)) + 
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

l5<-dftb[which(dftb$L1=="5"),]
l6<-dftb[which(dftb$L1=="6"),]
l7<-dftb[which(dftb$L1=="7"),]
l8<-dftb[which(dftb$L1=="8"),]
l9<-dftb[which(dftb$L1=="9"),]
l10<-dftb[which(dftb$L1=="10"),]
l11<-dftb[which(dftb$L1=="11"),]
l12<-dftb[which(dftb$L1=="12"),]
l13<-dftb[which(dftb$L1=="13"),]
l15<-dftb[which(dftb$L1=="15"),]

lm5<-lm(log(ha.mean)~log(bio_agg.mean), data=l5)
summary(lm5)
#p=0.14

lm6<-lm(log(ha.mean)~log(bio_agg.mean), data=l6)
summary(lm6)
#p=0.52

lm7<-lm(log(ha.mean)~log(bio_agg.mean), data=l7)
summary(lm7)
#p=0.64

lm8<-lm(log(ha.mean)~log(bio_agg.mean), data=l8)
summary(lm8)
#p=0.297

lm9<-lm(log(ha.mean)~log(bio_agg.mean), data=l9)
summary(lm9)
#p=0.463

lm10<-lm(log(ha.mean)~log(bio_agg.mean), data=l10)
summary(lm10)
#p=0.99

lm11<-lm(log(ha.mean)~log(bio_agg.mean), data=l11)
summary(lm11)
#p=0.328

lm12<-lm(log(ha.mean)~log(bio_agg.mean), data=l12)
summary(lm12)
#p=NA

lm13<-lm(log(ha.mean)~log(bio_agg.mean), data=l13)
summary(lm13)
#p=NA

lm15<-lm(log(ha.mean)~log(bio_agg.mean), data=l15)
summary(lm15)
#p=NA


lm1<-lm(log(ha.mean)~log(bio_agg.mean), data=dftb)
summary(lm1)
#p=5.09e-06
#y=-0.57x+7.38









#plot ind fires rather than ecoregion averages
library(dplyr)
setwd("C:/Users/rnagy/Dropbox/ecoregions/derived/")
regiontab<-read.csv("arc_map_regions.csv")
region<-as.data.frame(regiontab)

head(masterord4)
#names(df)[names(df) == 'old.var.name'] <- 'new.var.name'
names(masterord4)[names(masterord4) == 'ecocode'] <- 'NA_L3CODE'

dfgg<-left_join(masterord4,region,by="NA_L3CODE")
head(dfgg)

head(fireha)

dfll<-left_join(dfgg,ha.mean,by="NA_L3CODE")


p16 <- ggplot(data = dftt, aes(y = log(ha.mean), x = log(bio_agg.mean),color=region)) + 
  geom_point() +
  ylab('log (fire size (ha))') +
  xlab('log (biomass (g/m2))')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,8)+
  ylim(0,10)+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.text=element_text(size=18),legend.title=element_text(size=18))+
  scale_color_manual(values=c("east"="black","west"="dark gray"))+
  geom_smooth(method='lm', formula=y~x, se=FALSE)
p16

east<-dftt[which(dftt$region=="east"),]
west<-dftt[which(dftt$region=="west"),]

lm1<-lm(log(ha.mean)~log(bio_agg.mean), data=east)
summary(lm1)
#p=0.00124; (-)
lm2<-lm(log(ha.mean)~log(bio_agg.mean), data=west)
summary(lm2)
#p=0.11





#this one is also good- fm vs. biomass scaled by perh and number of fires (??)
p16 <- ggplot() + 
  geom_point(data = dfg, aes(x = fm1000.mean, y = bio_agg.mean, color=perh.mean, size=2)) +
  xlab('1000 hr fuel moisture') +
  ylab('biomass')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,30)+
  ylim(0,1200)+
  scale_colour_gradient(name="% human fires", low = "blue", high = "red", guide = "colourbar")+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.text=element_text(size=18),legend.title=element_text(size=18))
p16


#this is Figure 4
p16 <- ggplot() + 
  geom_point(data = dfg, aes(x = fm1000.mean, y = bio_agg.mean, color=log(ha10_hlrat), size=2)) +
  xlab('1000 hr fuel moisture') +
  ylab('biomass')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,30)+
  ylim(0,1100)+
  scale_colour_gradient2(name="log (ha10_hlrat)", low = "blue", mid="gray", high = "red", guide = "colourbar")+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.text=element_text(size=18),legend.title=element_text(size=18))
p16


###
#try with subset of ha10_hlrat>1
subby <- subset(dfg, ha10_hlrat > 1)
head(subby)

nonsubby<-subset(dfg, ha10_hlrat <= 1)

hhh<-unique(subby$ecn)
jjj<-unique(nonsubby$ecn)

#tubby<-subset(df, id %in% L)
tubby<-subset(masterord4, ecn %in% hhh)
nontubby<-subset(masterord4, ecn %in% jjj)

mean(tubby$fm1000)
#17.57
sd(tubby$fm1000)
#3.42

mean(tubby$bio_agg)
#250.12
sd(tubby$bio_agg)
#232.14

mean(nontubby$fm1000)
#16.39
sd(nontubby$fm1000)
#4.22

mean(nontubby$bio_agg)
#162.20
sd(nontubby$bio_agg)
#224.36



###



p16 <- ggplot() + 
  geom_point(data = subby, aes(x = fm1000.mean, y = bio_agg.mean, color=log(ha10_hlrat), size=2)) +
  xlab('1000 hr fuel moisture') +
  ylab('biomass')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,30)+
  ylim(0,1100)+
  scale_colour_gradient(name="log (ha10_hlrat)", low = "gray", high = "red", guide = "colourbar")+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.text=element_text(size=18),legend.title=element_text(size=18))
p16


p16 <- ggplot() + 
  geom_point(data = nonsubby, aes(x = fm1000.mean, y = bio_agg.mean, color=log(ha10_hlrat), size=2)) +
  xlab('1000 hr fuel moisture') +
  ylab('biomass')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,30)+
  ylim(0,1100)+
  scale_colour_gradient(name="log (ha10_hlrat)", low = "blue", high = "gray", guide = "colourbar")+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.text=element_text(size=18),legend.title=element_text(size=18))
p16

###

#this isn't working...stopped here 11/11/16
dfg$wt2 <- as.factor( as.numeric( cut(dfg$ha10_hlrat,5)))
cols <- c("0.2"="yellow","0.5" = "blue","1" = "darkgreen", "2" = "orange", "5"= "black")
p16 <- ggplot() + 
  geom_point(data = dfg, aes(x = fm1000.mean, y = bio_agg.mean, color=wt2, size=tot.mean)) +
  xlab('1000 hr fuel moisture') +
  ylab('biomass')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,30)+
  ylim(0,1200)+
  scale_colour_manual(values = cols)+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.text=element_text(size=18),legend.title=element_text(size=18))
p16


summary(dfg$ha10_hlrat)
#useful? biomass vs. log lightning scaled by perh and number of fires (??)
p16 <- ggplot() + 
  geom_point(data = dff, aes(x = log_light, y = bio_agg.mean, color=perh.mean, size=tot.mean)) +
  xlab('log lightning') +
  ylab('biomass')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,10)+
  ylim(0,1200)+
  scale_colour_gradient(name="% human fires", low = "blue", high = "red", guide = "colourbar")
p16




















######################
#try splitting into categories
head(dff)


summary(dff$zhuman.mean)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.1016  0.4248  1.1250  1.8060  2.4630 10.8100 

summary(dff$zlight.mean)
#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.001024 0.032450 0.100700 0.328400 0.377100 2.215000 

#split using means...consider using medians instead
dff$zhcat<-ifelse(dff$zhuman.mean<1.8060,"low","high")
dff$zlcat<-ifelse(dff$zlight.mean<0.3284,"low","high")
head(dff)

dff$z.com<-paste(dff$zhcat,dff$zlcat,sep=",")

#fm vs. biomass
p16 <- ggplot() + 
  geom_point(data = dff, aes(y = fm.mean, x=bio_agg.mean,color=z.com)) +
  ylab('mean fuel moisture') +
  xlab('biomass')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,1200)+
  ylim(0,30)+
  scale_color_manual(name="h/l fires",values=c("purple","red","blue","gray"))
p16

#fm vs. lightning
p16 <- ggplot() + 
  geom_point(data = dff, aes(y = fm.mean, x=light_log.mean,color=z.com)) +
  ylab('mean fuel moisture') +
  xlab('log lightning')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,4)+
  ylim(0,30)+
  scale_color_manual(name="h/l fires",values=c("purple","red","blue","gray"))
p16

#bio vs. lightning
p16 <- ggplot() + 
  geom_point(data = dff, aes(y = bio_agg.mean, x=light_log.mean,color=z.com)) +
  ylab('biomass') +
  xlab('log lightning')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,4)+
  ylim(0,1200)+
  scale_color_manual(name="h/l fires",values=c("purple","red","blue","gray"))
p16


#######################
#need to think about how to subset this data

#subset where n human >0 or >1??
masterordh<-subset(masterord4,masterord4$zhuman>0)
#subset where n lightning >0 or >1??
masterordl<-subset(masterord4,masterord4$zlight>0)

head(masterordh)

df1<-data.frame(fm=masterordh$fm,light_log=masterordh$light_log,z=masterordh$fhuman_avg,eco=masterordh$ecocode,hl=rep("human",nrow(masterordh)),bio=masterordh$bio_agg)
df2<-data.frame(fm=masterordl$fm,light_log=masterordl$light_log,z=masterordl$flight_avg,eco=masterordl$ecocode,hl=rep("light",nrow(masterordl)),bio=masterordl$bio_agg)

df3<-rbind(df1,df2)
head(df3)

lll<-length(unique(df3$eco))
#85

#remove NAs, these were probaby zeros
df4<-subset(df3,!(is.na(df3["eco"])))
lll<-length(unique(df4$eco))
#84

tt<-unique(df4$eco)
tt
head(df4)

tt1<-summaryBy(fm~eco+hl,data=df4,FUN=c(mean, sd, length))
tt2<-summaryBy(light_log~eco+hl,data=df4,FUN=c(mean, sd, length))
tt3<-summaryBy(z~eco+hl,data=df4,FUN=c(mean,sd,length,sum))
tt4<-summaryBy(bio~eco+hl,data=df4,FUN=c(mean,sd,length,sum))




head(tt1)

df5 <- merge(tt1,tt2,by=c("eco", "hl"))
df6 <- merge(df5,tt3,by=c("eco", "hl"))
df7 <- merge(df6,tt4,by=c("eco", "hl"))

head(df7)
tail(df7)
str(df7)

#scatterplot of fm vs. light with #fires
p16 <- ggplot() + 
  geom_point(data = df7, aes(x = fm.mean, y = light_log.mean, color=hl,size=z.mean)) +
  xlab('fuel moisture') +
  ylab('log lightning')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,30)+
  ylim(0,4)+
  scale_color_manual(name="hl",values=c("red","blue"))

p16


#scatterplot of fm vs. bio with #fires
p16 <- ggplot() + 
  geom_point(data = df7, aes(x = fm.mean, y = bio.mean, color=hl,size=z.mean)) +
  xlab('fuel moisture') +
  ylab('biomass')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,30)+
  ylim(0,1200)+
  scale_color_manual(name="hl",values=c("red","blue"))

p16



#scatterplot of light vs. bio with #fires
p16 <- ggplot() + 
  geom_point(data = df7, aes(x = bio.mean, y = light_log.mean, color=hl,size=z.mean)) +
  xlab('biomass') +
  ylab('log lightning')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,1200)+
  ylim(0,4)+
  scale_color_manual(name="hl",values=c("red","blue"))

p16

#these show niche conditions only, does not include data about # of fires
#barplot of fm vs. eco
ggplot(df7, aes (x=factor(eco), y=fm.mean, fill=hl))+ 
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(name="hl",values=c("red","blue"))

#barplot of lightning vs. eco
ggplot(df7, aes (x=factor(eco), y=light_log.mean, fill=hl))+ 
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(name="hl",values=c("red","blue"))

#barplot of biomass vs. eco
ggplot(df7, aes (x=factor(eco), y=bio.mean, fill=hl))+ 
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(name="hl",values=c("red","blue"))



#try plotting the difference in means instead
#first reshape dataframe
w <- reshape(df7, timevar = "hl", idvar = c("eco"), direction = "wide")
head(w)


#calculate the difference in means
w$fm.mean.diff<-w$fm.mean.human - w$fm.mean.light
w$light_log.mean.diff<-w$light_log.mean.human- w$light_log.mean.light
w$z.mean.diff<-w$z.mean.human-w$z.mean.light
w$bio.mean.diff<-w$bio.mean.human-w$bio.mean.light

#for the next four plots, where positive, human mean is greater than lightning mean
ggplot(w, aes (x=factor(eco), y=fm.mean.diff))+ geom_bar(stat="identity")
ggplot(w, aes (x=factor(eco), y=light_log.mean.diff))+ geom_bar(stat="identity")
ggplot(w, aes (x=factor(eco), y=z.mean.diff))+ geom_bar(stat="identity")
ggplot(w, aes (x=factor(eco), y=bio.mean.diff))+ geom_bar(stat="identity")



w$ecn<-as.character(gsub("\\.","",w$eco))
w$ecn<-as.numeric(w$ecn)
w$ecnnn<-c(1:84)

head(w)
tail(w)


#means in human subset vs. lightning subset
#fm
p16 <- ggplot() + 
  geom_point(data = w, color="blue", aes(y = fm.mean.light, x=ecnnn)) +
  geom_point(data = w, color="red", aes(y = fm.mean.human, x = ecnnn))+
  ylab('mean fuel moisture') +
  xlab('ecnnn')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,85)+
  ylim(0,30)

p16


#lightning strikes
p16 <- ggplot() + 
  geom_point(data = w, color="blue", aes(y = light_log.mean.light, x=ecnnn)) +
  geom_point(data = w, color="red", aes(y = light_log.mean.human, x = ecnnn))+
  ylab('mean (log) lighting density') +
  xlab('ecnnn')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,85)+
  ylim(0,5)

p16


#biomass
p16 <- ggplot() + 
  geom_point(data = w, color="blue", aes(y = bio.mean.light, x=ecnnn)) +
  geom_point(data = w, color="red", aes(y = bio.mean.human, x = ecnnn))+
  ylab('mean biomass') +
  xlab('ecnnn')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,85)+
  ylim(0,1200)

p16


#number of fires
p16 <- ggplot() + 
  geom_point(data = w, color="blue", aes(y = z.mean.light, x=ecnnn)) +
  geom_point(data = w, color="red", aes(y = z.mean.human, x = ecnnn))+
  ylab('mean number fires') +
  xlab('ecnnn')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,85)+
  ylim(0,15)

p16


#rename eco to NA_L3CODE to later join in Arc
head(w)
names(w)[1]<-"NA_L3CODE"
str(w)

#output table to plot in Arc
write.table(w, "/Users/rana7082/Dropbox/ecoregions/climate_ecn2.csv", sep=",", row.names=FALSE, append=FALSE)
write.table(w, "C:/Users/rnagy/Dropbox/ecoregions/climate_ecn200.csv", sep=",", row.names=FALSE, append=FALSE)

####################################################



#####################################################

#######################
#niche figure for only top 10% largest fires

######
#bring in and join perh for top 10 fires
setwd("/Users/rana7082/Dropbox/ecoregions/derived/")
perhtab10<-read.csv("perh_ecn_top_ten.csv")
perh10<-as.data.frame(perhtab10)
head(perh10)

names(perh10)[2]<-"human10"
names(perh10)[3]<-"lightning10"
names(perh10)[4]<-"tot10"
names(perh10)[5]<-"perh10"

#join by ecn
masterord5<-merge(masterord4,perh10, by="ecn",all=TRUE)
head(masterord5)
tail(masterord5)
#has NAs

#test
sub<-subset(masterord5,masterord5$ecn == 6215)
head(sub)
#looks good, matches the excel sheet



tt8<-summaryBy(perh10~ecn,data=masterord5,FUN=c(mean,sd,length,sum))

dfg <- merge(dff,tt8,by=c("ecn"))
head(dfg)


#plot 
p16 <- ggplot() + 
  geom_point(data = dfg, aes(x = fm.mean, y = light_log.mean, color=perh10.mean, size=bio_agg.mean)) +
  xlab('fuel moisture') +
  ylab('log lightning')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,30)+
  ylim(0,4)+
  scale_colour_gradient(name="% human fires 10", low = "blue", high = "red", guide = "colourbar")
p16

#this plot looks very similar to the one with all fires

tt9<-summaryBy(tot10~ecn,data=masterord5,FUN=c(mean,sd,length,sum))
dfh <- merge(dfg,tt9,by=c("ecn"))

head(dfh)
p16 <- ggplot() + 
  geom_point(data = dfh, aes(x = fm.mean, y = bio_agg.mean, color=perh10.mean, size=tot10.mean)) +
  xlab('fuel moisture') +
  ylab('biomass')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,30)+
  ylim(0,1200)+
  scale_colour_gradient(name="% human fires 10", low = "blue", high = "red", guide = "colourbar")
p16



p16 <- ggplot() + 
  geom_point(data = dfh, aes(x = light_log.mean, y = bio_agg.mean, color=perh10.mean, size=tot10.mean)) +
  xlab('log lightning') +
  ylab('biomass')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,4)+
  ylim(0,1200)+
  scale_colour_gradient(name="% human fires 10", low = "blue", high = "red", guide = "colourbar")
p16
#######################


















#####################################################
#old code to make niche plot for shorty paper
#combined figures 1,2, and 3 for radness: fire niche + fires
p16 <- ggplot() + 
  geom_point(data = masterord, alpha=0.4, color="light grey", aes(x = fm, y = light_log))+ 
  geom_point(data = df3, alpha=0.2, aes(x = fm, y = light_log, colour = hl, size=z)) +
  xlab('fuel moisture') +
  ylab('log lightning')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  xlim(0,35)+
  ylim(0,5)+
  facet_wrap(~hl)+
  scale_color_manual(name="hl",values=c("red","blue"))

p16

