#unused code

#######################################################
#from script #7 (shortyseasons_lev3_github)
####################################
#large fire data exploration

#what is max fire size of large fires?
summary(keep$FIRE_SIZE_ha)
#max=225895 ha
#use this as max for x axis and determining bin width

#look at histogram of fire sizes across all ecns
ggplot(data=keep, aes(keep$FIRE_SIZE_ha)) + 
  geom_histogram(binwidth=1000) 
#many relatively small 'large' fires


#######################

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








#######################################################
#from script #6 (extract_fm_monthly_github)
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

#######################################################


#######################################################
#unused code from climatevars_ecoregion script

#this code is part of the Short_large_fires project by Dr. R. Chelsea Nagy
#skip to line 214 to re-open and plot

library("ncdf4")
library(raster)
library(rgdal)
library(sp)
library(GISTools)
library(ggplot2)
library(doBy)
library(plyr)
library(dplyr)


#################################
#bring in fm, light, and fire data
masterord <- as.data.frame(read.csv("data/merged/masterord_update.csv"))

#make a copy
keeps<-masterord



#################################
#bring in centroids that have linked ecoregions
#this file below was made in Arc using a join (right click on layer) of the reprojected fishnet-centroids-conus.shp with the level 3 ecoregion shapefile
fcent_ecn <- readOGR(dsn="data/bounds/centroids/fcent_ecn.shp", layer="fcent_ecn")


#pull data
xx<-fcent_ecn@data

xxdf<-as.data.frame(xx)


#####################################
#add ecn to dataframe by matching X50k_ID
keeps$ecocode = xxdf[match(keeps$X50k_ID, xxdf$X50k_ID),"NA_L3CODE"] 
keeps$econame = xxdf[match(keeps$X50k_ID, xxdf$X50k_ID),"NA_L3NAME"] 

#bind back to masterord
masterord$ecocode = keeps[match(masterord$X50k_ID, keeps$X50k_ID),"ecocode"]
masterord$econame = keeps[match(masterord$X50k_ID, keeps$X50k_ID),"econame"] 


########################################################################################################################
####################
#maybe don't need this (lines 49-59)
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








##############################################
#from script extract_fm_wind_monthly



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








######################################
#from script script_shortyseasons_lev3

#top 10% largest fires; summary statistics; reported in Table S1 in manuscript
#create dataframe of number of fires, mean, sd of fire size by ecoregion (summary columns of the 10% largest fires)
#head(keep)
#output=NULL

#this is not correct; keep has already been subset to the top 10% fires
#for (i in tt33) {
#subby<-keep[keep$ecn==i,]
#ninety<-subset(subby, FIRE_SIZE_ha >= quantile(FIRE_SIZE_ha, 0.9))
#outty<- c(i,length(ninety$FIRE_SIZE_ha),mean(ninety$FIRE_SIZE_ha),sd(ninety$FIRE_SIZE_ha),median(ninety$FIRE_SIZE_ha),sum(ninety$FIRE_SIZE_ha))
#output<-rbind(output,outty)
#}
#head(output)
###

#colnames(output) <- c("ecn", "nobs", "mean","sd","median","sum")
#row.names(output)<-NULL

#add key to this dataframe
#tt19<-unique(keep[c("ecn", "NA_L3CODE")])
#sum1<-summaryBy(data=keep, FIRE_SIZE_ha~NA_L3NAME, FUN=c(length,mean, sd, median,min, max))
#sum1
#fff<-merge(tt20,tt19,by="ecn")
#head(fff)

#output table; this is Table S1
#write.table(sum1, "results/firehasum_ecn_top_ten_Short_update.csv", sep=",", row.names=FALSE, append=FALSE)


#####################################
#calculate % human by ecoregion for just top 10% largest fires
#use this to make Figure 1 

#summary statistics on each subset (human and lightning)
#human subset of large fires only
#head(keep)
#outputh=NULL

#for (i in tt33) {
#subh<-keep[keep$IGNITION=="Human",]
#subbyh<-subh[subh$ecn==i,]
#ninetyh<-subset(subbyh, FIRE_SIZE_ha >= quantile(FIRE_SIZE_ha, 0.9))
#outtyh<- c(i,length(ninetyh$FIRE_SIZE_ha),mean(ninetyh$FIRE_SIZE_ha),sd(ninetyh$FIRE_SIZE_ha),median(ninetyh$FIRE_SIZE_ha),sum(ninetyh$FIRE_SIZE_ha))
#outputh<-rbind(outputh,outtyh)
#}

#colnames(outputh) <- c("ecn", "hnobs", "hmean","hsd","hmedian","hsum")
#row.names(outputh)<-NULL
#head(outputh)

#summary(hmean)

#lightning subset of large fires only
#outputl=NULL

#for (i in tt33) {
#subl<-keep[keep$IGNITION=="Lightning",]
#subbyl<-subl[subl$ecn==i,]
#ninetyl<-subset(subbyl, FIRE_SIZE_ha >= quantile(FIRE_SIZE_ha, 0.9))
#outtyl<- c(i,length(ninetyl$FIRE_SIZE_ha),mean(ninetyl$FIRE_SIZE_ha),sd(ninetyl$FIRE_SIZE_ha),median(ninetyl$FIRE_SIZE_ha),sum(ninetyl$FIRE_SIZE_ha))
#outputl<-rbind(outputl,outtyl)
#}

#colnames(outputl) <- c("ecn", "lnobs", "lmean","lsd","lmedian","lsum")
#row.names(outputl)<-NULL
#head(outputl)


#merge two dataframes together
#output2<-merge(outputh,outputl,by="ecn")

#calculate the total number of fires of all ignitions
#output2$totfires<-output2$hnobs+output2$lnobs
#output2

#calculate the percent of human ignitions by ecoregion
#output2$perh<-output2$hnobs/output2$totfires*100

#put NA_L3CODE back in to join easily in Arc
#tt9<-unique(subz[c("ecn", "NA_L3CODE")])
#jjj<-left_join(output2,tt9,by="ecn")
#jjj
#output table; used to make Figure 1
#write.table(jjj, "results/firehasum_ecn_top_ten_Short_update_hl.csv", sep=",", row.names=FALSE, append=FALSE)

#what is the range of percent human started fires by ecoregion?
#summary(jjj$perh)
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
