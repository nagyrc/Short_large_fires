#this code is part of the Short_large_fires project by Dr. R. Chelsea Nagy
#Arson exploration


#open large fires
lrg_fires<- read.csv("data/merged/lrg_fires.csv")
head(lrg_fires)

#make new column of ecoregion number to easily index
lrg_fires$ecn<-as.character(gsub("\\.","",lrg_fires$NA_L3CODE))
lrg_fires$ecn<-as.numeric(lrg_fires$ecn)
head(lrg_fires)

#update FIRE_SIZE_ha here
lrg_fires$FIRE_SIZE_ha<-lrg_fires$FIRE_SIZE_m2*0.0001

how many fires are caused by Arson?
summary(lrg_fires$STAT_CAUSE_DESCR)
#Arson =42266 fires

tt3333<-unique(lrg_fires$ecn)

arson<-lrg_fires[which(lrg_fires$STAT_CAUSE_DESCR=="Arson"),]
#keep has 42266 fires
head(arson)

#number of arson fires by DOY
r3333 <- summaryBy(clean_id~DISCOVERY_DOY, data=arson, FUN=length)
r3333

plot(x=r3333$DISCOVERY_DOY, y=r3333$clean_id.length)

#number of arson fires by ecoregion
r4444 <- summaryBy(clean_id~NA_L3CODE, data=arson, FUN=length)
r4444

#mean size of arson fires by DOY 
r5555 <- summaryBy(FIRE_SIZE_ha~DISCOVERY_DOY, data=arson, FUN=mean)
r5555

plot(x=r5555$DISCOVERY_DOY, y=r5555$FIRE_SIZE_ha.mean)

#mean size of arson fires by ecoregion
r6666 <- summaryBy(FIRE_SIZE_ha~NA_L3CODE, data=arson, FUN=mean)
