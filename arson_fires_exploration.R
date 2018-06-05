#this code is part of the Short_large_fires project by Dr. R. Chelsea Nagy
#Arson exploration

library(sf)
library(doBy)
library(tidyverse)

#open large fires
lrg_fires<- read.csv("data/merged/lrg_fires.csv")
head(lrg_fires)

#make new column of ecoregion number to easily index
#lrg_fires$ecn<-as.character(gsub("\\.","",lrg_fires$NA_L3CODE))
#lrg_fires$ecn<-as.numeric(lrg_fires$ecn)
#head(lrg_fires)

#tt3333<-unique(lrg_fires$ecn)

#update FIRE_SIZE_ha here
lrg_fires$FIRE_SIZE_ha<-lrg_fires$FIRE_SIZE_m2*0.0001

#how many fires are caused by Arson?
summary(lrg_fires$STAT_CAUSE_DESCR)
#Arson =42266 fires


###########################
arson <- lrg_fires %>%
  filter(STAT_CAUSE_DESCR=="Arson") 

#%>%
  #group_by(DISCOVERY_DOY) %>%
  #summarise(n_fires = n(),
            #mean_size = mean(FIRE_SIZE_ha))

arson_doy <- arson %>%
  group_by(DISCOVERY_DOY) %>%
  summarise(n_fires = n(),
            mean_size = mean(FIRE_SIZE_ha))

arson_doy %>%
  ggplot() +
  geom_bar(aes(x = DISCOVERY_DOY,  y = n_fires), stat = "identity")
###########################

#subset just the arson fires
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

r7777 <- summaryBy(clean_id~NA_L3CODE+NA_L3NAME, data=arson, FUN=length)
r7777

#remove row numbers
#row.names(r7777) <- NULL

#then order
r7777o <-orderBy(~clean_id.length,r7777)
r7777o

#mean size of arson fires by DOY 
r5555 <- summaryBy(FIRE_SIZE_ha~DISCOVERY_DOY, data=arson, FUN=mean)
r5555

plot(x=r5555$DISCOVERY_DOY, y=r5555$FIRE_SIZE_ha.mean)

#mean size of arson fires by ecoregion
r6666 <- summaryBy(FIRE_SIZE_ha~NA_L3CODE, data=arson, FUN=mean)
r6666

# Import the Level 3 Ecoregions

# set projections
#EPSG:102003 USA_Contiguous_Albers_Equal_Area_Conic
proj_ea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

eco = paste0("data/raw/us_eco_l3")
ecoreg <- st_read(dsn = eco, layer = "us_eco_l3", quiet= TRUE) %>%
  st_transform(., proj_ea) %>%
  st_simplify(., preserveTopology = TRUE, dTolerance = 1000) %>%
  mutate(area_m2 = as.numeric(st_area(geometry)),
         EcoArea_km2 = area_m2/1000000)

#plot mean fire size and number of fires by ecoregion
colnames(r6666)[2] <- "arson_fire_size_ha"
ecoreg_arson1 <- left_join(ecoreg,r6666, by="NA_L3CODE")

colnames(r4444)[2] <- "num_arson_fires"
ecoreg_arson2 <- left_join(ecoreg_arson1,r4444, by="NA_L3CODE")

head(ecoreg_arson2)

plot(ecoreg_arson2["num_arson_fires"])
plot(ecoreg_arson2["arson_fire_size_ha"])

