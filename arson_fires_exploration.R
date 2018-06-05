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
#subset just the arson fires
arson <- lrg_fires %>%
  filter(STAT_CAUSE_DESCR=="Arson") 
#keep has 42266 fires

#number of and mean size of arson fires by day of year
arson_doy <- arson %>%
  group_by(DISCOVERY_DOY) %>%
  summarise(n_fires = n(),
            mean_size = mean(FIRE_SIZE_ha))

#this is cool; most Arson fires in Feb-Mar
arson_doy %>%
  ggplot() +
  geom_bar(aes(x = DISCOVERY_DOY,  y = n_fires), stat = "identity")

#not super informative
arson_doy %>%
  ggplot() +
  geom_bar(aes(x = DISCOVERY_DOY,  y = mean_size), stat = "identity")



###########################
#number of and mean size of arson fires by ecoregion
arson_eco <- arson %>%
  group_by(NA_L3CODE) %>%
  summarise(n_fires = n(),
            mean_size = mean(FIRE_SIZE_ha))

#then order
arson_ecoo <-orderBy(~n_fires,arson_eco)
arson_ecoo
###########################


# Import the Level 3 Ecoregions to plot

# set projections
#EPSG:102003 USA_Contiguous_Albers_Equal_Area_Conic
proj_ea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

#bring in ecoregion data
eco = paste0("data/raw/us_eco_l3")
ecoreg <- st_read(dsn = eco, layer = "us_eco_l3", quiet= TRUE) %>%
  st_transform(., proj_ea) %>%
  st_simplify(., preserveTopology = TRUE, dTolerance = 1000) %>%
  mutate(area_m2 = as.numeric(st_area(geometry)),
         EcoArea_km2 = area_m2/1000000)

#plot mean fire size and number of fires by ecoregion
ecoreg_arson <- left_join(ecoreg,arson_eco, by="NA_L3CODE")

head(ecoreg_arson)

plot(ecoreg_arson["n_fires"])
plot(ecoreg_arson["mean_size"])

