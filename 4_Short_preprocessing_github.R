#this code is part of the Short_large_fires project by Dr. R. Chelsea Nagy
# Here we import, project, intersect, organize data layers
# Key layers are the Short ignitions, level 3 ecoregions, mean monthly wind speed, mean monthly fuel moisture, biomass, and biophysical setting

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(gridExtra)
library(raster)
library(rgdal)
library(sf)
library(lubridate)
library(ncdf4)
library(doParallel)
library(foreach) 

# To be used in the parallelized sections of the code
UseCores <- detectCores() -1

# set projections
#EPSG:102003 USA_Contiguous_Albers_Equal_Area_Conic
proj_ea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

#EPSG:102005 USA_Contiguous_Equidistant_Conic
proj_ed <- "+proj=eqdc +lat_0=39 +lon_0=-96 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

#WGS 84 the gridmet projection
proj_ll <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


# import data layers -------------------------------------------------------

#Import the USA States layer
#usa_shp <- st_read(dsn = paste0("data/raw/conus"),
                   #layer = "cb_2016_us_state_20m", quiet= TRUE) %>%
  #st_transform(., proj_ea) %>%
  #subset(., NAME != "Alaska" &
           #NAME != "Hawaii" &
           #NAME != "Puerto Rico") %>%
  #mutate(area_m2 = as.numeric(st_area(geometry)),
         #StArea_km2 = area_m2/1000000,
         #group = 1) %>%
  #st_simplify(., preserveTopology = TRUE) 
#plot(usa_shp[5])

# Dissolve to the USA Boundary
#conus <- usa_shp %>%
  #group_by(group) %>%
  #st_union()
#plot(conus)

# Import the Level 3 Ecoregions
eco = paste0("data/raw/us_eco_l3")
ecoreg <- st_read(dsn = eco, layer = "us_eco_l3", quiet= TRUE) %>%
  st_transform(., proj_ea) %>%
  st_simplify(., preserveTopology = TRUE, dTolerance = 1000) %>%
  mutate(area_m2 = as.numeric(st_area(geometry)),
         EcoArea_km2 = area_m2/1000000)
plot(ecoreg[2])

# Intersects states with ecoregions
#state_eco <- st_intersection(usa_shp, ecoreg) %>%
  #dplyr::select(STUSPS, NAME, StArea_km2, US_L3CODE, US_L3NAME, EcoArea_km2, NA_L2NAME, NA_L1CODE, NA_L1NAME, geometry)
#plot(state_eco[2])


# Read the FPA (Short) database class
shrt_fire <- st_read(dsn = paste0("data/raw/fpa-fod/Data/FPA_FOD_20170508.gdb"),
                     layer = "Fires", quiet= TRUE) %>%
  st_transform(., proj_ea) %>%
  filter(STATE != "AK" & STATE != "PR" & STATE != "HI" & FIRE_SIZE >= 0.01) %>%
  dplyr::select(FPA_ID, LATITUDE, LONGITUDE, ICS_209_INCIDENT_NUMBER, ICS_209_NAME, MTBS_ID, MTBS_FIRE_NAME,
                FIRE_YEAR, DISCOVERY_DATE, DISCOVERY_DOY, STAT_CAUSE_DESCR, FIRE_SIZE, STATE) %>%
  mutate(IGNITION = ifelse(STAT_CAUSE_DESCR == "Lightning", "Lightning", "Human"),
         FIRE_SIZE_m2 = FIRE_SIZE*4046.86,
         FIRE_SIZE_km2 = FIRE_SIZE_m2/1000000,
         FIRE_SIZE_ha = FIRE_SIZE_m2*10000,
         DISCOVERY_DAY = day(DISCOVERY_DATE),
         DISCOVERY_MONTH = month(DISCOVERY_DATE),
         DISCOVERY_YEAR = FIRE_YEAR)

# add a column to indicate whether the fire id (FPA_ID) is duplicated
shrt_fire <- shrt_fire %>%
  mutate(is_id_duplicated = duplicated(FPA_ID))

stopifnot(sum(shrt_fire$is_id_duplicated) == 3)

# ensure that duplicated fire ids get modified to be unique
shrt_fire <- shrt_fire %>%
  mutate(row_id = 1:n(), 
         clean_id = ifelse(is_id_duplicated, 
                           paste(FPA_ID, row_id, sep = "_"), 
                           FPA_ID)) %>%
  dplyr::select(-row_id)

# verify that clean_id is a unique identifier (no repeats)
stopifnot(!any(duplicated(shrt_fire$clean_id)))


#Extract average monthly wind data to Short ------------------------------------------------
#Bring in average monthly wind data

wind_dl <- list.files(paste0("data/climate/ws/ws/"), pattern = "tif", full.names = TRUE)

wind <- lapply(wind_dl, raster) 

shrt_jan <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "1") %>%
  st_transform(., proj_ll)
shrt_jan <- as(shrt_jan, "Spatial")
wind_jan <- raster::extract(wind[[5]],
                            shrt_jan, sp = TRUE) 
wind_jan <- st_as_sf(wind_jan) %>%
  mutate(Wind = vs_Jan) %>%
  dplyr::select(-starts_with("vs_"))

shrt_feb <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "2") %>%
  st_transform(., proj_ll)
shrt_feb <- as(shrt_feb, "Spatial")
wind_feb <- raster::extract(wind[[4]],
                            shrt_feb, sp = TRUE) 
wind_feb <- st_as_sf(wind_feb) %>%
  mutate(Wind = vs_Feb) %>%
  dplyr::select(-starts_with("vs_"))

shrt_mar <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "3") %>%
  st_transform(., proj_ll)
shrt_mar <- as(shrt_mar, "Spatial")
wind_mar <- raster::extract(wind[[8]],
                            shrt_mar, sp = TRUE) 
wind_mar <- st_as_sf(wind_mar) %>%
  mutate(Wind = vs_Mar) %>%
  dplyr::select(-starts_with("vs_"))

shrt_apr <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "4") %>%
  st_transform(., proj_ll)
shrt_apr <- as(shrt_apr, "Spatial")
wind_apr <- raster::extract(wind[[1]],
                            shrt_apr, sp = TRUE) 
wind_apr <- st_as_sf(wind_apr) %>%
  mutate(Wind = vs_Apr) %>%
  dplyr::select(-starts_with("vs_"))

shrt_may <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "5") %>%
  st_transform(., proj_ll)
shrt_may <- as(shrt_may, "Spatial")
wind_may <- raster::extract(wind[[9]],
                            shrt_may, sp = TRUE) 
wind_may <- st_as_sf(wind_may) %>%
  mutate(Wind = vs_May) %>%
  dplyr::select(-starts_with("fm100_"))

shrt_jun <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "6") %>%
  st_transform(., proj_ll)
shrt_jun <- as(shrt_jun, "Spatial")
wind_jun <- raster::extract(wind[[7]],
                            shrt_jun, sp = TRUE) 
wind_jun <- st_as_sf(wind_jun) %>%
  mutate(Wind = vs_Jun) %>%
  dplyr::select(-starts_with("vs_"))

shrt_jul <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "7") %>%
  st_transform(., proj_ll)
shrt_jul <- as(shrt_jul, "Spatial")
wind_jul <- raster::extract(wind[[6]],
                            shrt_jul, sp = TRUE) 
wind_jul <- st_as_sf(wind_jul) %>%
  mutate(Wind = vs_Jul) %>%
  dplyr::select(-starts_with("vs_"))

shrt_aug <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "8") %>%
  st_transform(., proj_ll)
shrt_aug <- as(shrt_aug, "Spatial")
wind_aug <- raster::extract(wind[[2]],
                            shrt_aug, sp = TRUE) 
wind_aug <- st_as_sf(wind_aug) %>%
  mutate(Wind = vs_Aug) %>%
  dplyr::select(-starts_with("vs_"))

shrt_sep <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "9") %>%
  st_transform(., proj_ll)
shrt_sep <- as(shrt_sep, "Spatial")
wind_sep <- raster::extract(wind[[12]],
                            shrt_sep, sp = TRUE) 
wind_sep <- st_as_sf(wind_sep) %>%
  mutate(Wind = vs_Sep) %>%
  dplyr::select(-starts_with("vs_"))

shrt_oct <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "10") %>%
  st_transform(., proj_ll)
shrt_oct <- as(shrt_oct, "Spatial")
wind_oct <- raster::extract(wind[[11]],
                            shrt_oct, sp = TRUE) 
wind_oct <- st_as_sf(wind_oct) %>%
  mutate(Wind = vs_Oct) %>%
  dplyr::select(-starts_with("vs_"))

shrt_nov <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "11") %>%
  st_transform(., proj_ll)
shrt_nov <- as(shrt_nov, "Spatial")
wind_nov <- raster::extract(wind[[10]],
                            shrt_nov, sp = TRUE) 
wind_nov <- st_as_sf(wind_nov) %>%
  mutate(Wind = vs_Nov) %>%
  dplyr::select(-starts_with("vs_"))

shrt_dec <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "12") %>%
  st_transform(., proj_ll)
shrt_dec <- as(shrt_dec, "Spatial")
wind_dec <- raster::extract(wind[[3]],
                            shrt_dec, sp = TRUE) 
wind_dec <- st_as_sf(wind_dec) %>%
  mutate(Wind = vs_Dec) %>%
  dplyr::select(-starts_with("vs_"))

shrt_wind <- wind_jan %>%
  bind_rows(., wind_feb) %>%
  bind_rows(., wind_mar) %>%
  bind_rows(., wind_apr) %>%
  bind_rows(., wind_may) %>%
  bind_rows(., wind_jun) %>%
  bind_rows(., wind_jul) %>%
  bind_rows(., wind_aug) %>%
  bind_rows(., wind_sep) %>%
  bind_rows(., wind_oct) %>%
  bind_rows(., wind_nov) %>%
  bind_rows(., wind_dec)

str(shrt_wind)

#make into a data frame
shrt_wind_df <- as.data.frame(shrt_wind) %>%
  dplyr::select("clean_id", "Wind")


#Extract average monthly fuel moisture data to Short ------------------------------------------------
#Bring in average monthly fuel moisture data
fm_dl <- list.files(paste0("data/climate/fm/fm100/"), pattern = "tif", full.names = TRUE)

fm <- lapply(fm_dl, raster) 

shrt_jan <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "1") %>%
  st_transform(., proj_ll)
shrt_jan <- as(shrt_jan, "Spatial")
fm_jan <- raster::extract(fm[[5]],
                          shrt_jan, sp = TRUE) 
fm_jan <- st_as_sf(fm_jan) %>%
  mutate(fm = fm100_Jan) %>%
  dplyr::select(-starts_with("fm100"))


shrt_feb <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "2") %>%
  st_transform(., proj_ll)
shrt_feb <- as(shrt_feb, "Spatial")
fm_feb <- raster::extract(fm[[4]],
                          shrt_feb, sp = TRUE) 
fm_feb <- st_as_sf(fm_feb) %>%
  mutate(fm = fm100_Feb) %>%
  dplyr::select(-starts_with("fm100"))

shrt_mar <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "3") %>%
  st_transform(., proj_ll)
shrt_mar <- as(shrt_mar, "Spatial")
fm_mar <- raster::extract(fm[[8]],
                          shrt_mar, sp = TRUE) 
fm_mar <- st_as_sf(fm_mar) %>%
  mutate(fm = fm100_Mar) %>%
  dplyr::select(-starts_with("fm100"))

shrt_apr <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "4") %>%
  st_transform(., proj_ll)
shrt_apr <- as(shrt_apr, "Spatial")
fm_apr <- raster::extract(fm[[1]],
                          shrt_apr, sp = TRUE) 
fm_apr <- st_as_sf(fm_apr) %>%
  mutate(fm = fm100_Apr) %>%
  dplyr::select(-starts_with("fm100_"))

shrt_may <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "5") %>%
  st_transform(., proj_ll)
shrt_may <- as(shrt_may, "Spatial")
fm_may <- raster::extract(fm[[9]],
                          shrt_may, sp = TRUE) 
fm_may <- st_as_sf(fm_may) %>%
  mutate(fm = fm100_May) %>%
  dplyr::select(-starts_with("fm100_"))

shrt_jun <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "6") %>%
  st_transform(., proj_ll)
shrt_jun <- as(shrt_jun, "Spatial")
fm_jun <- raster::extract(fm[[7]],
                          shrt_jun, sp = TRUE) 
fm_jun <- st_as_sf(fm_jun) %>%
  mutate(fm = fm100_Jun) %>%
  dplyr::select(-starts_with("fm100_"))

shrt_jul <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "7") %>%
  st_transform(., proj_ll)
shrt_jul <- as(shrt_jul, "Spatial")
fm_jul <- raster::extract(fm[[6]],
                          shrt_jul, sp = TRUE) 
fm_jul <- st_as_sf(fm_jul) %>%
  mutate(fm = fm100_Jul) %>%
  dplyr::select(-starts_with("fm100_"))

shrt_aug <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "8") %>%
  st_transform(., proj_ll)
shrt_aug <- as(shrt_aug, "Spatial")
fm_aug <- raster::extract(fm[[2]],
                          shrt_aug, sp = TRUE) 
fm_aug <- st_as_sf(fm_aug) %>%
  mutate(fm = fm100_Aug) %>%
  dplyr::select(-starts_with("fm100_"))

shrt_sep <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "9") %>%
  st_transform(., proj_ll)
shrt_sep <- as(shrt_sep, "Spatial")
fm_sep <- raster::extract(fm[[12]],
                          shrt_sep, sp = TRUE) 
fm_sep <- st_as_sf(fm_sep) %>%
  mutate(fm = fm100_Sep) %>%
  dplyr::select(-starts_with("fm100_"))

shrt_oct <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "10") %>%
  st_transform(., proj_ll)
shrt_oct <- as(shrt_oct, "Spatial")
fm_oct <- raster::extract(fm[[11]],
                          shrt_oct, sp = TRUE) 
fm_oct <- st_as_sf(fm_oct) %>%
  mutate(fm = fm100_Oct) %>%
  dplyr::select(-starts_with("fm100_"))

shrt_nov <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "11") %>%
  st_transform(., proj_ll)
shrt_nov <- as(shrt_nov, "Spatial")
fm_nov <- raster::extract(fm[[10]],
                          shrt_nov, sp = TRUE) 
fm_nov <- st_as_sf(fm_nov) %>%
  mutate(fm = fm100_Nov) %>%
  dplyr::select(-starts_with("fm100_"))

shrt_dec <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "12") %>%
  st_transform(., proj_ll)
shrt_dec <- as(shrt_dec, "Spatial")
fm_dec <- raster::extract(fm[[3]],
                          shrt_dec, sp = TRUE) 
fm_dec <- st_as_sf(fm_dec) %>%
  mutate(fm = fm100_Dec) %>%
  dplyr::select(-starts_with("fm100_"))

shrt_fm <- fm_jan %>%
  bind_rows(., fm_feb) %>%
  bind_rows(., fm_mar) %>%
  bind_rows(., fm_apr) %>%
  bind_rows(., fm_may) %>%
  bind_rows(., fm_jun) %>%
  bind_rows(., fm_jul) %>%
  bind_rows(., fm_aug) %>%
  bind_rows(., fm_sep) %>%
  bind_rows(., fm_oct) %>%
  bind_rows(., fm_nov) %>%
  bind_rows(., fm_dec) 

#make into a dataframe
shrt_fm_df <- as.data.frame(shrt_fm) 
# %>% dplyr::select("FPA_ID", "fm", "clean_id", "NA_L3NAME")


#merge wind, fm, Short
shrt_wind_fm <- left_join(shrt_wind_df, shrt_fm_df, by = "clean_id")
head(shrt_wind_fm)

##################################
# Import biomass data ----------------------------------------------------
bio <- raster(paste0("data/raw/NBCD_countrywide_biomass_mosaic/NBCD_countrywide_biomass_mosaic.tif"))

# extract biomass to short data
shrt_bio <- raster::extract(bio, as(shrt_fire, "Spatial"), sp = TRUE)

#convert to dataframe
shrt_bio_df <-as.data.frame(shrt_bio) %>% 
  dplyr::select("clean_id", "NBCD_countrywide_biomass_mosaic")

#join with shrt_wind_fm
shrt_clim_bio <- left_join(shrt_wind_fm, shrt_bio_df, by = "clean_id")

###
#output in case bps crashes it
#this did not work
write.table(shrt_clim_bio, "data/merged/shrt_clim_bio.csv", sep=",", row.names=FALSE)
#this did not work
library(maptools)
writeSpatialShape(shrt_clim_bio, "data/merged/shrt_clim_bio.shp")
###


#Add ecoregion


# Import biophysical setting ---------------------------------------------
bps.ref <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
bps <- raster(paste0("data/raw/us_130bps/grid/us_130bps"))
#bps_trans <- st_transform(bps, proj_ea)

#Nate's code
shrt_bps <- shrt_fire %>%
  st_transform(., bps.ref)
shrt_bps <- raster::extract(bps, as(shrt_bps, "Spatial"), sp = TRUE)
shrt_bps <- st_transform(shrt_bps, proj_ea)

#My code
#extract bps to short data
shrt_bps <- raster::extract(bps_trans, as(shrt_fire, "Spatial"), sp = TRUE)
#convert to dataframe
shrt_bps_df <-as.data.frame(shrt_bps) %>% 
  dplyr::select("clean_id", "NBCD_countrywide_biomass_mosaic")








######################
#need to join shrt_veg , shrt_wind, shrt_fm plus ecoreg to one master dataset with all extracted variables
#start with shrt_wind and shrt_fm
#try left_join.sf
#this did not work
shrt_wind_fm <- left_join.sf(shrt_wind, shrt_fm_df, by = "FPA_ID")
shrt_wind_fm <- left_join.sf(shrt_wind, shrt_fm_df, by = "FPA_ID")

# check for NA values in the input data frames
mean(is.na(shrt_wind_df$Wind))
mean(is.na(shrt_fm_df$fm))

# convert the sf object to a data frame, then try to merge
merged <- shrt_wind_df %>%
  left_join(shrt_fm_df)

View(merged)

any(duplicated(merged$FPA_ID))
any(duplicated(shrt_wind_df$FPA_ID))
any(duplicated(shrt_fm_df$FPA_ID))

shrt_wind_df[duplicated(shrt_wind_df$FPA_ID), ]

shrt_wind_df %>%
  filter(FPA_ID == "ICS209_2009_KS-DDQ-128")

merged %>%
  filter(FPA_ID == "ICS209_2009_KS-DDQ-128")


all(unique(shrt_wind_df$FPA_ID) == unique(shrt_fm_df$FPA_ID))
all(levels(shrt_wind$FPA_ID) == levels(shrt_fm_df$FPA_ID))



#try st_join
#this did not work
shrt_clm<-st_join(shrt_wind, shrt_fm_df, left = TRUE, by = "FPA_ID")
shrt_clm<-st_join(shrt_wind, shrt_fm, left = TRUE)
shrt_clm<-st_join(shrt_wind_df, shrt_fm_df, left = TRUE, by = "FPA_ID")
######################






# Subset the FPA data to large fires (90th%tile) ---------------------------------------
cl <- makeCluster(UseCores)
lrg_shrt_fire <- foreach(i = 1:NROW(shrt_fire)) %dopar% {
  st_intersection(shrt_fire[i], ecoreg)} das
stopCluster(cl)

tt3<-unique(lrg_shrt_fire$L3CODE)
output=NULL
for (i in tt3) {
  subby<-shrt_fire[shrt_fire$L3CODE==i,]
  ninety<-subset(subby, FIRE_SIZE_ha >= quantile(FIRE_SIZE_ha, 0.90))
  output<-rbind(output,data.frame(ninety[,]))
}
