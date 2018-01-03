# Libraries ---------------------------------------------------------------
x <- c("data.table", "tidyverse", "magrittr", "sf", "gridExtra", "rgdal", "raster", "rgeos", "data.table",
       "assertthat", "purrr", "httr", "rvest", "lubridate", "parallel", "sp", "RColorBrewer", "ggmap")
lapply(x, library, character.only = TRUE, verbose = FALSE)

#source("src/functions/helper_functions.R")
#source("src/functions/make_grid.R")

# Raw data folders
prefix <- "data"
raw_prefix <- file.path(prefix, "raw")
ecoregion_prefix <- file.path(raw_prefix, "us_eco_l3")
fpa_prefix <- file.path(raw_prefix, "fpa-fod")
us_prefix <- file.path(raw_prefix, "conus")
biomass_prefix <- file.path(raw_prefix, "NBCD_countrywide_biomass_mosaic")
bps_prefix <- file.path(raw_prefix, "us_130bps")
ws_prefix <- file.path(raw_prefix, "ws")
fm_prefix <- file.path(raw_prefix, "fm100")

# Cleaned data output folders
bounds_crt <- file.path(prefix, "bounds")
conus_crt <- file.path(bounds_crt, "conus")
ecoreg_crt <- file.path(bounds_crt, "ecoregion")
fire_crt <- file.path(prefix, "fire")
bio_crt <- file.path(prefix, "bio")
biomass_crt <- file.path(bio_crt, "biomass")
bps_crt <- file.path(bio_crt, "bps")
climate_crt <- file.path(prefix, "climate")
ws_crt <- file.path(climate_crt, "ws")
fm_crt <- file.path(climate_crt, "fm")

us_out <- file.path(conus_crt, "cb_2016_us_state_20m")
ecoregion_out <- file.path(ecoreg_crt, "us_eco_l3")
fpa_out <- file.path(fire_crt, "fpa-fod")
biomass_out <- file.path(biomass_crt, "NBCD_countrywide_biomass_mosaic")
bps_out <- file.path(bps_crt, "us_130bps")
ws_out <- file.path(ws_crt, "ws")
fm_out <- file.path(fm_crt, "fm100")

# Check if directory exists for all variable aggregate outputs, if not then create
var_dir <- list(prefix, raw_prefix, ecoregion_prefix, fpa_prefix, conus_prefix, biomass_prefix, bps_prefix, fm_prefix, ws_prefix,
                bounds_crt, conus_crt, ecoreg_crt, fire_crt, bio_crt, biomass_crt, bps_crt, climate_crt, ws_crt, fm_crt,
                us_out, ecoregion_out, fpa_out, biomass_out, bps_out, ws_out, fm_out)
lapply(var_dir, function(x) if(!dir.exists(x)) dir.create(x, showWarnings = FALSE))
