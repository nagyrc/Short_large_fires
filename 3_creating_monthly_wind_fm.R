# Helper functions --------------------------------------------------------

netcdf_import <- function(file) {
  file_split <- file %>%
    basename %>%
    strsplit(split = "_") %>%
    unlist
  var <- file_split[1]
  year <- substr(file_split[2], start = 1, stop = 4)
  
  start_date <- as.Date(paste(year, "01", "01", sep = "-"))
  end_date <- as.Date(paste(year, "12", "31", sep = "-"))
  date_seq <- seq(start_date, end_date, by = "1 day") %m+% years(1)
  
  nc <- nc_open(tmp_dl[1])
  nc_att <- attributes(nc$var)$names
  ncvar <- ncvar_get(nc, nc_att)
  rm(nc)
  proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  rbrck <- brick(ncvar, crs= proj)
  rm(ncvar)
  extent(rbrck) <- c(-124.793, -67.043, 25.04186, 49.41686)
  names(rbrck) <- paste(var, unique(date_seq),
                        sep = "-")
  return(rbrck)
}      

step1 <- function(x, mask, fun.a) {
  file_split <- x %>%
    basename %>%
    strsplit(split = "_") %>%
    unlist
  var <- file_split[1]
  year <- substr(file_split[2], start = 1, stop = 4)
  
  # if (as.numeric(year) < 1992) {
  #   return("Year outside range of consideration")
  # }
  # 
  # if (as.numeric(year) > 2015) {
  #   return("Year outside range of consideration")
  # }
  
  start_date <- as.Date(paste(year, "01", "01", sep = "-"))
  end_date <- as.Date(paste(year, "12", "31", sep = "-"))
  date_seq <- seq(start_date, end_date, by = "1 day") %m+% years(1)
  monthly_seq <- seq(start_date, end_date, by = "1 month")
  
  proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  rbrck <- brick(x, crs= proj)
  rbrck <- stackApply(rbrck, month(date_seq), fun = fun.a)
  rbrck <- flip(t(rbrck), direction = "x")
  rbrck <- mask(rbrck, mask)
  
  names(rbrck) <- paste(var, unique(year(monthly_seq)), 
                        ifelse(nchar(unique(month(monthly_seq))) == 1, 
                               paste0("0", unique(month(monthly_seq))), 
                               unique(month(monthly_seq))),
                        unique(month(monthly_seq, label = TRUE)),
                        sep = "_")
  
  return(rbrck)
}  

step2 <- function(x, start, end, var) {
  
  start_date <- as.Date(paste(start, "01", "01", sep = "-"))
  end_date <- as.Date(paste(end, "12", "31", sep = "-"))
  monthly_seq <- month(seq(start_date, end_date, by = "1 month"))
  
  normals <- stackApply(x, indices = monthly_seq, fun = mean)
  names(normals) <- paste(var, unique(month(monthly_seq, label = TRUE)),
                          sep = "_")
  
  dir.create(paste0(dir, "Short_Update/", dir_proc,  var,  "/"), showWarnings = FALSE)
  out <- paste0(dir, "Short_Update/", dir_proc,  var,  "/")
  writeRaster(normals, filename = paste0(out,names(normals)),
              format = "GTiff", bylayer=TRUE, overwrite = TRUE)
}


# Produce the monthly normals --> Wind -------------------------------------------------------------
# This task only needs to be run 1 time 
# After this is ran then you will have produced the monthly normals from 1992-2015
cl <- makeCluster(UseCores)

# Step 1
wind_dl <- list.files(paste0(dir, "Short_Update/data/climate/windsp"), pattern = "nc", full.names = TRUE)

mask <- st_transform(usa_shp, proj_ll)

# Create monthly means to raster list

# Not parallelized
#windsp <- lapply(wind_dl, step1, mask = as(mask, "Spatial"), fun.a = mean)

# Parallelized
windsp <- foreach(i = 1:length(wind_dl)) %dopar% {
  step1(wind_dl[i], as(mask, "Spatial"), mean)}
stopCluster(cl)

# Stack to master brick
windsp <- do.call(stack,windsp)

# Step 2
# Calculate normals and write out to GeoTif
# The variable windsp will be a usable raster stack
windsp <- step2(windsp, "1979", "1981", "tmp")

# Produce the monthly normals -->  Fuel Moisture -------------------------------------------------------------
# This task only needs to be run 1 time 
# After this is ran then you will have produced the monthly normals from 1992-2015
cl <- makeCluster(UseCores)

# Step 1
fm_dl <- list.files(paste0(dir, "Short_Update/data/veg/fm100"), pattern = "nc", full.names = TRUE)

# Create monthly means to raster list

# Not parallelized
#fm <- lapply(fm_dl, step1, mask = as(mask, "Spatial"), fun.a = mean)

# Parallelized
fm <- foreach(i = 1:length(fm_dl)) %dopar% {
  step1(fm_dl[i], as(mask, "Spatial"), mean)}
stopCluster(cl)

# Stack to master brick
fm <- do.call(stack,fm)

# Step 2
# Calculate normals and write out to GeoTif
# The variable fm will be a usable raster stack
fm <- step2(fm, "1979", "1981", "tmp")

# Extract normals LOOPS NOT WORKING------------------------------------------------

extract_function <- function(y, x){
  file_split <- y %>%
    basename %>%
    strsplit(split = "_") %>%
    unlist
  var <- file_split[1]
  
  rstb <- stack()
  for(i in 1:NROW(y)){
    tempraster <- raster(y[i])
    rstb <- stack(rstb,tempraster)
  }
  
  # shrt_wind <- list()
  # for(i in 1:nrow(x)){
  #   if(x$DISCOVERY_MONTH > 0) {
  #     raster::extract(rstb, as(x[i], "Spatial"), sp = TRUE)
  #   } 
  extracList <- vector("list", length(x)) 
  cnt <- 0 
  for(i in 1:NROW(x)){
    cnt <- cnt + 1 
    if(x$DISCOVERY_MONTH > 1) {
      extracList[[cnt]] <- raster::extract(rstb, as(x[i], "Spatial"), sp = TRUE)
    } 
    else {
      stop("Didn't work sucka")
    }
  }
}

extract_function <- function(y, x){
  file_split <- y %>%
    basename %>%
    strsplit(split = "_") %>%
    unlist
  var <- file_split[1]
  
  rstb <- stack()
  for(i in 1:NROW(y)){
    tempraster <- raster(y[i])
    rstb <- stack(rstb,tempraster)
  }
  
  extracList <- vector("list", length(x)) 
  cnt <- 0 
  for(i in 1:NROW(x)){
    cnt <- cnt + 1 
    if(x$DISCOVERY_MONTH > 1) {
      extracList[[cnt]] <- raster::extract(rstb, as(x[i], "Spatial"), sp = TRUE)
    } 
    else {
      stop("Didn't work sucka")
    }
  }
  return(extracList)
}