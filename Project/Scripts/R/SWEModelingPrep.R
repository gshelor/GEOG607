##### Preparing data to model SWE over CONUS for 2010 #####
### 2010 because that's what R's sample() function gave me when I asked it to pick a year out of my study time period

### loading packages
library(pacman)
p_load(tidyverse, here, sf, terra, tidyterra, paletteer)


##### Reading in Data #####
### reading in tmin
tmin_oct <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/tmin2009_OctMean.tif")
tmin_nov <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/tmin2009_NovMean.tif")
tmin_dec <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/tmin2009_DecMean.tif")
tmin_jan <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/tmin2010JanMean.tif")
tmin_feb <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/tmin2010_FebMean.tif")
tmin_mar <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/tmin2010_MarMean.tif")
tmin_apr <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/tmin2010_AprMean.tif")
tmin_may <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/tmin2010_MayMean.tif")
### reading in tmean
tmean_oct <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/tmean2009_OctMean.tif")
tmean_nov <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/tmean2009_NovMean.tif")
tmean_dec <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/tmean2009_DecMean.tif")
tmean_jan <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/tmean2010JanMean.tif")
tmean_feb <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/tmean2010_FebMean.tif")
tmean_mar <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/tmean2010_MarMean.tif")
tmean_apr <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/tmean2010_AprMean.tif")
tmean_may <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/tmean2010_MayMean.tif")
### reading in tmax
tmax_oct <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/tmax2009_OctMean.tif")
tmax_nov <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/tmax2009_NovMean.tif")
tmax_dec <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/tmax2009_DecMean.tif")
tmax_jan <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/tmax2010JanMean.tif")
tmax_feb <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/tmax2010_FebMean.tif")
tmax_mar <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/tmax2010_MarMean.tif")
tmax_apr <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/tmax2010_AprMean.tif")
tmax_may <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/tmax2010_MayMean.tif")
### reading in prcp mean
prcpMean_oct <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/prcp2009_OctMean.tif")
prcpMean_nov <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/prcp2009_NovMean.tif")
prcpMean_dec <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/prcp2009_DecMean.tif")
prcpMean_jan <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/prcp2010JanMean.tif")
prcpMean_feb <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/prcp2010_FebMean.tif")
prcpMean_mar <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/prcp2010_MarMean.tif")
prcpMean_apr <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/prcp2010_AprMean.tif")
prcpMean_may <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/prcp2010_MayMean.tif")
### reading in prcp Sums
prcpSum_oct <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/prcp2009_OctSum.tif")
prcpSum_nov <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/prcp2009_NovSum.tif")
prcpSum_dec <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/prcp2009_DecSum.tif")
prcpSum_jan <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/prcp2010_JanSum.tif")
prcpSum_feb <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/prcp2010_FebSum.tif")
prcpSum_mar <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/prcp2010_MarSum.tif")
prcpSum_apr <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/prcp2010_AprSum.tif")
prcpSum_may <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/prcp2010_MaySum.tif")
### reading in srad mean
srad_oct <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/srad2010_OctMean.tif")
srad_nov <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/srad2010_NovMean.tif")
srad_dec <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/srad2010_DecMean.tif")
srad_jan <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/srad2010JanMean.tif")
srad_feb <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/srad2010_FebMean.tif")
srad_mar <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/srad2010_MarMean.tif")
srad_apr <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/srad2010_AprMean.tif")
srad_may <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/srad2010_MayMean.tif")
### reading in landcover
landcover2010 <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Landcover/LC_CONUS_2010.tif")
### reading in tmean CDM rasters
tmean_oct_CDM <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/tmean/tmean_Oct2009_CDM.tif")
tmean_nov_CDM <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/tmean/tmean_Nov2009_CDM.tif")
tmean_dec_CDM <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/tmean/tmean_Dec2009_CDM.tif")
tmean_jan_CDM <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/tmean/tmean_Jan2010_CDM.tif")
tmean_feb_CDM <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/tmean/tmean_Feb2010_CDM.tif")
tmean_mar_CDM <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/tmean/tmean_Mar2010_CDM.tif")
tmean_apr_CDM <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/tmean/tmean_Apr2010_CDM.tif")
tmean_may_CDM <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/tmean/tmean_May2010_CDM.tif")
### reading in prcpMean CDM rasters
prcpMean_oct_CDM <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/prcp/prcpMean_Oct2009_CDM.tif")
prcpMean_nov_CDM <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/prcp/prcpMean_Nov2009_CDM.tif")
prcpMean_dec_CDM <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/prcp/prcpMean_Dec2009_CDM.tif")
prcpMean_jan_CDM <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/prcp/prcpMean_Jan2010_CDM.tif")
prcpMean_feb_CDM <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/prcp/prcpMean_Feb2010_CDM.tif")
prcpMean_mar_CDM <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/prcp/prcpMean_Mar2010_CDM.tif")
prcpMean_apr_CDM <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/prcp/prcpMean_Apr2010_CDM.tif")
prcpMean_may_CDM <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/prcp/prcpMean_May2010_CDM.tif")
### reading in prcpSum CDM rasters
prcpSum_oct_CDM <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/prcp/prcpSum_Oct2009_CDM.tif")
prcpSum_nov_CDM <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/prcp/prcpSum_Nov2009_CDM.tif")
prcpSum_dec_CDM <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/prcp/prcpSum_Dec2009_CDM.tif")
prcpSum_jan_CDM <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/prcp/prcpSum_Jan2010_CDM.tif")
prcpSum_feb_CDM <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/prcp/prcpSum_Feb2010_CDM.tif")
prcpSum_mar_CDM <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/prcp/prcpSum_Mar2010_CDM.tif")
prcpSum_apr_CDM <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/prcp/prcpSum_Apr2010_CDM.tif")
prcpSum_may_CDM <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/prcp/prcpSum_May2010_CDM.tif")

### reading in topography data
CONUS_DEM <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/DEM/CONUSDEMMosaic.tif")
CONUS_Hillshade <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/DEM/CONUSHillshade.tif")
CONUS_Slope <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/DEM/CONUSSlope.tif")
CONUS_Aspect <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/DEM/CONUSAspect.tif")

### reading in SNOTEL data
Snotel2010 <- read_sf("/Users/gshelor/Documents/GEOG607Data/Project_Data/SNOTEL/SnotelData_CONUS_PeakSWE.gpkg") |>
  filter(WaterYear == 2010) |>
  mutate(log_peak_swe = log(peak_swe), .after = "peak_swe")

### reprojecting to match rasters
Snotel2010 <- st_transform(Snotel2010, "ESRI:102039")

### reading in AOI and creating random points with value of 0 for peak_swe
### reading in AOI
CONUS_AOI <- read_sf("/Users/gshelor/Documents/GEOG607Data/Project_Data/CONUS_AOI.gpkg")
CONUS_AOI_vect <- st_transform(CONUS_AOI, "ESRI:102039") |>
  filter(US_L3NAME == "Mojave Basin and Range" | US_L3NAME == "Arizona/New Mexico Plateau" | US_L3NAME == "Chihuahuan Deserts" | US_L3NAME == "Central California Valley" | US_L3NAME == "Sonoran Basin and Range" | US_L3NAME == "Southern California/Northern Baja Coast" | US_L3NAME == "Madrean Archipelago") |>
  vect()
# plot(CONUS_AOI_vect)

char_zero_points <- readline("How many zero points should be randomly generated? ")
num_zero_points <- as.numeric(char_zero_points)
if (num_zero_points == 0){
  print("no zero points, real data only")
} else{
  set.seed(802)
  Zero_points <- spatSample(CONUS_AOI_vect, size = num_zero_points)
  # points(Zero_points)
  # points(vect(Snotel2010), col = "blue")
  
  Zero_pts_sf <- st_as_sf(Zero_points) |>
    select(EPA_REGION, STATE_NAME)
  
  colnames(Zero_pts_sf) <- c("WaterYear", "state", "geometry")
  st_geometry(Zero_pts_sf) <- "geom"
  Zero_pts_sf <- Zero_pts_sf |>
    mutate(peak_swe = 0,
           log_peak_swe = -100,
           site_id = -999,
           site_name = "zero values",
           description = "randomly generated points to be assigned 0 for peak SWE",
           .before = "state") |>
    mutate(start = min(Snotel2010$start),
           end = max(Snotel2010$end),
           .after = "state")
  
  ### binding zero points to Actual SNOTEL data
  Snotel2010 <- rbind(Snotel2010, Zero_pts_sf)
}


# Peak_SWE_Plot <- ggplot() +
#   theme_bw() +
#   geom_spatraster(data = CONUS_DEM) +
#   # scale_fill_wiki_c(name = "Elevation (m)") +
#   scale_fill_grass_c(palette = "grey", name = "Elevation (m)") +
#   geom_sf(data = CONUS_AOI, fill = NA, color = "black") +
#   geom_sf(Snotel2010, mapping = aes(col = peak_swe)) +
#   scale_color_princess_c(palette = "snow", direction = 1, name = "Peak SWE (mm)") +
#   # scale_color_whitebox_c(palette = "viridi", direction = -1, name = "Peak SWE (mm)") +
#   ggtitle(label = "Peak SWE at CONUS SNOTEL Stations for WY 2010") +
#   ggspatial::annotation_scale(location = "br") +
#   ggspatial::annotation_north_arrow(location = "bl") +
#   theme(plot.title = element_text(hjust = 0.5))
# Peak_SWE_Plot
# ggsave("/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/Plots/PeakSWEPlot.png")


##### Aggregating Values Across WY 2010 #####
### srad
srad_sum <- srad_oct + srad_nov + srad_dec + srad_jan + srad_feb + srad_mar + srad_apr + srad_may
srad_mean <- srad_sum / 8
### writing rasters so I don't have to do this again
# writeRaster(srad_sum, "/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/Aggregates/srad_Sum.tif", overwrite = TRUE)
# writeRaster(srad_mean, "/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/Aggregates/srad_Mean.tif", overwrite = TRUE)
### tmin
tmin_sum <- tmin_oct + tmin_nov + tmin_dec + tmin_jan + tmin_feb + tmin_mar + tmin_apr + tmin_may
tmin_mean <- tmin_sum / 8
### writing rasters so I don't have to do this again
# writeRaster(tmin_sum, "/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/Aggregates/tmin_Sum.tif", overwrite = TRUE)
# writeRaster(tmin_mean, "/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/Aggregates/tmin_Mean.tif", overwrite = TRUE)
### tmax
tmax_sum <- tmax_oct + tmax_nov + tmax_dec + tmax_jan + tmax_feb + tmax_mar + tmax_apr + tmax_may
tmax_mean <- tmax_sum / 8
### writing rasters so I don't have to do this again
# writeRaster(tmax_sum, "/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/Aggregates/tmax_Sum.tif", overwrite = TRUE)
# writeRaster(tmax_mean, "/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/Aggregates/tmax_Mean.tif", overwrite = TRUE)



### tmean CDM
tmean_CDM_Sum <- tmean_oct_CDM + tmean_nov_CDM + tmean_dec_CDM + tmean_jan_CDM + tmean_feb_CDM + tmean_mar_CDM + tmean_apr_CDM + tmean_may_CDM
tmean_CDM_Mean <- tmean_CDM_Sum / 8
### writing rasters out so I don't have to do this again
# writeRaster(tmean_CDM_Sum, "/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/Aggregates/tmean_CDM_Sum.tif", overwrite = TRUE)
# writeRaster(tmean_CDM_Mean, "/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/Aggregates/tmean_CDM_Mean.tif", overwrite = TRUE)
### prcp mean CDM
prcpMean_CDM_Sum <- prcpMean_oct_CDM + prcpMean_nov_CDM + prcpMean_dec_CDM + prcpMean_jan_CDM + prcpMean_feb_CDM + prcpMean_mar_CDM + prcpMean_apr_CDM + prcpMean_may_CDM
prcpMean_CDM_Mean <- prcpMean_CDM_Sum / 8
### writing rasters out so I don't have to do this again
# writeRaster(prcpMean_CDM_Sum, "/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/Aggregates/prcpMean_CDM_Sum.tif", overwrite = TRUE)
# writeRaster(prcpMean_CDM_Mean, "/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/Aggregates/prcpMean_CDM_Mean.tif", overwrite = TRUE)
### prcp sum CDM
prcpSum_CDM_Sum <- prcpSum_oct_CDM + prcpSum_nov_CDM + prcpSum_dec_CDM + prcpSum_jan_CDM + prcpSum_feb_CDM + prcpSum_mar_CDM + prcpSum_apr_CDM + prcpSum_may_CDM
prcpSum_CDM_Mean <- prcpSum_CDM_Sum / 8
### writing rasters out so I don't have to do this again
# writeRaster(prcpSum_CDM_Sum, "/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/Aggregates/prcpSum_CDM_Sum.tif", overwrite = TRUE)
# writeRaster(prcpSum_CDM_Mean, "/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/Aggregates/prcpSum_CDM_Mean.tif", overwrite = TRUE)


##### Extracting values to SNOTEL points #####
### extracting topographic variables
Snotel2010$elevation <- terra::extract(CONUS_DEM, Snotel2010)[,2]
Snotel2010$slope <- terra::extract(CONUS_Slope, Snotel2010)[,2]
Snotel2010$aspect <- terra::extract(CONUS_Aspect, Snotel2010)[,2]
### extracting raster values
## prcp mean
Snotel2010$jan_prcpMean <- terra::extract(prcpMean_jan, Snotel2010)[,2]
Snotel2010$feb_prcpMean <- terra::extract(prcpMean_feb, Snotel2010)[,2]
Snotel2010$mar_prcpMean <- terra::extract(prcpMean_mar, Snotel2010)[,2]
Snotel2010$apr_prcpMean <- terra::extract(prcpMean_apr, Snotel2010)[,2]
Snotel2010$may_prcpMean <- terra::extract(prcpMean_may, Snotel2010)[,2]
# Snotel2010$june_prcpMean <- terra::extract(prcpMean_June, Snotel2010)[,2]
# Snotel2010$july_prcpMean <- terra::extract(prcpMean_July, Snotel2010)[,2]
# Snotel2010$aug_prcpMean <- terra::extract(prcpMean_Aug, Snotel2010)[,2]
# Snotel2010$sep_prcpMean <- terra::extract(prcpMean_Sep, Snotel2010)[,2]
Snotel2010$oct_prcpMean <- terra::extract(prcpMean_oct, Snotel2010)[,2]
Snotel2010$nov_prcpMean <- terra::extract(prcpMean_nov, Snotel2010)[,2]
Snotel2010$dec_prcpMean <- terra::extract(prcpMean_dec, Snotel2010)[,2]

## prcp sum
Snotel2010$jan_prcpSum <- terra::extract(prcpSum_jan, Snotel2010)[,2]
Snotel2010$feb_prcpSum <- terra::extract(prcpSum_feb, Snotel2010)[,2]
Snotel2010$mar_prcpSum <- terra::extract(prcpSum_mar, Snotel2010)[,2]
Snotel2010$apr_prcpSum <- terra::extract(prcpSum_apr, Snotel2010)[,2]
Snotel2010$may_prcpSum <- terra::extract(prcpSum_may, Snotel2010)[,2]
# Snotel2010$june_prcpSum <- terra::extract(prcpSum_June, Snotel2010)[,2]
# Snotel2010$july_prcpSum <- terra::extract(prcpSum_July, Snotel2010)[,2]
# Snotel2010$aug_prcpSum <- terra::extract(prcpSum_Aug, Snotel2010)[,2]
# Snotel2010$sep_prcpSum <- terra::extract(prcpSum_Sep, Snotel2010)[,2]
Snotel2010$oct_prcpSum <- terra::extract(prcpSum_oct, Snotel2010)[,2]
Snotel2010$nov_prcpSum <- terra::extract(prcpSum_nov, Snotel2010)[,2]
Snotel2010$dec_prcpSum <- terra::extract(prcpSum_dec, Snotel2010)[,2]

## tmin
Snotel2010$jan_tmin <- terra::extract(tmin_jan, Snotel2010)[,2]
Snotel2010$feb_tmin <- terra::extract(tmin_feb, Snotel2010)[,2]
Snotel2010$mar_tmin <- terra::extract(tmin_mar, Snotel2010)[,2]
Snotel2010$apr_tmin <- terra::extract(tmin_apr, Snotel2010)[,2]
Snotel2010$may_tmin <- terra::extract(tmin_may, Snotel2010)[,2]
# Snotel2010$june_tmin <- terra::extract(tmin_June, Snotel2010)[,2]
# Snotel2010$july_tmin <- terra::extract(tmin_July, Snotel2010)[,2]
# Snotel2010$aug_tmin <- terra::extract(tmin_Aug, Snotel2010)[,2]
# Snotel2010$sep_tmin <- terra::extract(tmin_Sep, Snotel2010)[,2]
Snotel2010$oct_tmin <- terra::extract(tmin_oct, Snotel2010)[,2]
Snotel2010$nov_tmin <- terra::extract(tmin_nov, Snotel2010)[,2]
Snotel2010$dec_tmin <- terra::extract(tmin_dec, Snotel2010)[,2]

## tmean
Snotel2010$jan_tmean <- terra::extract(tmean_jan, Snotel2010)[,2]
Snotel2010$feb_tmean <- terra::extract(tmean_feb, Snotel2010)[,2]
Snotel2010$mar_tmean <- terra::extract(tmean_mar, Snotel2010)[,2]
Snotel2010$apr_tmean <- terra::extract(tmean_apr, Snotel2010)[,2]
Snotel2010$may_tmean <- terra::extract(tmean_may, Snotel2010)[,2]
# Snotel2010$june_tmean <- terra::extract(tmean_June, Snotel2010)[,2]
# Snotel2010$july_tmean <- terra::extract(tmean_July, Snotel2010)[,2]
# Snotel2010$aug_tmean <- terra::extract(tmean_Aug, Snotel2010)[,2]
# Snotel2010$sep_tmean <- terra::extract(tmean_Sep, Snotel2010)[,2]
Snotel2010$oct_tmean <- terra::extract(tmean_oct, Snotel2010)[,2]
Snotel2010$nov_tmean <- terra::extract(tmean_nov, Snotel2010)[,2]
Snotel2010$dec_tmean <- terra::extract(tmean_dec, Snotel2010)[,2]

## tmax
Snotel2010$jan_tmax <- terra::extract(tmax_jan, Snotel2010)[,2]
Snotel2010$feb_tmax <- terra::extract(tmax_feb, Snotel2010)[,2]
Snotel2010$mar_tmax <- terra::extract(tmax_mar, Snotel2010)[,2]
Snotel2010$apr_tmax <- terra::extract(tmax_apr, Snotel2010)[,2]
Snotel2010$may_tmax <- terra::extract(tmax_may, Snotel2010)[,2]
# Snotel2010$june_tmax <- terra::extract(tmax_June, Snotel2010)[,2]
# Snotel2010$july_tmax <- terra::extract(tmax_July, Snotel2010)[,2]
# Snotel2010$aug_tmax <- terra::extract(tmax_Aug, Snotel2010)[,2]
# Snotel2010$sep_tmax <- terra::extract(tmax_Sep, Snotel2010)[,2]
Snotel2010$oct_tmax <- terra::extract(tmax_oct, Snotel2010)[,2]
Snotel2010$nov_tmax <- terra::extract(tmax_nov, Snotel2010)[,2]
Snotel2010$dec_tmax <- terra::extract(tmax_dec, Snotel2010)[,2]

## srad
Snotel2010$jan_srad <- terra::extract(srad_jan, Snotel2010)[,2]
Snotel2010$feb_srad <- terra::extract(srad_feb, Snotel2010)[,2]
Snotel2010$mar_srad <- terra::extract(srad_mar, Snotel2010)[,2]
Snotel2010$apr_srad <- terra::extract(srad_apr, Snotel2010)[,2]
Snotel2010$may_srad <- terra::extract(srad_may, Snotel2010)[,2]
# Snotel2010$june_srad <- terra::extract(srad_June, Snotel2010)[,2]
# Snotel2010$july_srad <- terra::extract(srad_July, Snotel2010)[,2]
# Snotel2010$aug_srad <- terra::extract(srad_Aug, Snotel2010)[,2]
# Snotel2010$sep_srad <- terra::extract(srad_Sep, Snotel2010)[,2]
Snotel2010$oct_srad <- terra::extract(srad_oct, Snotel2010)[,2]
Snotel2010$nov_srad <- terra::extract(srad_nov, Snotel2010)[,2]
Snotel2010$dec_srad <- terra::extract(srad_dec, Snotel2010)[,2]

## landcover
Snotel2010$lc_class <- terra::extract(landcover2010, Snotel2010)[,2]

## tmean CDM values
Snotel2010$jan_tmean_CDM <- terra::extract(tmean_jan_CDM, Snotel2010)[,2]
Snotel2010$feb_tmean_CDM <- terra::extract(tmean_feb_CDM, Snotel2010)[,2]
Snotel2010$mar_tmean_CDM <- terra::extract(tmean_mar_CDM, Snotel2010)[,2]
Snotel2010$apr_tmean_CDM <- terra::extract(tmean_apr_CDM, Snotel2010)[,2]
Snotel2010$may_tmean_CDM <- terra::extract(tmean_may_CDM, Snotel2010)[,2]
# Snotel2010$june_tmean_CDM <- terra::extract(tmean_June_CDM, Snotel2010)[,2]
# Snotel2010$july_tmean_CDM <- terra::extract(tmean_July_CDM, Snotel2010)[,2]
# Snotel2010$aug_tmean_CDM <- terra::extract(tmean_Aug_CDM, Snotel2010)[,2]
# Snotel2010$sep_tmean_CDM <- terra::extract(tmean_Sep_CDM, Snotel2010)[,2]
Snotel2010$oct_tmean_CDM <- terra::extract(tmean_oct_CDM, Snotel2010)[,2]
Snotel2010$nov_tmean_CDM <- terra::extract(tmean_nov_CDM, Snotel2010)[,2]
Snotel2010$dec_tmean_CDM <- terra::extract(tmean_dec_CDM, Snotel2010)[,2]

## prcpMean CDM values
Snotel2010$jan_prcpMean_CDM <- terra::extract(prcpMean_jan_CDM, Snotel2010)[,2]
Snotel2010$feb_prcpMean_CDM <- terra::extract(prcpMean_feb_CDM, Snotel2010)[,2]
Snotel2010$mar_prcpMean_CDM <- terra::extract(prcpMean_mar_CDM, Snotel2010)[,2]
Snotel2010$apr_prcpMean_CDM <- terra::extract(prcpMean_apr_CDM, Snotel2010)[,2]
Snotel2010$may_prcpMean_CDM <- terra::extract(prcpMean_may_CDM, Snotel2010)[,2]
# Snotel2010$june_prcpMean_CDM <- terra::extract(prcpMean_June_CDM, Snotel2010)[,2]
# Snotel2010$july_prcpMean_CDM <- terra::extract(prcpMean_July_CDM, Snotel2010)[,2]
# Snotel2010$aug_prcpMean_CDM <- terra::extract(prcpMean_Aug_CDM, Snotel2010)[,2]
# Snotel2010$sep_prcpMean_CDM <- terra::extract(prcpMean_Sep_CDM, Snotel2010)[,2]
Snotel2010$oct_prcpMean_CDM <- terra::extract(prcpMean_oct_CDM, Snotel2010)[,2]
Snotel2010$nov_prcpMean_CDM <- terra::extract(prcpMean_nov_CDM, Snotel2010)[,2]
Snotel2010$dec_prcpMean_CDM <- terra::extract(prcpMean_dec_CDM, Snotel2010)[,2]

## prcpSum CDM values
Snotel2010$jan_prcpSum_CDM <- terra::extract(prcpSum_jan_CDM, Snotel2010)[,2]
Snotel2010$feb_prcpSum_CDM <- terra::extract(prcpSum_feb_CDM, Snotel2010)[,2]
Snotel2010$mar_prcpSum_CDM <- terra::extract(prcpSum_mar_CDM, Snotel2010)[,2]
Snotel2010$apr_prcpSum_CDM <- terra::extract(prcpSum_apr_CDM, Snotel2010)[,2]
Snotel2010$may_prcpSum_CDM <- terra::extract(prcpSum_may_CDM, Snotel2010)[,2]
# Snotel2010$june_prcpSum_CDM <- terra::extract(prcpSum_June_CDM, Snotel2010)[,2]
# Snotel2010$july_prcpSum_CDM <- terra::extract(prcpSum_July_CDM, Snotel2010)[,2]
# Snotel2010$aug_prcpSum_CDM <- terra::extract(prcpSum_Aug_CDM, Snotel2010)[,2]
# Snotel2010$sep_prcpSum_CDM <- terra::extract(prcpSum_Sep_CDM, Snotel2010)[,2]
Snotel2010$oct_prcpSum_CDM <- terra::extract(prcpSum_oct_CDM, Snotel2010)[,2]
Snotel2010$nov_prcpSum_CDM <- terra::extract(prcpSum_nov_CDM, Snotel2010)[,2]
Snotel2010$dec_prcpSum_CDM <- terra::extract(prcpSum_dec_CDM, Snotel2010)[,2]

## tmean CDM aggregates
Snotel2010$tmean_CDM_mean <- terra::extract(tmean_CDM_Mean, Snotel2010)[,2]
Snotel2010$tmean_CDM_sum <- terra::extract(tmean_CDM_Sum, Snotel2010)[,2]

## prcpMean CDM aggregates
Snotel2010$prcpMean_CDM_mean <- terra::extract(prcpMean_CDM_Mean, Snotel2010)[,2]
Snotel2010$prcpMean_CDM_sum <- terra::extract(prcpMean_CDM_Sum, Snotel2010)[,2]

## prcpSum CDM aggregates
Snotel2010$prcpSum_CDM_mean <- terra::extract(prcpSum_CDM_Mean, Snotel2010)[,2]
Snotel2010$prcpSum_CDM_sum <- terra::extract(prcpSum_CDM_Sum, Snotel2010)[,2]

## srad aggregates
Snotel2010$srad_mean <- terra::extract(srad_mean, Snotel2010)[,2]
Snotel2010$srad_sum <- terra::extract(srad_sum, Snotel2010)[,2]

## tmin aggregates
Snotel2010$tmin_mean <- terra::extract(tmin_mean, Snotel2010)[,2]
Snotel2010$tmin_sum <- terra::extract(tmin_sum, Snotel2010)[,2]

## tmax aggregates
Snotel2010$tmax_mean <- terra::extract(tmax_mean, Snotel2010)[,2]
Snotel2010$tmax_sum <- terra::extract(tmax_sum, Snotel2010)[,2]

### writing out Snotel2010 as gpkg so I don't have to reextract if I ever close or reopen this project
st_write(Snotel2010, paste0("/Users/gshelor/Documents/GEOG607Data/Project_Data/SNOTEL/Snotel2010_CONUS_PeakSWE_", char_zero_points, "_ZeroPts_Covars.gpkg"), append = FALSE)


##### End of Script #####