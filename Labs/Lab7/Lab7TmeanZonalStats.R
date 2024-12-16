##### Aggregating Tmean climatology from Water Years 1993-2020 to use for GEOG 607 Lab 7 #####
### loading packages
library(pacman)
p_load(here, sf, terra, tidyverse, snotelr, gstat)

# CONUS_AOI <- vect("/Users/gshelor/Documents/GEOG607Data/Lab7_Data/CONUS_AOI.gpkg")
tmean <- rast("/Users/gshelor/Documents/GEOG607Data/Lab7_Data/tmean_Month1MeanClimatology.tif")
prcp <- rast("/Users/gshelor/Documents/GEOG607Data/Lab7_Data/prcp_Month1MeanClimatology.tif")



# 
# writeVector(CONUS_AOI, "/Users/gshelor/Documents/GEOG607Data/Lab7_Data/CONUS_AOI_TmeanZonal.gpkg", overwrite = TRUE)


##### doing IDW stuff now #####
SWE_Climatology <- read_csv("/Users/gshelor/Documents/GEOG607Data/Lab7_Data/SnotelData_CONUS_WY9320_PeakSWE.csv") |>
  group_by(site_id) |>
  reframe(mean_peak_swe = mean(peak_swe),
          state = state,
          longitude = longitude,
          latitude = latitude) |>
  unique()

SWE_Climatology_sf <- st_as_sf(SWE_Climatology, coords = c("longitude", "latitude"), crs = 4326)
SWE_Climatology_sf <- st_transform(SWE_Climatology_sf, crs(tmean))


CONUS_AOI <- vect("/Users/gshelor/Documents/GEOG607Data/Lab7_Data/CONUS_AOI_TmeanZonal.gpkg")


CONUS_AOI <- st_as_sf(CONUS_AOI) #|>
  # drop_na()
CONUS_AOI <- vect(CONUS_AOI)

### cropping Jan prcp and doing zonal statistics for that as well
prcp <- crop(prcp, CONUS_AOI, mask = TRUE)



writeVector(CONUS_AOI, "/Users/gshelor/Documents/GEOG607Data/Lab7_Data/CONUS_AOI_SWETmeanZonal.gpkg", overwrite = TRUE)



SWE_Climatology_MW_sf <- SWE_Climatology_sf |>
  filter(state == "CO" | state == "UT" | state == "WY")
MW_AOI <- st_as_sf(CONUS_AOI) |>
  filter(STATE_NAME == "Colorado" | STATE_NAME == "Wyoming" | STATE_NAME == "Utah")

tmean_MW <- crop(tmean, MW_AOI, mask = TRUE)
prcp_MW <- crop(prcp, MW_AOI, mask = TRUE)
tmean_df <- as.data.frame(tmean_MW, xy = TRUE)

tmean_sf <- st_as_sf(tmean_df, coords = c("x", "y"), crs = crs(tmean))

### idw of SWE
SWE_IDW <- gstat::idw(mean_peak_swe ~ 1, locations = SWE_Climatology_MW_sf, newdata = tmean_sf, idp = 2.5)

SWE_IDW_rast <- rasterize(vect(SWE_IDW), tmean_MW, field = "var1.pred")

plot(SWE_IDW_rast)
writeRaster(SWE_IDW_rast, "/Users/gshelor/Documents/GEOG607Data/Lab7_Data/SWE_IDW_MW.tif")


### calculating zonal statistics
MW_AOI <- vect(MW_AOI)
MW_AOI$tmean_zonal_mean <- terra::zonal(tmean_MW, MW_AOI, fun = "mean", na.rm = TRUE)
MW_AOI$swe_zonal_mean <- terra::zonal(SWE_IDW_rast, MW_AOI, fun = "mean", na.rm = TRUE)
MW_AOI$prcp_zonal_mean <- terra::zonal(prcp_MW, MW_AOI, fun = "mean", na.rm = TRUE)

writeVector(MW_AOI, "/Users/gshelor/Documents/GEOG607Data/Lab7_Data/MW_AOI_SWETmeanZonal.gpkg", overwrite = TRUE)
MW_AOI_sf <- st_as_sf(MW_AOI)
