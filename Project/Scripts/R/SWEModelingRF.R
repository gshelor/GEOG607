##### Modeling peak SWE across CONUS for Water Year 2010 #####

### loading packages
library(pacman)
p_load(tidyverse, here, gstat, sf, terra, tidyterra, randomForest, ranger, rstan, spdep, rsample, spatialsample, RColorBrewer, viridis, ModelMetrics, ggthemes)

##### reading in data #####
Snotel2010_0ZeroPts <- read_sf("/Users/gshelor/Documents/GEOG607Data/Project_Data/SNOTEL/Snotel2010_CONUS_PeakSWE_0_ZeroPts_Covars.gpkg") |>
  drop_na()
Snotel2010_10ZeroPts <- read_sf("/Users/gshelor/Documents/GEOG607Data/Project_Data/SNOTEL/Snotel2010_CONUS_PeakSWE_10_ZeroPts_Covars.gpkg") |>
  drop_na()
Snotel2010_25ZeroPts <- read_sf("/Users/gshelor/Documents/GEOG607Data/Project_Data/SNOTEL/Snotel2010_CONUS_PeakSWE_25_ZeroPts_Covars.gpkg") |>
  drop_na()
Snotel2010_50ZeroPts <- read_sf("/Users/gshelor/Documents/GEOG607Data/Project_Data/SNOTEL/Snotel2010_CONUS_PeakSWE_50_ZeroPts_Covars.gpkg") |>
  drop_na()
Snotel2010_100ZeroPts <- read_sf("/Users/gshelor/Documents/GEOG607Data/Project_Data/SNOTEL/Snotel2010_CONUS_PeakSWE_100_ZeroPts_Covars.gpkg") |>
  drop_na()
Snotel2010_150ZeroPts <- read_sf("/Users/gshelor/Documents/GEOG607Data/Project_Data/SNOTEL/Snotel2010_CONUS_PeakSWE_150_ZeroPts_Covars.gpkg") |>
  drop_na()
Snotel2010_200ZeroPts <- read_sf("/Users/gshelor/Documents/GEOG607Data/Project_Data/SNOTEL/Snotel2010_CONUS_PeakSWE_200_ZeroPts_Covars.gpkg") |>
  drop_na()
Snotel2010_250ZeroPts <- read_sf("/Users/gshelor/Documents/GEOG607Data/Project_Data/SNOTEL/Snotel2010_CONUS_PeakSWE_250_ZeroPts_Covars.gpkg") |>
  drop_na()
Snotel2010_0ZeroPts$lc_class <- as.factor(Snotel2010_0ZeroPts$lc_class)
Snotel2010_10ZeroPts$lc_class <- as.factor(Snotel2010_10ZeroPts$lc_class)
Snotel2010_25ZeroPts$lc_class <- as.factor(Snotel2010_25ZeroPts$lc_class)
Snotel2010_50ZeroPts$lc_class <- as.factor(Snotel2010_50ZeroPts$lc_class)
Snotel2010_100ZeroPts$lc_class <- as.factor(Snotel2010_100ZeroPts$lc_class)
Snotel2010_150ZeroPts$lc_class <- as.factor(Snotel2010_150ZeroPts$lc_class)
Snotel2010_200ZeroPts$lc_class <- as.factor(Snotel2010_200ZeroPts$lc_class)
Snotel2010_250ZeroPts$lc_class <- as.factor(Snotel2010_250ZeroPts$lc_class)
CONUS_AOI <- read_sf("/Users/gshelor/Documents/GEOG607Data/Project_Data/CONUS_AOI.gpkg")
CONUS_AOI_vect <- CONUS_AOI |>
  filter(US_L3NAME == "Mojave Basin and Range" | US_L3NAME == "Arizona/New Mexico Plateau" | US_L3NAME == "Chihuahuan Deserts" | US_L3NAME == "Central California Valley" | US_L3NAME == "Sonoran Basin and Range" | US_L3NAME == "Southern California/Northern Baja Coast" | US_L3NAME == "Madrean Archipelago") |>
  vect()


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
landcover2010 <- as.factor(rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Landcover/LC_CONUS_2010.tif"))
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

### reading in CDM aggregates
tmean_CDM_Sum <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/Aggregates/tmean_CDM_Sum.tif")
tmean_CDM_Mean <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/Aggregates/tmean_CDM_Mean.tif")
### prcp mean CDM
prcpMean_CDM_Sum <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/Aggregates/prcpMean_CDM_Sum.tif")
prcpMean_CDM_Mean <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/Aggregates/prcpMean_CDM_Mean.tif")
### prcp sum CDM
prcpSum_CDM_Sum <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/Aggregates/prcpSum_CDM_Sum.tif")
prcpSum_CDM_Mean <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/CDM/Aggregates/prcpSum_CDM_Mean.tif")

### reading in aggregates of other variables
tmin_sum <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/Aggregates/tmin_Sum.tif")
tmin_mean <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/Aggregates/tmin_Mean.tif")
tmax_sum <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/Aggregates/tmax_Sum.tif")
tmax_mean <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/Aggregates/tmax_Mean.tif")
srad_sum <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/Aggregates/srad_Sum.tif")
srad_mean <- rast("/Users/gshelor/Documents/GEOG607Data/Project_Data/Daymet/Aggregates/srad_Mean.tif")

##### Testing/Plotting for spatial autocorrelation #####
### rgeoda not available for this version of R, so I ran both global and local Moran's I tests in GeoDa itself, got nothing. at most 75-80 points displaying significance
### making plot from geoda output with 
# geoda_output <- read_sf("/Users/gshelor/Documents/GEOG607Data/Project_Data/Snotel/Snotel2010_Geoda.gpkg")
# geoda_plot <- ggplot() +
#   theme_bw() +
#   geom_sf(data = CONUS_AOI, fill = NA, color = "black") +
#   geom_sf(geoda_output, mapping = aes(color = LISA_P)) +
#   scale_color_whitebox_c(palette = "bl_yl_rd", direction = -1, name = NULL) +
#   ggtitle(label = "Local Moran's I p-values for Peak SWE in Water Year 2010", subtitle = "Weighting method: Minimum Distance Band with IDW (Power = 1)") +
#   theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
# geoda_plot
# ggsave("/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/LISA_SigMap.png")
# rm(geoda_output, geoda_plot)

##### Random Forest Modeling, I guess #####
### splitting dataset into training and testing
set.seed(802)
data_split <- initial_split(as.data.frame(Snotel2010_0ZeroPts), prop = 0.75)
set.seed(802)
data_split2 <- initial_split(as.data.frame(Snotel2010_10ZeroPts), prop = 0.75)
set.seed(802)
data_split3 <- initial_split(as.data.frame(Snotel2010_25ZeroPts), prop = 0.75)
set.seed(802)
data_split4 <- initial_split(as.data.frame(Snotel2010_50ZeroPts), prop = 0.75)
set.seed(802)
data_split5 <- initial_split(as.data.frame(Snotel2010_100ZeroPts), prop = 0.75)
set.seed(802)
data_split6 <- initial_split(as.data.frame(Snotel2010_150ZeroPts), prop = 0.75)
set.seed(802)
data_split7 <- initial_split(as.data.frame(Snotel2010_200ZeroPts), prop = 0.75)
set.seed(802)
data_split8 <- initial_split(as.data.frame(Snotel2010_250ZeroPts), prop = 0.75)
Snotel_training <- training(data_split)
Snotel_training2 <- training(data_split2)
Snotel_training3 <- training(data_split3)
Snotel_training4 <- training(data_split4)
Snotel_training5 <- training(data_split5)
Snotel_training6 <- training(data_split6)
Snotel_training7 <- training(data_split7)
Snotel_training8 <- training(data_split8)
Snotel_test <- testing(data_split)
Snotel_test2 <- testing(data_split2)
Snotel_test3 <- testing(data_split3)
Snotel_test4 <- testing(data_split4)
Snotel_test5 <- testing(data_split5)
Snotel_test6 <- testing(data_split6)
Snotel_test7 <- testing(data_split7)
Snotel_test8 <- testing(data_split8)

### fitting RF models
### Model 1
## 0 zero points
## 1000 trees, 2 vars selected at each split
set.seed(802)
RFModel1 <- randomForest(peak_swe ~ tmean_CDM_sum + prcpSum_CDM_sum + srad_mean + tmin_mean + tmax_mean + elevation + slope, data = Snotel_training, ntree = 1000, mtry = 2, importance = TRUE)
Predicted_vals_RF1 <- predict(RFModel1, Snotel_test)
rmse(Predicted_vals_RF1, Snotel_test$peak_swe)
RFModel1
varImpPlot(RFModel1)
randomForest::importance(RFModel1)

### Model 2 (used log of peak_SWE but formula and parameters are the same)
## 0 zero points
## 1000 trees, 2 vars selected at each split
set.seed(802)
RFModel2 <- randomForest(log_peak_swe ~ tmean_CDM_sum + prcpSum_CDM_sum + srad_mean + tmin_mean + tmax_mean + elevation + slope, data = Snotel_training, ntree = 1000, mtry = 2, importance = TRUE)
Predicted_vals_RF2 <- predict(RFModel2, Snotel_test)
rmse(exp(Predicted_vals_RF2), Snotel_test$peak_swe)
varImpPlot(RFModel2)

### Model 3
## 0 zero points
## 1500 trees, 2 vars selected at each split
set.seed(802)
RFModel1 <- randomForest(peak_swe ~ tmean_CDM_sum + prcpSum_CDM_sum + srad_mean + tmin_mean + tmax_mean + elevation + slope, data = Snotel_training, ntree = 1000, mtry = 2, importance = TRUE)
Predicted_vals_RF1 <- predict(RFModel1, Snotel_test)
rmse(Predicted_vals_RF1, Snotel_test$peak_swe)
RFModel1
varImpPlot(RFModel1)
randomForest::importance(RFModel1)

### Model 4 (used log of peak_SWE but formula and parameters are the same)
## 0 zero points
## 1500 trees, 2 vars selected at each split
set.seed(802)
RFModel2 <- randomForest(log_peak_swe ~ tmean_CDM_sum + prcpSum_CDM_sum + srad_mean + tmin_mean + tmax_mean + elevation + slope, data = Snotel_training, ntree = 1000, mtry = 2, importance = TRUE)
Predicted_vals_RF2 <- predict(RFModel2, Snotel_test)
rmse(exp(Predicted_vals_RF2), Snotel_test$peak_swe)
varImpPlot(RFModel2)

### Model 5
## 0 zero points
## 2000 trees, 2 vars selected at each split
set.seed(802)
RFModel1 <- randomForest(peak_swe ~ tmean_CDM_sum + prcpSum_CDM_sum + srad_mean + tmin_mean + tmax_mean + elevation + slope, data = Snotel_training, ntree = 1000, mtry = 2, importance = TRUE)
Predicted_vals_RF1 <- predict(RFModel1, Snotel_test)
rmse(Predicted_vals_RF1, Snotel_test$peak_swe)
RFModel1
varImpPlot(RFModel1)
randomForest::importance(RFModel1)

### Model 6 (used log of peak_SWE but formula and parameters are the same)
## 0 zero points
## 2000 trees, 2 vars selected at each split
set.seed(802)
RFModel2 <- randomForest(log_peak_swe ~ tmean_CDM_sum + prcpSum_CDM_sum + srad_mean + tmin_mean + tmax_mean + elevation + slope, data = Snotel_training, ntree = 1000, mtry = 2, importance = TRUE)
Predicted_vals_RF2 <- predict(RFModel2, Snotel_test)
rmse(exp(Predicted_vals_RF2), Snotel_test$peak_swe)
varImpPlot(RFModel2)




### Model ???????? (predicting raw SWE, 0 zero pts)
set.seed(802)
RFModel????????? <- randomForest(log_peak_swe ~ tmean_CDM_sum + tmean_CDM_mean + prcpSum_CDM_sum + prcpMean_CDM_mean + tmin_mean + tmax_mean + elevation + slope, data = Snotel_training, ntree = 2000, mtry = 2, importance = TRUE)
Predicted_vals_RF3 <- predict(RFModel3, Snotel_test)
rmse(exp(Predicted_vals_RF3), Snotel_test$peak_swe)
randomForest::importance(RFModel3)



##### Stacking Rasters for making spatial predictions #####
Prediction_Rasters <- c(tmean_CDM_Sum, prcpSum_CDM_Sum, srad_mean, tmin_mean, tmax_mean, CONUS_DEM, CONUS_Slope)
Prediction_Rasters
names(Prediction_Rasters) <- c("tmean_CDM_sum", "prcpSum_CDM_sum","srad_mean", "tmin_mean", "tmax_mean", "elevation", "slope")
names(Prediction_Rasters)

##### Making Spatial Predictions #####
set.seed(802)
RF1_PredictionMap <- terra::predict(Prediction_Rasters, RFModel1)
# writeRaster(RF1_PredictionMap, "/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/RFModel1_SWE.tif")
plot(RF1_PredictionMap, main = "RF Model of Peak SWE")

set.seed(802)
RF2_PredictionMap <- terra::predict(Prediction_Rasters, RFModel2)
# writeRaster(RF2_PredictionMap, "/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/RFModel2_LogSWE.tif")
plot(exp(RF2_PredictionMap), main = "Random Forest Model of log(peak_SWE)")
RF2_PredictionMap_exp <- exp(RF2_PredictionMap)


##### Making ggplots of Predicted Peak SWE #####
RFModel1_plot <- ggplot() +
  theme_bw() +
  geom_spatraster(data = RF1_PredictionMap) +
  scale_fill_princess_c(palette = "snow", direction = 1, name = "Peak SWE (mm)") +
  geom_sf(data = CONUS_AOI, fill = NA, color = "black") +
  ggtitle(label = "Predicted Peak SWE for WY 2010", subtitle = "Model 1") +
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(location = "bl") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
RFModel1_plot
# ggsave("/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/Plots/RFModel1_plot.png")

### Model 2
RFModel2_plot <- ggplot() +
  theme_bw() +
  geom_spatraster(data = RF2_PredictionMap_exp) +
  scale_fill_princess_c(palette = "snow", direction = 1, name = "Peak SWE (mm)") +
  geom_sf(data = CONUS_AOI, fill = NA, color = "black") +
  ggtitle(label = "Predicted Peak SWE for WY 2010", subtitle = "Model 2") +
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(location = "bl") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
RFModel2_plot
# ggsave("/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/Plots/RFModel2_plot.png")
