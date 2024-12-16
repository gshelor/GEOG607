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
# CONUS_AOI_vect <- CONUS_AOI |>
#   filter(US_L3NAME == "Mojave Basin and Range" | US_L3NAME == "Arizona/New Mexico Plateau" | US_L3NAME == "Chihuahuan Deserts" | US_L3NAME == "Central California Valley" | US_L3NAME == "Sonoran Basin and Range" | US_L3NAME == "Southern California/Northern Baja Coast" | US_L3NAME == "Madrean Archipelago") |>
#   vect()


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
### 2000 trees identified as best number after testing different combinations of ntree and mtry based on RMSE on test data split
### Model 1
## 0 zero points
## 1000 trees, 2 vars selected at each split
set.seed(802)
RFModel1 <- randomForest(peak_swe ~ tmean_CDM_sum + prcpSum_CDM_sum + srad_mean + tmin_mean + tmax_mean + elevation + slope, data = Snotel_training, ntree = 2000, mtry = 2, importance = TRUE)
Predicted_vals_RF1 <- predict(RFModel1, Snotel_test)
rmse(Predicted_vals_RF1, Snotel_test$peak_swe)
RFModel1
varImpPlot(RFModel1)
randomForest::importance(RFModel1)

### Model 2 (used log of peak_SWE but formula and parameters are the same)
## 0 zero points
## 1000 trees, 2 vars selected at each split
set.seed(802)
RFModel2 <- randomForest(log_peak_swe ~ tmean_CDM_sum + prcpSum_CDM_sum + srad_mean + tmin_mean + tmax_mean + elevation + slope, data = Snotel_training, ntree = 2000, mtry = 2, importance = TRUE)
RFModel2
Predicted_vals_RF2 <- predict(RFModel2, Snotel_test)
rmse(exp(Predicted_vals_RF2), Snotel_test$peak_swe)
varImpPlot(RFModel2)

### Model 3
## 10 zero points
## 2000 trees, 2 vars selected at each split
set.seed(802)
RFModel3 <- randomForest(peak_swe ~ tmean_CDM_sum + prcpSum_CDM_sum + srad_mean + tmin_mean + tmax_mean + elevation + slope, data = Snotel_training2, ntree = 2000, mtry = 2, importance = TRUE)
Predicted_vals_RF3 <- predict(RFModel3, Snotel_test2)
rmse(Predicted_vals_RF3, Snotel_test2$peak_swe)
RFModel3
varImpPlot(RFModel3)
randomForest::importance(RFModel3)

### Model 4 (used log of peak_SWE but formula and parameters are the same)
## 10 zero points
## 2000 trees, 2 vars selected at each split
set.seed(802)
RFModel4 <- randomForest(log_peak_swe ~ tmean_CDM_sum + prcpSum_CDM_sum + srad_mean + tmin_mean + tmax_mean + elevation + slope, data = Snotel_training2, ntree = 2000, mtry = 2, importance = TRUE)
RFModel4
Predicted_vals_RF4 <- predict(RFModel4, Snotel_test2)
rmse(exp(Predicted_vals_RF4), Snotel_test2$peak_swe)
varImpPlot(RFModel4)

### Model 5
## 25 zero points
## 2000 trees, 2 vars selected at each split
set.seed(802)
RFModel5 <- randomForest(peak_swe ~ tmean_CDM_sum + prcpSum_CDM_sum + srad_mean + tmin_mean + tmax_mean + elevation + slope, data = Snotel_training3, ntree = 2000, mtry = 2, importance = TRUE)
Predicted_vals_RF5 <- predict(RFModel5, Snotel_test3)
rmse(Predicted_vals_RF5, Snotel_test3$peak_swe)
RFModel5
varImpPlot(RFModel5)
randomForest::importance(RFModel5)

### Model 6 (used log of peak_SWE but formula and parameters are the same)
## 25 zero points
## 2000 trees, 2 vars selected at each split
set.seed(802)
RFModel6 <- randomForest(log_peak_swe ~ tmean_CDM_sum + prcpSum_CDM_sum + srad_mean + tmin_mean + tmax_mean + elevation + slope, data = Snotel_training3, ntree = 2000, mtry = 2, importance = TRUE)
RFModel6
Predicted_vals_RF6 <- predict(RFModel6, Snotel_test3)
rmse(exp(Predicted_vals_RF6), Snotel_test3$peak_swe)
varImpPlot(RFModel6)

### Model 7
## 50 zero points
## 2000 trees, 2 vars selected at each split
set.seed(802)
RFModel7 <- randomForest(peak_swe ~ tmean_CDM_sum + prcpSum_CDM_sum + srad_mean + tmin_mean + tmax_mean + elevation + slope, data = Snotel_training4, ntree = 2000, mtry = 2, importance = TRUE)
Predicted_vals_RF7 <- predict(RFModel7, Snotel_test4)
rmse(Predicted_vals_RF7, Snotel_test4$peak_swe)
RFModel7
varImpPlot(RFModel7)
randomForest::importance(RFModel7)

### Model 8 (used log of peak_SWE but formula and parameters are the same)
## 50 zero points
## 2000 trees, 2 vars selected at each split
set.seed(802)
RFModel8 <- randomForest(log_peak_swe ~ tmean_CDM_sum + prcpSum_CDM_sum + srad_mean + tmin_mean + tmax_mean + elevation + slope, data = Snotel_training4, ntree = 2000, mtry = 2, importance = TRUE)
Predicted_vals_RF8 <- predict(RFModel8, Snotel_test4)
rmse(exp(Predicted_vals_RF8), Snotel_test4$peak_swe)
varImpPlot(RFModel8)


### Model 9
## 100 zero points
## 2000 trees, 2 vars selected at each split
set.seed(802)
RFModel9 <- randomForest(peak_swe ~ tmean_CDM_sum + prcpSum_CDM_sum + srad_mean + tmin_mean + tmax_mean + elevation + slope, data = Snotel_training5, ntree = 2000, mtry = 2, importance = TRUE)
Predicted_vals_RF9 <- predict(RFModel9, Snotel_test5)
rmse(Predicted_vals_RF9, Snotel_test5$peak_swe)
RFModel9
varImpPlot(RFModel9)
randomForest::importance(RFModel9)

### Model 10 (used log of peak_SWE but formula and parameters are the same)
## 100 zero points
## 2000 trees, 2 vars selected at each split
set.seed(802)
RFModel10 <- randomForest(log_peak_swe ~ tmean_CDM_sum + prcpSum_CDM_sum + srad_mean + tmin_mean + tmax_mean + elevation + slope, data = Snotel_training5, ntree = 2000, mtry = 2, importance = TRUE)
Predicted_vals_RF10 <- predict(RFModel10, Snotel_test5)
rmse(exp(Predicted_vals_RF10), Snotel_test5$peak_swe)
varImpPlot(RFModel10)

### Model 11
## 150 zero points
## 2000 trees, 2 vars selected at each split
set.seed(802)
RFModel11 <- randomForest(peak_swe ~ tmean_CDM_sum + prcpSum_CDM_sum + srad_mean + tmin_mean + tmax_mean + elevation + slope, data = Snotel_training6, ntree = 2000, mtry = 2, importance = TRUE)
Predicted_vals_RF11 <- predict(RFModel11, Snotel_test6)
rmse(Predicted_vals_RF11, Snotel_test6$peak_swe)
RFModel11
varImpPlot(RFModel11)
randomForest::importance(RFModel11)

### Model 12 (used log of peak_SWE but formula and parameters are the same)
## 150 zero points
## 2000 trees, 2 vars selected at each split
set.seed(802)
RFModel12 <- randomForest(log_peak_swe ~ tmean_CDM_sum + prcpSum_CDM_sum + srad_mean + tmin_mean + tmax_mean + elevation + slope, data = Snotel_training6, ntree = 2000, mtry = 2, importance = TRUE)
Predicted_vals_RF12 <- predict(RFModel12, Snotel_test6)
rmse(exp(Predicted_vals_RF12), Snotel_test6$peak_swe)
varImpPlot(RFModel12)

### Model 13
## 200 zero points
## 2000 trees, 2 vars selected at each split
set.seed(802)
RFModel13 <- randomForest(peak_swe ~ tmean_CDM_sum + prcpSum_CDM_sum + srad_mean + tmin_mean + tmax_mean + elevation + slope, data = Snotel_training7, ntree = 2000, mtry = 2, importance = TRUE)
Predicted_vals_RF13 <- predict(RFModel13, Snotel_test7)
rmse(Predicted_vals_RF13, Snotel_test7$peak_swe)
RFModel13
varImpPlot(RFModel13)
randomForest::importance(RFModel13)

### Model 14 (used log of peak_SWE but formula and parameters are the same)
## 200 zero points
## 2000 trees, 2 vars selected at each split
set.seed(802)
RFModel14 <- randomForest(log_peak_swe ~ tmean_CDM_sum + prcpSum_CDM_sum + srad_mean + tmin_mean + tmax_mean + elevation + slope, data = Snotel_training7, ntree = 2000, mtry = 2, importance = TRUE)
Predicted_vals_RF14 <- predict(RFModel14, Snotel_test7)
rmse(exp(Predicted_vals_RF14), Snotel_test7$peak_swe)
varImpPlot(RFModel14)

### Model 15
## 250 zero points
## 2000 trees, 2 vars selected at each split
set.seed(802)
RFModel15 <- randomForest(peak_swe ~ tmean_CDM_sum + prcpSum_CDM_sum + srad_mean + tmin_mean + tmax_mean + elevation + slope, data = Snotel_training8, ntree = 2000, mtry = 2, importance = TRUE)
Predicted_vals_RF15 <- predict(RFModel15, Snotel_test8)
rmse(Predicted_vals_RF15, Snotel_test8$peak_swe)
RFModel15
varImpPlot(RFModel15)
randomForest::importance(RFModel15)

### Model 16 (used log of peak_SWE but formula and parameters are the same)
## 250 zero points
## 2000 trees, 2 vars selected at each split
set.seed(802)
RFModel16 <- randomForest(log_peak_swe ~ tmean_CDM_sum + prcpSum_CDM_sum + srad_mean + tmin_mean + tmax_mean + elevation + slope, data = Snotel_training8, ntree = 2000, mtry = 2, importance = TRUE)
Predicted_vals_RF16 <- predict(RFModel16, Snotel_test8)
rmse(exp(Predicted_vals_RF16), Snotel_test8$peak_swe)
varImpPlot(RFModel16)



##### Stacking Rasters for making spatial predictions #####
Prediction_Rasters <- c(tmean_CDM_Sum, prcpSum_CDM_Sum, srad_mean, tmin_mean, tmax_mean, CONUS_DEM, CONUS_Slope)
Prediction_Rasters
names(Prediction_Rasters) <- c("tmean_CDM_sum", "prcpSum_CDM_sum","srad_mean", "tmin_mean", "tmax_mean", "elevation", "slope")
names(Prediction_Rasters)

##### Making Spatial Predictions #####
set.seed(802)
RF1_PredictionMap <- terra::predict(Prediction_Rasters, RFModel1)
writeRaster(RF1_PredictionMap, "/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/RFModel1_SWE.tif", overwrite = TRUE)
# plot(RF1_PredictionMap, main = "RF Model of Peak SWE")

set.seed(802)
RF2_PredictionMap <- terra::predict(Prediction_Rasters, RFModel2)
writeRaster(RF2_PredictionMap, "/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/RFModel2_LogSWE.tif", overwrite = TRUE)
RF2_PredictionMap_exp <- exp(RF2_PredictionMap)
# plot(RF2_PredictionMap_exp, main = "Random Forest Model of log(peak_SWE)")

set.seed(802)
RF3_PredictionMap <- terra::predict(Prediction_Rasters, RFModel3)
# writeRaster(RF1_PredictionMap, "/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/RFModel1_SWE.tif")
# plot(RF3_PredictionMap, main = "RF Model of Peak SWE, Model 3")

set.seed(802)
RF4_PredictionMap <- terra::predict(Prediction_Rasters, RFModel4)
# writeRaster(RF2_PredictionMap, "/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/RFModel2_LogSWE.tif")
RF4_PredictionMap_exp <- exp(RF4_PredictionMap)
# plot(RF4_PredictionMap_exp, main = "Random Forest Model of log(peak_SWE), Model 4")

set.seed(802)
RF5_PredictionMap <- terra::predict(Prediction_Rasters, RFModel5)
# writeRaster(RF1_PredictionMap, "/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/RFModel1_SWE.tif")
# plot(RF5_PredictionMap, main = "RF Model of Peak SWE, Model 5")

set.seed(802)
RF6_PredictionMap <- terra::predict(Prediction_Rasters, RFModel6)
# writeRaster(RF2_PredictionMap, "/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/RFModel2_LogSWE.tif")
RF6_PredictionMap_exp <- exp(RF6_PredictionMap)
# plot(RF6_PredictionMap_exp, main = "Random Forest Model of log(peak_SWE), Model 6")

set.seed(802)
RF7_PredictionMap <- terra::predict(Prediction_Rasters, RFModel7)
# writeRaster(RF1_PredictionMap, "/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/RFModel1_SWE.tif")
# plot(RF7_PredictionMap, main = "RF Model of Peak SWE, Model 7")

set.seed(802)
RF8_PredictionMap <- terra::predict(Prediction_Rasters, RFModel8)
# writeRaster(RF2_PredictionMap, "/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/RFModel2_LogSWE.tif")
RF8_PredictionMap_exp <- exp(RF8_PredictionMap)
# plot(RF8_PredictionMap_exp, main = "Random Forest Model of log(peak_SWE), Model 8")

set.seed(802)
RF9_PredictionMap <- terra::predict(Prediction_Rasters, RFModel9)
# writeRaster(RF1_PredictionMap, "/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/RFModel1_SWE.tif")
# plot(RF9_PredictionMap, main = "RF Model of Peak SWE, Model 9")

set.seed(802)
RF10_PredictionMap <- terra::predict(Prediction_Rasters, RFModel10)
# writeRaster(RF2_PredictionMap, "/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/RFModel2_LogSWE.tif")
RF10_PredictionMap_exp <- exp(RF10_PredictionMap)
# plot(RF10_PredictionMap_exp, main = "Random Forest Model of log(peak_SWE), Model 10")

set.seed(802)
RF11_PredictionMap <- terra::predict(Prediction_Rasters, RFModel11)
# writeRaster(RF2_PredictionMap, "/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/RFModel2_LogSWE.tif")
# plot(RF10_PredictionMap_exp, main = "Random Forest Model of log(peak_SWE), Model 10")

set.seed(802)
RF12_PredictionMap <- terra::predict(Prediction_Rasters, RFModel12)
# writeRaster(RF2_PredictionMap, "/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/RFModel2_LogSWE.tif")
RF12_PredictionMap_exp <- exp(RF12_PredictionMap)
# plot(RF10_PredictionMap_exp, main = "Random Forest Model of log(peak_SWE), Model 10")

set.seed(802)
RF13_PredictionMap <- terra::predict(Prediction_Rasters, RFModel13)
# writeRaster(RF2_PredictionMap, "/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/RFModel2_LogSWE.tif")
# plot(RF10_PredictionMap_exp, main = "Random Forest Model of log(peak_SWE), Model 10")

set.seed(802)
RF14_PredictionMap <- terra::predict(Prediction_Rasters, RFModel14)
# writeRaster(RF2_PredictionMap, "/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/RFModel2_LogSWE.tif")
RF14_PredictionMap_exp <- exp(RF14_PredictionMap)
# plot(RF10_PredictionMap_exp, main = "Random Forest Model of log(peak_SWE), Model 10")

set.seed(802)
RF15_PredictionMap <- terra::predict(Prediction_Rasters, RFModel15)
writeRaster(RF15_PredictionMap, "/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/RFModel15_LogSWE.tif")
# plot(RF10_PredictionMap_exp, main = "Random Forest Model of log(peak_SWE), Model 10")

set.seed(802)
RF16_PredictionMap <- terra::predict(Prediction_Rasters, RFModel16)
RF16_PredictionMap_exp <- exp(RF16_PredictionMap)
writeRaster(RF16_PredictionMap_exp, "/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/RFModel16_LogSWE.tif")
# plot(RF10_PredictionMap_exp, main = "Random Forest Model of log(peak_SWE), Model 10")


##### Making ggplots of Predicted Peak SWE #####
RFModel1_plot <- ggplot() +
  theme_bw() +
  geom_spatraster(data = RF1_PredictionMap) +
  scale_fill_princess_c(palette = "snow", direction = 1, name = "Peak SWE (mm)") +
  geom_sf(data = CONUS_AOI, fill = NA, color = "black") +
  ggtitle(label = "Predicted Peak SWE for WY 2010", subtitle = "Raw SWE, 0 Zero points") +
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(location = "bl") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
RFModel1_plot
ggsave("/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/Plots/RFModel1_plot.png")

### Model 2
RFModel2_plot <- ggplot() +
  theme_bw() +
  geom_spatraster(data = RF2_PredictionMap_exp) +
  scale_fill_princess_c(palette = "snow", direction = 1, name = "Peak SWE (mm)") +
  geom_sf(data = CONUS_AOI, fill = NA, color = "black") +
  ggtitle(label = "Predicted Peak SWE for WY 2010", subtitle = "Log SWE, 0 Zero points") +
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(location = "bl") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
RFModel2_plot
ggsave("/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/Plots/RFModel2_plot.png")

### Model 3
RFModel3_plot <- ggplot() +
  theme_bw() +
  geom_spatraster(data = RF3_PredictionMap) +
  scale_fill_princess_c(palette = "snow", direction = 1, name = "Peak SWE (mm)") +
  geom_sf(data = CONUS_AOI, fill = NA, color = "black") +
  ggtitle(label = "Predicted Peak SWE for WY 2010", subtitle = "Raw SWE, 10 zero points") +
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(location = "bl") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
RFModel3_plot
ggsave("/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/Plots/RFModel3_plot.png")

### Model 4
RFModel4_plot <- ggplot() +
  theme_bw() +
  geom_spatraster(data = RF4_PredictionMap_exp) +
  scale_fill_princess_c(palette = "snow", direction = 1, name = "Peak SWE (mm)") +
  geom_sf(data = CONUS_AOI, fill = NA, color = "black") +
  ggtitle(label = "Predicted Peak SWE for WY 2010", subtitle = "Log SWE, 10 Zero points") +
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(location = "bl") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
RFModel4_plot
ggsave("/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/Plots/RFModel4_plot.png")

### Model 5
RFModel5_plot <- ggplot() +
  theme_bw() +
  geom_spatraster(data = RF5_PredictionMap) +
  scale_fill_princess_c(palette = "snow", direction = 1, name = "Peak SWE (mm)") +
  geom_sf(data = CONUS_AOI, fill = NA, color = "black") +
  ggtitle(label = "Predicted Peak SWE for WY 2010", subtitle = "Raw SWE 25 Zero pts") +
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(location = "bl") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
RFModel5_plot
ggsave("/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/Plots/RFModel5_plot.png")

### Model 6
RFModel6_plot <- ggplot() +
  theme_bw() +
  geom_spatraster(data = RF6_PredictionMap_exp) +
  scale_fill_princess_c(palette = "snow", direction = 1, name = "Peak SWE (mm)") +
  geom_sf(data = CONUS_AOI, fill = NA, color = "black") +
  ggtitle(label = "Predicted Peak SWE for WY 2010", subtitle = "Model 6 log swe 25 Zero pts") +
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(location = "bl") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
RFModel6_plot
ggsave("/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/Plots/RFModel6_plot.png")

### Model 7
RFModel7_plot <- ggplot() +
  theme_bw() +
  geom_spatraster(data = RF7_PredictionMap) +
  scale_fill_princess_c(palette = "snow", direction = 1, name = "Peak SWE (mm)") +
  geom_sf(data = CONUS_AOI, fill = NA, color = "black") +
  ggtitle(label = "Predicted Peak SWE for WY 2010", subtitle = "Model 7, 50 Zero points") +
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(location = "bl") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
RFModel7_plot
ggsave("/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/Plots/RFModel7_plot.png")

### Model 8
RFModel8_plot <- ggplot() +
  theme_bw() +
  geom_spatraster(data = RF8_PredictionMap_exp) +
  scale_fill_princess_c(palette = "snow", direction = 1, name = "Peak SWE (mm)") +
  geom_sf(data = CONUS_AOI, fill = NA, color = "black") +
  ggtitle(label = "Predicted Peak SWE for WY 2010", subtitle = "Model 8 (log) 50 zero points") +
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(location = "bl") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
RFModel8_plot
ggsave("/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/Plots/RFModel8_plot.png")

### Model 9
RFModel9_plot <- ggplot() +
  theme_bw() +
  geom_spatraster(data = RF9_PredictionMap) +
  scale_fill_princess_c(palette = "snow", direction = 1, name = "Peak SWE (mm)") +
  geom_sf(data = CONUS_AOI, fill = NA, color = "black") +
  ggtitle(label = "Predicted Peak SWE for WY 2010", subtitle = "Model 9, 100 Zero points") +
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(location = "bl") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
RFModel9_plot
ggsave("/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/Plots/RFModel9_plot.png")

### Model 10
RFModel10_plot <- ggplot() +
  theme_bw() +
  geom_spatraster(data = RF10_PredictionMap_exp) +
  scale_fill_princess_c(palette = "snow", direction = 1, name = "Peak SWE (mm)") +
  geom_sf(data = CONUS_AOI, fill = NA, color = "black") +
  ggtitle(label = "Predicted Peak SWE for WY 2010", subtitle = "Model 10, log(peak_swe), 100 Zero Points") +
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(location = "bl") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
RFModel10_plot
ggsave("/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/Plots/RFModel10_plot.png")


### Model 11
RFModel11_plot <- ggplot() +
  theme_bw() +
  geom_spatraster(data = RF11_PredictionMap) +
  scale_fill_princess_c(palette = "snow", direction = 1, name = "Peak SWE (mm)") +
  geom_sf(data = CONUS_AOI, fill = NA, color = "black") +
  ggtitle(label = "Predicted Peak SWE for WY 2010", subtitle = "Model 11, 150 Zero points") +
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(location = "bl") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
RFModel11_plot
ggsave("/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/Plots/RFModel11_plot.png")

### Model 12
RFModel12_plot <- ggplot() +
  theme_bw() +
  geom_spatraster(data = RF12_PredictionMap_exp) +
  scale_fill_princess_c(palette = "snow", direction = 1, name = "Peak SWE (mm)") +
  geom_sf(data = CONUS_AOI, fill = NA, color = "black") +
  ggtitle(label = "Predicted Peak SWE for WY 2010", subtitle = "Model 12, log(peak_swe), 150 Zero Points") +
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(location = "bl") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
RFModel12_plot
ggsave("/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/Plots/RFModel12_plot.png")


### Model 13
RFModel13_plot <- ggplot() +
  theme_bw() +
  geom_spatraster(data = RF13_PredictionMap) +
  scale_fill_princess_c(palette = "snow", direction = 1, name = "Peak SWE (mm)") +
  geom_sf(data = CONUS_AOI, fill = NA, color = "black") +
  ggtitle(label = "Predicted Peak SWE for WY 2010", subtitle = "Model 13, 200 Zero points") +
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(location = "bl") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
RFModel13_plot
ggsave("/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/Plots/RFModel13_plot.png")

### Model 14
RFModel14_plot <- ggplot() +
  theme_bw() +
  geom_spatraster(data = RF14_PredictionMap_exp) +
  scale_fill_princess_c(palette = "snow", direction = 1, name = "Peak SWE (mm)") +
  geom_sf(data = CONUS_AOI, fill = NA, color = "black") +
  ggtitle(label = "Predicted Peak SWE for WY 2010", subtitle = "Model 14, log(peak_swe), 200 Zero Points") +
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(location = "bl") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
RFModel14_plot
ggsave("/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/Plots/RFModel14_plot.png")


### Model 15
RFModel15_plot <- ggplot() +
  theme_bw() +
  geom_spatraster(data = RF15_PredictionMap) +
  scale_fill_princess_c(palette = "snow", direction = 1, name = "Peak SWE (mm)") +
  geom_sf(data = CONUS_AOI, fill = NA, color = "black") +
  ggtitle(label = "Predicted Peak SWE for WY 2010", subtitle = "Model 15, 250 Zero points") +
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(location = "bl") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
RFModel15_plot
ggsave("/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/Plots/RFModel15_plot.png")

### Model 16
RFModel16_plot <- ggplot() +
  theme_bw() +
  geom_spatraster(data = RF16_PredictionMap_exp) +
  scale_fill_princess_c(palette = "snow", direction = 1, name = "Peak SWE (mm)") +
  geom_sf(data = CONUS_AOI, fill = NA, color = "black") +
  ggtitle(label = "Predicted Peak SWE for WY 2010", subtitle = "Model 16, log(peak_swe), 250 Zero Points") +
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(location = "bl") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
RFModel16_plot
ggsave("/Users/gshelor/Documents/GEOG607Data/Project_Data/Outputs/Plots/RFModel16_plot.png")
