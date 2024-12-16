##### SWE Stan Models #####

### loading packages
library(pacman)
p_load(tidyverse, here, gstat, sf, terra, tidyterra, rstan, rsample, spatialsample, RColorBrewer, viridis, ModelMetrics)


##### Reading in Data #####
##### reading in data #####
Snotel2010 <- read_sf("/Users/gshelor/Documents/GEOG607Data/Project_Data/SNOTEL/Snotel2010_CONUS_PeakSWE_Covars.gpkg")
Snotel2010$lc_class <- as.factor(Snotel2010$lc_class)
CONUS_AOI <- read_sf("/Users/gshelor/Documents/GEOG607Data/Project_Data/CONUS_AOI.gpkg")

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

##### Stacking Rasters and splitting data for making spatial predictions #####
Prediction_Rasters <- c(tmean_CDM_Sum, prcpSum_CDM_Sum, srad_mean, tmin_mean, tmax_mean, CONUS_DEM, CONUS_Slope)
Prediction_Rasters
names(Prediction_Rasters) <- c("tmean_CDM_sum", "prcpSum_CDM_sum","srad_mean", "tmin_mean", "tmax_mean", "elevation", "slope")
names(Prediction_Rasters)

### splitting dataset into training and testing
set.seed(802)
data_split <- initial_split(as.data.frame(Snotel2010, xy = TRUE), prop = 0.75)
Snotel_training <- training(data_split)
Snotel_test <- testing(data_split)


##### Stan Model 1 (modeling peak SWE) #####
### making list of data to declare what goes into stan model
SWEStan_datalist <- list(N = nrow(Snotel_training), peak_swe = Snotel_training$peak_swe, tmean_CDM_sum = Snotel_training$tmean_CDM_sum, prcpSum_CDM_sum = Snotel_training$prcpSum_CDM_sum, srad_mean = Snotel_training$srad_mean, tmin_mean = Snotel_training$tmin_mean, tmax_mean = Snotel_training$tmax_mean, elevation = Snotel_training$elevation, slope = Snotel_training$slope)

### fitting stan model
set.seed(802)
options(mc.cores = parallel::detectCores())
SWEStan_fit <- stan(file = here("Project", "Scripts", "Stan", "SWE2010Model.stan"), data = SWEStan_datalist, chains = 3, iter = 5000, warmup = 2000, seed = 802)
SWEStan_fit


### Extracting Parameters
SWEStan_pars <- rstan::extract(SWEStan_fit, c("b0", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "sigma"))

### creating matrix to hold ratings
### adding in process uncertainty
# Off_VoA_Ratings <- matrix(NA, length(Off_VoA_pars$b0), nrow(VoA_Variables))
### creating empty concatenation of rasters
SWEStan_Rasters <- c()

### creating rasters (to be averaged)
set.seed(802)
for (p in 1:length(SWEStan_pars$b0)){
  temprast <- SWEStan_pars$b0[p] + SWEStan_pars$b1[p] * Prediction_Rasters[[1]] + SWEStan_pars$b2[p] * Prediction_Rasters[[2]] + SWEStan_pars$b3[p] * Prediction_Rasters[[3]] + SWEStan_pars$b4[p] * Prediction_Rasters[[4]] + SWEStan_pars$b5[p] * Prediction_Rasters[[5]] + SWEStan_pars$b6[p] * Prediction_Rasters[[6]] + SWEStan_pars$b7[p]
  SWEStan_Rasters <- c(SWEStan_Rasters, temprast)
}

MeanSWERaster_Mod1 <- app(SWEStan_Rasters, "mean")

### generating median and mean and quantile ratings
# MeanPred <- apply(Off_VoA_Ratings,2,mean)
# MedianPred <- apply(Off_VoA_Ratings,2,median)
# Upper <- apply(Off_VoA_Ratings,2,quantile, prob=.95)
# Lower <- apply(Off_VoA_Ratings,2,quantile, prob=.05)



##### Stan Model 2 (modeling log of peak SWE) #####
### making list of data to declare what goes into stan model
SWEStan_datalist2 <- list(N = nrow(Snotel_training), peak_swe = Snotel_training$log_peak_swe, tmean_CDM_sum = Snotel_training$tmean_CDM_sum, prcpSum_CDM_sum = Snotel_training$prcpSum_CDM_sum, srad_mean = Snotel_training$srad_mean, tmin_mean = Snotel_training$tmin_mean, tmax_mean = Snotel_training$tmax_mean, elevation = Snotel_training$elevation, slope = Snotel_training$slope)

### fitting stan model
set.seed(802)
options(mc.cores = parallel::detectCores())
SWEStan_fit2 <- stan(file = here("Project", "Scripts", "Stan", "SWE2010Model.stan"), data = SWEStan_datalist2, chains = 3, iter = 5000, warmup = 2000, seed = 802)
SWEStan_fit2


### Extracting Parameters
SWEStan_pars2 <- rstan::extract(SWEStan_fit2, c("b0", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "sigma"))

### creating matrix to hold ratings
### adding in process uncertainty
# Off_VoA_Ratings <- matrix(NA, length(Off_VoA_pars$b0), nrow(VoA_Variables))
### creating empty concatenation of rasters
SWEStan_Rasters2 <- c()

### creating rasters (to be averaged)
set.seed(802)
for (p in 1:length(SWEStan_pars2$b0)){
  temprast <- SWEStan_pars2$b0[p] + SWEStan_pars2$b1[p] * Prediction_Rasters[[1]] + SWEStan_pars2$b2[p] * Prediction_Rasters[[2]] + SWEStan_pars2$b3[p] * Prediction_Rasters[[3]] + SWEStan_pars2$b4[p] * Prediction_Rasters[[4]] + SWEStan_pars2$b5[p] * Prediction_Rasters[[5]] + SWEStan_pars2$b6[p] * Prediction_Rasters[[6]] + SWEStan_pars2$b7[p]
  SWEStan_Rasters2 <- c(SWEStan_Rasters2, temprast)
}

MeanSWERaster_Mod2 <- exp(app(SWEStan_Rasters2, "mean"))



### generating median and mean and quantile ratings
# MeanPred <- apply(Off_VoA_Ratings,2,mean)
# MedianPred <- apply(Off_VoA_Ratings,2,median)
# Upper <- apply(Off_VoA_Ratings,2,quantile, prob=.95)
# Lower <- apply(Off_VoA_Ratings,2,quantile, prob=.05)