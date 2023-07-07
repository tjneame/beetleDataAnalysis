#set up----------------------------------------------------
library(tidyverse)
library(mgcv)
library(gratia)

#ELYTRA LENGTH by distance and GDD
#Outdated: read in previously saved models--------------------------------
m1<-read_rds("elytraLength_GAMSHASH_2.rds")
m2<-read_rds("elytraLength_GAMSHASH_3.rds")
m3<-read_rds("elytraLength_GAMGAULSS_9.rds")
m4<-read_rds("elytraLength_GAMGAULSS_8.rds")
m5<-read_rds("elytraLength_GAMPoly_11.rds")
m6<-read_rds("elytraLength_GAMGAULSS_10.rds")

## Plot the predictions from MGCV GAM SHASH models
#Outdated: m1-----------------------------------------------
summary(m1)
## Capture the original data back from the model
m1D <- m1$model %>%
  as_tibble()

## Points to predict from
toPredict_m1D <- expand.grid(dist=seq(0, 200, by=1),
                             GDD=c(200, 425, 600, 775),
                             year=2021,
                             lon_dup=613521.3,
                             lat_dup=5804287,
                             BLID="41007") %>%
  as_tibble()

## Prepare the predictions, while excluding variance due
## to BLID and within-field spatial autocorrelation
predicted_m1D <- predict.gam(m1, newdata=toPredict_m1D, se.fit=TRUE,
                             type="response",
                             exclude="s(BLID)")

predicted_m1D <- bind_cols(predicted_m1D$fit, predicted_m1D$se.fit)
names(predicted_m1D) <- c("location", "scale", "skew", "kurtosis",
                          "se_location", "se_scale", "se_skew", "se_kurtosis")

predicted_m1D <- predicted_m1D %>%
  bind_cols(toPredict_m1D)

## Plot the location, at the specified levels of GDD
ggplot(predicted_m1D) +
  geom_line(aes(x=dist, y=location, colour=as.factor(GDD)), lwd=2) +
  geom_ribbon(aes(x=dist, ymin=location-2*se_location, ymax=location+2*se_location, fill=as.factor(GDD)), 
              colour=NA, alpha=0.25) +
  facet_wrap(~as.factor(GDD))


## Plot the scale, at the specified levels of GDD
ggplot(predicted_m1D) +
  geom_line(aes(x=dist, y=scale, colour=as.factor(GDD)), lwd=2) +
  geom_ribbon(aes(x=dist, ymin=scale-2*se_scale, ymax=scale+2*se_scale, fill=as.factor(GDD)), 
              colour=NA, alpha=0.25) +
  facet_wrap(~as.factor(GDD))



## Plot the location as the line, and the scale as the ribbon (rather than using the 
## standard errors of these estimates for the ribbon). This is not usually what is 
## shown with a ribbon. So it requires extra special care to explain it to your
## audience (who may have their own -- incorrect -- ideas about what is right)
ggplot(predicted_m1D) +
  geom_line(aes(x=dist, y=location, colour=as.factor(GDD)), lwd=2) +
  geom_ribbon(aes(x=dist, ymin=location+scale, ymax=location-scale, fill=as.factor(GDD)), 
              colour=NA, alpha=0.5) +
  facet_wrap(~as.factor(GDD))
predicted_m1D

#Test if model is good?
appraise(m1) #Probably not applicable because gratia may not know how to deal with SHASH distributions? See note below about QGAM?

#Outdated: m2--------------------------------------------------------
summary(m2)
## Capture the original data back from the model
m2D <- m2$model %>%
  as_tibble()

## Points to predict from
toPredict_m2D <- expand.grid(dist=seq(0, 200, by=1),
                             GDD=518,
                             year=2021,
                             lon_dup=613521.3,
                             lat_dup=5804287,
                             BLID="41007") %>%
  as_tibble()

## Prepare the predictions, while excluding variance due
## to BLID and within-field spatial autocorrelation
predicted_m2D <- predict.gam(m2, newdata=toPredict_m2D, se.fit=TRUE,
                             type="response",
                             exclude="s(BLID)")

predicted_m2D <- bind_cols(predicted_m2D$fit, predicted_m2D$se.fit)
names(predicted_m2D) <- c("location", "scale", "skew", "kurtosis",
                          "se_location", "se_scale", "se_skew", "se_kurtosis")

predicted_m2D <- predicted_m2D %>%
  bind_cols(toPredict_m2D)

## Plot the location, at the specified levels of GDD
ggplot(predicted_m2D) +
  geom_line(aes(x=dist, y=location, colour=as.factor(GDD)), lwd=2) +
  geom_ribbon(aes(x=dist, ymin=location-2*se_location, ymax=location+2*se_location, fill=as.factor(GDD)), 
              colour=NA, alpha=0.25) +
  facet_wrap(~as.factor(GDD))


## Plot the scale, at the specified levels of GDD
ggplot(predicted_m2D) +
  geom_line(aes(x=dist, y=scale, colour=as.factor(GDD)), lwd=2) +
  geom_ribbon(aes(x=dist, ymin=scale-2*se_scale, ymax=scale+2*se_scale, fill=as.factor(GDD)), 
              colour=NA, alpha=0.25) +
  facet_wrap(~as.factor(GDD))



## Plot the location as the line, and the scale as the ribbon (rather than using the 
## standard errors of these estimates for the ribbon). This is not usually what is 
## shown with a ribbon. So it requires extra special care to explain it to your
## audience (who may have their own -- incorrect -- ideas about what is right)
ggplot(predicted_m2D) +
  geom_line(aes(x=dist, y=location, colour=as.factor(GDD)), lwd=2) +
  geom_ribbon(aes(x=dist, ymin=location+scale, ymax=location-scale, fill=as.factor(GDD)), 
              colour=NA, alpha=0.5) +
  facet_wrap(~as.factor(GDD))
predicted_m2D

#Test if model is good?
appraise(m2) #Probably not applicable because gratia may not know how to deal with SHASH distributions? See note below about QGAM?


# plot predictions from MGCV GAM GAULSS models
#Outdated: m3 ---------------------------------------------------------------
summary(m3)
## Capture the original data back from the model
m3D <- m3$model %>%
  as_tibble()

## Points to predict from
toPredict_m3D <- expand.grid(dist=seq(0, 200, by=1),
                             GDD=c(200, 425, 600, 775),
                             year=2021,
                             lon_dup=613521.3,
                             lat_dup=5804287,
                             BLID="41007") %>%
  as_tibble()

## Prepare the predictions, while excluding variance due
## to BLID and within-field spatial autocorrelation
predicted_m3D <- predict.gam(m3, newdata=toPredict_m3D, se.fit=TRUE,
                             type="response")

predicted_m3D <- bind_cols(predicted_m3D$fit, predicted_m3D$se.fit)
names(predicted_m3D) <- c("location", "scale",
                          "se_location", "se_scale")

predicted_m3D <- predicted_m3D %>%
  bind_cols(toPredict_m3D)

## Plot the location, at the specified levels of GDD
ggplot(predicted_m3D) +
  geom_line(aes(x=dist, y=location, colour=as.factor(GDD)), lwd=2) +
  geom_ribbon(aes(x=dist, ymin=location-2*se_location, ymax=location+2*se_location, fill=as.factor(GDD)), 
              colour=NA, alpha=0.25) +
  facet_wrap(~as.factor(GDD))


## Plot the scale, at the specified levels of GDD
ggplot(predicted_m3D) +
  geom_line(aes(x=dist, y=scale, colour=as.factor(GDD)), lwd=2) +
  geom_ribbon(aes(x=dist, ymin=scale-2*se_scale, ymax=scale+2*se_scale, fill=as.factor(GDD)), 
              colour=NA, alpha=0.25) +
  facet_wrap(~as.factor(GDD))



## Plot the location as the line, and the scale as the ribbon (rather than using the 
## standard errors of these estimates for the ribbon). This is not usually what is 
## shown with a ribbon. So it requires extra special care to explain it to your
## audience (who may have their own -- incorrect -- ideas about what is right)
ggplot(predicted_m3D) +
  geom_line(aes(x=dist, y=location, colour=as.factor(GDD)), lwd=2) +
  geom_ribbon(aes(x=dist, ymin=location+scale, ymax=location-scale, fill=as.factor(GDD)), 
              colour=NA, alpha=0.5) +
  facet_wrap(~as.factor(GDD))
predicted_m3D

#Test if model is good?
appraise(m3) #Probably not applicable because gratia may not know how to deal with SHASH distributions? See note below about QGAM?


#Outdated: m4 ------------------------------------------------------------
summary(m4)
## Capture the original data back from the model
m4D <- m4$model %>%
  as_tibble()

## Points to predict from
toPredict_m4D <- expand.grid(dist=seq(0, 200, by=1),
                             GDD=c(200, 425, 600, 775),
                             year=2021,
                             lon_dup=613521.3,
                             lat_dup=5804287,
                             BLID="41007") %>%
  as_tibble()

## Prepare the predictions, while excluding variance due
## to BLID and within-field spatial autocorrelation
predicted_m4D <- predict.gam(m4, newdata=toPredict_m4D, se.fit=TRUE,
                             type="response")

predicted_m4D <- bind_cols(predicted_m4D$fit, predicted_m4D$se.fit)
names(predicted_m4D) <- c("location", "scale",
                          "se_location", "se_scale")

predicted_m4D <- predicted_m4D %>%
  bind_cols(toPredict_m4D)

## Plot the location, at the specified levels of GDD
ggplot(predicted_m4D) +
  geom_line(aes(x=dist, y=location, colour=as.factor(GDD)), lwd=2) +
  geom_ribbon(aes(x=dist, ymin=location-2*se_location, ymax=location+2*se_location, fill=as.factor(GDD)), 
              colour=NA, alpha=0.25) +
  facet_wrap(~as.factor(GDD))


## Plot the scale, at the specified levels of GDD
ggplot(predicted_m4D) +
  geom_line(aes(x=dist, y=scale, colour=as.factor(GDD)), lwd=2) +
  geom_ribbon(aes(x=dist, ymin=scale-2*se_scale, ymax=scale+2*se_scale, fill=as.factor(GDD)), 
              colour=NA, alpha=0.25) +
  facet_wrap(~as.factor(GDD))



## Plot the location as the line, and the scale as the ribbon (rather than using the 
## standard errors of these estimates for the ribbon). This is not usually what is 
## shown with a ribbon. So it requires extra special care to explain it to your
## audience (who may have their own -- incorrect -- ideas about what is right)
ggplot(predicted_m4D) +
  geom_line(aes(x=dist, y=location, colour=as.factor(GDD)), lwd=2) +
  geom_ribbon(aes(x=dist, ymin=location+scale, ymax=location-scale, fill=as.factor(GDD)), 
              colour=NA, alpha=0.5) +
  facet_wrap(~as.factor(GDD))
predicted_m4D

#Test if model is good?
appraise(m4) #Probably not applicable because gratia may not know how to deal with SHASH distributions? See note below about QGAM?

#Outdated: m5 -------------------------------------------------------------------------
summary(m5)
## Capture the original data back from the model
m5D <- m5$model %>%
  as_tibble()

## Points to predict from
toPredict_m5D <- expand.grid(dist=seq(0, 200, by=1),
                             GDD=c(200, 425, 600, 775),
                             year=2021,
                             lon_dup=613521.3,
                             lat_dup=5804287,
                             BLID="41007") %>%
  as_tibble()

## Prepare the predictions, while excluding variance due
## to BLID and within-field spatial autocorrelation
predicted_m5D <- predict.gam(m5, newdata=toPredict_m5D, se.fit=TRUE,
                             type="response")

predicted_m5D <- bind_cols(predicted_m5D$fit, predicted_m5D$se.fit)
names(predicted_m5D) <- c("location", "scale",
                          "se_location", "se_scale")

predicted_m5D <- predicted_m5D %>%
  bind_cols(toPredict_m5D)

## Plot the location, at the specified levels of GDD
ggplot(predicted_m5D) +
  geom_line(aes(x=dist, y=location, colour=as.factor(GDD)), lwd=2) +
  geom_ribbon(aes(x=dist, ymin=location-2*se_location, ymax=location+2*se_location, fill=as.factor(GDD)), 
              colour=NA, alpha=0.25) +
  facet_wrap(~as.factor(GDD))


## Plot the scale, at the specified levels of GDD
ggplot(predicted_m5D) +
  geom_line(aes(x=dist, y=scale, colour=as.factor(GDD)), lwd=2) +
  geom_ribbon(aes(x=dist, ymin=scale-2*se_scale, ymax=scale+2*se_scale, fill=as.factor(GDD)), 
              colour=NA, alpha=0.25) +
  facet_wrap(~as.factor(GDD))



## Plot the location as the line, and the scale as the ribbon (rather than using the 
## standard errors of these estimates for the ribbon). This is not usually what is 
## shown with a ribbon. So it requires extra special care to explain it to your
## audience (who may have their own -- incorrect -- ideas about what is right)
ggplot(predicted_m5D) +
  geom_line(aes(x=dist, y=location, colour=as.factor(GDD)), lwd=2) +
  geom_ribbon(aes(x=dist, ymin=location+scale, ymax=location-scale, fill=as.factor(GDD)), 
              colour=NA, alpha=0.5) +
  facet_wrap(~as.factor(GDD))
predicted_m5D

#Test if model is good?
appraise(m5) #Probably not applicable because gratia may not know how to deal with SHASH distributions? See note below about QGAM?


#Outdated: m6 ------------------------------------------------------------
summary(m6)
## Capture the original data back from the model
m6D <- m6$model %>%
  as_tibble()

## Points to predict from
toPredict_m6D <- expand.grid(dist=seq(0, 200, by=1),
                             GDD=c(200, 425, 600, 775),
                             year=2021,
                             lon_dup=613521.3,
                             lat_dup=5804287,
                             BLID="41007") %>%
  as_tibble()

## Prepare the predictions, while excluding variance due
## to BLID and within-field spatial autocorrelation
predicted_m6D <- predict.gam(m6, newdata=toPredict_m6D, se.fit=TRUE,
                             type="response")

predicted_m6D <- bind_cols(predicted_m6D$fit, predicted_m6D$se.fit)
names(predicted_m6D) <- c("location", "scale",
                          "se_location", "se_scale")

predicted_m6D <- predicted_m6D %>%
  bind_cols(toPredict_m6D)

## Plot the location, at the specified levels of GDD
ggplot(predicted_m6D) +
  geom_line(aes(x=dist, y=location, colour=as.factor(GDD)), lwd=2) +
  geom_ribbon(aes(x=dist, ymin=location-2*se_location, ymax=location+2*se_location, fill=as.factor(GDD)), 
              colour=NA, alpha=0.25) +
  facet_wrap(~as.factor(GDD))


## Plot the scale, at the specified levels of GDD
ggplot(predicted_m6D) +
  geom_line(aes(x=dist, y=scale, colour=as.factor(GDD)), lwd=2) +
  geom_ribbon(aes(x=dist, ymin=scale-2*se_scale, ymax=scale+2*se_scale, fill=as.factor(GDD)), 
              colour=NA, alpha=0.25) +
  facet_wrap(~as.factor(GDD))



## Plot the location as the line, and the scale as the ribbon (rather than using the 
## standard errors of these estimates for the ribbon). This is not usually what is 
## shown with a ribbon. So it requires extra special care to explain it to your
## audience (who may have their own -- incorrect -- ideas about what is right)
ggplot(predicted_m6D) +
  geom_line(aes(x=dist, y=location, colour=as.factor(GDD)), lwd=2) +
  geom_ribbon(aes(x=dist, ymin=location+scale, ymax=location-scale, fill=as.factor(GDD)), 
              colour=NA, alpha=0.5) +
  facet_wrap(~as.factor(GDD))
predicted_m6D

#Test if model is good?
appraise(m6) #Probably not applicable because gratia may not know how to deal with SHASH distributions? See note below about QGAM?


#Notes ----------------------------------------------------------------------
## Here are some ideas for making models run faster
## 1. Assume that skewness and kurtosis are not interesting (therefore use gaulss)(DONE)
## 2. Remove BLID as a random effect (it is clearly not important when the field-level smoothers are added) (DONE)
## 3. Instead of fitting a te(dist, GDD) try dist*GDD or as poly(dist, 4)*GDD 
##    - TRY ON BOTH location and scale at same time (DONE)
## 4. Fit with gaulss (DONE)
## 5. Quantile regression - QGAM (Simon Woods)
## need to center the Lat Lon!!!!

## QGAMS Supplementary code from Paul -----------------------------------------

## Set working directory to location of RDS files
setwd("/Users/tobynneame/Documents/School/MastersData/beetleDataAnalysis/QGAMs")

## LOAD THE QGAM RDS OBJECTS
## PLACE THEM IN A LIST OBJECT
objectsToLoad <- paste0("elytraLength_", "QGAM_", 1:9, "_LINE.rds")
allModels <- map(objectsToLoad, read_rds)
names(allModels) <- objectsToLoad


## Some quick tricks for visualizing results
## These will work with any GAM or qGAM 

## Look at only one of the smooths
## from the fifth model (i.e. median)
## This one shows the map of BLID 41007
draw(smooth_estimates(allModels[[5]], "s(lon_dup,lat_dup):BLID41007"))
## The pattern is:
## draw(smooth_estimates(MODEL_GOES_HERE, "the exact smooth name as it appears in summary"))

## Plotting the quantile smooths for distance

## GRAB THE DATA THAT WAS USED TO CREATE THE MODEL FROM
## THE FIRST QGAM OBJECT
inD <- allModels[[1]]$model

## SPECIFY THE QUANTILES OF GDD THAT YOU WANT TO PREDICT FOR
## (0.43 is the mean in this dataset)
quantilesGDD <- c(0.1, 0.43, 0.9)

## SPECIFY THE DATA ON WHICH TO PREDICT THE SMOOTHS
## These three levels of GDD are roughly the mean, and the
## 25% and 75% quantiles of the GDD distribution.
## (In other words: early season, mid-season and late-season)
## If you change them you need to make sure the numbers
## are consistent further down.
pr_inD <- expand.grid(
  dist = seq(0, 300),
  GDD=c(200,400, 750)) %>%
  bind_cols(
    tibble(BLID="41014",
           lon_dup=0,
           lat_dup=0,
           year="2022")) %>%
  as_tibble()

## PREDICT THE SMOOTHS
## Note how exclude contains the same BLID interaction
## as the the BLID that was used when predicting.
## This is important!
prD <- tibble(quantile=map(allModels, function(x) 
  x$family$getQu()) %>% 
    as.numeric(),
  y_hat=map(allModels, function(x) 
    predict(x, 
            newdata = pr_inD, 
            exclude=c("s(BLID)", "s(lon_dup,lat_dup):BLID41014"),
            type="response", se.fit=TRUE) %>%
      bind_cols() %>%
      bind_cols(pr_inD))) %>%
  unnest(y_hat)

## Draw the quantiles at the chosen levels of GDD
ggplot(prD) +
  geom_line(aes(x=dist, y=fit, colour=quantile, group=quantile)) + 
  facet_wrap(~as.factor(GDD)) 


## Prepare the raw data for plotting using three GDD panels
## identifying which data point is within a specified GDDs of
## the panel mean for plotting
## Here the GDD breaks are 200, 400 and 750, and any point sampled
## more than 75 GDDs on either side of those breaks is removed
## If you change the breaks here, you must change them above too.
inD2 <- inD %>%
  mutate(fA=ifelse(abs(200-GDD) < 75, 200, 0),
         fB=ifelse(abs(400-GDD) < 75, 400, 0),
         fC=ifelse(abs(750-GDD) < 75, 750, 0)) %>%
  mutate(GDD=fA+fB+fC) %>%
  dplyr::select(-c(fA, fB, fC)) %>%
  filter(GDD != 0)



## A three-panel plot showing the beetles collected within +/- 75 GDDs
## of each of the three times, and the relationship with distance
## at nine different deciles.
ggplot(prD) +
  geom_line(aes(x=dist, y=fit, colour=quantile, group=quantile), size=1.25) +
  geom_jitter(data=inD2, aes(x=dist, y=elytraLength), 
              alpha=0.05, width=10, height=0, colour="black") + 
  facet_wrap(~as.factor(GDD), ncol=3) +
  theme_light() +
  scale_colour_viridis_c(option="E") +
  coord_cartesian(ylim=c(1.75,11)) +
  ylab("Elytra length (mm)") +
  xlab("Distance from field margin (m)")

#Try with smooth qgams --------------------------------------------------

## LOAD THE QGAM RDS OBJECTS
## PLACE THEM IN A LIST OBJECT
smoothObjectsToLoad<- c("elytraLength_QGAM_1.rds","elytraLength_QGAM_25.rds","elytraLength_QGAM_5.rds","elytraLength_QGAM_75.rds","elytraLength_QGAM_9.rds")
allModelsSmooth <- map(smoothObjectsToLoad, read_rds)
names(allModelsSmooth) <- smoothObjectsToLoad

## Look at only of the smooths
## from the fifth model (i.e. median)
## This one shows the map of BLID 41007
draw(smooth_estimates(allModelsSmooth[[5]], "s(lon_dup,lat_dup):BLID41007"))
## The pattern is:
## draw(smooth_estimates(MODEL_GOES_HERE, "the exact smooth name as it appears in summary"))

#Plotting the quantile smooths for distance

## GRAB THE DATA THAT WAS USED TO CREATE THE MODEL FROM
## THE FIRST QGAM OBJECT
inDSmooth <- allModelsSmooth[[1]]$model

## SPECIFY THE QUANTILES OF GDD THAT YOU WANT TO PREDICT FOR
## (0.43 is the mean in this dataset)
quantilesGDD <- c(0.1, 0.43, 0.9)

## SPECIFY THE DATA ON WHICH TO PREDICT THE SMOOTHS
## These three levels of GDD are roughly the mean, and the
## 25% and 75% quantiles of the GDD distribution.
## (In other words: early season, mid-season and late-season)
## If you change them you need to make sure the numbers
## are consistent further down.
pr_inD <- expand.grid(
  dist = seq(0, 300),
  GDD=c(200,400, 750)) %>%
  bind_cols(
    tibble(BLID="41014",
           lon_dup=0,
           lat_dup=0,
           year="2022")) %>%
  as_tibble()

## PREDICT THE SMOOTHS
## Note how exclude contains the same BLID interaction
## as the the BLID that was used when predicting.
## This is important!
prDSmooth <- tibble(quantile=map(allModelsSmooth, function(x) 
  x$family$getQu()) %>% 
    as.numeric(),
  y_hat=map(allModelsSmooth, function(x) 
    predict(x, 
            newdata = pr_inD, 
            exclude=c("s(BLID)", "s(lon_dup,lat_dup):BLID41014"),
            type="response", se.fit=TRUE) %>%
      bind_cols() %>%
      bind_cols(pr_inD))) %>%
  unnest(y_hat)

## Draw the quantiles at the chosen levels of GDD
ggplot(prDSmooth) +
  geom_line(aes(x=dist, y=fit, colour=quantile, group=quantile)) + 
  facet_wrap(~as.factor(GDD)) 


## Prepare the raw data for plotting using three GDD panels
## identifying which data point is within a specified GDDs of
## the panel mean for plotting
## Here the GDD breaks are 200, 400 and 750, and any point sampled
## more than 75 GDDs on either side of those breaks is removed
## If you change the breaks here, you must change them above too.
inD2Smooth <- inDSmooth %>%
  mutate(fA=ifelse(abs(200-GDD) < 75, 200, 0),
         fB=ifelse(abs(400-GDD) < 75, 400, 0),
         fC=ifelse(abs(750-GDD) < 75, 750, 0)) %>%
  mutate(GDD=fA+fB+fC) %>%
  dplyr::select(-c(fA, fB, fC)) %>%
  filter(GDD != 0)



## A three-panel plot showing the beetles collected within +/- 75 GDDs
## of each of the three times, and the relationship with distance
## at nine different deciles.
ggplot(prDSmooth) +
  geom_line(aes(x=dist, y=fit, colour=quantile, group=quantile), size=1.25) +
  geom_jitter(data=inD2Smooth, aes(x=dist, y=elytraLength), 
              alpha=0.05, width=10, height=0, colour="black") + 
  facet_wrap(~as.factor(GDD), ncol=3) +
  theme_light() +
  scale_colour_viridis_c(option="E") +
  coord_cartesian(ylim=c(1.75,11)) +
  ylab("Elytra length (mm)") +
  xlab("Distance from field margin (m)")



#Try with learning rate smooth qgams -----------------------------------------

## LOAD THE QGAM RDS OBJECTS
## PLACE THEM IN A LIST OBJECT
LRObjectsToLoad<- c("elytraLength_QGAM_1_LR.rds","elytraLength_QGAM_25_LR.rds","elytraLength_QGAM_5_LR.rds","elytraLength_QGAM_75_LR.rds","elytraLength_QGAM_9_LR.rds")
allModelsLR <- map(LRObjectsToLoad, read_rds)
names(allModelsLR) <- LRObjectsToLoad

## Look at only of the smooths
## from the fifth model (i.e. median)
## This one shows the map of BLID 41007
draw(smooth_estimates(allModelsLR[[5]], "s(lon_dup,lat_dup):BLID41007"))
## The pattern is:
## draw(smooth_estimates(MODEL_GOES_HERE, "the exact smooth name as it appears in summary"))

#Plotting the quantile smooths for distance

## GRAB THE DATA THAT WAS USED TO CREATE THE MODEL FROM
## THE FIRST QGAM OBJECT
inDLR <- allModelsLR[[1]]$model

## SPECIFY THE QUANTILES OF GDD THAT YOU WANT TO PREDICT FOR
## (0.43 is the mean in this dataset)
quantilesGDD <- c(0.1, 0.43, 0.9)

## SPECIFY THE DATA ON WHICH TO PREDICT THE SMOOTHS
## These three levels of GDD are roughly the mean, and the
## 25% and 75% quantiles of the GDD distribution.
## (In other words: early season, mid-season and late-season)
## If you change them you need to make sure the numbers
## are consistent further down.
pr_inD <- expand.grid(
  dist = seq(0, 300),
  GDD=c(200,400, 750)) %>%
  bind_cols(
    tibble(BLID="41014",
           lon_dup=0,
           lat_dup=0,
           year="2022")) %>%
  as_tibble()

## PREDICT THE SMOOTHS
## Note how exclude contains the same BLID interaction
## as the the BLID that was used when predicting.
## This is important!
prDLR <- tibble(quantile=map(allModelsLR, function(x) 
  x$family$getQu()) %>% 
    as.numeric(),
  y_hat=map(allModelsLR, function(x) 
    predict(x, 
            newdata = pr_inD, 
            exclude=c("s(BLID)", "s(lon_dup,lat_dup):BLID41014"),
            type="response", se.fit=TRUE) %>%
      bind_cols() %>%
      bind_cols(pr_inD))) %>%
  unnest(y_hat)

## Draw the quantiles at the chosen levels of GDD
ggplot(prDLR) +
  geom_line(aes(x=dist, y=fit, colour=quantile, group=quantile)) + 
  facet_wrap(~as.factor(GDD)) 


## Prepare the raw data for plotting using three GDD panels
## identifying which data point is within a specified GDDs of
## the panel mean for plotting
## Here the GDD breaks are 200, 400 and 750, and any point sampled
## more than 75 GDDs on either side of those breaks is removed
## If you change the breaks here, you must change them above too.
inD2LR <- inDLR %>%
  mutate(fA=ifelse(abs(200-GDD) < 75, 200, 0),
         fB=ifelse(abs(400-GDD) < 75, 400, 0),
         fC=ifelse(abs(750-GDD) < 75, 750, 0)) %>%
  mutate(GDD=fA+fB+fC) %>%
  dplyr::select(-c(fA, fB, fC)) %>%
  filter(GDD != 0)



## A three-panel plot showing the beetles collected within +/- 75 GDDs
## of each of the three times, and the relationship with distance
## at nine different deciles.
ggplot(prDLR) +
  geom_line(aes(x=dist, y=fit, colour=quantile, group=quantile), size=1.25) +
  geom_jitter(data=inD2LR, aes(x=dist, y=elytraLength), 
              alpha=0.05, width=10, height=0, colour="black") + 
  facet_wrap(~as.factor(GDD), ncol=3) +
  theme_light() +
  scale_colour_viridis_c(option="E") +
  coord_cartesian(ylim=c(1.75,11)) +
  ylab("Elytra length (mm)") +
  xlab("Distance from field margin (m)")



#Visualize SHASH Models-------------------------------------------------------
## Set working directory to location of RDS files
setwd("/Users/tobynneame/Documents/School/MastersData/beetleDataAnalysis/SHASH")
## LOAD THE GAM RDS OBJECTS
m13<-read_rds("elytraLength_GAMSHASH_13.rds")
m14<-read_rds("elytraLength_GAMSHASH_14.rds")
m15<-read_rds("elytraLength_GAMSHASH_15.rds")
m16<-read_rds("elytraLength_GAMSHASH_16.rds")

#m13--------------------------------------------------------------------------
summary(m13)
## Capture the original data back from the model
m13D <- m13$model %>%
  as_tibble()

## Points to predict from
toPredict_m13D <- expand.grid(dist=seq(0, 200, by=1),
                             GDD=c(200, 425, 600, 775),
                             year=2021,
                             lon_dup=613521.3,
                             lat_dup=5804287,
                             BLID="41007") %>%
  as_tibble()

## Prepare the predictions, while excluding variance due
## to BLID and within-field spatial autocorrelation
predicted_m13D <- predict.gam(m13, newdata=toPredict_m13D, se.fit=TRUE,
                             type="response",
                             exclude="s(BLID)")

predicted_m13D <- bind_cols(predicted_m13D$fit, predicted_m13D$se.fit)
names(predicted_m13D) <- c("location", "scale", "skew", "kurtosis",
                          "se_location", "se_scale", "se_skew", "se_kurtosis")

predicted_m13D <- predicted_m13D %>%
  bind_cols(toPredict_m13D)

## Plot the location, at the specified levels of GDD
ggplot(predicted_m13D) +
  geom_line(aes(x=dist, y=location, colour=as.factor(GDD)), lwd=2) +
  geom_ribbon(aes(x=dist, ymin=location-2*se_location, ymax=location+2*se_location, fill=as.factor(GDD)), 
              colour=NA, alpha=0.25) +
  facet_wrap(~as.factor(GDD))


## Plot the scale, at the specified levels of GDD
ggplot(predicted_m13D) +
  geom_line(aes(x=dist, y=scale, colour=as.factor(GDD)), lwd=2) +
  geom_ribbon(aes(x=dist, ymin=scale-2*se_scale, ymax=scale+2*se_scale, fill=as.factor(GDD)), 
              colour=NA, alpha=0.25) +
  facet_wrap(~as.factor(GDD))



## Plot the location as the line, and the scale as the ribbon (rather than using the 
## standard errors of these estimates for the ribbon). This is not usually what is 
## shown with a ribbon. So it requires extra special care to explain it to your
## audience (who may have their own -- incorrect -- ideas about what is right)
ggplot(predicted_m13D) +
  geom_line(aes(x=dist, y=location, colour=as.factor(GDD)), lwd=2) +
  geom_ribbon(aes(x=dist, ymin=location+scale, ymax=location-scale, fill=as.factor(GDD)), 
              colour=NA, alpha=0.5) +
  facet_wrap(~as.factor(GDD))
predicted_m13D


#Visualisation of Abundance GAMS--------------------------------------
#read in the models
setwd("/Users/tobynneame/Documents/School/MastersData/beetleDataAnalysis/abundance")
m1<-read_rds("beetCountGAMNB_1.rds") #smooth
m2<-read_rds("beetCountGAMNB_2.rds") #linear
m3<-read_rds("beetCountGAMNB_3.rds") #linear
m4<-read_rds("beetCountGAMNB_4.rds") #crop v non-crop
m5<-read_rds("beetCountGAMNB_5.rds") #crop v non-crop

#decide which model to use
AIC(m1, m2, m3)
AIC(m4, m5)

#visualize using Sam's code - number of beetles over distance
newdat <- expand.grid(dist=seq(0,200,by=5),GDD=c(300,500,700),inCrop=TRUE,
                      year='2021',BLID='41007',lon_dup=0,lat_dup=0) 
newdat <- predict.gam(m1,newdata=newdat,se.fit = TRUE,
                      exclude = c('s(BLID)',paste0('s(lon_dup,lat_dup):BLID',levels(beetDatAbCrop$BLID)))) %>% 
  do.call('data.frame',.) %>% 
  mutate(upr=fit+se.fit*1.96,lwr=fit-se.fit*1.96) %>% 
  mutate(across(c(fit,upr,lwr),exp)) %>% 
  bind_cols(dplyr::select(newdat,dist,GDD),.)

cols <- c('blue','purple','red')

(p <- newdat %>% mutate(GDD=factor(GDD,labels = c('Early (300)','Mid (500)','Late (700)'))) %>% 
    ggplot(aes(x=dist))+geom_ribbon(aes(ymax=upr,ymin=lwr,fill=GDD),alpha=0.2)+
    geom_line(aes(y=fit,col=GDD))+
    geom_text(data=beetDatAbCrop,aes(x=dist,y=0.05),label='|',position=position_jitter(width = 1, height=0),alpha=0.4,size=2)+
    labs(x='Distance from field edge',y='Number of carabids')+
    xlim(0,200)+
    scale_color_manual(values=cols)+scale_fill_manual(values=cols)+
    theme(legend.position = c(0.85,0.85),legend.background = element_rect(colour='grey'))
)

(p <- newdat %>% mutate(GDD=factor(GDD,labels = c('Early (300)','Mid (500)','Late (700)'))) %>% 
    ggplot()+geom_ribbon(aes(x=dist,ymax=upr,ymin=lwr),alpha=0.2)+
    geom_line(aes(x=dist,y=fit))+
    geom_text(data=dplyr::select(beetDatAbCrop,dist),aes(x=dist,y=0.05),label='|',position=position_jitter(width = 1, height=0),alpha=0.4,size=2)+
    facet_wrap(~GDD)+
    labs(x='Distance from field edge',y='Number of carabids')+
    xlim(0,200)+
    scale_color_manual(values=cols)+scale_fill_manual(values=cols)+
    theme(legend.position = c(0.15,0.85),legend.background = element_rect(colour='grey'))
)

#visualize number of beetles over time
newdat2 <- expand.grid(GDD=seq(0,900,by=5),dist=c(5,100,200),
                      year='2021',BLID='41007',lon_dup=0,lat_dup=0) 
newdat2 <- predict.gam(m1,newdata=newdat2,se.fit = TRUE,
                      exclude = c('s(BLID)',paste0('s(lon_dup,lat_dup):BLID',levels(beetDatAbCrop$BLID)))) %>% 
  do.call('data.frame',.) %>% 
  mutate(upr=fit+se.fit*1.96,lwr=fit-se.fit*1.96) %>% 
  mutate(across(c(fit,upr,lwr),exp)) %>% 
  bind_cols(dplyr::select(newdat2,GDD,dist),.)

cols <- c('blue','purple','red')

(p <- newdat2 %>% mutate(dist=factor(dist,labels = c('Near (5)','Mid (100)','Far (200)'))) %>% 
    ggplot()+geom_ribbon(aes(x=GDD,ymax=upr,ymin=lwr),alpha=0.2)+
    geom_line(aes(x=GDD,y=fit))+
    geom_text(data=dplyr::select(beetDatAbCrop,GDD),aes(x=GDD,y=0.05),label='|',position=position_jitter(width = 1, height=0),alpha=0.4,size=2)+
    facet_wrap(~dist)+
    labs(x='time in GDD',y='Number of carabids')+
    xlim(125,900)+
    scale_color_manual(values=cols)+scale_fill_manual(values=cols)+
    theme(legend.position = c(0.15,0.85),legend.background = element_rect(colour='grey'))
)
(p <- newdat2 %>% mutate(dist=factor(dist,labels = c('Near (5)','Mid (100)','Far (200)'))) %>% 
    ggplot(aes(x=GDD))+geom_ribbon(aes(ymax=upr,ymin=lwr,fill=dist),alpha=0.2)+
    geom_line(aes(y=fit,col=dist))+
    geom_text(data=beetDatAbCrop,aes(x=GDD,y=0.05),label='|',position=position_jitter(width = 1, height=0),alpha=0.4,size=2)+
    labs(x='time in GDD',y='Number of carabids')+
    xlim(125,900)+
    scale_color_manual(values=cols)+scale_fill_manual(values=cols)+
    theme(legend.position = c(0.5,0.85),legend.background = element_rect(colour='grey'))
)

#Abundance between crop and non-crop
newdat3 <- expand.grid(station=c("Crop", "nonCrop"),GDD=c(300,500,700),
                      year='2021',BLID='41007',lon_dup=0,lat_dup=0) 
newdat3 <- predict.gam(m5,newdata=newdat3,se.fit = TRUE,
                      exclude = c('s(BLID)',paste0('s(lon_dup,lat_dup):BLID',levels(beetDatAbNC$BLID)))) %>% 
  do.call('data.frame',.) %>% 
  mutate(upr=fit+se.fit*1.96,lwr=fit-se.fit*1.96) %>% 
  mutate(across(c(fit,upr,lwr),exp)) %>% 
  bind_cols(dplyr::select(newdat3,station,GDD),.)

(p <- newdat3 %>% mutate(GDD=factor(GDD,labels = c('Early (300)','Mid (500)','Late (700)'))) %>% 
    ggplot()+geom_errorbar(aes(x=station,ymax=upr,ymin=lwr),alpha=0.2)+
    geom_point(aes(x=station,y=fit))+
    #geom_text(data=dplyr::select(beetDatAbNC,station),aes(x=station,y=0.05),label='|',position=position_jitter(width = 1, height=0),alpha=0.4,size=2)+
    facet_wrap(~GDD)+
    labs(x='crop vs non-crop',y='Number of carabids')+
    #xlim()+
    scale_color_manual(values=cols)+scale_fill_manual(values=cols)
)
