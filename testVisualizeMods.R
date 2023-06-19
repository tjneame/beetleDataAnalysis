#set up----------------------------------------------------
library(tidyverse)
library(mgcv)
library(gratia)

#read in previously saved models
m1<-read_rds("elytraLength_GAMSHASH_2.rds")
m2<-read_rds("elytraLength_GAMSHASH_3.rds")
m3<-read_rds("elytraLength_GAMGAULSS_9.rds")
m4<-read_rds("elytraLength_GAMGAULSS_8.rds")
m5<-read_rds("elytraLength_GAMPoly_11.rds")
m6<-read_rds("elytraLength_GAMGAULSS_10.rds")

## Plot the predictions from an MGCV SHASH or GAULSS model

## m1-----------------------------------------------
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

#A better way to show raw data?
ggplot(m1D) + geom_density2d_filled(aes(x=dist, y=elytraLength), bins=50) +
  theme(legend.position="none") + 
  geom_jitter(aes(x=dist, y=elytraLength), width=3, colour="white", alpha=0.05, size=0.1)

#Test if model is good?
appraise(m1) #Probably not applicable because gratia may not know how to deal with SHASH distributions? See note below about QGAM?

#m2--------------------------------------------------------
summary(m2)
## Capture the original data back from the model
m2D <- m2$model %>%
  as_tibble()

## Points to predict from
toPredict_m2D <- expand.grid(dist=seq(0, 200, by=1),
                             GDD=c(200, 425, 600, 775),
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

#A better way to show raw data?
ggplot(m2D) + geom_density2d_filled(aes(x=dist, y=elytraLength), bins=50) +
  theme(legend.position="none") + 
  geom_jitter(aes(x=dist, y=elytraLength), width=3, colour="white", alpha=0.05, size=0.1)

#Test if model is good?
appraise(m2) #Probably not applicable because gratia may not know how to deal with SHASH distributions? See note below about QGAM?


# test predictions from GAULSS models------------------------------
#m3 ---------------------------------------------------------------
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

#A better way to show raw data?
ggplot(m3D) + geom_density2d_filled(aes(x=dist, y=elytraLength), bins=50) +
  theme(legend.position="none") + 
  geom_jitter(aes(x=dist, y=elytraLength), width=3, colour="white", alpha=0.05, size=0.1)

#Test if model is good?
appraise(m3) #Probably not applicable because gratia may not know how to deal with SHASH distributions? See note below about QGAM?


#m4 ------------------------------------------------------------
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

#A better way to show raw data?
ggplot(m4D) + geom_density2d_filled(aes(x=dist, y=elytraLength), bins=50) +
  theme(legend.position="none") + 
  geom_jitter(aes(x=dist, y=elytraLength), width=3, colour="white", alpha=0.05, size=0.1)

#Test if model is good?
appraise(m4) #Probably not applicable because gratia may not know how to deal with SHASH distributions? See note below about QGAM?

#m5 -------------------------------------------------------------------------
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

#A better way to show raw data? DOESNT WORK BECAUSE OF THE POLYNOMIAL?
ggplot(m5D) + geom_density2d_filled(aes(x=dist, y=elytraLength), bins=50) +
  theme(legend.position="none") + 
  geom_jitter(aes(x=dist, y=elytraLength), width=3, colour="white", alpha=0.05, size=0.1)

#Test if model is good?
appraise(m5) #Probably not applicable because gratia may not know how to deal with SHASH distributions? See note below about QGAM?


#m6 ------------------------------------------------------------
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

#A better way to show raw data?
ggplot(m6D) + geom_density2d_filled(aes(x=dist, y=elytraLength), bins=50) +
  theme(legend.position="none") + 
  geom_jitter(aes(x=dist, y=elytraLength), width=3, colour="white", alpha=0.05, size=0.1)

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