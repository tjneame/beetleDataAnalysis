#set up----------------------------------------------------
library(tidyverse)
library(mgcv)
library(gratia)

setwd("/Users/tobynneame/Documents/School/MastersData/beetleDataAnalysis")
#read in CSV
#If working locally
beetDat<-read_csv("beetleData.csv")
beetDatAb<-read_csv("beetleDataAbundance.csv")
#If working remotely
#beetDat<-read_sheet("https://docs.google.com/spreadsheets/d/1JZwSNahFrIhxU3e5hNoBBQpLskxCO9A_Top1VlaJe0Y/edit#gid=140253595")
#beetDatAb<-read_sheet("https://docs.google.com/spreadsheets/d/10-PmZo250m1IqwaqbrAOjKIBZQAassMdgP70SG5OVwI/edit?usp=sharing")
is_tibble(beetDat)

#make some columns into factors that have been erroneously read as integers and take out the extra BLID
beetDat <- beetDat %>%
  mutate(BLID=as.factor(BLID)) %>%
  mutate(year=as.factor(year)) %>%
  filter(BLID!="51056") %>%
  droplevels()

beetDatAb <- beetDatAb %>%
  mutate(BLID=as.factor(BLID)) %>%
  mutate(year=as.factor(year)) %>%
  mutate(station=as.factor(station)) %>%
  filter(BLID!="51056") %>%
  droplevels() 

#Center the lat-lon on their means 
beetDat<-beetDat %>%
  group_by(BLID) %>% mutate(cLon=mean(lon_dup),cLat=mean(lat_dup)) %>%
  ungroup()%>% mutate(lon_dup=lon_dup-cLon,lat_dup=lat_dup-cLat)

beetDatAb<-beetDatAb %>%
  group_by(BLID) %>% mutate(cLon=mean(lon_dup),cLat=mean(lat_dup)) %>%
  ungroup()%>% mutate(lon_dup=lon_dup-cLon,lat_dup=lat_dup-cLat)

#make NX give negative values for dist
beetDat <- beetDat %>% 
  mutate(dist = case_when(station == "N1" ~ -dist,
                          station == "N2" ~ -dist,
                          station == "N3" ~ -dist,
                          station == "N3" ~ -dist,
                          station == "N4" ~ -dist,
                          station == "N5" ~ -dist,
                          station == "N6" ~ -dist,
                          TRUE ~ dist)) 

beetDatAb <- beetDatAb %>% 
  mutate(dist = case_when(station == "N1" ~ -dist,
                          station == "N2" ~ -dist,
                          station == "N3" ~ -dist,
                          station == "N3" ~ -dist,
                          station == "N4" ~ -dist,
                          station == "N5" ~ -dist,
                          station == "N6" ~ -dist,
                          TRUE ~ dist)) 


#make datasets for different analyses
#first take out variables that wont ever be used (can be commented out so that they can be used if they need to be)
beetDat<-beetDat %>% 
  dplyr::select(-BBID) %>% #BBID
  dplyr::select(-bodyLength) %>% #body length
  dplyr::select(-order) %>% #Order
  dplyr::select(-family) %>% #Family
  dplyr::select(-genus) %>% #genus
  dplyr::select(-species) %>% #species
  dplyr::select(-identifyer) %>% #identifyer
  dplyr::select(-NOTES) %>% #Notes
  dplyr::select(-trapID) %>% #trapID
  dplyr::select(-stnDate) %>% #stnDate
  dplyr::select(-BTID) %>% #BTID
  dplyr::select(-weatherStn) %>% #weather stn
  dplyr::select(-midDate) %>% #midDate
  dplyr::select(-GDDSourceFlag) %>% #GDD source flag
  dplyr::select(-cLon,-cLat) #clon and clat

beetDatAb<-beetDatAb %>% 
  dplyr::select(-cLon,-cLat) #clon and clat

#crop only still with distance
beetDatCrop<-beetDat %>%
  filter(station != "N1",
         station != "N2",
         station != "N3",
         station != "N3",
         station != "N4",
         station != "N5",
         station != "N6") %>%
  drop_na()

beetDatAbCrop<-beetDatAb %>%
  filter(station != "N1",
         station != "N2",
         station != "N3",
         station != "N3",
         station != "N4",
         station != "N5",
         station != "N6") %>%
  drop_na()

#NonCrop and crop with no distance
beetDatN<-beetDat %>% 
  filter(station != "C1",
         station != "C2",
         station != "C3",
         station != "C3",
         station != "C4",
         station != "C5",
         station != "C6",
         station != "C7",
         station != "C8",
         station != "C9") %>%
  mutate(station="nonCrop")

beetDatC<-beetDat %>%
  filter(station != "N1",
         station != "N2",
         station != "N3",
         station != "N3",
         station != "N4",
         station != "N5",
         station != "N6") %>%
  mutate(station="Crop")

beetDatNC<-bind_rows(beetDatC,beetDatN) %>%
  dplyr::select(-dist)%>%
  mutate(station=as.factor(station))%>%
  drop_na()

rm(beetDatC)
rm(beetDatN)

beetDatAbN<-beetDatAb %>% 
  filter(station != "C1",
         station != "C2",
         station != "C3",
         station != "C3",
         station != "C4",
         station != "C5",
         station != "C6",
         station != "C7",
         station != "C8",
         station != "C9") %>%
  mutate(station="nonCrop")

beetDatAbC<-beetDatAb %>%
  filter(station != "N1",
         station != "N2",
         station != "N3",
         station != "N3",
         station != "N4",
         station != "N5",
         station != "N6") %>%
  mutate(station="Crop")

beetDatAbNC<-bind_rows(beetDatAbC,beetDatAbN) %>%
  dplyr::select(-dist)%>%
  mutate(station=as.factor(station))%>%
  drop_na()

rm(beetDatAbC)
rm(beetDatAbN)
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

#m2
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
#Outdated: m3 
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


#Outdated: m4 
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

#Outdated: m5
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


#Outdated: m6 
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


## QGAMS Supplementary code from Paul - Elytra length distance -----------------------------------------

## Set working directory to location of RDS files
setwd("/Users/tobyn.neame/Documents/MScWorking/beetleDataAnalysis/QGAM")

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
  geom_line(aes(x=dist, y=fit, colour=quantile, group=quantile), linewidth=1.25) +
  geom_jitter(data=inD2, aes(x=dist, y=elytraLength), 
              alpha=0.05, width=10, height=0, colour="black") + 
  facet_wrap(~as.factor(GDD), ncol=3) +
  theme_light() +
  scale_colour_viridis_c(option="E") +
  coord_cartesian(ylim=c(1.75,11)) +
  ylab("Elytra length (mm)") +
  xlab("Distance from field margin (m)")

#Try with smooth qgams - elytra length distance --------------------------------------------------

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



#Visualize elytra length distance SHASH Models-------------------

#gam19 
#######################################################/
setwd("C:/Users/tobyn.neame/Documents/MScWorking/beetleDataAnalysis")
gam19<-read_rds("elytraLength_GAMSHASH_19.rds")
summary(gam19)
## Capture the original data back from the model
gam19D <- gam19$model %>%
  as_tibble()

## Points to predict from
toPredict_gam19D <- expand.grid(dist=seq(0, 200, by=1),
                             GDD=c(200, 425, 600, 775),
                             year=2021,
                             lon_dup=0,
                             lat_dup=0,
                             BLID="41007") %>%
  as_tibble()

## Prepare the predictions, while excluding variance due
## to BLID and within-field spatial autocorrelation
predicted_gam19D <- predict.gam(gam19, newdata=toPredict_gam19D, se.fit=TRUE,
                             type="response",
                             exclude=c("s(BLID)", "s(lon_dup,lat_dup):BLID41007"))

predicted_gam19D <- bind_cols(predicted_gam19D$fit, predicted_gam19D$se.fit)
names(predicted_gam19D) <- c("location", "scale", "skew", "kurtosis",
                          "se_location", "se_scale", "se_skew", "se_kurtosis")

predicted_gam19D <- predicted_gam19D %>%
  bind_cols(toPredict_gam19D)

## Plot the location, at the specified levels of GDD
ggplot(predicted_gam19D) +
  geom_line(aes(x=dist, y=location, colour=as.factor(GDD)), lwd=2) +
  geom_ribbon(aes(x=dist, ymin=location-2*se_location, ymax=location+2*se_location, fill=as.factor(GDD)), 
              colour=NA, alpha=0.25) +
  facet_wrap(~as.factor(GDD))


## Plot the scale, at the specified levels of GDD
ggplot(predicted_gam19D) +
  geom_line(aes(x=dist, y=scale, colour=as.factor(GDD)), lwd=2) +
  geom_ribbon(aes(x=dist, ymin=scale-2*se_scale, ymax=scale+2*se_scale, fill=as.factor(GDD)), 
              colour=NA, alpha=0.25) +
  facet_wrap(~as.factor(GDD))



## Plot the location as the line, and the scale as the ribbon (rather than using the 
## standard errors of these estimates for the ribbon). This is not usually what is 
## shown with a ribbon. So it requires extra special care to explain it to your
## audience (who may have their own -- incorrect -- ideas about what is right)
ggplot(predicted_gam19D) +
  geom_line(aes(x=dist, y=location, colour=as.factor(GDD)), lwd=2) +
  geom_ribbon(aes(x=dist, ymin=location+scale, ymax=location-scale, fill=as.factor(GDD)), 
              colour=NA, alpha=0.5) +
  facet_wrap(~as.factor(GDD))
predicted_gam19D

#gam14 (TEST)
#####################################################################/
summary(m14)
## Capture the original data back from the model
m14D <- m14$model %>%
  as_tibble()

## Points to predict from
toPredict_m14D <- expand.grid(dist=seq(0, 200, by=1),
                                GDD=c(200, 425, 600, 775),
                                year=2021,
                                lon_dup=0,
                                lat_dup=0,
                                BLID="41007") %>%
  as_tibble()

## Prepare the predictions, while excluding variance due
## to BLID and within-field spatial autocorrelation
predicted_m14D <- predict.gam(m14, newdata=toPredict_m14D, se.fit=TRUE,
                                type="response",
                                exclude="s(BLID)")

predicted_m14D <- bind_cols(predicted_m14D$fit, predicted_m14D$se.fit)
names(predicted_m14D) <- c("location", "scale", "skew", "kurtosis",
                             "se_location", "se_scale", "se_skew", "se_kurtosis")

predicted_m14D <- predicted_m14D %>%
  bind_cols(toPredict_m14D)

## Plot the location, at the specified levels of GDD
ggplot(predicted_m14D) +
  geom_line(aes(x=dist, y=location, colour=as.factor(GDD)), lwd=2) +
  geom_ribbon(aes(x=dist, ymin=location-2*se_location, ymax=location+2*se_location, fill=as.factor(GDD)), 
              colour=NA, alpha=0.25) +
  facet_wrap(~as.factor(GDD))


## Plot the scale, at the specified levels of GDD
ggplot(predicted_m14D) +
  geom_line(aes(x=dist, y=scale, colour=as.factor(GDD)), lwd=2) +
  geom_ribbon(aes(x=dist, ymin=scale-2*se_scale, ymax=scale+2*se_scale, fill=as.factor(GDD)), 
              colour=NA, alpha=0.25) +
  facet_wrap(~as.factor(GDD))



## Plot the location as the line, and the scale as the ribbon (rather than using the 
## standard errors of these estimates for the ribbon). This is not usually what is 
## shown with a ribbon. So it requires extra special care to explain it to your
## audience (who may have their own -- incorrect -- ideas about what is right)
ggplot(predicted_m14D) +
  geom_line(aes(x=dist, y=location, colour=as.factor(GDD)), lwd=2) +
  geom_ribbon(aes(x=dist, ymin=location+scale, ymax=location-scale, fill=as.factor(GDD)), 
              colour=NA, alpha=0.5) +
  facet_wrap(~as.factor(GDD))
predicted_m14D

#GAM 12 (The big one)
############################################################################/

m12<-read_rds("elytraLength_GAMSHASH_12.rds")
#summary(gam12) #this takes awhile given that the model is 5.6GB
m12D <- m12$model %>%
  as_tibble()

## Points to predict from
toPredict_m12D <- expand.grid(dist=seq(0, 200, by=1),
                              GDD=c(200, 425, 600, 775),
                              year=2021,
                              lon_dup=0,
                              lat_dup=0,
                              BLID="41007") %>%
  as_tibble()

## Prepare the predictions, while excluding variance due
## to BLID and within-field spatial autocorrelation
predicted_m12D <- predict.gam(m12, newdata=toPredict_m12D, se.fit=TRUE,
                              type="response",
                              exclude="s(BLID)")

predicted_m12D <- bind_cols(predicted_m12D$fit, predicted_m12D$se.fit)
names(predicted_m12D) <- c("location", "scale", "skew", "kurtosis",
                           "se_location", "se_scale", "se_skew", "se_kurtosis")

predicted_m12D <- predicted_m12D %>%
  bind_cols(toPredict_m12D)

## Plot the location, at the specified levels of GDD
ggplot(predicted_m12D) +
  geom_line(aes(x=dist, y=location, colour=as.factor(GDD)), lwd=2) +
  geom_ribbon(aes(x=dist, ymin=location-2*se_location, ymax=location+2*se_location, fill=as.factor(GDD)), 
              colour=NA, alpha=0.25) +
  facet_wrap(~as.factor(GDD))


## Plot the scale, at the specified levels of GDD
ggplot(predicted_m12D) +
  geom_line(aes(x=dist, y=scale, colour=as.factor(GDD)), lwd=2) +
  geom_ribbon(aes(x=dist, ymin=scale-2*se_scale, ymax=scale+2*se_scale, fill=as.factor(GDD)), 
              colour=NA, alpha=0.25) +
  facet_wrap(~as.factor(GDD))



## Plot the location as the line, and the scale as the ribbon (rather than using the 
## standard errors of these estimates for the ribbon). This is not usually what is 
## shown with a ribbon. So it requires extra special care to explain it to your
## audience (who may have their own -- incorrect -- ideas about what is right)
ggplot(predicted_m12D) +
  geom_line(aes(x=dist, y=location, colour=as.factor(GDD)), lwd=2) +
  geom_ribbon(aes(x=dist, ymin=location+scale, ymax=location-scale, fill=as.factor(GDD)), 
              colour=NA, alpha=0.5) +
  facet_wrap(~as.factor(GDD))

#Visualize skew
ggplot(predicted_m12D) +
  geom_line(aes(x=dist, y=skew, colour=as.factor(GDD)), lwd=2) +
  geom_ribbon(aes(x=dist, ymin=skew-2*se_skew, ymax=skew+2*se_skew, fill=as.factor(GDD)), 
              colour=NA, alpha=0.25) +
  facet_wrap(~as.factor(GDD))
#Visualize kurtosis
ggplot(predicted_m12D) +
  geom_line(aes(x=dist, y=kurtosis, colour=as.factor(GDD)), lwd=2) +
  geom_ribbon(aes(x=dist, ymin=kurtosis-2*se_kurtosis, ymax=kurtosis+2*se_kurtosis, fill=as.factor(GDD)), 
              colour=NA, alpha=0.25) +
  facet_wrap(~as.factor(GDD))

#visualize crop/noncrop elytra qGAMS-------------------------------------------

setwd("/Users/tobyn.neame/Documents/MScWorking/beetleDataAnalysis/QGAM")

## LOAD THE QGAM RDS OBJECTS
## PLACE THEM IN A LIST OBJECT
objectsToLoad <- paste0("qGAM", 1:9, "LineNC.rds")
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
  station = c("nonCrop","Crop"),
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
  geom_line(aes(x=station, y=fit, colour=quantile, group=quantile)) + 
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
  geom_line(aes(x=station, y=fit, colour=quantile, group=quantile), size=1.25) +
  geom_jitter(data=inD2, aes(x=station, y=elytraLength), 
              alpha=0.05, width=.05, height=0, colour="black") + 
  facet_wrap(~as.factor(GDD), ncol=3) +
  theme_light() +
  scale_colour_viridis_c(option="E") +
  coord_cartesian(ylim=c(1.75,11)) +
  ylab("Elytra length (mm)") +
  xlab("Crop vs non-crop")

#Summary crop/noncrop elytra SHASH--------------------------------------------------
setwd("C:/Users/tobyn.neame/Documents/MScWorking/beetleDataAnalysis")
m20<-read_rds("cropNonCrop_GAMSHASH_20.rds")
summary(m20)
#Visualization of Abundance GAMS--------------------------------------
#read in the models
setwd("/Users/tobynneame/Documents/School/MastersData/beetleDataAnalysis/abundance")
m1<-read_rds("beetCountGAMNB_1.rds") #smooth
m2<-read_rds("beetCountGAMNB_2.rds") #linear
m3<-read_rds("beetCountGAMNB_3.rds") #linear
m4<-read_rds("beetCountGAMNB_4.rds") #crop v non-crop
m5<-read_rds("beetCountGAMNB_5.rds") #crop v non-crop

#decide which model to use
AIC(m1, m2, m3) #m1
AIC(m4, m5) #m5

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
    labs(x='Distance from nearest non-crop vegetation area (m)',y='Number of carabids')+
    xlim(0,200)+
    scale_color_manual(values=cols)+scale_fill_manual(values=cols)+
    theme(legend.position = c(0.15,0.85),legend.background = element_rect(colour='grey'))+
    theme_bw()
)
ggsave('./figures/beetCount1.png', p, height=6, width = 10)

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
    theme(legend.position = c(0.5,0.85),legend.background = element_rect(colour='grey'))+
    theme_bw()
)

#Abundance between crop and non-crop
newdat3 <- expand.grid(station=c("nonCrop", "Crop"),GDD=c(300,500,700),
                       year='2021',BLID='41007',lon_dup=0,lat_dup=0) 
newdat3 <- predict.gam(m5,newdata=newdat3,se.fit = TRUE,
                       exclude = c('s(BLID)',paste0('s(lon_dup,lat_dup):BLID',levels(beetDatAbNC$BLID)))) %>% 
  do.call('data.frame',.) %>% 
  mutate(upr=fit+se.fit*1.96,lwr=fit-se.fit*1.96) %>% 
  mutate(across(c(fit,upr,lwr),exp)) %>% 
  bind_cols(dplyr::select(newdat3,station,GDD),.)

(p <- newdat3 %>% mutate(GDD=factor(GDD,labels = c('Early (300)','Mid (500)','Late (700)'))) %>% 
    ggplot()+geom_errorbar(aes(x=station,ymax=upr,ymin=lwr),alpha=0.9, width=0.1, linewidth=1)+
    geom_point(aes(x=station,y=fit), size=4)+
    #geom_text(data=dplyr::select(beetDatAbNC,station),aes(x=station,y=0.05),label='|',position=position_jitter(width = 1, height=0),alpha=0.4,size=2)+
    facet_wrap(~GDD)+
    labs(x='Non-Crop vs Crop',y='Number of carabids')+
    #xlim()+
    scale_color_manual(values=cols)+scale_fill_manual(values=cols)+
    theme_bw()
)
ggsave('./figures/beetCount2.png', p, height=6, width = 10)
