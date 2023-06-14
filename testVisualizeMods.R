## Plot the predictions from an MGCV SHASH or GAULSS model

## Assume model is called m1

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

## Here are some ideas for making models run faster
## 1. Assume that skewness and kurtosis are not interesting (therefore use gaulss)
## 2. Remove BLID as a random effect (it is clearly not important when the field-level smoothers are added)
## 3. Instead of fitting a te(dist, GDD) try dist*GDD or as poly(dist, 4)*GDD 
##    - TRY ON BOTH location and scale at same time
## 4. Fit with gaulss
## 5. Quantile regression - QGAM (Simon Woods)