#Title: beetleDataAnalysis
#Author: Tobyn Neame

#requires beetleData.csv

#Set up -----------------------------------------------------------

library(tidyverse)
library(mgcv)
library(gamlss)
library(googlesheets4)
library(beepr)

#read in CSV
#If working locally
#beetDat<-read_csv("beetleData.csv")
#If working remotely
beetDat<-read_sheet("https://docs.google.com/spreadsheets/d/1JZwSNahFrIhxU3e5hNoBBQpLskxCO9A_Top1VlaJe0Y/edit#gid=140253595")

is_tibble(beetDat)

#make some columns into factors that have been erroneously read as integers
beetDat <- beetDat %>%
  mutate(BLID=as.factor(BLID)) %>%
  mutate(year=as.factor(year))

#Center the lat-lon on their means 
beetDat<-beetDat %>%
  group_by(BLID) %>% mutate(cLon=mean(lon_dup),cLat=mean(lat_dup)) %>%
  ungroup()%>% mutate(lon_dup=lon_dup-cLon,lat_dup=lat_dup-clat)

#Look at data ------------------------------------------------------

#Size by distance
beetDat %>%
  ggplot(aes(x=dist,y=elytraLength))+geom_point() #Plots elytra size by distance

#Size by distance just P.mel
beetDatPMel<-dplyr::filter(beetDat, species=='melanarius')
beetDatPMel %>%
  ggplot(aes(x=dist,y=elytraLength))+geom_point() #Plots elytra size of P.mel by distance

#Size by distance, no P.mel
beetDatNoP<-dplyr::filter(beetDat, is.na(genus))
beetDatNoP %>%
  ggplot(aes(x=dist,y=elytraLength))+geom_point() #Plots elytra size of beetles that arent P.mel by distance


#Size by distance, late season
beetDatLate<-dplyr::filter(beetDat, GDD>600)
beetDatLate %>%
  ggplot(aes(x=dist,y=elytraLength))+geom_point() #Plots elytra size of beetles that arent P.mel by distance

#Size by distance, mid season
beetDatMid<-dplyr::filter(beetDat, GDD<600, GDD>300)
beetDatMid %>%
  ggplot(aes(x=dist,y=elytraLength))+
  geom_point()

#Size by distance with jitter and alpha transparency and a simple GAM line
beetDat %>%
  ggplot(aes(x=dist,y=elytraLength))+
  geom_point(alpha=.1, position=position_jitter(width = 3))+
  xlab("Distance from NCV (m)")+
  ylab("Length of elytron (mm)")+
  geom_smooth(method="gam", formula=y~s(x,k=10,bs="ts"))

#Start some modeling ------------------------------------------------

#Couldn't run
gam1<-gam(list(elytraLength~te(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
          ~te(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
          ~te(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
          ~te(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year),
          family=shash,
          data=beetDat, method="REML")

#Could run
gam2<-gam(list(elytraLength~te(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
               ~te(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
               ~1,
               ~1),
          family=shash,
          data=beetDat, method="REML")
beep(5)

#try making the data set smaller to see if the models run with that - HINT** They don't
beetDatSmall <- beetDat %>%
  sample_n(floor(nrow(beetDat)/20), replace=FALSE)

#couldn't run
gam3<-gam(list(elytraLength~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
               ~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
               ~1,
               ~1),
          family=shash,
          data=beetDatSmall, method="REML")

#couldn't run
gam4<-gam(list(elytraLength~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
               ~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year),
          family=gaulss,
          data=beetDatSmall, method="REML")

beep(8)

#couldn't run
gam5<-gam(list(elytraLength~s(dist, k=5, bs="ts")+
                 s(GDD, k=5, bs="ts")+
                 ti(dist,GDD, k=c(5,5))
               +s(BLID,bs="re")+
                 s(lon_dup,lat_dup,by=BLID, bs="ts")+
                 year,
         ~ s(dist, k=5, bs="ts")+
           s(GDD, k=5, bs="ts")+
           ti(dist,GDD, k=c(5,5))+ 
           s(BLID,bs="re")+
           s(lon_dup,lat_dup,by=BLID, bs="ts")+
           year),
         family=gaulss,
         data=beetDatSmall, method="REML")
beep(8)

#Couldn't run
gam6<-gam(list(elytraLength~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
         ~ s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
         ~ s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
         ~1),
         family=shash,
         data=beetDat, method="REML")
beep(8)    

#couldn't run    
gam7<-gam(list(elytraLength~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
               ~ s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
               ~ s(dist)+s(GDD)+ti(dist,GDD),
               ~1),
          family=shash,
          data=beetDat, method="REML")
beep(8)

#Start new modeling with advice from Paul ----------------------------
## 1. Assume that skewness and kurtosis are not interesting (therefore use gaulss)
## 2. Remove BLID as a random effect (it is clearly not important when the field-level smoothers are added)
## 3. Instead of fitting a te(dist, GDD) try dist*GDD or as poly(dist, 4)*GDD 
##    - TRY ON BOTH location and scale at same time

gam8<-gam(list(elytraLength~s(dist)+s(GDD)+ti(dist,GDD)+s(lon_dup,lat_dup,by=BLID)+year,
               ~s(dist)+s(GDD)+ti(dist,GDD)+s(lon_dup,lat_dup,by=BLID)+year),
          family=gaulss,
          data=beetDat, method="REML")

gam9<-gam(list(elytraLength~te(dist,GDD)+s(lon_dup,lat_dup,by=BLID)+year,
               ~te(dist,GDD)+s(lon_dup,lat_dup,by=BLID)+year),
          family=gaulss,
          data=beetDat, method="REML")

gam10<-gam(list(elytraLength~dist*GDD+s(lon_dup,lat_dup,by=BLID)+year,
               ~dist*GDD+s(lon_dup,lat_dup,by=BLID)+year),
          family=gaulss,
          data=beetDat, method="REML")


#Poly does not allow missing data

beetDatNoNA<-beetDat%>%dplyr::filter(!is.na(dist))

gam11<-gam(list(elytraLength~poly(dist,4*GDD)+s(lon_dup,lat_dup,by=BLID)+year,
               ~poly(dist,4)*GDD+s(lon_dup,lat_dup,by=BLID)+year),
          family=gaulss,
          data=beetDatNoNA, method="REML")

#Interpret the models ------------------------------------------------

summary(gam2)
summary(gam3)
summary(gam4)
summary(gam5)

#Save the models -----------------------------------------------------

write_rds(gam2,"elytraLength_GAMGAULSS_9.rds")
write_rds(gam3, "elytraLength_GAMSHASH_3.rds")
write_rds(gam9, "elytraLength_GAMGAULSS_9.rds")
write_rds(gam5, "elytraLength_GAMSHASH_5.rds")

