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

#read in previously saved models (must have USB with the models and have models in the working directory)
gam2<-read_rds("elytraLength_GAMSHASH_2.rds")

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

#Don't run gam1 unless you're on a super computer
gam1<-gam(list(elytraLength~te(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
          ~te(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
          ~te(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
          ~te(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year),
          family=shash,
          data=beetDat, method="REML")

gam2<-gam(list(elytraLength~te(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
               ~te(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
               ~1,
               ~1),
          family=shash,
          data=beetDat, method="REML")
beep(5)

gam3<-gam(list(elytraLength~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
               ~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
               ~1,
               ~1),
          family=shash,
          data=beetDat, method="REML")
beep(8)

#Interpret the models ------------------------------------------------

summary(gam2)

#Save the models -----------------------------------------------------

write_rds(gam2,"elytraLength_GAMSHASH_2.rds")

