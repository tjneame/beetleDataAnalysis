#Title: beetleDataAnalysis

#Set up -----------------------------------------------------------

library(tidyverse)
library(mgcv)
library(googlesheets4)
library(beepr)
library(qgam)

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

#Start some modeling for Elytra Length ------------------------------------------------

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

#Start new modeling with advice from Paul for Elytra Length ----------------------------
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

#PAUL Start Here: New models with centered lat lon for Elytra Length -------------------------------------

#Tobyn started June 22
form12<-list(elytraLength~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+te(lon_dup,lat_dup,by=BLID)+year,
~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+te(lon_dup,lat_dup,by=BLID)+year,
~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+te(lon_dup,lat_dup,by=BLID)+year,
~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+te(lon_dup,lat_dup,by=BLID)+year)
gam12<-gam(form12,
          family=shash,
          data=beetDat,
          method="REML")
write_rds(gam12, "elytraLength_GAMSHASH_12.rds")

#Tobyn started June 22
form13<-list(elytraLength~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+te(lon_dup,lat_dup,by=BLID)+year,
             ~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+te(lon_dup,lat_dup,by=BLID)+year,
             ~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+te(lon_dup,lat_dup,by=BLID)+year,
             ~1)
gam13<-gam(form13,
          family=shash,
          data=beetDat, 
          method="REML")
write_rds(gam13, "elytraLength_GAMSHASH_13.rds")

#Tobyn started June 22
form14<-list(elytraLength~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+te(lon_dup,lat_dup,by=BLID)+year,
             ~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+te(lon_dup,lat_dup,by=BLID)+year,
             ~1,
             ~1)
gam14<-gam(form14,
                family=shash,
                data=beetDat, method="REML")
write_rds(gam14, "elytraLength_GAMSHASH_14.rds")

#Tobyn started June 22
form15<-list(elytraLength~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+te(lon_dup,lat_dup,by=BLID)+year,
             ~1,
             ~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+te(lon_dup,lat_dup,by=BLID)+year,
             ~1)
gam15<-gam(form15,
                family=shash,
                data=beetDat, method="REML")
write_rds(gam15, "elytraLength_GAMSHASH_15.rds")

#Tobyn started June 22
form16<-list(elytraLength~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+year,
             ~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+year,
             ~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+year,
             ~1)
gam16<-gam(form16,
                family=shash,
                data=beetDat, method="REML")
write_rds(gam16, "elytraLength_GAMSHASH_16.rds")

#poly requires no NA values
beetDatNoNA<-beetDat%>%dplyr::filter(!is.na(dist))

#Tobyn started June 22
form17<-list(elytraLength~poly(dist,4*GDD)+s(BLID,bs="re")+te(lon_dup,lat_dup,by=BLID)+year,
             ~poly(dist,4*GDD)+s(BLID,bs="re")+te(lon_dup,lat_dup,by=BLID)+year,
             ~1,
             ~1)
gam17<-gam(form17,
           family=shash,
           data=beetDatNoNA, method="REML")
write_rds(gam17, "elytraLength_GAMPoly_17.rds")

#Tobyn started June 22
form18<-list(elytraLength~poly(dist,4*GDD)+s(BLID,bs="re")+te(lon_dup,lat_dup,by=BLID)+year,
             ~poly(dist,4*GDD)+s(BLID,bs="re")+te(lon_dup,lat_dup,by=BLID)+year,
             ~poly(dist,4*GDD)+s(BLID,bs="re")+te(lon_dup,lat_dup,by=BLID)+year,
             ~1)
gam18<-gam(form18,
                family=shash,
                data=beetDatNoNA, method="REML")
write_rds(gam18, "elytraLength_GAMPoly_18.rds")

#QGAMs for Elytra Length ----------------------------------------
#run the qform first, this allows each qGAM model to be identical.
#Then run each qGAM plus the code to save it together.   
qform<-as.formula(elytraLength~s(dist)+
                    s(GDD)+
                    ti(dist,GDD)+
                    s(BLID,bs="re")+
                    s(lon_dup,lat_dup,by=BLID)+
                    year)

qGAM1<-qgam(data=beetDat, qu=0.1, form=qform)
write_rds(qGAM1, "elytraLength_QGAM_1.rds")

qGAM25<-qgam(data=beetDat, qu=0.25, form=qform)
write_rds(qGAM25, "elytraLength_QGAM_25.rds")

qGAM5<-qgam(data=beetDat, qu=0.5, form=qform)
write_rds(qGAM5, "elytraLength_QGAM_5.rds")

qGAM75<-qgam(data=beetDat, qu=0.75, form=qform)
write_rds(qGAM75, "elytraLength_QGAM_75.rds")

qGAM9<-qgam(data=beetDat, qu=0.9, form=qform)
write_rds(qGAM9, "elytraLength_QGAM_9.rds")

#Beetle abundance model ---------------------------------------------------- 
#set up the dataset (should be ~900-915 obs of 19 variables)
beetDatAbund<-beetDat %>% 
  add_count(trapPassID) %>% 
  dplyr::select(-BBID,-elytraLength,-bodyLength,-order,-family,-genus,-species,-identifyer,-NOTES)%>%
  distinct()%>%
  rename(beetCount=n)

form <- as.formula(beetCount ~ s(dist) + #Distance from edge
                     s(GDD) + #Growing degree day
                     ti(dist,GDD) + #Distance:GDD interaction
                     year + #Year 
                     s(lon_dup,lat_dup,by=BLID) + #Within-field distances
                     s(BLID,bs='re')) #Between-field distances (centroid of each field)   

beetCountGAMNB_1 <- gam(formula = form,
                        family = nb,
                        data=beetDatAbund)
write_rds(beetCountGAMNB_1, "beetCountGAMNB_1.rds")
