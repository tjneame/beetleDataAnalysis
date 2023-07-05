#Title: beetleDataAnalysis

#Set up -----------------------------------------------------------

library(tidyverse)
library(mgcv)
library(googlesheets4)
library(qgam)
library(gratia)

setwd("/Users/tobynneame/Documents/School/MastersData/beetleDataAnalysis")
#read in CSV
#If working locally
beetDat<-read_csv("beetleData.csv")
#If working remotely
#beetDat<-read_sheet("https://docs.google.com/spreadsheets/d/1JZwSNahFrIhxU3e5hNoBBQpLskxCO9A_Top1VlaJe0Y/edit#gid=140253595")

is_tibble(beetDat)

#make some columns into factors that have been erroneously read as integers and take out the extra BLID
beetDat <- beetDat %>%
  mutate(BLID=as.factor(BLID)) %>%
  mutate(year=as.factor(year)) %>%
  filter(BLID!="51056") %>%
  droplevels()
  

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
  
#Abundance (don't use this yet, we need to add in the zeros maybe) 
#(should be ~900-915 obs of 19 variables)
beetDatAbund<-beetDat %>% 
  add_count(trapPassID) %>% 
  dplyr::select(-elytraLength) %>%
  distinct() %>%
  rename(beetCount=n) %>%
  drop_na()

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

#try making the data set smaller to see if the models run with that - HINT** They don't
beetDatSmall <- beetDat %>%
  sample_n(floor(nrow(beetDat)/20), replace=FALSE)

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

#Size by crop vs non-crop
beetDatNC %>%
  ggplot(aes(x=station, y=elytraLength))+
  geom_point(alpha=.1, position=position_jitter(width=.02))

#abundance by distance
beetDatAbund %>%
  ggplot(aes(x=dist,y=beetCount))+
  geom_point(alpha=.1, position=position_jitter(width = 3))+
  xlab("Distance from NCV (m)")+
  ylab("Number of Carabids")+
  geom_smooth(method="gam", formula=y~s(x,k=3,bs="ts"))

#Start some modeling for Elytra Length ------------------------------------------------

#Couldn't run, SHASH family with tensors (te) for dist and GDD. LSSK
gam1<-gam(list(elytraLength~te(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
          ~te(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
          ~te(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
          ~te(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year),
          family=shash,
          data=beetDat, method="REML")

#Could run, SHASH family with tensors (te) for dist and GDD. LS
gam2<-gam(list(elytraLength~te(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
               ~te(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
               ~1,
               ~1),
          family=shash,
          data=beetDat, method="REML")

#couldn't run, SHASH family with smoothers (s) for dist and GDD and a tensor for interaction. LS
gam3<-gam(list(elytraLength~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
               ~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
               ~1,
               ~1),
          family=shash,
          data=beetDat, method="REML")

#couldn't run, GAULSS family with smoothers (s) for dist and GDD and a tensor for interaction
gam4<-gam(list(elytraLength~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
               ~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year),
          family=gaulss,
          data=beetDat, method="REML")

#couldn't run, GAULSS family with smoothers (s) for dist and GDD and a tensor 
#for interaction. Plus specified k and bs for the smooths to make run faster 
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

#Couldn't run, SHASH family with smoothers (s) for dist and GDD and a tensor 
#for interaction. LSS
gam6<-gam(list(elytraLength~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
         ~ s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
         ~ s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
         ~1),
         family=shash,
         data=beetDat, method="REML")

#couldn't run, SHASH family with smoothers (s) for dist and GDD and a tensor 
#for interaction. LS(S)
gam7<-gam(list(elytraLength~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
               ~ s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
               ~ s(dist)+s(GDD)+ti(dist,GDD),
               ~1),
          family=shash,
          data=beetDat, method="REML")

#Start new modeling with advice from Paul for Elytra Length ----------------------------
## 1. Assume that skewness and kurtosis are not interesting (therefore use gaulss)
## 2. Remove BLID as a random effect (it is clearly not important when the field-level smoothers are added)
#^^^NOTE BLID is probably important but we were using the wrong data without centred lat lons
## 3. Instead of fitting a te(dist, GDD) try dist*GDD or as poly(dist, 4)*GDD 
##    - TRY ON BOTH location and scale at same time

#The following 4 models don't include BLID and are GAULSS family.
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

beetDatNoNA<-beetDat%>%dplyr::filter(!is.na(dist))#Poly does not allow missing data

gam11<-gam(list(elytraLength~poly(dist,4*GDD)+s(lon_dup,lat_dup,by=BLID)+year,
               ~poly(dist,4)*GDD+s(lon_dup,lat_dup,by=BLID)+year),
          family=gaulss,
          data=beetDatNoNA, method="REML")

#New models for Elytra Length within the crop as a function of distance --------
#this is where we centred the lat lon and made the data only within the crop

#Tobyn started June 23, SHASH family with smoothers for dist and GDD (s) and 
#tensor for interaction. LSSK
form12<-list(elytraLength~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year)
gam12<-gam(form12,
          family=shash,
          data=beetDatCrop,
          method="REML")
write_rds(gam12, "elytraLength_GAMSHASH_12.rds")

#Tobyn started June 22, SHASH family with smoothers for dist and GDD (s) and 
#tensor for interaction. LSS
form13<-list(elytraLength~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~1)
gam13<-gam(form13,
          family=shash,
          data=beetDatCrop, 
          method="REML")
write_rds(gam13, "elytraLength_GAMSHASH_13.rds")

#Tobyn started June 22, SHASH family with smoothers for dist and GDD (s) and 
#tensor for interaction. LS
form14<-list(elytraLength~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~1,
             ~1)
gam14<-gam(form14,
                family=shash,
                data=beetDatCrop, method="REML")
write_rds(gam14, "elytraLength_GAMSHASH_14.rds")

#Tobyn started June 22, SHASH family with smoothers for dist and GDD (s) and 
#tensor for interaction. L.S
form15<-list(elytraLength~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~1,
             ~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~1)
gam15<-gam(form15,
                family=shash,
                data=beetDatCrop, method="REML")
write_rds(gam15, "elytraLength_GAMSHASH_15.rds")

#Tobyn started June 22, SHASH family with smoothers for dist and GDD (s) and 
#tensor for interaction. LSS
form16<-list(elytraLength~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+year,
             ~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+year,
             ~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+year,
             ~1)
gam16<-gam(form16,
                family=shash,
                data=beetDatCrop, method="REML")
write_rds(gam16, "elytraLength_GAMSHASH_16.rds")

#SHASH family with polygons for dist and GDD (poly(d,4*G)). LS
form17<-list(elytraLength~poly(dist,4*GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~poly(dist,4*GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~1,
             ~1)
gam17<-gam(form17,
           family=shash,
           data=beetDatCrop, method="REML")
write_rds(gam17, "elytraLength_GAMPoly_17.rds")

#SHASH family with polygons for dist and GDD (poly(d,4*G)). LSS
form18<-list(elytraLength~poly(dist,4*GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~poly(dist,4*GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~poly(dist,4*GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~1)
gam18<-gam(form18,
                family=shash,
                data=beetDatCrop, method="REML")
write_rds(gam18, "elytraLength_GAMPoly_18.rds")

#SHASH family with linear for dist and GDD. LS
form19<-list(elytraLength~dist*GDD+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~dist*GDD+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~1,
             ~1)
gam19<-gam(form19,
           family = shash,
           data=beetDatCrop, method="REML")
write_rds(gam19,"elytraLength_GAMSHASH_19.rds")

#QGAMs for Elytra Length in crop as a function of distance -------------------
#run the qform first, this allows each qGAM model to be identical.

qformSmooth<-as.formula(elytraLength~s(dist)+
                    s(GDD)+
                    ti(dist,GDD)+
                    s(BLID,bs="re")+
                    s(lon_dup,lat_dup,by=BLID)+
                    year)

qGAM1Smooth<-qgam(data=beetDatCrop, qu=0.1, form=qformSmooth)
write_rds(qGAM1Smmoth, "elytraLength_QGAM_1.rds")

qGAM25Smooth<-qgam(data=beetDatCrop, qu=0.25, form=qformSmooth)
write_rds(qGAM25Smooth, "elytraLength_QGAM_25.rds")

qGAM5Smooth<-qgam(data=beetDatCrop, qu=0.5, form=qformSmooth)
write_rds(qGAM5Smooth, "elytraLength_QGAM_5.rds")

qGAM75Smooth<-qgam(data=beetDatCrop, qu=0.75, form=qformSmooth)
write_rds(qGAM75Smooth, "elytraLength_QGAM_75.rds")

qGAM9Smooth<-qgam(data=beetDatCrop, qu=0.9, form=qformSmooth)
write_rds(qGAM9Smooth, "elytraLength_QGAM_9.rds")

#Paul made some more given that the QGAMs run pretty fast with a good computer. 
#Below is what I assume the code could be for those

#QGAM, no smooths, these are probably the best ones.
qformLine<-as.formula(elytraLength~dist*GDD+
                    s(BLID,bs="re")+
                    s(lon_dup,lat_dup,by=BLID)+
                    year)

qGAM1Line<-qgam(data=beetDatCrop, qu=0.1, form=qformLine)

qGAM2Line<-qgam(data=beetDatCrop, qu=0.2, form=qformLine)

qGAM25Line<-qgam(data=beetDatCrop, qu=0.25, form=qformLine)

qGAM3Line<-qgam(data=beetDatCrop, qu=0.3, form=qformLine)

qGAM4Line<-qgam(data=beetDatCrop, qu=0.4, form=qformLine)

qGAM5Line<-qgam(data=beetDatCrop, qu=0.5, form=qformLine)

qGAM6Line<-qgam(data=beetDatCrop, qu=0.6, form=qformLine)

qGAM7Line<-qgam(data=beetDatCrop, qu=0.7, form=qformLine)

qGAM75Line<-qgam(data=beetDatCrop, qu=0.75, form=qformLine)

qGAM8Line<-qgam(data=beetDatCrop, qu=0.8, form=qformLine)

qGAM9Line<-qgam(data=beetDatCrop, qu=0.9, form=qformLine)

#I am not sure how to specify the learning rate in QGAM

#Elytra length in Crop vs non-crop------------------------------------------
#Notes: With only two levels for the factor "station" a smooth cannot be fit to 
#either the main or interaction terms containing it. We could use elytraLength~station+(GDD, by=station)
#however, this is okay because we are ignoring smooths on the mains and interaction for now

form20<-list(elytraLength~station*GDD+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~station*GDD+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~1,
             ~1)
gam20<-gam(form20,
           family=shash,
           data=beetDatNC,
           method="REML")
write_rds(gam20, "cropNonCrop_GAMSHASH_20.rds")

#QGAMs for Elytra Length in crop vs non-crop
#run the qform first, this allows each qGAM model to be identical.
#Then run each qGAM plus the code to save it together.   
qformLineNC<-as.formula(elytraLength~station*GDD+
                        s(BLID,bs="re")+
                        s(lon_dup,lat_dup,by=BLID)+
                        year)

qGAM1LineNC<-qgam(data=beetDatNC, qu=0.1, form=qformLineNC)

qGAM2LineNC<-qgam(data=beetDatNC, qu=0.2, form=qformLineNC)

qGAM3LineNC<-qgam(data=beetDatNC, qu=0.3, form=qformLineNC)

qGAM4LineNC<-qgam(data=beetDatNC, qu=0.4, form=qformLineNC)

qGAM5LineNC<-qgam(data=beetDatNC, qu=0.5, form=qformLineNC)

qGAM6LineNC<-qgam(data=beetDatNC, qu=0.6, form=qformLineNC)

qGAM7LineNC<-qgam(data=beetDatNC, qu=0.7, form=qformLineNC)

qGAM8LineNC<-qgam(data=beetDatNC, qu=0.8, form=qformLineNC)

qGAM9LineNC<-qgam(data=beetDatNC, qu=0.9, form=qformLineNC)

#Beetle abundance model ---------------------------------------------------- 

formBC1 <- as.formula(beetCount ~ s(dist) + #Distance from edge
                     s(GDD) + #Growing degree day
                     ti(dist,GDD) + #Distance:GDD interaction
                     year + #Year 
                     s(lon_dup,lat_dup,by=BLID) + #Within-field distances
                     s(BLID,bs='re')) #Between-field distances (centroid of each field)   

beetCountGAMNB_1 <- gam(formula = form,
                        family = nb,
                        data=beetDatAbund)
write_rds(beetCountGAMNB_1, "beetCountGAMNB_1.rds")
