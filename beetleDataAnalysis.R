#Title: beetleDataAnalysis

#Set up -----------------------------------------------------------

library(tidyverse)
library(mgcv)
library(googlesheets4)
library(beepr)
library(qgam)
library(gratia)

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
  
#Abundance (dont use this yet, we need to add in the zeros maybe) 
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

#Tobyn started June 23
form12<-list(elytraLength~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year)
gam12<-gam(form12,
          family=shash,
          data=beetDatCrop,
          method="REML")
write_rds(gam12, "elytraLength_GAMSHASH_12.rds")

#Tobyn started June 22
form13<-list(elytraLength~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~1)
gam13<-gam(form13,
          family=shash,
          data=beetDatCrop, 
          method="REML")
write_rds(gam13, "elytraLength_GAMSHASH_13.rds")

#Tobyn started June 22
form14<-list(elytraLength~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~1,
             ~1)
gam14<-gam(form14,
                family=shash,
                data=beetDatCrop, method="REML")
write_rds(gam14, "elytraLength_GAMSHASH_14.rds")

#Tobyn started June 22
form15<-list(elytraLength~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~1,
             ~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~1)
gam15<-gam(form15,
                family=shash,
                data=beetDatCrop, method="REML")
write_rds(gam15, "elytraLength_GAMSHASH_15.rds")

#Tobyn started June 22
form16<-list(elytraLength~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+year,
             ~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+year,
             ~s(dist)+s(GDD)+ti(dist,GDD)+s(BLID,bs="re")+year,
             ~1)
gam16<-gam(form16,
                family=shash,
                data=beetDatCrop, method="REML")
write_rds(gam16, "elytraLength_GAMSHASH_16.rds")

#poly requires no NA values
#beetDatNoNA<-beetDatCrop%>%dplyr::filter(!is.na(dist))

form17<-list(elytraLength~poly(dist,4*GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~poly(dist,4*GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~1,
             ~1)
gam17<-gam(form17,
           family=shash,
           data=beetDatCrop, method="REML")
write_rds(gam17, "elytraLength_GAMPoly_17.rds")


form18<-list(elytraLength~poly(dist,4*GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~poly(dist,4*GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~poly(dist,4*GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~1)
gam18<-gam(form18,
                family=shash,
                data=beetDatCrop, method="REML")
write_rds(gam18, "elytraLength_GAMPoly_18.rds")

form19<-list(elytraLength~dist*GDD+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~dist*GDD+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~1,
             ~1)
gam19<-gam(form19,
           family = shash,
           data=beetDatCrop, method="REML")
write_rds(gam19,"elytraLength_GAMSHASH_19.rds")
#QGAMs for Elytra Length ----------------------------------------
#run the qform first, this allows each qGAM model to be identical.
#Then run each qGAM plus the code to save it together.   
qform<-as.formula(elytraLength~s(dist)+
                    s(GDD)+
                    ti(dist,GDD)+
                    s(BLID,bs="re")+
                    s(lon_dup,lat_dup,by=BLID)+
                    year)

qGAM1<-qgam(data=beetDatCrop, qu=0.1, form=qform)
write_rds(qGAM1, "elytraLength_QGAM_1.rds")

qGAM25<-qgam(data=beetDatCrop, qu=0.25, form=qform)
write_rds(qGAM25, "elytraLength_QGAM_25.rds")

qGAM5<-qgam(data=beetDatCrop, qu=0.5, form=qform)
write_rds(qGAM5, "elytraLength_QGAM_5.rds")

qGAM75<-qgam(data=beetDatCrop, qu=0.75, form=qform)
write_rds(qGAM75, "elytraLength_QGAM_75.rds")

qGAM9<-qgam(data=beetDatCrop, qu=0.9, form=qform)
write_rds(qGAM9, "elytraLength_QGAM_9.rds")

#Below this isn't gonna work yet-------------------------------------------
#Notes: With only two levels for the factor "station" a smooth cannot be fit to eitehr the main or interaction terms containing it
#Elytra length in Crop vs non-crop
form20<-list(elytraLength~station+s(GDD)+ti(station,GDD,k=3)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~station+s(GDD)+ti(station,GDD,k=3)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~station+s(GDD)+ti(station,GDD,k=3)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~station+s(GDD)+ti(station,GDD,k=3)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year)
gam20<-gam(form20,
           family=shash,
           data=beetDatNC,
           method="REML")
write_rds(gam20, "cropNonCrop_GAMSHASH_20.rds")


form20<-list(elytraLength~s(station)+s(GDD)+ti(station,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~s(station)+s(GDD)+ti(station,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~s(station)+s(GDD)+ti(station,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~1)
gam20<-gam(form20,
           family=shash,
           data=beetDatNC, 
           method="REML")
write_rds(gam20, "cropNonCrop_GAMSHASH_20.rds")


form21<-list(elytraLength~s(station)+s(GDD)+ti(station,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~s(station)+s(GDD)+ti(station,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~1,
             ~1)
gam21<-gam(form21,
           family=shash,
           data=beetDatNC, method="REML")
write_rds(gam21, "cropNonCrop_GAMSHASH_21.rds")


form22<-list(elytraLength~s(station)+s(GDD)+ti(station,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~1,
             ~s(station)+s(GDD)+ti(station,GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~1)
gam22<-gam(form22,
           family=shash,
           data=beetDatNC, method="REML")
write_rds(gam22, "cropNonCrop_GAMSHASH_22.rds")


form23<-list(elytraLength~station+s(GDD)+ti(station,GDD)+s(BLID,bs="re")+year,
             ~station+s(GDD)+ti(station,GDD)+s(BLID,bs="re")+year,
             ~station+s(GDD)+ti(station,GDD)+s(BLID,bs="re")+year,
             ~1)
gam23<-gam(form23,
           family=shash,
           data=beetDatNC, method="REML")
write_rds(gam23, "cropNonCrop_GAMSHASH_23.rds")

#poly requires no NA values
#beetDatNoNA<-beetDatCrop%>%dplyr::filter(!is.na(dist))
form24<-list(elytraLength~poly(station,4*GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~poly(station,4*GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~1,
             ~1)
gam24<-gam(form24,
           family=shash,
           data=beetDatNC, method="REML")
write_rds(gam24, "cropNonCrop_GAMPoly_24.rds")


form25<-list(elytraLength~poly(station,4*GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~poly(station,4*GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~poly(station,4*GDD)+s(BLID,bs="re")+s(lon_dup,lat_dup,by=BLID)+year,
             ~1)
gam25<-gam(form25,
           family=shash,
           data=beetDatNC, method="REML")
write_rds(gam25, "cropNonCrop_GAMPoly_25.rds")

#QGAMs for Elytra Length
#run the qform first, this allows each qGAM model to be identical.
#Then run each qGAM plus the code to save it together.   
qform2<-as.formula(elytraLength~s(station)+
                    s(GDD)+
                    ti(station,GDD)+
                    s(BLID,bs="re")+
                    s(lon_dup,lat_dup,by=BLID)+
                    year)

qGAM1<-qgam(data=beetDatNC, qu=0.1, form=qform2)
write_rds(qGAM1, "elytraLength_QGAM_1.rds")

qGAM25<-qgam(data=beetDatNC, qu=0.25, form=qform2)
write_rds(qGAM25, "elytraLength_QGAM_25.rds")

qGAM5<-qgam(data=beetDatNC, qu=0.5, form=qform2)
write_rds(qGAM5, "elytraLength_QGAM_5.rds")

qGAM75<-qgam(data=beetDatNC, qu=0.75, form=qform2)
write_rds(qGAM75, "elytraLength_QGAM_75.rds")

qGAM9<-qgam(data=beetDatNC, qu=0.9, form=qform2)
write_rds(qGAM9, "elytraLength_QGAM_9.rds")

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
