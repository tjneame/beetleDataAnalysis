# TITLE: beetleDataConglomerate
# Author: Tobyn Neame

# Requires local data from Tobyn's laptop
#SET UP ------------------------------------------------------------------------

library(tidyverse)
library(sf)

theme_set(theme_bw())

# Functions from Sam 
geom2cols <- function(d,x=lon,y=lat,removeGeom=TRUE,epsg=NA){
  require(sf); require(dplyr)
  if(!('sf' %in% class(d))) stop('Data must be an sf object')
  if(is_grouped_df(d)){ #If data are grouped
    warning('Data are grouped. Will ungroup data before converting 
geometry column.')
    d <- ungroup(d)
  }
  if(!is.na(epsg)) d <- st_transform(d,epsg) #Transform to new CRS
  d <- d %>% 
    mutate({{x}}:=st_coordinates(.)[,1],{{y}}:=st_coordinates(.)[,2]) #Make 
  #new columns from coordinates
  if(removeGeom) d <- st_drop_geometry(d) #Drop geometry
  return(d)
}

#read in local info
setwd("/Users/tobynneame/Documents/School/MastersData/beetleDataAnalysis")
beetDat<-read_csv("beetleDataClean.csv")

#get stuff from other locations
setwd("~/Documents/School/MastersData/SentinelPrey")

#Read in shapefiles
fieldWhole <- read_sf("FieldWhole2.shp") %>% st_transform(3402)
grass <- read_sf("Grass2.shp") %>% st_transform(3402)
infra <- read_sf("Infrastructure2.shp") %>% st_transform(3402)
trees <- read_sf("Trees2.shp") %>% st_transform(3402)
wet <- read_sf("Wetlands2.shp") %>% st_transform(3402)

#Read in csvs
sampleLocs <- read.csv('SampleSiteLocations.csv',stringsAsFactors = FALSE) %>% 
  mutate(lat_dup=lat,lon_dup=lon) %>%
  st_as_sf(coords=c('lon','lat')) %>% #Add spatial feature info
  st_set_crs(4326) %>%
  st_transform(3402) %>%
  geom2cols(.,x=lon_dup, y=lat_dup, removeGeom = FALSE, epsg=3402)

ACISDat <- read.csv('ACISDailyData-20210601-20220831-PID131006430.csv')

sentPrey <- read.csv('SentinelPreyCaterpillars.csv',stringsAsFactors = FALSE)

#Take a look ------------------------------------
ggplot(fieldWhole)+geom_sf()
ggplot(grass)+geom_sf()
ggplot(infra)+geom_sf()
ggplot(trees)+geom_sf()

#Take a look at BLID 41121
ggplot()+
  geom_sf(data=filter(fieldWhole,BLID==41121))+ #Field
  geom_sf(data=filter(grass,BLID==41121),fill='green')+ #Grass
  geom_sf(data=filter(trees,BLID==41121),fill='darkgreen')+ #Trees
  geom_sf(data=filter(wet,BLID==41121), fill='darkblue')+ #Wetland
  geom_sf(data=filter(sampleLocs,BLID==41121))

# Figure out the distance from each point to boundary lines of grass/tree polygons ---------------

# #Quicker example at a single field
# treeLine <- st_cast(filter(trees,BLID==41021),'MULTILINESTRING')
# grassLine <- st_cast(filter(grass,BLID==41021),'MULTILINESTRING')
# treeGrassLine <- st_union(treeLine,grassLine)
# 
# ggplot()+
#   geom_sf(data=filter(fieldWhole,BLID==41021))+ #Field
#   geom_sf(data=filter(grass,BLID==41021),fill='green')+ #Grass
#   geom_sf(data=filter(trees,BLID==41021),fill='darkgreen')+ #Trees
#   geom_sf(data=filter(sampleLocs,BLID==41021))+
#   geom_sf(data=treeGrassLine,col='red')
# 
# filter(sampleLocs,BLID==41021) %>% 
#   mutate(dist=apply(st_distance(filter(sampleLocs,BLID==41021),treeGrassLine),1,min))

#Turns polygons into lines (multi-line strings)
treeLine <- st_cast(trees,'MULTILINESTRING')
grassLine <- st_cast(grass,'MULTILINESTRING')
treeGrassLine <- st_union(treeLine,grassLine) #Joins grass/trees together into a single object

#Gets distances from tree/grass lines - TAKES A WHILE
distMat <- st_distance(sampleLocs,treeGrassLine) #Gets distances from sample locations to grass/tree lines in a matrix

minDist <- apply(distMat,1,min) #Gets minimum distances from each row (each point)

sampleLocs <- sampleLocs %>%  mutate(dist=minDist) #Joins minDist into sampleLocs dataframe

#Merges sample distances with beetle records
sampleLocs <- sampleLocs %>% unite(col = trapID, c(BLID,stationID),sep='-',remove = FALSE) #Creates a "Trap ID" column

#Merges the tables together by "trapID" 
beetDatDist <- st_drop_geometry(sampleLocs) %>%
  dplyr::select(trapID, lon_dup, lat_dup, dist) %>% 
  full_join(beetDat,by = 'trapID')

# Add in growing degree data -------------------------------------------

#merges beetle data and sentinel prey to gain weather station and mid date columns
wStnDat <- sentPrey %>% unite(col = trapPassID, c(BLID,site,return),sep='-',remove = FALSE) %>% #Creates a "trapPassID" column
  dplyr::select(trapPassID,weatherStn,midDate) %>% #makes a weather station data set from sent prey
  distinct(.keep_all = TRUE)

beetDatDist <- beetDatDist %>% unite(col = trapPassID, c(BLID,station,pass),sep="-",remove = FALSE) #Creates a "trapPassID" column

beetDatTry1<- left_join(beetDatDist, wStnDat, by = 'trapPassID')

#some elytra lengths are in pixels, these will have to be fixed.

beetDatPixel<-filter(beetDatTry1, elytraLength>80) %>%
  mutate(elytraLength=elytraLength/23.2333011)

beetDatENA<-subset(beetDatTry1,is.na(elytraLength)) #Only include for the abundance data

beetDatTry1<-filter(beetDatTry1, elytraLength<80) %>%
  bind_rows(beetDatPixel) %>%
  bind_rows(beetDatENA) #only included for the abundance data

#Merges beetle records and sample distances with GDD data
beetDat <- beetDatTry1 %>% unite(col = stnDate, c(weatherStn,midDate),sep='-',remove = FALSE) #Creates a "station date" column called stnDate
ACISDat <- ACISDat %>% unite(col=stnDate, c(Station_Name,Date), sep = "-", remove = FALSE) #Creates a "station date" column called stnDate

#Merges the tables together by "stnDate"
beetDat <- left_join(beetDat, ACISDat, by = 'stnDate')

#Take out duplicate columns
beetDat <- dplyr::select(beetDat, -Station_Name, -Date)

#some years are input wrong - Fix this
beetDatYear<-filter(beetDat, year<2021) %>%
  mutate(year=2022)

beetDat<-bind_rows(filter(beetDat, year>2020), beetDatYear)

beetDatYear<-filter(beetDat, year>2022) %>%
  mutate(year=2022)

beetDat<-bind_rows(filter(beetDat, year<2023), beetDatYear)

setwd("/Users/tobynneame/Documents/School/MastersData/beetleDataAnalysis")
#write the CSV
#write_csv(beetDat, "beetleData.csv")

#Make data set to use for abundance models
beetDatAbund<-beetDat %>% 
  add_count(trapPassID) %>% 
  dplyr::select(-BBID, -elytraLength,-bodyLength,-order,-family,-genus,-species,-identifyer,-NOTES)%>%
  distinct()%>%
  rename(beetCount=n)

#add in zeros
sampleLocs1 <- filter(sampleLocs, BLID<41077)
sampleLocs1Expand <- sampleLocs1[rep(row.names(sampleLocs1), 3), 1:13]
sampleLocs1Pass <- sampleLocs1Expand %>%
  mutate(pass=rep(1:3, each=150)) %>%
  unite(col = trapPassID, c(trapID,pass),sep='-',remove = FALSE)

sampleLocs2 <- filter(sampleLocs, between(BLID, 41077, 41091))
sampleLocs2Expand <- sampleLocs2[rep(row.names(sampleLocs2), 1), 1:13]
sampleLocs2Pass <- sampleLocs2Expand %>%
  mutate(pass=1) %>%
  unite(col = trapPassID, c(trapID,pass),sep='-',remove = FALSE)

sampleLocs3 <- filter(sampleLocs, BLID>41091)
sampleLocs3Expand <- sampleLocs3[rep(row.names(sampleLocs3), 4), 1:13]
sampleLocs3Pass <- sampleLocs3Expand %>%
  mutate(pass=rep(1:4, each=105)) %>%
  unite(col = trapPassID, c(trapID,pass),sep='-',remove = FALSE)

#NOTES: 
#Merge the sampleLocs back together
sampleLocs<-bind_rows(sampleLocs1Pass, sampleLocs2Pass, sampleLocs3Pass)
#Need to add GDD to the sampleLocs
sampleLocs<- left_join(sampleLocs, wStnDat, by = 'trapPassID')
sampleLocs <- sampleLocs %>% unite(col = stnDate, c(weatherStn,midDate),sep='-',remove = FALSE) #Creates a "station date" column called stnDate
sampleLocs <- left_join(sampleLocs, ACISDat, by = 'stnDate') #Merges the tables together by "stnDate"

#clean up the beetDatAbund and the sampleLocs to make them better for merging
beetDatAbund<-beetDatAbund %>%
  dplyr::select(trapPassID, beetCount, year, trapType)
sampleLocs<-sampleLocs %>%
  #mutate(station=stationID) %>%
  dplyr::select(trapPassID, BLID, station, pass, lat_dup, lon_dup, dist, GDD)%>%
  st_drop_geometry()

#Need to merge only the trapPassIDs that don't already exist in the beetDatAbund
beetDatAbund<-left_join(sampleLocs, beetDatAbund, by="trapPassID")

#there will be lots of NAs in some columns so those will have to be rectified
beetDatAbund<-beetDatAbund %>%
  filter(trapType=="PF"|is.na(trapType)) %>%
  distinct()

beetDatAbund$trapType[is.na(beetDatAbund$trapType)] <- "PF"
beetDatAbund$beetCount[is.na(beetDatAbund$beetCount)] <- 0

beetDatAbund<-beetDatAbund %>%
  mutate(year=case_when(BLID>41100~2022,
                        BLID<41100~2021))


write_csv(beetDatAbund,"beetleDataAbundance.csv")
