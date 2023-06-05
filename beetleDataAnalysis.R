#Title: beetleDataAnalysis
#Author: Tobyn Neame

#requires beetleData.csv

#Set up -----------------------------------------------------------

library(tidyverse)
library(mgcv)
library(gamlss)

#read in CSV
beetDat<-read_csv("beetleData.csv")

is_tibble(beetDat)

#make some columns into factors that have been erroneously read as integers
beetDat <- beetDat %>%
  mutate(BLID=as.factor(BLID)) %>%
  mutate(year=as.factor(year))

#Look at data ------------------------------------------------------

#Size by distance
beetDat %>%
  ggplot(aes(x=dist,y=elytraLength))+geom_point() #Plots bite marks by distance




