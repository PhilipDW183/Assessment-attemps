library(sf)
library(tmap)
library(leaflet)
library(tmaptools)
library(tidyverse)
library(plyr)
library(classInt)
library(shiny)
library(RColorBrewer)

Brexit <- read.csv("C:/Users/cex/Documents/Smart Cities and Urban Analytics/GIS/Assessment/EU-referendum-result-data.csv")

UKGVA <- read.csv("C:/Users/cex/Documents/Smart Cities and Urban Analytics/GIS/Assessment/UKdata3.csv")

UKMAPSF <- st_read("C:/Users/cex/Downloads/NUTS_Level_3_January_2018_Full_Clipped_Boundaries_in_the_United_Kingdom/NUTS_Level_3_January_2018_Full_Clipped_Boundaries_in_the_United_Kingdom.shp")

UKDataMap <- merge(UKMAPSF,
                   UKGVA,
                   by.x = "nuts318cd",
                   by.y = "ï..NUTS.code",
                   no.dups = TRUE) 

UKCompleteMap <- merge(UKDataMap,
                       Brexit,
                       by.x = "nuts318nm",
                       by.y = "Area",
                       no.dups = TRUE)
class(UKMAPSF)
class(UKDataMap)
class(UKCompleteMap)

tmap_mode("plot")
tm_shape(UKCompleteMap)+
  tm_polygons(c("Pct_Leave", "Percentage_change"),
              style = c("jenks","pretty"), 
              palette=list("OrRd","PuRd"),
              auto.palletee.mapping=FALSE,
              title = c("Percentage voting leave", "percentage change versus average"))+
  tmap_options(max.categories = 10)


