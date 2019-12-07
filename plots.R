library(sf)
library(tmap)
library(leafpop)
library(leaflet)
library(tmaptools)
library(tidyverse)
library(plyr)
library(classInt)
library(RColorBrewer)
library(geojsonio)

#importing the NUTS3 geojson
NUTS3 <- geojson_read("https://opendata.arcgis.com/datasets/473aefdcee19418da7e5dbfdeacf7b90_2.geojson", what = "sp")

#switching the geojson to NUTS
NUTS3_SF <- st_as_sf(NUTS3)

#The 2019 LAD do not have projections so I must attempt to draw LAD19s onto NUTS3 projectsion
#To do this do a point in polygon analysis
#get the LAD19 gejson
LAD19 <- geojson_read("https://opendata.arcgis.com/datasets/2b95585accc4437b97d766f31c5568cb_0.geojson", what = "sp")

#shift this to SF
LAD19_SF <- st_as_sf(LAD19)

#make sure both the NUTS3 map and LAD19 map have the same projection
LAD19_SF <- st_transform(LAD19_SF, 27700)
NUTS3_SF <- st_transform(NUTS3_SF, 27700)

#convert the LAD19 boundaries into points
LAD19_centroids <- st_centroid(LAD19_SF)

#set the CRS for both of these
st_crs(LAD19_centroids) <- 27700
st_crs(NUTS3_SF) <- 27700

#join them on the line at which they intersect
Joined <- st_join(LAD19_centroids, NUTS3_SF, join = st_intersects)

#create a df of the joined sf df so that I can use VLOOKUP in excel to map LAD19s onto 
df <- Joined[,c("lad19nm", "lad19cd", "lad19nmw", "nuts318cd")]


#remove the geometry from the data set
df_df <- df %>% st_set_geometry(NULL)
#check that it has been removed
class(df_df)

#write the dataset to a csv to be able to use VLOOKUP in excel to map LAD19s onto NUTS3 regions
write.csv(df_df, 'LAD19.csv')

#the mapping of boundaries of one onto another was done outside of R and in excel
#this was due to the difficulty of doing so in R
#They were mapped using the Countif and sumif functions to average out if moving up into a higher mapping level


#reading in the data that was created as a result
NUTS3_data <- read.csv("Data/NUTS3(1).csv", na=c("n/a", "na"))

#merging the sf object with the data created to be able to analyse this later
UKNUTS3Map <- merge(NUTS3_SF,
                    NUTS3_data,
                    by.x = "nuts318cd",
                    by.y = "NUTS.code",
                    no.dups=TRUE)

#setting the breaks for the GVA data
breaks <- c(0,75,80,85,90,95,100,105,110,115,120,125,1500)

breaksGVA <- c(0,55,70,85,100,115,130,145,1500)

breaks2 <- c(0,10,14,18,22,26,30,34,38,42,50)

options(digits = 3)

#finding the mean and standard deviation for the relevant columns to be able to set the breaks


UEM <- mean(UKNUTS3Map$U.E.rate...Jul.2018.Jun.2019, na.rm=TRUE)
UESD <- sd(UKNUTS3Map$U.E.rate...Jul.2018.Jun.2019, na.rm=TRUE)
UEMin <- min(UKNUTS3Map$U.E.rate...Jul.2018.Jun.2019, na.rm=TRUE)
UEMax <- max(UKNUTS3Map$U.E.rate...Jul.2018.Jun.201, na.rm=TRUE)

breaksUE <- c(UEMin, UEM-2*UESD, UEM-1.25*UESD, UEM-0.5*UESD, UEM, UEM+0.5*UESD, UEM+1.25*UESD, UEM+2*UESD, UEMax)

breaksUE

IMDM <- mean(UKNUTS3Map$IMD.2019...Average.score, na.rm=TRUE)
IMDSD <- sd(UKNUTS3Map$IMD.2019...Average.score, na.rm=TRUE)
IMDMax <- max(UKNUTS3Map$IMD.2019...Average.score, na.rm=TRUE)
IMDMin <- min(UKNUTS3Map$IMD.2019...Average.score, na.rm=TRUE)

breaksIMD <- c(IMDMin, IMDM-1.5*IMDSD, IMDM-0.75*IMDSD, IMDM, IMDM+0.75*IMDSD, IMDM+1.5*IMDSD, IMDMax)

breaksIMD

LEMM <- mean(UKNUTS3Map$LE.males.2015.2017,na.rm=TRUE)
LEMSD <- sd(UKNUTS3Map$LE.males.2015.2017, na.rm=TRUE)
LEMMin <- min(UKNUTS3Map$LE.males.2015.2017, na.rm=TRUE)
LEMMax <- max(UKNUTS3Map$LE.males.2015.2017, na.rm=TRUE)

breaksLEM <- c(LEMMin, LEMM-1.5*LEMSD, LEMM-0.75*LEMSD, LEMM, LEMM+0.75*LEMSD, LEMM+1.5*LEMSD, LEMMax)

breaksLEM

LEFM <- mean(UKNUTS3Map$LE.females.2015.2017, na.rm = TRUE)
LEFSD <- sd(UKNUTS3Map$LE.females.2015.2017, na.rm = TRUE)
LEFMin <- min(UKNUTS3Map$LE.females.2015.2017, na.rm=TRUE)
LEFMax <- max(UKNUTS3Map$LE.females.2015.2017, na.rm=TRUE)

breaksLEFM <- c(LEFMin, LEFM-1.5*LEFSD, LEFM-0.75*LEFSD, LEFM, LEFM+0.75*LEFSD, LEFM+1.5*LEFSD, LEFMax)

breaksLEFM

GCSEM <- mean(UKNUTS3Map$GCSE.2018.A..C.., na.rm=TRUE)
GCSESD <- sd(UKNUTS3Map$GCSE.2018.A..C.., na.rm=TRUE)
GCSEMin <- min(UKNUTS3Map$GCSE.2018.A..C.., na.rm=TRUE)
GCSEMax <- max(UKNUTS3Map$GCSE.2018.A..C.., na.rm=TRUE)

breaksGCSE <- c(GCSEMin, GCSEM-2*GCSESD, GCSEM-GCSESD, GCSEM, GCSEM+GCSESD, GCSEM+2*GCSESD, GCSEMax)

breaksGCSE

LeaveM <- mean(UKNUTS3Map$Leave, na.rm=TRUE)
LeaveSD <- sd(UKNUTS3Map$Leave, na.rm=TRUE)
LeaveMin <- min(UKNUTS3Map$Leave, na.rm=TRUE)
LeaveMax <- max(UKNUTS3Map$Leave, na.rm=TRUE)

breaksLeave <- c(LeaveMin, LeaveM-1.5*LeaveSD, LeaveM-0.75*LeaveSD, LeaveM, LeaveM+0.75*LeaveSD, LeaveM+1.5*LeaveSD, LeaveMax)

breaksLeave

tmap_mode("plot")

RdBu7 <- get_brewer_pal("RdBu", n=7)
RdBu5 <- get_brewer_pal("RdBu", n=5)
RdBu8 <- get_brewer_pal("RdBu", n=8)
ReverseRdBu <- get_brewer_pal("-RdBu", n =6)
ReversedRdBU <- get_brewer_pal("-RdBu", n=8)

tm1 <- tm_shape(UKNUTS3Map)+
  tm_polygons("GVA.in..2017", 
              breaks=breaksGVA, 
              palette = RdBu8,
              border.col = "grey",
              border.alpha= 0.5)+
  tm_legend(title.size = 0.01,
            show=TRUE, 
            position = c(0.05,0.4))+
  tm_layout(title = "GVA in 2017",
            title.size = 0.2,
            title.position = c("left", "top"),
            frame=TRUE)+
  tm_compass(type = "rose",
             position = c(0.75, 0.8,
             color.light="grey90"),
             size = 4)+
  tm_scale_bar(breaks = c(0,50,100),
               text.size=1)

tm1


tm2 <- tm_shape(UKNUTS3Map)+
  tm_fill("Dorling")+
  tm_legend(show=FALSE)+
  tm_layout(frame=FALSE)+
  tm_credits("(a)", position = c(0.3,0.80), size=1.5)

tm3 <- tm_shape(UKNUTS3Map)+
  tm_fill("Rowthorn")+
  tm_legend(show=FALSE)+
  tm_layout(frame=FALSE)+
  tm_credits("(b)", position = c(0.3,0.80), size=1.5)

tm4 <- tm_shape(UKNUTS3Map)+
  tm_fill("SE")+
  tm_legend(show=FALSE)+
  tm_layout(frame=FALSE)+
  tm_credits("(c)", 
             position = c(0.3,0.80), 
             size=1.5)
  
tm1
tm2
tm3
tm4

ggplot(UKNUTS3Map, aes(x="U.E.rate...Jul.2018.Jun.2019")) + geom_histogram(aes(y = ..density..),binwidth = 1) + geom_density(colour="red", size=1, adjust=1)

tmap_arrange(tm2,tm3,tm4, ncol=1)

tmap_options(max.categories = 15)
tm5 <- tm_shape(UKNUTS3Map)+
  tm_polygons("U.E.rate...Jul.2018.Jun.2019",
              breaks=breaksUE,
              palette = ReversedRdBU,
              border.col = "grey",
              border.alpha= 0.5)+
  tm_legend(title.size = 0.01,
            show=TRUE, 
            position = c(0.05,0.3)
            )+
  tm_layout(title = "Unemployment Rate 2018-2019",
            title.position = c("left", "top"),
                               legend.format=list(fun = function(x) paste0(formatC(x, digits = 1, format = "f"))),
            frame=TRUE,
            legend.title.size = 0.001,
            legend.show = TRUE,
            legend.position = c(0.05, 0.35),
            legend.width=3)+
  tm_compass(type = "rose",
             position = c(0.75, 0.8,
                          color.light="grey90"),
             size = 3)+
  tm_scale_bar(breaks = c(0,50,100),
               text.size=1)

tm5

tm10 <- tm_shape(UKNUTS3Map)+
  tm_polygons("IMD.2019...Average.score",
              breaks= breaksIMD, 
              palette = ReverseRdBu,
              border.col = "grey",
              border.alpha= 0.5)+
  tm_legend(title.size = 0.01,
            show=TRUE, 
            position = c(0.05,0.35))+
  tm_layout(title = "IMD 2019 average score",
            title.position = c("left", "top"),
            legend.format=list(fun = function(x) paste0(formatC(x, digits = 1, format = "f"))),
            frame=TRUE)+
  tm_compass(type = "rose",
             position = c(0.75, 0.8,
                          color.light="grey90"),
             size = 3)+
  tm_scale_bar(breaks = c(0,50,100),
               text.size=1)

breaksIMD
tm10

tm6 <- tm_shape(UKNUTS3Map)+
  tm_polygons("LE.females.2015.2017",
              breaks= breaksLEFM, 
              palette = RdBu,
              border.col = "grey",
              border.alpha= 0.5,
              legend.digits = 3)+
  tm_legend(title.size = 0.01,
            show=TRUE, 
            position = c(0.05,0.4))+
  tm_layout(title = "Female life expectancy 2015-2017 ",
            title.position = c("left", "top"),
            legend.format=list(fun = function(x) paste0(formatC(x, digits = 1, format = "f"))),
            frame=TRUE)+
  tm_compass(type = "rose",
             position = c(0.75, 0.8,
                          color.light="grey90"),
             size = 3)+
  tm_scale_bar(breaks = c(0,50,100),
               text.size=1)

tm6

tm7 <- tm_shape(UKNUTS3Map)+
  tm_polygons("LE.males.2015.2017",
              breaks= breaksLEM, 
              palette = RdBu,
              border.col = "grey",
              border.alpha= 0.5,
              legend.digits = 3)+
  tm_legend(title.size = 0.01,
            show=TRUE, 
            position = c(0.05,0.35))+
  tm_layout(title = "Male Life expactancy 2015-2017",
            title.position = c("left", "top"),
            legend.format=list(fun = function(x) paste0(formatC(x, digits = 1, format = "f"))),
            frame=TRUE)+
  tm_compass(type = "rose",
             position = c(0.75, 0.8,
                          color.light="grey90"),
             size = 3)+
  tm_scale_bar(breaks = c(0,50,100),
               text.size=1)

tm7

#hist(NUTS3_data$GCSE.2018.A..C.., main="Histogram of unemployment", xlab="Unemployment", xlim=c(45,78), breaks=30)


tm8 <- tm_shape(UKNUTS3Map)+
  tm_polygons("GCSE.2011.A..C..",
              breaks= breaksGCSE, 
              palette = RdBu,
              border.col = "grey",
              border.alpha= 0.5,
              legends.digits = 3)+
  tm_legend(title.size = 0.01,
            show=TRUE, 
            position = c(0.05,0.4))+
  tm_layout(title = "Percentage of A*-C GCSE grades 2018",
            title.position = c("left", "top"),
            legend.format=list(fun = function(x) paste0(formatC(x, digits = 1, format = "f"))),
            frame=TRUE)+
  tm_compass(type = "rose",
             position = c(0.75, 0.8,
                          color.light="grey90"),
             size = 3)+
  tm_scale_bar(breaks = c(0,50,100),
             text.size=1)

tm8

#hist(NUTS3_data$Leave, main="Histogram of unemployment", xlab="Unemployment", xlim=c(20,80), breaks=100)

tm9 <- tm_shape(UKNUTS3Map)+
  tm_polygons("Leave",
              breaks= breaksLeave, 
              palette = ReverseRdBu,
              border.col = "grey",
              border.alpha= 0.5,
              legend.width = 4,
              legend.digits = 3)+
  tm_legend(title.size = 0.01,
            show=TRUE, 
            position = c(0.05,0.4))+
  tm_layout(title = "Percentage voting Leave in 2016",
            title.position = c("left", "top"),
            legend.format=list(fun = function(x) paste0(formatC(x, digits = 1, format = "f"))),
            frame=TRUE)+
  tm_compass(type = "rose",
             position = c(0.75, 0.8,
                          color.light="grey90"),
             size = 3)+
  tm_scale_bar(breaks = c(0,50,100),
               text.size=1)

tm9

#tm5
#tm6
#tm7
#tm8
#tm9
#tm10

t = tmap_arrange(tm5, tm6, tm7,tm8,tm10,tm9, ncol=3)
t
#this is then exported as a png




#setting up a function to run a linear regression
regressionmodel <- function(data1,data2){
  #setting the regression function
  modelx <- lm(data1 ~ data2)
  #assigning the summary to a variable
  modelx_summary <- summary(modelx)
  modelx_res <- tidy(modelx)
  #returning the summary of the regression
  return(modelx_summary)
}

GVA_Dorling <- regressionmodel(NUTS3_data$GVA.in..2017, NUTS3_data$Dorling)
GVA_dorling

GVA_Rowthorn <- regressionmodel(NUTS3_data$GVA.in..2017, NUTS3_data$Rowthorn)
GVA_Rowthorn

GVA_SE <- regressionmodel(NUTS3_data$GVA.in..2017, NUTS3_data$SE)
GVA_SE

UE_Dorling <- regressionmodel(NUTS3_data$U.E.rate...Jul.2018.Jun.2019, NUTS3_data$Dorling)
UE_Dorling

UE_Rowthorn <- regressionmodel(NUTS3_data$U.E.rate...Jul.2018.Jun.2019, NUTS3_data$Rowthorn)
UE_Rowthorn

UE_SE <- regressionmodel(NUTS3_data$U.E.rate...Jul.2018.Jun.2019, NUTS3_data$SE)
UE_SE

#male
MaleLE_Dorling <- regressionmodel(NUTS3_data$LE.males.2015.2017, NUTS3_data$Dorling)
MaleLE_Dorling

MaleLE_Rowthorn <- regressionmodel(NUTS3_data$LE.males.2015.2017, NUTS3_data$Rowthorn)
MaleLE_Rowthorn

MaleLE_SE <- regressionmodel(NUTS3_data$LE.males.2015.2017, NUTS3_data$SE)
MaleLE_SE

#female
femaleLE_Dorling <- regressionmodel(NUTS3_data$LE.females.2015.2017, NUTS3_data$Dorling)
femaleLE_Dorling

femaleLE_Rowthorn <- regressionmodel(NUTS3_data$LE.females.2015.2017, NUTS3_data$Rowthorn)
femaleLE_Rowthorn

femaleLE_SE <- regressionmodel(NUTS3_data$LE.females.2015.2017, NUTS3_data$SE)
femaleLE_SE

#GCSE
GCSE_Dorling <- regressionmodel(NUTS3_data$GCSE.2018.A..C.., NUTS3_data$Dorling)
GCSE_Dorling

GCSE_Rowthorn <- regressionmodel(NUTS3_data$GCSE.2018.A..C.., NUTS3_data$Rowthorn)
GCSE_Rowthorn

GCSE_SE <- regressionmodel(NUTS3_data$GCSE.2018.A..C.., NUTS3_data$SE)
GCSE_SE

#IMD
IMD_Dorling <- regressionmodel(NUTS3_data$IMD.2019...Average.score, NUTS3_data$Dorling)
IMD_Dorling

IMD_Rowthorn <- regressionmodel(NUTS3_data$IMD.2019...Average.score, NUTS3_data$Rowthorn)
IMD_Rowthorn

IMD_SE <- regressionmodel(NUTS3_data$IMD.2019...Average.score, NUTS3_data$SE)
IMD_SE

#Brexit leave
Brexit_Dorling <- regressionmodel(NUTS3_data$Leave, NUTS3_data$Dorling)
Brexit_Dorling

Brexit_Rowthorn <- regressionmodel(NUTS3_data$Leave, NUTS3_data$Rowthorn)
Brexit_Rowthorn

Brexit_SE <- regressionmodel(NUTS3_data$Leave, NUTS3_data$SE)
Brexit_SE


#creating interactive maps

library(leafpop)
library(leaflet)



UKNUTS3MapWGS <- st_transform(UKNUTS3Map, 4326)

popGVA <- popupTable(UKNUTS3MapWGS, zcol=c("nuts318cd", "nuts318nm", "GVA.in..2017"))
popUE <- popupTable(UKNUTS3MapWGS, zcol=c("nuts318cd", "nuts318nm", "U.E.rate...Jul.2018.Jun.2019"))
popMLE <- popupTable(UKNUTS3MapWGS, zcol=c("nuts318cd", "nuts318nm", "LE.males.2015.2017"))
popFLE <- popupTable(UKNUTS3MapWGS, zcol=c("nuts318cd", "nuts318nm", "LE.females.2015.2017"))
popGCSE <- popupTable(UKNUTS3MapWGS, zcol=c("nuts318cd", "nuts318nm", "GCSE.2018.A..C.."))
popIMD <- popupTable(UKNUTS3MapWGS, zcol=c("nuts318cd", "nuts318nm", "IMD.2019...Average.score"))
popBrexit <- popupTable(UKNUTS3MapWGS, zcol=c("nuts318cd", "nuts318nm", "Leave"))

pal1 <- colorBin(palette="RdBu", domain=UKNUTS3MapWGS$GVA.in..2017, bins=breaksGVA)
pal2 <- colorBin(palette=ReverseRdBu, domain=as.numeric(UKNUTS3MapWGS$U.E.rate...Jul.2018.Jun.2019), bins=breaksUE)
pal3 <- colorBin(palette=ReverseRdBu, domain=as.numeric(UKNUTS3MapWGS$IMD.2019...Average.score), bins=breaksIMD)
pal4 <- colorBin(palette="RdBu", domain=as.numeric(UKNUTS3MapWGS$LE.females.2015.2017), bins=breaksLEFM)
pal5 <- colorBin(palette="RdBu", domain=as.numeric(UKNUTS3MapWGS$LE.males.2015.2017), bins=breaksLEM)
pal6 <- colorBin(palette = "RdBu", domain=as.numeric(UKNUTS3MapWGS$GCSE.2018.A..C..), bins = breaksGCSE)
pal7 <- colorBin(palette = ReverseRdBu, domain=as.numeric(UKNUTS3MapWGS$Leave), bins = breaksLeave)

map <- leaflet(UKNUTS3MapWGS) %>%
  
  #creating basemap options
  addTiles(group = "OSM (default)") %>%
  
  addPolygons(fillColor = ~pal1(GVA.in..2017),
              color = "white",
              weight = 2,
              opacity = 1,
              dashArray = "3",
              popup = popGVA,
              fillOpacity = 0.7,
              group = "GVA",
              highlight = highlightOptions(
                dashArray = "",
                fillOpacity = 0.8,
                weight = 3,
                color = "Grey",
                bringToFront = TRUE)
              ) %>%
  
  addPolygons(fillColor = ~pal2(as.numeric(U.E.rate...Jul.2018.Jun.2019)),
              color = "white",
              weight = 2,
              opacity = 1,
              dashArray = "3",
              popup = popUE,
              fillOpacity = 0.7,
              group = "UE",
              highlight = highlightOptions(
                dashArray = "",
                fillOpacity = 0.8,
                weight = 3,
                color = "Grey",
                bringToFront = TRUE)) %>%
  
  addPolygons(fillColor = ~pal3(IMD.2019...Average.score),
              color = "white",
              weight = 2,
              opacity = 1,
              dashArray = "3",
              popup = popIMD,
              fillOpacity = 0.7,
              group = "IMD",
              highlight = highlightOptions(
                dashArray = "",
                fillOpacity = 0.8,
                weight = 3,
                color = "Grey",
                bringToFront = TRUE)) %>%
  
  addPolygons(fillColor = ~pal4(LE.females.2015.2017),
              color = "white",
              weight = 2,
              opacity = 1,
              dashArray = "3",
              popup = popFLE,
              fillOpacity = 0.7,
              group = "FLE",
              highlight = highlightOptions(
                dashArray = "",
                fillOpacity = 0.8,
                weight = 3,
                color = "Grey",
                bringToFront = TRUE)) %>%
  
  addPolygons(fillColor = ~pal5(LE.males.2015.2017),
              color = "white",
              weight = 2,
              opacity = 1,
              dashArray = "3",
              popup = popMLE,
              fillOpacity = 0.7,
              group = "MLE",
              highlight = highlightOptions(
                dashArray = "",
                fillOpacity = 0.8,
                weight = 3,
                color = "Grey",
                bringToFront = TRUE)) %>%
  
  addPolygons(fillColor = ~pal6(GCSE.2018.A..C..),
              color = "white",
              weight = 2,
              opacity = 1,
              dashArray = "3",
              popup = popGCSE,
              fillOpacity = 0.7,
              group = "GCSE",
              highlight = highlightOptions(
                dashArray = "",
                fillOpacity = 0.8,
                weight = 3,
                color = "Grey",
                bringToFront = TRUE
              )) %>%
  
  addPolygons(fillColor = ~pal7(Leave),
              color = "white",
              weight = 2,
              opacity = 1,
              dashArray = "3",
              popup = popBrexit,
              fillOpacity = 0.7,
              group = "Brexit",
              highlight = highlightOptions(
                dashArray = "",
                fillOpacity = 0.8,
                weight = 3,
                color = "Grey",
                bringToFront = TRUE)) %>%
  
  #adding legends for each polygon
  addLegend(
    #setting the pallete to predetermined one
    pal=pal1,
    #where to get the values from        
    values = UKNUTS3MapWGS$GVA.in..2017,
    #group this with the GVA polygon        
    group = c("GVA"),
    #setting the title of the legend        
    title = "GVA in 2017 as a percentage of the UK average",
    #setting the position as bottom left      
    position = "bottomleft",
    #formatting the label so that it only shows to three significant figures to make it look cleaner     
    labFormat = labelFormat(digits=3))%>%
  
  addLegend(pal=pal2,
            values = as.numeric(UKNUTS3MapWGS$U.E.rate...Jul.2018.Jun.2019),
            group = c("UE"),
            title = "Unemployment rate",
            position = "bottomleft",
            labFormat = labelFormat(digits=3))%>%
  
  addLegend(pal=pal3,
            values = NUTS3_Map$IMD.2019...Average.score,
            group = "IMD",
            title = "Indices of multiple deprivation score",
            position = "bottomleft",
            labFormat = labelFormat(digits=3))%>%
  
  addLegend(pal=pal4,
            values = NUTS3_Map$LE.females.2015.2017,
            group = "FLE",
            title = "Female life expectancy(years)",
            position = "bottomleft",
            labFormat = labelFormat(digits=3))%>%
  
  addLegend(pal=pal5,
            values = NUTS3_Map$LE.males.2015.2017,
            group = "MLE",
            title = "Male life expectancy(years)",
            position = "bottomleft",
            labFormat = labelFormat(digits=3))%>%
  
  addLegend(pal=pal6,
            values = NUTS3_Map$GCSE.2018.A..C..,
            group = "GCSE",
            title = "Percentage of 5 A*-C grades at GCSE",
            position = "bottomleft",
            labFormat = labelFormat(digits=3))%>%
  
  addLegend(pal=pal7,
           values = NUTS3_Map$Leave,
           group = "Brexit",
           title = "Percentage voting leave in 2016 EU referendum",
           position = "bottomleft",
           labFormat = labelFormat(digits=3))%>%
  
  addLayersControl(
    baseGroups = "OSM (default)",
    overlayGroup = c("UE", "GVA", "IMD", "FLE", "MLE", "GCSE", "Brexit"),
    options = layersControlOptions(collapsed = FALSE))%>%
  hideGroup(c("UE", "IMD", "FLE", "MLE", "GCSE", "Brexit"))
              

map
