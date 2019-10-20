



library(sf)
library(tmap)
library(leaflet)
library(tmaptools)
library(tidyverse)
library(plyr)
library(classInt)
library(shiny)
library(RColorBrewer)

UK <- st_read("C:/Users/cex/Downloads/NUTS_Level_3_January_2018_Full_Clipped_Boundaries_in_the_United_Kingdom/NUTS_Level_3_January_2018_Full_Clipped_Boundaries_in_the_United_Kingdom.shp")

UKData <- read.csv("C:/Users/cex/Documents/Smart Cities and Urban Analytics/GIS/Assessment/UKdata3.csv", na="n/a")



UKDataMap <- merge(UK,
                   UKData,
                   by.x = "nuts318cd",
                   by.y = "ï..NUTS.code",
                   no.dups=TRUE)


choice =c("GVA_._1997","GVA_._1998","GVA_._1999","GVA_._2000","GVA_._2001","GVA_._2002","GVA_._2003","GVA_._2004","GVA_._2005","GVA_._2006","GVA_._2007",
           "GVA_._2008","GVA_._2009","GVA_._2010","GVA_._2011","GVA_._2012","GVA_._2013","GVA_._2014","GVA_._2015","GVA_._2016","GVA_._2017")


ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    #leaflet will call the map from the server function
    leafletOutput("map", width = "100%", height = "100%"),
    # this sets theput panel placement
    titlePanel("UK local GVA 1997-2017"),
    absolutePanel(top = 10, right = 10,
                  selectInput("years",
                              label = "year",
                              choices = choice,
                              multiple = FALSE
                  ),
                  selectInput("colourbrewerpalette", "Color Scheme",
                              rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                  ),
                  sliderInput("slide","local GVA",
                              min(as.numeric(UKDataMap$GVA_._2017), na.rm=TRUE),
                              max(as.numeric(UKDataMap$GVA_._2017), na.rm=TRUE),
                              value = range(as.numeric(UKDataMap$GVA_._2017), na.rm=TRUE),
                              step = 1,
                              sep = ""
                  ),
                  
    )
)



####################################### server
server <- function(input, output, session) {
    output$map <- renderLeaflet({
        #use leaflet here, and only include aspects of the map that won't need to change dramatically
        leaflet(UKDataMap) %>% addTiles() %>% setView(-0.5, 53, zoom = 6)
    })
    observe({
        (UKDataMap2 <- ({UKDataMap[as.numeric(UKDataMap[[input$years]])>=input$slide[1]&as.numeric(UKDataMap[[input$years]]) <= input$slide[2],]}))
        breaks<-classIntervals(as.numeric(UKDataMap2[[input$years]]), n=11, style= "fixed", fixedBreaks =c(0,75,80,85,90,95,100,105,110,115,120,1000))
        breaks <- breaks$brks
        pal <- colorBin(palette = input$colourbrewerpalette, 
                        domain = UKDataMap2[[input$years]],
                        bins = breaks)
        leafletProxy("map", data=UKDataMap2) %>%
            clearShapes() %>% 
            addPolygons(color="white", 
                        weight = 2,
                        opacity = 1,
                        dashArray = "3",
                        # a popup of region name and %
                        popup = paste(UKDataMap2$nuts318nm,"- %",UKDataMap2[[input$years]]),
                        fillOpacity = 0.5, 
                        fillColor = ~pal(UKDataMap2[[input$years]])
            )
    })
    
    observe({
        # call the filter again for this observer
        (UKDataMap2<-({UKDataMap[as.numeric(UKDataMap[[input$years]]) >= input$slide[1] & as.numeric(UKDataMap[[input$years]]) <=
                                     input$slide[2],]}))
        
        # this observer follows the same pattern
        # but adds a legend 
        breaks<-classIntervals(as.numeric(UKDataMap2[[input$years]]), n=11, style= "fixed", fixedBreaks =c(0,75,80,85,90,95,100,105,110,115,120,1000))
        breaks <- breaks$brks
        
        pal <- colorBin(palette = input$colourbrewerpalette, 
                        domain = UKDataMap2[[input$years]],
                        #create bins using the breaks object from earlier
                        bins = breaks
        )
        # here is the Legend
        proxy <- leafletProxy("map", data = UKDataMap2)
        proxy %>% clearControls() %>%
            addLegend("bottomright", 
                      pal= pal, 
                      values = ~UKDataMap2[[input$years]], 
                      title = input$years, 
                      labFormat = labelFormat(prefix = ""),
                      opacity = 1
            )
    })
}

shinyApp(ui, server)

