install.packages("leafpop")
install.packages('rsconnect')
rsconnect::setAccountInfo(name='phillllllip',
                          token='7A0C2AB9C5858337DC209BB2201815F2',
                          secret='ggAIn7b7Ijot0rGuk6AV4K4CozXAooN3iI1QPIpU')
library(rsconnect)


library(sf)
library(tmap)
library(leafpop)
library(leaflet)
library(tmaptools)
library(tidyverse)
library(plyr)
library(classInt)
library(shiny)
library(RColorBrewer)

UK <- st_read("C:/Users/cex/Downloads/NUTS_Level_3_January_2018_Full_Clipped_Boundaries_in_the_United_Kingdom/NUTS_Level_3_January_2018_Full_Clipped_Boundaries_in_the_United_Kingdom.shp")

UKdata <- read.csv("C:/Users/cex/Documents/Smart Cities and Urban Analytics/GIS/Assessment/UKdata2.csv", na = "n/a")

UKDataMap <- merge(UK,
                   UKData,
                   by.x = "nuts318cd",
                   by.y = "NUTS code",
                   no.dups=TRUE)
class(UKDataMap)

choice=c("x1997","x1998","x1999","x2000","x2001","x2002","x2003",
         "x2004","x2005","x2006","x2007","x2008","x2009","x2010",
         "x2011","x2012","x2013","x2014","x2015","x2016","x20173")



ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  # we're using leaflet and have title the outputID map
  # this will call it from our server function below
  leafletOutput("map", width = "100%", height = "100%"),
  # this sets our input panel placement
  titlePanel("UK local GVA 1997-2017"),
  absolutePanel(top = 10, right = 10,
                selectInput("years",
                            label = "year",
                            choices = choice,
                            multiple = FALSE
                ),
                selectInput("colourbrewerpalette", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq","div")))
                ),
                sliderInput("slide","local GVA",
                            min(UKDataMap$x1997, na.rm=TRUE),
                            max(UKDataMap$x20173, na.rm=TRUE),
                            value = range(UKDataMap$x1997, na.rm=TRUE),
                            step = 1000,
                            sep = ""
                ),
                selectInput("classIntStyle", "Interval Style",
                            c("Jenks Natural Breaks" = "jenks",
                              "Quantile" = "quantile",
                              "Equal Interval" = "equal",
                              "Pretty" = "pretty"))
                
  )
)



####################################### server
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    #use leaflet here, and only include aspects of the map that won't need to change dramatically
    leaflet(UKDataMap) %>% addTiles() %>% setView(-0.5, 53, zoom = 6)
  })
  observe({
    (UKDataMap2 <- ({UKDataMap[UKDataMap[[input$years]]>=input$slide[1]&UKDataMap[[input$years]] <= input$slide[2],]}))
    breaks<-classIntervals(as.numeric(UKDataMap2[[input$years]]), n=9, style=input$classIntStyle)
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
                  # add a popup of borough name and count based on
                  # the drop down of accomodation (hotel or airbnb)
                  # remember the ID we gave to that was Accom
                  popup = paste(UKDataMap2$nuts318nm,"- £",UKDataMap2[[input$years]]),
                  fillOpacity = 0.5, 
                  fillColor = ~pal(UKDataMap2[[input$years]])
      )
  })
  
  observe({
    # call the filter again for this observer
    (UKDataMap2<-({UKDataMap[UKDataMap[[input$years]] >= input$slide[1] & UKDataMap[[input$years]] <=
                               input$slide[2],]}))
    
    # this observer follows the same pattern
    # but adds a legend 
    breaks<-classIntervals(as.numeric(UKDataMap2[[input$years]]), n=9, style=input$classIntStyle)
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
