library(sf)
library(tmap)
library(leaflet)
library(tmaptools)
library(tidyverse)
library(classInt)
library(shiny)
library(RColorBrewer)
library(here)

#dir <- getwd()

#here()

NUTS3 <- geojson_read("https://opendata.arcgis.com/datasets/473aefdcee19418da7e5dbfdeacf7b90_2.geojson", what = "sp")

NUTS3_SF <- st_as_sf(NUTS3)
#reading in the UK shapefile

#reading in the UK data for regional GVA
UKData <- read.csv("Data/NUTS3.csv", na="n/a")


#merging the shapefile and the regional data on the basis of the NUTS3 classifications
NUTS3 <- merge(NUTS3_SF,
                UKData,
                by.x = "nuts318cd",
                by.y = "NUTS.code",
                no.dups=TRUE)

names(NUTS3) <- c("NUTS level", "NUTS.code", "Region name", "in_1997","in_1998","in_1999","in_2000","in_2001","in_2002","in_2003","in_2004","in_2005","in_2006","in_2007",
                    "in_2008","in_2009","in_2010","in_2011","in_2012","in_2013","in_2014","in_2015","in_2016","in_2017")


choice =c("GVA.in.1997", "GVA.in.1998", "GVA.in.1999", "GVA.in.2000", "GVA.in.2001", "GVA.in.2002", "GVA.in.2003", "GVA.in.2004", "GVA.in.2005", "GVA.in.2006", "GVA.in.2007", "GVA.in.2008", "GVA.in.2009", "GVA.in.2010", "GVA.in.2011", "GVA.in.2012", "GVA.in.2013", "GVA.in.2014", "GVA.in.2015", "GVA.in.2016", "GVA.in.2017")
#setting the column headers to use when running the git



#establishing the user interface for the shiny app
ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    #leaflet will call the map from the server function
    leafletOutput("map", width = "100%", height = "100%"),
    #this sets theput panel placement
    #setting an absolute panel as want it to cover the whole of the webpage
    absolutePanel(top = 10, right = 10,
                  #we want them to select the years so we create select input
                  #this is given the id years (to be called in the server) and label it years
                  #the choices that the user can select come from the choices 
                  selectInput("Geography",
                              label = "Choose your geography",
                              choices = c("NUTS3", "NUTS2", "NUTS1"),
                              multiple=FALSE),
                  
                  selectInput("Factor",
                              label = "Choose your factor",
                              choices = c("GVA", "Unemployment", "IMD", "LE males", "LE females", "Education", "Brexit"),
                              multiple=FALSE),
                  
                  #we want them to select years based on what geography they have selected
                  selectInput("years",
                              label = "year",
                              choices = choice,
                              multiple = FALSE
                  ),
                  #we also want the user to select the colour pallete to see the differences
                  #we set the id to colour brewer pallete and the user will see it as 
                  selectInput("colourbrewerpalette", "Color Scheme",
                              rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                  ),
                  #the final slider allows the user to select the regional GVA % they want to see
                  sliderInput("slide","Values",
                              #this sets the minimum GVA that will be shown
                              min(as.numeric(NUTS3_Map$GVA.in..2017), na.rm=TRUE),
                              #this shows the maximum GVA that will be shown
                              max(as.numeric(UKDataMap$GVA_._2017), na.rm=TRUE),
                              #this sets the range of the values, we want it to be as big as possible
                              value = range(as.numeric(UKDataMap$GVA_._2017), na.rm=TRUE),
                              #the step of percentages is in a step of 1
                              step = 1,
                              sep = ""
                  ),
                  
    )
)



####################################### server
server <- function(input, output, session) {
    
    output$map <- renderLeaflet({
        #use leaflet here, and only include aspects of the map that won't need to change dramatically
        whichmap <- reactive(paste0(input$Geography, '_', input$Factor))
        
        leaflet(whichmap) %>% addTiles() %>% setView(-0.5, 53, zoom = 6)
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
                        popup = paste(NUTS3_Map$nuts318nm,"- %",UKDataMap2[[input$years]]),
                        fillOpacity = 0.5, 
                        fillColor = ~pal(UKDataMap2[[input$years]])
            )
    })
    
    observe({
        # call the filter again for this observer to be able to create the legend
        (UKDataMap2<-({UKDataMap[as.numeric(UKDataMap[[input$years]]) >= input$slide[1] & as.numeric(UKDataMap[[input$years]]) <=
                                     input$slide[2],]}))
        
        breaks<-classIntervals(as.numeric(UKDataMap2[[input$years]]), n=11, style= "fixed", fixedBreaks =c(0,75,80,85,90,95,100,105,110,115,120,1000))
        breaks <- breaks$brks
        
        pal <- colorBin(palette = input$colourbrewerpalette, 
                        domain = UKDataMap2[[input$years]],
                        #create bins using the breaks object from earlier
                        bins = breaks
        )
        #This is the legend
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

#intialising the shinyapp using the server and ui created above
shinyApp(ui, server)
