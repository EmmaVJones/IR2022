library(config)
library(pins)
library(shiny)
library(rgdal)
library(sf)

ui <- fluidPage(
  fileInput('regionalAUshapefile','Choose your Regional Assessment Unit shapefile.',
                      accept = c(".dbf",".prj",".sbn",".sbx",".shp","shp.xml",".shx"))
                      
)

#server <- 


BRROworkingAU <- st_read('userDataToUpload/AU_working/va_2020_aus_riverine_DRAFT_BRRO.shp') %>%
  st_transform(4326)   # transform to WQS84 for spatial intersection 


  
  
pin(BRROworkingAU, description = "BRRO AUS", board = "rsconnect")
