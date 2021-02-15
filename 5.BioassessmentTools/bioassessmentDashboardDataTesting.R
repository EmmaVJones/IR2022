source('global.R')

assessmentRegions <- st_read( 'data/GIS/AssessmentRegions_simple.shp')
ecoregion <- st_read('data/GIS/vaECOREGIONlevel3__proj84.shp') 

collectorFilter <- sort(unique(benSamps$`Collected By`))[3] #NULL
basinFilter <- "Rappahannock" #NULL # sort(unique(benSamps$Basin_Code))[3]
stationFilter <- NULL #sort(unique(benSamps$StationID))
repFilter <- NULL #sort(unique(benSamps$RepNum))

benSampsFilter <- benSamps %>%
  {if(!is.null(collectorFilter))
    filter(., `Collected By` %in% collectorFilter)
    else . } %>% 
  {if(!is.null(basinFilter))
    filter(., Basin_Code %in% basinFilter)
    else . } %>%
  {if(!is.null(stationFilter))
    filter(., StationID %in% stationFilter)
    else . } %>%
  {if(!is.null(repFilter))
    filter(., RepNum %in% repFilter)
    else .}

