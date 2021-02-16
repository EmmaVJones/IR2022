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

benSampsFilterStations <- filter(benSampsStations, StationID %in% benSampsFilter$StationID)

# Map 
CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE) %>%
  setView(-78, 37.5, zoom=6) %>%
  addCircleMarkers(data=benSampsFilterStations,color='yellow', fillColor='blue', radius = 5,
                   fillOpacity = 0.5,opacity=1,weight = 2,stroke=T,group="sites",
                   label = ~StationID,
                   popup=~paste(sep='<br>',
                                paste(strong('StationID : '), StationID),
                                paste(strong('Total Station Visits (Not Sample Reps) :'), `Total Station Visits (Not Sample Reps)`),
                                paste(strong('Ecoregion : '), US_L3CODE),
                                paste(strong('Ecoregion Name : '), US_L3NAME), 
                                paste(strong('Basin : '), Basin_Code),
                                paste(strong('HUC12 Name : '), HU_12_NAME),
                                paste(strong('HUC12 : '), HUC_12),
                                paste(strong('DEQ Region : '), ASSESS_REG),
                                paste(strong('VAHU6 : '), VAHU6))) %>%
  inlmisc::AddHomeButton(raster::extent(-83.89, -74.80, 36.54, 39.98), position = "topleft") %>%
  inlmisc::AddSearchButton(group = "sites", zoom = 15,propertyName = "label",
                           textPlaceholder = "Search stations") %>%
  addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                   overlayGroups = c('sites'),
                   options=layersControlOptions(collapsed=T),
                   position='topleft')

# SCI Information
# choose appropriate SCI based on Ecoregion
SCI_filter <- filter(VSCIresults, BenSampID %in% filter(benSampsFilter, ! US_L3CODE %in% c(63,65))$BenSampID) %>%
  bind_rows(
    filter(VCPMI63results, BenSampID %in% filter(benSampsFilter,  US_L3CODE %in% c(63))$BenSampID)  ) %>%
  bind_rows(
    filter(VCPMI65results, BenSampID %in% filter(benSampsFilter,  US_L3CODE %in% c(65))$BenSampID)  ) %>%
  mutate(SeasonGradient = as.factor(paste0(Season, " (",Gradient,")")),
         SeasonGradientColor = case_when(SeasonGradient == "Spring (Riffle)" ~  "#66C2A5",
                                         SeasonGradient == "Spring (Boatable)" ~  "#66C2A5",
                                         SeasonGradient == "Spring (MACS)" ~  "#66C2A5",
                                         SeasonGradient == "Outside Sample Window (Riffle)" ~ "#FC8D62",
                                         SeasonGradient == "Outside Sample Window (Boatable)" ~ "#FC8D62",
                                         SeasonGradient == "Outside Sample Window (MACS)" ~ "#FC8D62",
                                         SeasonGradient == "Fall (Riffle)" ~ "#8DA0CB",
                                         SeasonGradient == "Fall (Boatable)" ~ "#8DA0CB",
                                         SeasonGradient == "Fall (MACS)" ~ "#8DA0CB",
                                         TRUE ~ as.character(NA)) ) %>%
  left_join(dplyr::select(benSampsFilter, StationID, Sta_Desc)) %>%
  dplyr::select(StationID, Sta_Desc, BenSampID, `Collection Date`, RepNum, everything())
SCI_filter$SeasonGradient <- factor(SCI_filter$SeasonGradient,levels=c("Spring (Riffle)",
                                                                       "Spring (Boatable)",
                                                                       "Spring (MACS)",
                                                                       "Outside Sample Window (Riffle)",
                                                                       "Outside Sample Window (Boatable)",
                                                                       "Outside Sample Window (MACS)",
                                                                       "Fall (Riffle)",
                                                                       "Fall (Boatable)",
                                                                       "Fall (MACS)")) %>% 
  droplevels()


plot_ly(SCI_filter,
        x = ~`Collection Date`, y = ~`SCI Score`, type = 'bar', 
        color = ~SeasonGradient,  #marker = list(color = ~SeasonGradientColor,width = 0.5), # throws off color for some reason
        stroke = list(color = 'rgb(0, 0, 0)', width = 3),
        hoverinfo="text", text=~paste(sep="<br>",
                                      paste("StationID: ", StationID),
                                      paste("Collection Date: ", as.Date(`Collection Date`)),
                                      paste('Replicate: ', RepNum),
                                      paste("Collector ID: ",`Collected By`),
                                      paste("BenSampID: ", BenSampID),
                                      paste("SCI Score: ", format(`SCI Score`, digits=2)),
                                      paste("Gradient: ", Gradient)),
        name = ~paste('Rep ',RepNum, SeasonGradient)) %>%
  add_lines(., x = c(min(SCI_filter$`Collection Date`), max(SCI_filter$`Collection Date`)), y = c(60,60),
            text = 'VSCI Limit', name = 'VSCI Limit', color = ~SeasonGradient,
            ) %>%
  # {if('VSCI' %in% SCI_filter$SCI)
  #   add_lines(., x = c(min(SCI_filter$`Collection Date`), max(SCI_filter$`Collection Date`)), y = c(60))
  #   else . } %>%
  layout(
    yaxis=list(title="SCI"),
    xaxis=list(title="Sample Date",tickfont = list(size = 10),
               type = 'date',tickformat = "%Y"))  

# organize reps into separate objects so traces don't stack, drop extra levels so colors turn out correctly
rep1s <- SCIresults %>% filter(RepNum == 1) %>% droplevels()
rep2s <- SCIresults %>% filter(RepNum == 2) %>% droplevels()
plot_ly(rep1s,
        x = ~`Collection Date`, y = ~`SCI Score`, type = 'bar', 
        color = ~SeasonGradient,  #marker = list(color = ~SeasonGradientColor,width = 0.5), # throws off color for some reason
        stroke = list(color = 'rgb(0, 0, 0)', width = 3),
        hoverinfo="text", text=~paste(sep="<br>",
                                      paste("StationID: ", StationID),
                                      paste("Collection Date: ", as.Date(`Collection Date`)),
                                      paste('Replicate: ', RepNum),
                                      paste("Collector ID: ",`Collected By`),
                                      paste("BenSampID: ", BenSampID),
                                      paste("SCI Score: ", format(`SCI Score`, digits=2)),
                                      paste("Gradient: ", Gradient)),
        name = ~paste('Rep 1', SeasonGradient)) %>%
  {if(nrow(rep2s) > 0)
    add_trace(., data = rep2s, inherit = FALSE,
              x = ~`Collection Date`, y = ~`SCI Score`, type = 'bar',
              color = ~SeasonGradient,  #marker = list(color = ~SeasonGradientColor,width = 0.5), # throws off color for some reason
              stroke = list(color = 'rgb(0, 0, 0)', width = 3),
              hoverinfo="text", text=~paste(sep="<br>",
                                            paste("StationID: ", StationID),
                                            paste("Collection Date: ", as.Date(`Collection Date`)),
                                            paste('Replicate: ', RepNum),
                                            paste("Collector ID: ",`Collected By`),
                                            paste("BenSampID: ", BenSampID),
                                            paste("SCI Score: ", format(`SCI Score`, digits=2)),
                                            paste("Gradient: ", Gradient)),
              name = ~paste('Rep 2', SeasonGradient)) 
    else .} %>%
  {if('VSCI' %in% SCI_filter$SCI)
    add_lines(., x = c(min(SCI_filter$`Collection Date`), max(SCI_filter$`Collection Date`)), y = c(60))
    else . } %>%
  layout(#showlegend=FALSE,
   #shapes = list(hline(sciLimit , 'red', text="SCI Limit")),
    yaxis=list(title="SCI"),
    xaxis=list(title="Sample Date",tickfont = list(size = 10),
               type = 'date',tickformat = "%Y"))  


