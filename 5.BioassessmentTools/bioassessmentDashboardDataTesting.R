source('global.R')

assessmentRegions <- st_read( 'data/GIS/AssessmentRegions_simple.shp')
ecoregion <- st_read('data/GIS/vaECOREGIONlevel3__proj84.shp') 

collectorFilter <- sort(unique(benSamps$`Collected By`))[6] #NULL
basinFilter <- 'New'#NULL#"Potomac-Lower" #NULL # sort(unique(benSamps$Basin_Code))[3]
stationFilter <- '9-ADR000.13'# NULL#c('1AWOT000.92', '1ACAA001.18') #sort(unique(benSamps$StationID))
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
# CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE) %>%
#   setView(-78, 37.5, zoom=6) %>%
#   addCircleMarkers(data=benSampsFilterStations,color='yellow', fillColor='blue', radius = 5,
#                    fillOpacity = 0.5,opacity=1,weight = 2,stroke=T,group="sites",
#                    label = ~StationID,
#                    popup=~paste(sep='<br>',
#                                 paste(strong('StationID : '), StationID),
#                                 paste(strong('Total Station Visits (Not Sample Reps) :'), `Total Station Visits (Not Sample Reps)`),
#                                 paste(strong('Ecoregion : '), US_L3CODE),
#                                 paste(strong('Ecoregion Name : '), US_L3NAME), 
#                                 paste(strong('Basin : '), Basin_Code),
#                                 paste(strong('HUC12 Name : '), HU_12_NAME),
#                                 paste(strong('HUC12 : '), HUC_12),
#                                 paste(strong('DEQ Region : '), ASSESS_REG),
#                                 paste(strong('VAHU6 : '), VAHU6))) %>%
#   inlmisc::AddHomeButton(raster::extent(-83.89, -74.80, 36.54, 39.98), position = "topleft") %>%
#   inlmisc::AddSearchButton(group = "sites", zoom = 15,propertyName = "label",
#                            textPlaceholder = "Search stations") %>%
#   addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
#                    overlayGroups = c('sites'),
#                    options=layersControlOptions(collapsed=T),
#                    position='topleft')

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
  left_join(dplyr::select(benSampsFilter, StationID, Sta_Desc, BenSampID), by = c('StationID', 'BenSampID')) %>%
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

glimpse(SCI_filter)



plot_ly(#SCI_filter,
  #filter(SCI_filter, StationID == '1ACAA001.18'), #%>%
  SCI_filter,
        x = ~`Collection Date`, y = ~`SCI Score`, type = 'bar', 
        color = ~SeasonGradient,  #marker = list(color = ~SeasonGradientColor,width = 0.5), # throws off color for some reason
        stroke = list(color = 'rgb(0, 0, 0)', width = 1),
        hoverinfo="text", text=~paste(sep="<br>",
                                      paste("StationID: ", StationID),
                                      paste("Collection Date: ", as.Date(`Collection Date`)),
                                      paste('Replicate: ', RepNum),
                                      paste("Collector ID: ",`Collected By`),
                                      paste("BenSampID: ", BenSampID),
                                      paste("SCI Score: ", format(`SCI Score`, digits=2)),
                                      paste("Gradient: ", Gradient)),
        name = ~paste('Rep ',RepNum, SeasonGradient)) %>%
 {if('VSCI' %in% SCI_filter$SCI)
    add_segments(., x = as.Date('2015-01-01'), xend =as.Date('2020-12-31'), y = 60, yend = 60, 
                 text = 'VSCI Criteria = 60', name = 'VSCI Criteria = 60',line = list(color = 'red'))
    else . } %>%
  {if(any(c('VCPMI + 63', 'VCPMI - 65') %in% SCI_filter$SCI))
    add_segments(., x = as.Date('2015-01-01'), xend =as.Date('2020-12-31'), y = 40, yend = 40, 
                 text = 'VCPMI Criteria = 40', name = 'VCPMI Criteria = 40',  line = list(color = 'red'))
    else . }%>%
  layout(
    yaxis=list(title="SCI"),
    xaxis=list(title="Sample Date",tickfont = list(size = 10),
               type = 'date',tickformat = "%B %Y"))  


# SCI averages a bunch of ways
View(averageSCI_windows(benSampsFilter, SCI_filter, assessmentCycle))

## Habitat Data
habSamps_Filter <- filter(habSamps, StationID %in% benSampsFilterStations$StationID) 
habObs_Filter <- filter(habObs, HabSampID %in% habSamps_Filter$HabSampID)
habValues_Filter <- filter(habValues, HabSampID %in% habSamps_Filter$HabSampID)


totalHab <- habSamps_Filter %>%
  group_by(HabSampID) %>%
  # get total habitat values
  left_join(totalHabScore(habValues_Filter), by = 'HabSampID') %>%
  mutate(Season = factor(Season,levels=c("Spring","Outside Sample Window","Fall"))) %>%
  dplyr::select(StationID, HabSampID, everything()) %>%
  arrange(`Collection Date`) %>%
  ungroup()

avgTotalHab <- averageTotHab_windows(totalHab)

habitatCrosstab <- bind_rows(habitatTemplate,
                             left_join(habValues_Filter, 
                                       dplyr::select(habSamps_Filter, HabSampID, StationID, `Collection Date`),
                                       by = 'HabSampID') %>%
                               group_by(StationID, HabSampID, `Collection Date`) %>%
                               arrange(HabParameterDescription) %>% ungroup() %>%
                               pivot_wider(id_cols = c('StationID','HabSampID','Collection Date'), names_from = HabParameterDescription, values_from = HabValue) %>%
                               left_join(dplyr::select(totalHab, HabSampID, `Total Habitat Score`), by = 'HabSampID') %>%
                               dplyr::select(StationID, HabSampID, `Collection Date`, `Total Habitat Score`, everything()) ) %>%
  drop_na(StationID) %>%
  arrange(StationID, `Collection Date`) 





plot_ly(totalHab, #%>% mutate( hab1 = 100, hab2 = 130, hab3 = 150, hab4= 200),
        x = ~`Collection Date`, y = ~`Total Habitat Score` , type = 'bar', 
        color = ~Season,  #marker = list(color = ~SeasonGradientColor,width = 0.5), # throws off color for some reason
        stroke = list(color = 'rgb(0, 0, 0)', width = 3),
        hoverinfo="text", text=~paste(sep="<br>",
                                      paste("StationID: ", StationID),
                                      paste("Collection Date: ", as.Date(`Collection Date`)),
                                      #paste('Replicate: ', RepNum),
                                      #paste("Collector ID: ",`Collected By`),
                                      #paste("BenSampID: ", BenSampID),
                                      #paste("SCI Score: ", format(`SCI Score`, digits=2)),
                                      paste("Gradient: ", Gradient)),
        name = ~paste0(Season, " (", Gradient, " Method)")) %>%
  add_segments(x = as.Date('2015-01-01'), xend =as.Date('2020-12-31'),  y = 200, yend = 200, 
               text = 'No Probability of Stress to Aquatic Life', 
               name = 'No Probability of Stress to Aquatic Life',line = list(color = '#0072B2')) %>%
  add_segments(x = as.Date('2015-01-01'), xend =as.Date('2020-12-31'),  y = 150, yend = 150, 
               text = 'Low Probability of Stress to Aquatic Life', 
               name = 'Low Probability of Stress to Aquatic Life',line = list(color = '#009E73')) %>%
  add_segments(x = as.Date('2015-01-01'), xend =as.Date('2020-12-31'),  y = 130, yend = 130, 
               text = 'Medium Probability of Stress to Aquatic Life', 
               name = 'Medium Probability of Stress to Aquatic Life',line = list(color = '#F0E442')) %>%
  add_segments(x = as.Date('2015-01-01'), xend =as.Date('2020-12-31'),  y = 100, yend = 100, 
               text = 'High Probability of Stress to Aquatic Life', 
               name = 'High Probability of Stress to Aquatic Life',line = list(color = 'red')) %>%
  layout(showlegend=TRUE,
         yaxis=list(title="Total Habitat Score"),
         xaxis=list(title="Sample Date",tickfont = list(size = 10),
                    type = 'date',tickformat = "%B %Y"))  


# Set how many colors you will use and call them out by hex name
brks <- 1:19
clrs <- c("#8B0000", "#9D0000", "#AF0000", "#C10000", "#D40000", "#E60000", "#F80000", "#FF1415", "#FF3235", "#FF5055", "#FF6F75",
          "#FF8D95", "#FFABB5", "#FFC3CD", "#FFCDD5", "#FFD7DE", "#FFE1E6", "#FFEBEE", "#FFF5F6", "#FFFFFF")

datatable(habitatCrosstab, escape = F, rownames = F, extensions = 'Buttons',
          options = list(dom='Bift', scrollX= TRUE, scrollY = '300px',
                         pageLength = nrow(habitatCrosstab), buttons=list('copy','colvis'))) %>%
  formatStyle(c("Bank Stability", "Channel Alteration", "Channel Flow Status", "Channel Sinuosity", "Embeddedness",
                "Epifaunal Substrate / Available Cover", "Frequency of riffles (or bends)", "Pool Substrate Characterization", 
                "Pool Variability", "Riparian Vegetative Zone Width", "Sediment Deposition", "Vegetative Protection",
                "Velocity / Depth Regime"),
              backgroundColor = styleInterval(brks, clrs), 
              textAlign = 'center', `font-family` = 'Arial') %>%
  formatStyle(c("Bank Stability", "Channel Alteration", "Channel Flow Status", "Channel Sinuosity", "Embeddedness",
                "Epifaunal Substrate / Available Cover", "Frequency of riffles (or bends)", "Pool Substrate Characterization", 
                "Pool Variability", "Riparian Vegetative Zone Width", "Sediment Deposition", "Vegetative Protection",
                "Velocity / Depth Regime"),
              fontWeight = styleInterval(10, c('bold','normal')), 
              textAlign = 'center', `font-family` = 'Arial') %>%
  formatStyle('Total Habitat Score', backgroundColor = "lightgray")



cat(names(habitatCrosstab), sep = ', ')





###



































box1 <- tibble(`Collection Date` = c(as.Date('2015-01-01'), as.Date('2015-01-01'), as.Date('2020-12-31'),as.Date('2020-12-31')), y = c(0, 100, 100, 0))

#totalHab %>% ungroup() %>%
  plot_ly(totalHab) %>% #,x = ~`Collection Date`, y = ~`Total Habitat Score`, type = 'bar') %>% #,
           #color = ~Season, width = 0.5, stroke = list(color = 'rgb(0, 0, 0)', width = 3)) %>% #,
           #marker = list(line = list(width = 1.5)),
           #hoverinfo="text", text=~paste(sep="<br>",
                                         #paste("StationID: ", StationID),
                                         #paste("Collection Date: ", as.Date(`Collection Date`)),
                                         ##paste('Replicate: ', RepNum),
                                         #paste("Field Team: ",`Field Team`),
                                         #paste("HabSampID: ", HabSampID),
                                         #paste("Total Habitat Score: ", `Total Habitat Score`))) %>%
  
  
  #plot_ly(data = box1)%>%
  add_polygons(data = box1, x =  ~box1$`Collection Date`, y = ~box1$y, fillcolor = "firebrick",opacity=0.6, line = list(width = 0),
               hoverinfo="text", name =paste('High Probability of Stress to Aquatic Life')) %>%
  add_trace(data = totalHab, x = ~`Collection Date`, y = ~`Total Habitat Score`, type = 'bar', 
             color = ~Season, width = 0.5, stroke = list(color = 'rgb(0, 0, 0)', width = 3),
             #marker = list(line = list(width = 1.5)),
             hoverinfo="text", text=~paste(sep="<br>",
                                           paste("StationID: ", StationID),
                                           paste("Collection Date: ", as.Date(`Collection Date`)),
                                           #paste('Replicate: ', RepNum),
                                           paste("Field Team: ",`Field Team`),
                                           paste("HabSampID: ", HabSampID),
                                           paste("Total Habitat Score: ", `Total Habitat Score`))) %>%
  layout(#showlegend=FALSE,
    #shapes = list(hline(sciLimit , 'red', text="SCI Limit")),
    yaxis=list(title="Total Habitat Score"),
    xaxis=list(title="Sample Date",tickfont = list(size = 10),
               type = 'date',tickformat = "%Y")) 










totalHab <- totalHab%>%
  mutate(hab0 = 0, hab1 = 100, hab2 = 130, hab3 = 150, hab4= 200)



box1 <- tibble(`Collection Date` = c(min(totalHab$`Collection Date`) - months(1), 
                                min(totalHab$`Collection Date`) - months(1), 
                                max(totalHab$`Collection Date`) + months(1),
                                max(totalHab$`Collection Date`) + months(1)), 
                   y = c(0, 100, 100, 0))
box2 <- tibble(`Collection Date` = c(min(totalHab$`Collection Date`) - months(1), 
                                min(totalHab$`Collection Date`) - months(1), 
                                max(totalHab$`Collection Date`) + months(1),
                                max(totalHab$`Collection Date`) + months(1)), 
                   y = c(100, 130, 130, 100))
box3 <- tibble(`Collection Date` = c(min(totalHab$`Collection Date`) - months(1), 
                                min(totalHab$`Collection Date`) - months(1), 
                                max(totalHab$`Collection Date`) + months(1),
                                max(totalHab$`Collection Date`) + months(1)), 
                   y = c(130, 150, 150, 130))
box4 <- tibble(`Collection Date` = c(min(totalHab$`Collection Date`) - months(1), 
                                min(totalHab$`Collection Date`) - months(1), 
                                max(totalHab$`Collection Date`) + months(1),
                                max(totalHab$`Collection Date`) + months(1)), 
                   y = c(150, 200, 200, 150)) 

plot_ly(totalHab ) %>%
  # add_polygons( data = box1, x = ~`Collection Date`, y = ~y, fillcolor = "firebrick",opacity=0.6, line = list(width = 0),
  #              hoverinfo="text", name =paste('High Probability of Stress to Aquatic Life')) %>%
  # add_polygons(data = box2, x = ~`Collection Date`, y = ~y, fillcolor = "#F0E442",opacity=0.6, line = list(width = 0),
  #              hoverinfo="text", name =paste('Medium Probability of Stress to Aquatic Life')) %>%
  # add_polygons(data = box3, x = ~`Collection Date`, y = ~y, fillcolor = "#009E73",opacity=0.6, line = list(width = 0),
  #              hoverinfo="text", name =paste('Low Probability of Stress to Aquatic Life')) %>%
  # add_polygons(data = box4, x = ~`Collection Date`, y = ~y, fillcolor = "#0072B2",opacity=0.6, line = list(width = 0),
  #              hoverinfo="text", name =paste('No Probability of Stress to Aquatic Life')) %>%
  add_lines(x = ~`Collection Date`, y = ~hab1, type = 'scatter', mode = 'lines',
            line = list(color = 'rgb(205, 12, 24)', width = 500),
            #line = list(color = "firebrick", width = 5),
            hoverinfo="text", name ='High Probability of Stress to Aquatic Life',
            text='High Probability of Stress to Aquatic Life') %>%
  add_lines(data = totalHab, x = ~`Collection Date`, y = ~hab2, line = list(color = "#F0E442"),
            hoverinfo="text", name ='Medium Probability of Stress to Aquatic Life',
            text = 'Medium Probability of Stress to Aquatic Life') %>%
  add_lines(data = totalHab, x = ~`Collection Date`, y = ~hab3, line = list(color = "#009E73"),
            hoverinfo="text", name ='Low Probability of Stress to Aquatic Life',
            text = 'Low Probability of Stress to Aquatic Life' ) %>%
  add_lines(data = totalHab, x = ~`Collection Date`, y = ~hab4, line = list(color = "#0072B2"),
            hoverinfo="text", name = 'No Probability of Stress to Aquatic Life',
            text = 'No Probability of Stress to Aquatic Life') %>%
  add_bars(data = totalHab, x = ~`Collection Date`, y = ~`Total Habitat Score`, type = 'bar', color = ~Season,
           hoverinfo="text", text=~paste(sep="<br>", width = .10,
                                         paste("StationID: ", StationID),
                                         paste("Collection Date: ", `Collection Date`),
                                         paste("Field Team: ", `Field Team`),
                                         paste("Total Habitat Score: ", `Total Habitat Score`))) %>%
  layout(showlegend=TRUE,
         yaxis=list(title="SCI"),
         xaxis=list(title="Sample Date",tickfont = list(size = 10)))


totalHab1 <- totalHab %>% ungroup() %>%dplyr::select( date1 = `Collection Date`, TotalHabitat = `Total Habitat Score`) %>%
  mutate(hab1 = 100)
totalHab2 <- totalHab %>% ungroup() %>%dplyr::select(`Collection Date`, `Total Habitat Score`)
totalHab3 <- totalHab %>% ungroup() #%>% droplevels(Season)

plot_ly(totalHab1, x = ~date1, y = ~TotalHabitat, type = 'scatter', mode = 'lines') %>%
  add_lines(x = ~date1, y = ~hab1, type = 'scatter', mode = 'lines',
            #line = list(color = 'rgb(205, 12, 24)', width = 500),
            #line = list(color = "firebrick", width = 5),
            hoverinfo="text", name ='High Probability of Stress to Aquatic Life',
            text='High Probability of Stress to Aquatic Life')


plot_ly(totalHab2, x = ~`Collection Date`, y = ~`Total Habitat Score`, type = 'scatter', mode = 'lines')
plot_ly(totalHab3, x = ~`Collection Date`, y = ~`Total Habitat Score`, type = 'bar', color = ~Season,
        hoverinfo="text", text=~paste(sep="<br>",
                                      paste("StationID: ", StationID),
                                      paste("Collection Date: ", `Collection Date`),
                                      paste("Field Team: ", `Field Team`),
                                      paste("Total Habitat Score: ", `Total Habitat Score`))) %>%
  add_lines(x = ~`Collection Date`, y = ~hab1, type = 'scatter', mode = 'lines',
           #line = list(color = 'rgb(205, 12, 24)', width = 500),
           #line = list(color = "firebrick", width = 5),
           hoverinfo="text", name ='High Probability of Stress to Aquatic Life',
           text='High Probability of Stress to Aquatic Life')

