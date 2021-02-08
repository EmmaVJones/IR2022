source('global.R')

### All conventionals sites
####conventionals_D <- st_read('GIS/conventionals_D.shp') %>%
conventionals_DWQS <- readRDS('data/conventionals_D.RDS') %>%
  st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
           remove = F, # don't remove these lat/lon cols from df
           crs = 4326) %>%
  mutate(StationID= FDT_STA_ID)

assessmentRegions <- st_read( 'data/GIS/AssessmentRegions_simple.shp')
assessmentLayer <- st_read('data/GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326)) 
subbasins <- st_read('data/GIS/DEQ_VAHUSB_subbasins_EVJ.shp') %>%
  rename('SUBBASIN' = 'SUBBASIN_1')



shinyServer(function(input, output, session) {
  
########################################################################################################################################################  
  
  ## Assessment Unit side of application 
  
  # color palette for assessment polygons
  pal <- colorFactor(
    palette = topo.colors(7),
    domain = assessmentRegions$ASSESS_REG)
  
  
  # empty reactive objects list
  reactive_objects = reactiveValues() # for AU part of app

  
  ## Watershed Selection Tab Assessment Unit
  
  # Update map Subbasin based on user selection
  output$AUDEQregionSelection_ <- renderUI({
    req(input$assessmentType)
    op <- filter(subbasinOptionsByWQStype, waterbodyType %in% input$assessmentType) %>%
      distinct(AssessmentRegion) %>% 
      pull()
    selectInput("AUDEQregionSelection", "Select DEQ Assessment Region", multiple = FALSE,
                choices= sort(op))  })
  
  output$AUsubbasinSelection_ <- renderUI({
    req(input$assessmentType, input$AUDEQregionSelection)
    op <- filter(subbasinOptionsByWQStype, waterbodyType %in% input$assessmentType) %>%
      filter(AssessmentRegion %in% input$AUDEQregionSelection) %>%
      {if(input$AUDEQregionSelection == 'TRO') # no AU polygons in this combo even though in WQS
        filter(., Basin_Code != 'Chowan-Dismal')
        else .} %>%
      distinct(Basin_Code) %>% 
      pull() 
    selectInput("AUsubbasinSelection", "Select Subbasin", multiple = FALSE,
                choices= sort(op))  })
  
  basinCodesAU <- reactive({
    req(input$assessmentType,input$AUDEQregionSelection, input$AUsubbasinSelection)
    filter(subbasinOptionsByWQStype, waterbodyType %in% input$assessmentType) %>%
      filter(AssessmentRegion %in% input$AUDEQregionSelection) %>%
      filter(Basin_Code %in% input$AUsubbasinSelection) %>%
      distinct(SubbasinOptions) %>% 
      pull() })
  
  
    AUs <- eventReactive(input$begin, {
      req(input$AUsubbasinSelection, input$assessmentType)
      typeName <- filter(WQSlayerConversion, waterbodyType %in% input$assessmentType) %>%
        distinct(WQS_ID) %>% 
        pull()    
      withProgress(message = 'Reading in Large Spatial File',
                   if(length(basinCodesAU()) >1){ # in case more than 1 basin code in basin
                     paste0('data/GIS/processedAUs_2020draft/AU_', typeName[1],'_',basinCodesAU(),'.shp' ) %>%                       # change to final
                       map(st_read) %>%
                       reduce(rbind) %>%
                       st_transform(4326) %>%
                       st_zm()
                   } else {
                     st_zm(
                       st_read(paste0('data/GIS/processedAUs_2020draft/AU_', typeName[1],'_',basinCodesAU(),'.shp' ))) %>%           # change to final
                       st_transform(4326)   } )       })
  
 
  
  ## Map output of basin and assessmentType_sf
  output$VAmap <- renderLeaflet({
    req(input$begin)
    
    subbasins <- filter(subbasins, BASIN_CODE %in% as.character(basinCodesAU())) %>%
      filter(ASSESS_REG %in% input$AUDEQregionSelection)
    
    m <- mapview( subbasins, label= subbasins$SUBBASIN, layer.name = 'Selected Subbasin',
                  popup= leafpop::popupTable(subbasins, zcol=c('BASIN_NAME', 'BASIN_CODE', 'SUBBASIN', 'ASSESS_REG', 'VAHU6_NOTE')))
    m@map %>% setView(sum(st_bbox(subbasins)$xmax, st_bbox(subbasins)$xmin)/2,
                      sum(st_bbox(subbasins)$ymax, st_bbox(subbasins)$ymin)/2,
                      zoom = 7)    })
  
  
  observeEvent(input$begin, {
    # Weblink component based on input$assessmentType
    if(input$assessmentType == 'Riverine'){reactive_objects$otherLayers <- ";2020%20Draft%20ADB%20WQA%20Layers;2020%20Rivers%20(Any%20Use)"}
    if(input$assessmentType == 'Lacustrine'){reactive_objects$otherLayers <- ";2020%20Draft%20ADB%20WQA%20Layers;2020%20Reservoirs%20(Any%20Use)"}
    if(input$assessmentType == 'Estuarine'){reactive_objects$otherLayers <- ";2020%20Draft%20ADB%20WQA%20Layers;2020%20Estuaries%20(Any%20Use)"}
    
    
    # Data already analyzed by some user
    reactive_objects$userReviews <-  loadData("AUlookupTable")
    
    # All sites that were analyzed in preprocessing
    reactive_objects$original_input <- read.csv('data/preAnalyzedAUdata.csv') %>% # read_csv was not working with parsing errors
      rename('Buffer Distance' = 'Buffer.Distance') %>%
      mutate(`Spatially Snapped` = case_when(is.na(`Buffer Distance`) ~ F,
                                             TRUE ~ TRUE),
             Comments = NA) %>% # mark what needs to be reviewed and add comment field
      mutate(`Buffer Distance` = ifelse(`Buffer Distance` == 'In polygon',NA, as.character(`Buffer Distance`)), # and change polygons back to NA to not mess up color pal
             ToHUC = as.numeric(as.character(ToHUC))) %>% # make sure factors don't get wonky
      filter(! FDT_STA_ID %in% reactive_objects$userReviews$FDT_STA_ID) %>% # drop stations users have reviewed
      #rbind(reactive_objects$userReviews)
      bind_rows(reactive_objects$userReviews) %>%
      filter(FDT_STA_ID != 'FakeStation') #drop fake line of data that forces userReviews into proper data format
    
    
    #reactive_objects$original_input <-  loadData("AUlookupTable") %>% #read.csv('data/preAnalyzedAUdata.csv') %>% # read_csv was not working with parsing errors
    #  rename('Buffer Distance' = 'Buffer.Distance',
    #         'Spatially Snapped' = "Spatially.Snapped") %>%
    #  mutate(`Spatially Snapped` = case_when(is.na(`Buffer Distance`) ~ F,
    #                                         TRUE ~ TRUE)) %>% # mark what needs to be reviewed
    #  mutate(`Buffer Distance` = ifelse(`Buffer Distance` == 'In polygon',NA, as.character(`Buffer Distance`))) # and change polygons back to NA to not mess up color pal
    
    # All sites limited to waterbody type and subbasin and region
    reactive_objects$snap_input_region <- reactive_objects$original_input %>%
      filter(ASSESS_REG %in% input$AUDEQregionSelection) %>% # region filter
      {if(input$AUDEQregionSelection == 'CO')
        . 
        # extra step to make sure central office weird AU naming schema isn't dropped bc not all have _
        else filter(., gsub("_", "", str_extract(ID305B_1, ".{1}_")) %in% 
                      str_extract(filter(WQSlayerConversion, waterbodyType %in% input$assessmentType) %>%
                                    distinct(WQS_ID) %>%
                                    {if(input$assessmentType == 'Estuarine')
                                      filter(., WQS_ID == 'EP') 
                                      else .} %>%
                                    pull(), ".{1}" ) ) }  %>% # complicated assessment type filter
      
      # filter(gsub("_", "", str_extract(ID305B_1, ".{1}_")) %in% 
      #          str_extract(filter(WQSlayerConversion, waterbodyType %in% input$assessmentType) %>%
      #                        distinct(WQS_ID) %>%
      #                        {if(input$assessmentType == 'Estuarine')
      #                          filter(., WQS_ID == 'EP') 
      #                          else .} %>%
      #                        pull(), ".{1}" ) )  %>% # complicated assessment type filter
      # add back in sites that did not snap to anything bc they are lost on the previous filter based on ID305B_1 field
      bind_rows(
        reactive_objects$original_input %>%
          filter(ASSESS_REG %in% input$AUDEQregionSelection) %>%
          filter(is.na(ID305B_1))
      ) %>%
      # first make sure everyone has lat/lng info
      left_join(dplyr::select(conventionals_DWQS, FDT_STA_ID, Latitude, Longitude), by = 'FDT_STA_ID' ) %>% # just using this for spatial data since some missing from original_input
      st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
               remove = T, # remove these lat/lon cols from df
               crs = 4326) 
    
    #subbasin intersection (string searching FDT_STA_ID is too unreliable here bc mix of 2A and 2-)
    reactive_objects$snap_input_region1 <-  st_intersection(reactive_objects$snap_input_region, dplyr::select(subbasins, subbasin)) %>%
      st_drop_geometry() %>% # back to tibble
      rename('Buffer Distance' = 'Buffer.Distance', 'Spatially Snapped' = 'Spatially.Snapped')
    
    # add back in stations that were dropped by joining to subbasins because they fall outside polygon boundary
    if(nrow(reactive_objects$snap_input_region) != nrow(reactive_objects$snap_input_region1)){
      reactive_objects$snap_input_region <- bind_rows(reactive_objects$snap_input_region1,
                                                      filter(reactive_objects$snap_input_region %>% st_drop_geometry(), !FDT_STA_ID %in% reactive_objects$snap_input_region1$FDT_STA_ID) %>%
                                                        # if outside the spatial framework then just take the basin information from CEDS data
                                                        left_join(dplyr::select(subbasins, BASIN_CODE, subbasin, ASSESS_REG) %>% st_drop_geometry(),
                                                                  by = c('VAHUSB' = 'subbasin', 'ASSESS_REG')) %>%
                                                        mutate(BASIN_CODE.x = BASIN_CODE.y) %>%
                                                        rename('BASIN_CODE' = 'BASIN_CODE.x') %>%
                                                        dplyr::select(-BASIN_CODE.y) ) %>%
        # after fixing all subbasin issues outside the polygon borders for region then filter to chosen subbasin
        filter(BASIN_CODE %in% basinCodesAU()) 
        
        
        # bind_rows(reactive_objects$snap_input_region1,
        #                               filter(reactive_objects$snap_input_region %>% st_drop_geometry(), !FDT_STA_ID %in% reactive_objects$snap_input_region1$FDT_STA_ID) %>%
        #                                 mutate(subbasinJoin = str_extract(FDT_STA_ID, ".{2}")) %>% # grab first two characters to identify subbasins
        #                                 mutate(subbasinJoin = case_when(subbasinJoin == '9-' ~ '9',
        #                                                                 subbasinJoin == '3-' ~ '3',
        #                                                                 subbasinJoin == '8-' ~ '8', 
        #                                                                 TRUE ~ as.character(subbasinJoin))) %>% # alter X- format to something that can join
        #                                 left_join(filter(subbasins, BASIN_CODE %in% basinCodesAU()) %>%
        #                                             st_drop_geometry() %>%
        #                                             dplyr::select(BASIN_CODE, subbasin),#dplyr::select(subbasins, BASIN_CODE, subbasin) %>% st_drop_geometry(),
        #                                           by = c('subbasinJoin' = 'BASIN_CODE')) %>%
        #                                 distinct(FDT_STA_ID, .keep_all = TRUE) %>% # run a distinct here to avoid duplicate rows, just chooses first alphabetically
        #                                 dplyr::select(-subbasinJoin)) %>%
        # # after fixing all subbasin issues outside the polygon borders for region then filter to chosen subbasin
        # filter(BASIN_CODE %in% basinCodesAU())
    } else {reactive_objects$snap_input_region <- reactive_objects$snap_input_region1 %>%
      # be sure to filter to chosen subbasin
      filter(BASIN_CODE %in% basinCodesAU()) }
    ## careful this method drops stations that fall outside polygons
    ##st_intersection(., filter(subbasins, BASIN_CODE %in% basinCodesAU()) %>%
    ##                    dplyr::select(subbasin)) %>%
    ##  st_drop_geometry() %>% # back to tibble
    ##  rename('Buffer Distance' = 'Buffer.Distance', 'Spatially Snapped' = 'Spatially.Snapped')

    
    # No longer needed???????????????????????????????????????????????????????????
#    # Fix messed up string search bc VACB ID305B doesn't follow conventions
#    if(input$assessmentType == 'Estuarine' & input$AUDEQregionSelection == 'CO'){
#      reactive_objects$snap_input_region <- rbind(reactive_objects$snap_input_region,
#                                                  filter(reactive_objects$original_input, str_extract(ID305B_1, ".{4}") == 'VACB') %>%
#                                                    left_join(dplyr::select(conventionals_DWQS, FDT_STA_ID, Latitude, Longitude), by = 'FDT_STA_ID' ) %>% # just using this for spatial data since some missing from original_input
#                                                    mutate(subbasin = 'CB') %>% 
#                                                    dplyr::select(names(reactive_objects$snap_input_region))    )}
    
    # limit conventionals_DWQS to just chosen subbasin
    reactive_objects$conventionals_DAU_Region <- st_intersection(conventionals_DWQS,
                                                                 filter(subbasins, BASIN_CODE %in% basinCodesAU())) %>%
      mutate(`DEQ GIS Web App Link` =  paste0(webLinkpart1, StationID, webLinkpart2, reactive_objects$otherLayers, webLinkpart3)) %>%
      dplyr::select(`DEQ GIS Web App Link`, everything())
    
    # Make dataset of all sites for highlighting purposes, preliminary list
    reactive_objects$sitesUnique <- reactive_objects$snap_input_region %>%
      full_join(dplyr::select(conventionals_DWQS, FDT_STA_ID, Latitude, Longitude), by = 'FDT_STA_ID') %>%
      left_join(dplyr::select(reactive_objects$conventionals_DAU_Region, FDT_STA_ID, `DEQ GIS Web App Link` ) %>% 
                  st_drop_geometry(), by = 'FDT_STA_ID') %>%
      filter(!is.na(Latitude) | !is.na(Longitude)) %>%
      st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
               remove = F, # don't remove these lat/lon cols from df
               crs = 4326)
    
    # Make dataset of multiple segments snapped to single site IN REGION
    reactive_objects$tooMany <- filter(reactive_objects$snap_input_region, n > 1) %>%
      filter(`Spatially Snapped` == T) %>% # only want ones user needs to deal with
      group_by(FDT_STA_ID) %>% mutate(colorFac = row_number()) %>% ungroup() 
    # Make a dataset of actual segments for plotting
    reactive_objects$tooMany_sf <- filter(AUs(), ID305B %in% reactive_objects$tooMany$ID305B_1) %>%
      left_join(reactive_objects$tooMany, by = c('ID305B' = 'ID305B_1')) %>%
      dplyr::select(FDT_STA_ID, ID305B, `Buffer Distance`, n, everything())
    # Make dataset of sites associated with too many segments IN REGION
    reactive_objects$tooMany_sites <- filter(reactive_objects$sitesUnique, FDT_STA_ID %in% reactive_objects$tooMany$FDT_STA_ID) %>%
      distinct(FDT_STA_ID, .keep_all = T)# %>%
    #dplyr::select(-c(WQS_ID, `Buffer Distance`, n))
 
    # Make dataset of sites that snapped to a single AU and IN REGION
    reactive_objects$snapSingle1 <- filter(reactive_objects$sitesUnique, n == 1) %>%
      filter(FDT_STA_ID %in% filter(reactive_objects$snap_input_region, n == 1)$FDT_STA_ID) %>%#
      filter(`Spatially Snapped` == T) # only want ones user needs to deal with
    if(all(is.na(reactive_objects$snapSingle1$`Buffer Distance`))){ # the filter doesn't work if all NA for some reason
      reactive_objects$snapSingle <- mutate(reactive_objects$snapSingle1, `Buffer Distance` = as.factor(`Buffer Distance`))
    } else{
      if(input$AUDEQregionSelection != 'CO') # special case again for CO, CO only appears in Estuarine so doesn't change other things
        reactive_objects$snapSingle <- filter(reactive_objects$snapSingle1, `Buffer Distance` != 'No connections within 80 m') %>%
          mutate(`Buffer Distance` = as.factor(`Buffer Distance`))
      else{reactive_objects$snapSingle <-  mutate(reactive_objects$snapSingle1, `Buffer Distance` = as.factor(`Buffer Distance`))} }
    
    
#      {if(input$AUDEQregionSelection != 'CO') # special case again for CO, CO only appears in Estuarine so doesn't change other things
#        filter(., `Buffer Distance` != 'No connections within 80 m') %>%
#          mutate(`Buffer Distance` = as.factor(`Buffer Distance`))
#        else  mutate(., `Buffer Distance` = as.factor(`Buffer Distance`))}
     
     # filter(`Buffer Distance` != 'No connections within 80 m') %>%
      # mutate(`Buffer Distance` = as.factor(`Buffer Distance`))
    
    
    # Make a dataset of actual segments that snapped to a single site for plotting
    reactive_objects$snapSingle_sf <- filter(AUs(), ID305B %in% reactive_objects$snapSingle$ID305B_1) %>%
      left_join(dplyr::select( reactive_objects$snapSingle, FDT_STA_ID, `Buffer Distance`, n, ID305B_1) %>% st_drop_geometry(), 
                by = c('ID305B' = 'ID305B_1')) %>%
      dplyr::select(FDT_STA_ID, ID305B, `Buffer Distance`, n, everything())
    
    
    # Make dataset of sites associated with no segments IN REGION
    reactive_objects$snapNone <-  filter(reactive_objects$sitesUnique, is.na(ID305B_1)) %>%
      filter(FDT_STA_ID %in% filter(reactive_objects$snap_input_region, is.na(ID305B_1) | `Buffer Distance` != 'No connections within 80 m')$FDT_STA_ID) %>%
      filter(`Spatially Snapped` == T) %>% # only want ones user needs to deal with
      filter(FDT_STA_ID %in% reactive_objects$snap_input_region$FDT_STA_ID) # limit assignment to just what falls in a region
    # Make empty dataset of sites that assessors touched
    reactive_objects$sitesAdjusted <-  reactive_objects$sitesUnique[0,]  %>%
      mutate(Comments = as.character())
    # Make dataset for user to download
    #reactive_objects$finalAU <- reactive_objects$original_input
    reactive_objects$finalAU <- reactive_objects$userReviews %>%
      rbind(  reactive_objects$sitesAdjusted %>% 
                st_drop_geometry() %>%
                dplyr::select(-c(subbasin, Latitude,Longitude,`DEQ GIS Web App Link`)) )  # drop extra data
  })
  
  
  
  # UI summaries of data pulled in to app, first and second tab
  output$singleSnapSummary1 <- renderPrint({ req(reactive_objects$snap_input_region)
    cat(paste0('There are ', nrow(reactive_objects$snapSingle), ' stations that snapped to 1 AU segment in preprocessing.'))})
  output$singleSnapSummary2 <- renderPrint({ req(reactive_objects$snap_input_region)
    cat(paste0('There are ', nrow(reactive_objects$snapSingle), ' stations that snapped to 1 AU segment in preprocessing.'))})
  output$snapTooManySummary1 <- renderPrint({ req(reactive_objects$snap_input_region)
    cat(paste0('There are ', nrow(reactive_objects$tooMany_sites), ' stations that snapped to > 1 AU segment in preprocessing.'))})
  output$snapTooManySummary2 <- renderPrint({ req(reactive_objects$snap_input_region)
    cat(paste0('There are ', nrow(reactive_objects$tooMany_sites), ' stations that snapped to > 1 AU segment in preprocessing.'))})
  output$noSnapSummary1 <- renderPrint({ req(reactive_objects$snap_input_region)
    cat(paste0('There are ', nrow(reactive_objects$snapNone), ' stations that snapped to 0 AU segments in preprocessing.'))})
  output$noSnapSummary2 <- renderPrint({ req(reactive_objects$snap_input_region)
    cat(paste0('There are ', nrow(reactive_objects$snapNone), ' stations that snapped to 0 AU segments in preprocessing.'))})
  
  
  
  ### AU REVIEW TAB ##################################################################################
  
  # AU Map
  output$AUmap <- renderLeaflet({
    req(reactive_objects$snap_input_region)
    CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE, 
                 options= leafletOptions(zoomControl = TRUE,minZoom = 3, maxZoom = 20,
                                         preferCanvas = TRUE)) %>%
      setView(-78, 37.5, zoom=7)  %>% 
      addCircleMarkers(data = reactive_objects$conventionals_DAU_Region, color='blue', fillColor='yellow', radius = 4,
                       fillOpacity = 0.5,opacity=0.8,weight = 1,stroke=T, group="Conventionals Stations in Basin",
                       label = ~FDT_STA_ID, layerId = ~FDT_STA_ID) %>%
      addPolygons(data= assessmentRegions,  color = 'black', weight = 1,
                  fillColor= ~pal(assessmentRegions$ASSESS_REG), fillOpacity = 0.5,stroke=0.1,
                  group="Assessment Regions",
                  popup=leafpop::popupTable(assessmentRegions, zcol=c('ASSESS_REG'))) %>% hideGroup('Assessment Regions') %>% #,'VAHU6','FedName'))) %>% hideGroup('Assessment Regions') %>%
      inlmisc::AddHomeButton(raster::extent(-83.89, -74.80, 36.54, 39.98), position = "topleft") %>%
      inlmisc::AddSearchButton(group = "Conventionals Stations in Basin", zoom = 15,propertyName = "label",
                               textPlaceholder = "Search Conventionals Stations in Basin") %>%
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c('Conventionals Stations in Basin',
                                         #"All WQS in selected Region/Basin",
                                         'Assessment Regions'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')  %>%
      hideGroup("Conventionals Stations in Basin")    
  })
  
  AUmap_proxy <- leafletProxy("AUmap")
  
  # Add layers to map as requested- Single snapped sites
  observeEvent(input$plotSingleSnapSummary, {
    if (nrow(reactive_objects$snapSingle) > 0 ){
      AUmap_proxy %>%
        addCircleMarkers(data=reactive_objects$snapSingle,
                         layerId = ~paste0(FDT_STA_ID,'_snapSingle'), # need unique layerID 
                         label=~FDT_STA_ID, group="Stations Snapped to 1 AU Segment", 
                         radius = 5, fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T, color = 'black',
                         fillColor= ~palBufferDistance(reactive_objects$snapSingle$`Buffer Distance`)) %>%
        {if(nrow(reactive_objects$snapSingle_sf) > 0 & "sfc_MULTIPOLYGON" %in% class(st_geometry(reactive_objects$snapSingle_sf))) 
          addPolygons(., data=reactive_objects$snapSingle_sf,
                      layerId = ~paste0(ID305B,'_snapSingle'),  # need unique layerID 
                      label=~ID305B, group="AU Segments of Stations Snapped to 1 Segment", 
                      color = 'blue', weight = 3,stroke=T,
                      popup=leafpop::popupTable(reactive_objects$snapSingle_sf),
                      popupOptions = popupOptions( maxHeight = 100 )) %>% 
            hideGroup("AU Segments of Stations Snapped to 1 Segment")
          else . } %>%
        {if(nrow(reactive_objects$snapSingle_sf) > 0 & "sfc_MULTILINESTRING" %in% class(st_geometry(reactive_objects$snapSingle_sf)))
          addPolylines(., data=reactive_objects$snapSingle_sf,
                       layerId = ~paste0(ID305B,'_snapSingle'),  # need unique layerID 
                       label=~ID305B, group="AU Segments of Stations Snapped to 1 Segment", 
                       color = 'blue', weight = 3,stroke=T,
                       popup=leafpop::popupTable(reactive_objects$snapSingle_sf),
                       popupOptions = popupOptions( maxHeight = 100 )) %>%
            hideGroup("AU Segments of Stations Snapped to 1 Segment")
          else . } %>%
        addLegend(position = 'topright', pal = palBufferDistance, values = reactive_objects$snapSingle$`Buffer Distance`, 
                  group = 'Stations Snapped to 1 AU Segment') %>%
        addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                         overlayGroups = c("Adjusted Sites",
                                           "Stations Snapped to 1 AU Segment",
                                           "AU Segments of Stations Snapped to 1 Segment",
                                           "Stations Snapped to > 1 AU Segment",
                                           "AU Segments of Stations Snapped to > 1 Segment",
                                           "Stations Snapped to 0 AU Segments",
                                           'Conventionals Stations in Basin',
                                           #"All AU in selected Region/Basin",
                                           'Assessment Regions'),
                         options=layersControlOptions(collapsed=T),
                         position='topleft')
    } else {
      showNotification("There are no sites that snapped to only 1 AU segment in preprocessing steps. Nothing to plot.")
    }
  })
  
  
  # Add layers to map as requested- Too many snapped sites
  observeEvent(input$plotSnapTooManySummary, {
    
    if(nrow(reactive_objects$tooMany_sites) > 0){
      palTooMany <- colorNumeric(c('green','yellow', 'blue','red', 'pink','purple'), domain = reactive_objects$tooMany$colorFac)
      
      AUmap_proxy %>%
        addCircleMarkers(data=reactive_objects$tooMany_sites,
                         layerId = ~paste0(FDT_STA_ID,'_tooMany'),  # need unique layerID 
                         label=~FDT_STA_ID, group="Stations Snapped to > 1 AU Segment", 
                         color='black', fillColor='red', radius = 5,
                         fillOpacity = 0.8,opacity=0.5,weight = 2,stroke=T) %>%
        {if(nrow(reactive_objects$tooMany_sf) > 0 & "sfc_MULTIPOLYGON" %in% class(st_geometry(reactive_objects$tooMany_sf))) 
          addPolygons(., data=reactive_objects$tooMany_sf,
                      layerId = ~paste0(ID305B,'_tooMany'),  # need unique layerID 
                      label=~ID305B, group="AU Segments of Stations Snapped to > 1 Segment", 
                      color = ~palTooMany(reactive_objects$tooMany_sf$colorFac),weight = 3,stroke=T,
                      popup=leafpop::popupTable(reactive_objects$tooMany_sf),
                      popupOptions = popupOptions( maxHeight = 100 )) %>% 
            hideGroup("AU Segments of Stations Snapped to > 1 Segment")
          else . } %>%
        {if(nrow(reactive_objects$tooMany_sf) > 0 & "sfc_MULTILINESTRING" %in% class(st_geometry(reactive_objects$tooMany_sf)))
          addPolylines(., data=reactive_objects$tooMany_sf,
                       layerId = ~paste0(ID305B,'_tooMany'),  # need unique layerID 
                       label=~ID305B, group="AU Segments of Stations Snapped to > 1 Segment", 
                       color = ~palTooMany(reactive_objects$tooMany_sf$colorFac),weight = 3,stroke=T,
                       popup=leafpop::popupTable(reactive_objects$tooMany_sf),
                       popupOptions = popupOptions( maxHeight = 100 )) %>%
            hideGroup("AU Segments of Stations Snapped to > 1 Segment")
          else . } %>%
        addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                         overlayGroups = c("Adjusted Sites",
                                           "Stations Snapped to 1 AU Segment",
                                           "AU Segments of Stations Snapped to 1 Segment",
                                           "Stations Snapped to > 1 AU Segment",
                                           "AU Segments of Stations Snapped to > 1 Segment",
                                           "Stations Snapped to 0 AU Segments",
                                           'Conventionals Stations in Basin',
                                           #"All AU in selected Region/Basin",
                                           'Assessment Regions'),
                         options=layersControlOptions(collapsed=T),
                         position='topleft') 
    } else {
      showNotification("There are no sites that snapped to > 1 AU segment in preprocessing steps. Nothing to plot.")
    }   })
  
  
  # Add layers to map as requested- 0 snapped sites
  observeEvent(input$plotNoSnapSummary, {
    if (nrow(reactive_objects$snapNone) > 0 ){
      AUmap_proxy %>%
        addCircleMarkers(data=reactive_objects$snapNone,
                         layerId = ~paste0(FDT_STA_ID,'_snapNone'), # need unique layerID 
                         label=~FDT_STA_ID, 
                         group="Stations Snapped to 0 AU Segments", 
                         color='black', fillColor='orange', radius = 5,
                         fillOpacity = 1,opacity=0.5,weight = 2,stroke=T) %>%
        addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                         overlayGroups = c("Adjusted Sites",
                                           "Stations Snapped to 1 AU Segment",
                                           "AU Segments of Stations Snapped to 1 Segment",
                                           "Stations Snapped to > 1 AU Segment",
                                           "AU Segments of Stations Snapped to > 1 Segment",
                                           "Stations Snapped to 0 AU Segments",
                                           'Conventionals Stations in Basin',
                                           #"All AU in selected Region/Basin",
                                           'Assessment Regions'),
                         options=layersControlOptions(collapsed=T),
                         position='topleft')
    } else {
      showNotification("There are no sites that snapped to 0 AU segments in preprocessing steps. Nothing to plot.")
    }
  })
  
  # Map marker click (to identify selected sites)
  observeEvent(input$AUmap_marker_click, {
    site_click <- input$AUmap_marker_click # this is all the info based on your click
    siteid <- strsplit(site_click$id, "_")[[1]][1] # this is just the layerID associated with your click
    # have to remove the unique layerID after _ to make sense of StationID
    
    if(!is.null(siteid)){ # if you clicked a point with info, find all Stations that match (with a round)
      # first find site matches from user input dataset, by lat and long
      siteMatches <- filter(reactive_objects$sitesUnique, 
                            FDT_STA_ID %in% siteid) %>%
        st_drop_geometry() %>%
        pull(FDT_STA_ID)
      
      # and save all this info for later
      siteid_current <-  c(siteMatches)#, as.character(existingSiteMatches))
      
      # add the current site(s) to the selected list for highlighting and displaying in table
      if(is.null(reactive_objects$namesToSmash)){
        reactive_objects$namesToSmash <- siteid_current
      } else {
        reactive_objects$namesToSmash <- append(siteid_current, reactive_objects$namesToSmash)    }
    }
  })
  
  # Update map marker highlights
  observeEvent(reactive_objects$namesToSmash, ignoreNULL=F, {
    if(!is.null(reactive_objects$namesToSmash)){
      AUmap_proxy %>%
        clearGroup(group='highlight') %>%
        addCircleMarkers(data=filter(reactive_objects$sitesUnique, FDT_STA_ID %in% reactive_objects$namesToSmash),
                         layerId = ~paste0(FDT_STA_ID,'_sitesHighlighted'),  # need unique layerID 
                         group='highlight', 
                         radius = 20, 
                         color='chartreuse', opacity = 0.75, fillOpacity = 0.4)  
    } else {
      AUmap_proxy %>%
        clearGroup(group='highlight') }  })
  
  ## Clear all selected sites
  observeEvent(input$clear_allAU, {
    reactive_objects$namesToSmash=NULL
    AUmap_proxy %>%
      clearGroup(group='highlight')  })
  
  ## Accept Snapped AU Modal
  observeEvent(input$acceptAU, {
    showModal(modalDialog(title = 'Accept Snapped AU', size = 'l',easyClose = T,
                          DT::renderDataTable({
                            filter(reactive_objects$sitesUnique, FDT_STA_ID %in% reactive_objects$namesToSmash) %>%
                              st_drop_geometry() %>%
                              dplyr::select(FDT_STA_ID, ID305B_1, everything()) %>%
                              datatable(rownames = F,  editable = 'cell', 
                                        options = list(dom = 't', scrollX= TRUE, scrollY = '125px'))  }),
                          br(), br(),
                          textInput('acceptCommentAU', 'Additional Comments and Documentation'),
                          actionButton('accept_okAU', 'Accept', 
                                       style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', 
                                       icon=icon('check-circle')),
                          actionButton('accept_cancelAU', 'Cancel', 
                                       style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', 
                                       icon=icon('window-close'))    )) 
    jqui_draggable(selector = '.modal-content')   })
  
  # Do something with AU Accept Modal
  observeEvent(input$accept_cancelAU, {removeModal()})
  observeEvent(input$accept_okAU, {
    # Get name and ID305B_1 information from tooMany
    sitesUpdated <- filter(reactive_objects$sitesUnique, FDT_STA_ID %in% reactive_objects$namesToSmash) %>%
      dplyr::select(-c(subbasin, Latitude,Longitude,`DEQ GIS Web App Link`)) %>% # drop extra data
      mutate(`Buffer Distance` = paste0('Manual Review | ', `Buffer Distance`),
             `Spatially Snapped` = FALSE, # update this field so next import it will not be brought into app
             Comments = paste0('Manual Accept | ',input$acceptCommentAU))# %>%
    # dplyr::select(FDT_STA_ID, ID305B_1, `Buffer Distance`, Comments)
    
    
    # add the current site(s) to the adjusted list 
    #if(nrow(reactive_objects$sitesAdjusted) == 0){
    #  reactive_objects$sitesAdjusted
    #} else {
    reactive_objects$sitesAdjusted <- rbind(reactive_objects$sitesAdjusted, sitesUpdated) # rbind works better for sf objects
    #}
    
    dropMe <- unique(sitesUpdated$FDT_STA_ID)
    
    ## Remove Site from "to do' list
    # remove from snap to > 1 AU sites and segments
    reactive_objects$tooMany_sites <- filter(reactive_objects$tooMany_sites, !(FDT_STA_ID %in% dropMe)) # drop sites
    reactive_objects$tooMany_sf <- filter(reactive_objects$tooMany_sf, !(FDT_STA_ID %in% dropMe)) # drop segments
    
    
    # and if part of snap to 1 AU, fix that data
    reactive_objects$snapSingle <- filter(reactive_objects$snapSingle, !(FDT_STA_ID%in% dropMe)) # drop sites
    reactive_objects$snapSingle_sf <- filter(reactive_objects$snapSingle_sf, !(FDT_STA_ID %in% dropMe)) # drop segments
    
    # and if part of snap to 0 AU, fix that data
    reactive_objects$snapNone <- filter(reactive_objects$snapNone, !(FDT_STA_ID %in% dropMe)) # drop sites
    
    # update output dataset
    reactive_objects$finalAU <-# filter(reactive_objects$finalAU, !(FDT_STA_ID %in% dropMe)) %>% bind_rows(sitesUpdated)
      bind_rows(reactive_objects$finalAU, sitesUpdated %>% st_drop_geometry()) 
    
    
    # Empty map selection
    reactive_objects$namesToSmash <- NULL
    
    ### Clear modal
    removeModal()
  })
  
  
  ## Manual AU Adjustment Modal
  observeEvent(input$changeAU, {
    showModal(modalDialog(title = 'Manually Adjust AU', size = 'l', easyClose = T, 
                          DT::renderDataTable({
                            filter(reactive_objects$sitesUnique, FDT_STA_ID %in% reactive_objects$namesToSmash) %>%
                              st_drop_geometry() %>%
                              dplyr::select(FDT_STA_ID, ID305B_1, everything()) %>%
                              datatable(rownames = F,  editable = 'cell',
                                        options = list(dom = 't', scrollX= TRUE, scrollY = '125px'))  }),
                          br(), br(),
                          textInput('mergeAUID', "Manually input the ID305B you want connected to the selected station."),
                          helpText("Hint: Copy/Paste is your friend."),
                          textInput('adjustCommentAU', 'Additional Comments and Documentation'),
                          actionButton('adjust_okAU', 'Accept', 
                                       style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', 
                                       icon=icon('check-circle')),
                          actionButton('adjust_cancelAU', 'Cancel', 
                                       style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', 
                                       icon=icon('window-close'))    ))  
    jqui_draggable(selector = '.modal-content')  })
  
  # Do something with AU Adjustment Modal
  observeEvent(input$adjust_cancelAU, {removeModal()})
  observeEvent(input$adjust_okAU, {
    # Get name and location information from tooMany
    sitesUpdated <- filter(reactive_objects$sitesUnique, FDT_STA_ID %in% reactive_objects$namesToSmash) %>%
      dplyr::select(-c(subbasin, Latitude,Longitude,`DEQ GIS Web App Link`)) %>% # drop extra data
      distinct(FDT_STA_ID, .keep_all = T) %>% # this time you do want to run a distinct to avoid duplicated rows
      mutate(ID305B_1 = input$mergeAUID,
             `Spatially Snapped` = FALSE, # update this field so next import it will not be brought into app
             `Buffer Distance` = paste0('Manual Review | ', `Buffer Distance`),
             Comments = paste0('Manual Accept | ',input$adjustCommentAU)) #%>%
    # dplyr::select(FDT_STA_ID, ID305B_1, `Buffer Distance`, Comments)
    
    print(glimpse(sitesUpdated))
    
    # add the current site(s) to the adjusted list 
    reactive_objects$sitesAdjusted <- rbind(reactive_objects$sitesAdjusted, sitesUpdated) # rbind works better for sf objects
    
    dropMe <- unique(sitesUpdated$FDT_STA_ID)
    
    ## Remove Site from "to do' list
    # remove from snap to > 1 AU sites and segments
    reactive_objects$tooMany_sites <- filter(reactive_objects$tooMany_sites, !(FDT_STA_ID %in% dropMe)) # drop sites
    reactive_objects$tooMany_sf <- filter(reactive_objects$tooMany_sf, !(FDT_STA_ID %in% dropMe)) # drop segments
    
    # and if part of snap to 1 AU, fix that data
    reactive_objects$snapSingle <- filter(reactive_objects$snapSingle, !(FDT_STA_ID%in% dropMe)) # drop sites
    reactive_objects$snapSingle_sf <- filter(reactive_objects$snapSingle_sf, !(FDT_STA_ID %in% dropMe)) # drop segments
    
    # and if part of snap to 0 AU, fix that data
    reactive_objects$snapNone <- filter(reactive_objects$snapNone, !(FDT_STA_ID %in% dropMe)) # drop sites
    
    # update output dataset
    reactive_objects$finalAU <- # filter(reactive_objects$finalAU, !(FDT_STA_ID %in% dropMe)) %>% bind_rows(sitesUpdated)
      bind_rows(reactive_objects$finalAU, sitesUpdated %>% st_drop_geometry()) 
    
    # Empty map selection
    reactive_objects$namesToSmash <- NULL
    
    ### Clear modal
    removeModal()
  })
  
  
  
  # update AUmap after AU adjustment
  observe({
    req(reactive_objects$sitesAdjusted)
    if(nrow(reactive_objects$tooMany_sites)> 0){
      palTooMany <- colorNumeric(c('green','yellow', 'blue','red', 'pink','purple'), domain = reactive_objects$tooMany$colorFac)
    }
    
    ## Update proxy map
    if(nrow(reactive_objects$sitesAdjusted) > 0){
      AUmap_proxy %>% 
        # have to manually clear old sites to 'wipe' leaflet memory of joined sites
        clearGroup("Stations Snapped to 0 AU Segments") %>%
        clearGroup("Stations Snapped to 1 AU Segment") %>%
        clearGroup("AU Segments of Stations Snapped to 1 Segment") %>%
        clearGroup("Stations Snapped to > 1 AU Segment") %>%
        clearGroup("AU Segments of Stations Snapped to > 1 Segment") %>%
        
        
        {if(nrow(reactive_objects$tooMany_sites) > 0)
          addCircleMarkers(., data=reactive_objects$tooMany_sites,
                           layerId = ~paste0(FDT_STA_ID,'_tooMany'),  # need unique layerID 
                           label=~FDT_STA_ID, group="Stations Snapped to > 1 AU Segment", 
                           color='black', fillColor='red', radius = 5,
                           fillOpacity = 0.8,opacity=0.5,weight = 2,stroke=T) 
          else . } %>%
        {if(nrow(reactive_objects$tooMany_sf) > 0) 
        {if("sfc_POLYGON" %in% class(st_geometry(reactive_objects$tooMany_sf))) 
          addPolygons(., data=reactive_objects$tooMany_sf,
                      layerId = ~paste0(ID305B,'_tooMany'),  # need unique layerID 
                      label=~ID305B, group="AU Segments of Stations Snapped to > 1 Segment", 
                      color = ~palTooMany(reactive_objects$tooMany_sf$colorFac),weight = 3,stroke=T,
                      popup=leafpop::popupTable(reactive_objects$tooMany_sf),
                      popupOptions = popupOptions( maxHeight = 100 )) %>% 
            hideGroup("AU Segments of Stations Snapped to > 1 Segment")
          else
            addPolylines(., data=reactive_objects$tooMany_sf,
                         layerId = ~paste0(ID305B,'_tooMany'),  # need unique layerID 
                         label=~ID305B, group="AU Segments of Stations Snapped to > 1 Segment", 
                         color = ~palTooMany(reactive_objects$tooMany_sf$colorFac),weight = 3,stroke=T,
                         popup=leafpop::popupTable(reactive_objects$tooMany_sf),
                         popupOptions = popupOptions( maxHeight = 100 )) %>%
            hideGroup("AU Segments of Stations Snapped to > 1 Segment")}
          else . } %>%
        {if(nrow(reactive_objects$snapSingle) > 0)
          addCircleMarkers(., data=reactive_objects$snapSingle,
                           layerId = ~paste0(FDT_STA_ID,'_snapSingle'), # need unique layerID 
                           label=~FDT_STA_ID, group="Stations Snapped to 1 AU Segment", 
                           radius = 5, fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T, color = 'black',
                           fillColor= ~palBufferDistance(reactive_objects$snapSingle$`Buffer Distance`)) #%>%
          else .}  %>%
        {if(nrow(reactive_objects$snapSingle_sf) > 0) 
        {if("sfc_MULTIPOLYGON" %in% class(st_geometry(reactive_objects$snapSingle_sf))) 
          addPolygons(., data=reactive_objects$snapSingle_sf,
                      layerId = ~paste0(ID305B,'_tooMany'),  # need unique layerID 
                      label=~ID305B, group="AU Segments of Stations Snapped to 1 Segment", 
                      color = "blue",weight = 3,stroke=T,
                      popup=leafpop::popupTable(reactive_objects$snapSingle_sf),
                      popupOptions = popupOptions( maxHeight = 100 )) %>% 
            hideGroup("AU Segments of Stations Snapped to 1 Segment")
          else
            addPolylines(., data=reactive_objects$snapSingle_sf,
                         layerId = ~paste0(ID305B,'_tooMany'),  # need unique layerID 
                         label=~ID305B, group="AU Segments of Stations Snapped to 1 Segment", 
                         color = "blue",weight = 3,stroke=T,
                         popup=leafpop::popupTable(reactive_objects$snapSingle_sf),
                         popupOptions = popupOptions( maxHeight = 100 )) %>%
            hideGroup("AU Segments of Stations Snapped to 1 Segment")}
          else . } %>%
        {if(nrow(reactive_objects$snapNone) > 0)
          addCircleMarkers(., data=reactive_objects$snapNone,
                           layerId = ~paste0(FDT_STA_ID,'_snapNone'), # need unique layerID 
                           label=~FDT_STA_ID, 
                           group="Stations Snapped to 0 AU Segments", 
                           color='black', fillColor='orange', radius = 5,
                           fillOpacity = 1,opacity=0.5,weight = 2,stroke=T)
          else . } %>%
        addCircleMarkers(data=reactive_objects$sitesAdjusted,
                         layerId = ~paste0(FDT_STA_ID,'_sitesAdjusted'),  # need unique layerID 
                         label=~FDT_STA_ID, group="Adjusted Sites", 
                         color='black', fillColor='purple', radius = 5,
                         fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T) %>% 
        addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                         overlayGroups = c("Adjusted Sites",
                                           "Stations Snapped to 1 AU Segment",
                                           "AU Segments of Stations Snapped to 1 Segment",
                                           "Stations Snapped to > 1 AU Segment",
                                           "AU Segments of Stations Snapped to > 1 Segment",
                                           "Stations Snapped to 0 AU Segments",
                                           'Conventionals Stations in Basin',
                                           #"All AU in selected Region/Basin",
                                           'Assessment Regions'),
                         options=layersControlOptions(collapsed=T),
                         position='topleft') 
    }    })
  
  
  
  
  
  
  ### Stations Data and Spatially Joined AU Tab
  output$selectedSiteTableAU <- DT::renderDataTable({
    req(reactive_objects$namesToSmash)
    filter(reactive_objects$sitesUnique, FDT_STA_ID %in% reactive_objects$namesToSmash) %>%
      st_drop_geometry() %>%
      dplyr::select(FDT_STA_ID, ID305B_1, `DEQ GIS Web App Link`, `Buffer Distance`, n, everything()) %>%
      datatable(rownames = F, escape= F, options = list(dom = 't', scrollX= TRUE, scrollY = '100px'))  })
  
  output$associatedAUTable <- DT::renderDataTable({
    req(reactive_objects$namesToSmash)
    filter(AUs(), ID305B %in% filter(reactive_objects$snap_input_region, FDT_STA_ID %in% reactive_objects$namesToSmash)$ID305B_1) %>%
      st_drop_geometry() %>%
      dplyr::select(ID305B, everything()) %>%
      dplyr::select(-OBJECTID) %>%
      datatable(rownames = F, options = list(dom = 't', scrollX= TRUE, scrollY = '200px'))  })
  
  ### Updated Stations Data and Manually QAed AU Tab
  output$adjustedStationsTableAU <- DT::renderDataTable({
    req(reactive_objects$namesToSmash, reactive_objects$sitesAdjusted)
    filter(reactive_objects$sitesAdjusted, FDT_STA_ID %in% reactive_objects$namesToSmash) %>%
      st_drop_geometry() %>%
      dplyr::select(FDT_STA_ID, ID305B_1, Comments, everything()) %>%
      datatable(rownames = F, options = list(dom = 't', scrollX= TRUE, scrollY = '100px'))  })
  
  ## User adjusted AU table, AU details
  output$associatedAUTableAUQA <- DT::renderDataTable({
    req(reactive_objects$namesToSmash, reactive_objects$sitesAdjusted)
    filter(AUs() %>% st_drop_geometry(), ID305B %in% filter(reactive_objects$sitesAdjusted, FDT_STA_ID %in% reactive_objects$namesToSmash)$ID305B_1) %>%
      dplyr::select(ID305B, everything()) %>%
      dplyr::select(-OBJECTID) %>%
      datatable(rownames = F, options = list(dom = 't', scrollX= TRUE, scrollY = '200px'))  })
  
  
  
  observeEvent(input$saveAU, {
    saveData(reactive_objects$finalAU %>%
               # get rid of geometry if needed
               {if('geometry' %in% names(reactive_objects$finalAU))
                 dplyr::select(., -geometry) 
                 else . } %>%
               as.data.frame(), "AUlookupTable")
    showNotification("AU information saved on Connect server.")
    
  })  
  
  
  
  
  
  
  
  
  #  output$testAU <- renderPrint({
  #    req(input$begin, reactive_objects$snap_input_region)
  #    glimpse(reactive_objects$sitesUnique)
  #    glimpse(reactive_objects$sitesAdjusted)
  #    glimpse(reactive_objects$finalAU)
  #    })
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##################################################################################################################################################
  
  ## WQS Side of Application
  
  palBufferDistance <- colorFactor(
    palette = terrain.colors(5),#colorRamps::blue2red(5),
    levels = c("20 m", "40 m", "60 m", "80 m", "No connections within 80 m"))
  
  
  # empty reactive objects list
  WQSreactive_objects = reactiveValues() # for WQS
  
  ## Watershed Selection Tab WQS
  
  ################################# PRE-SPLIT WQS LAYER METHOD, FASTER FOR APP RENDERING ######################################
  
  # Update map Subbasin based on user selection
  output$WQSDEQregionSelection_ <- renderUI({
    req(input$WQSwaterbodyType)
    op <- filter(subbasinOptionsByWQStype, waterbodyType %in% input$WQSwaterbodyType) %>%
      distinct(AssessmentRegion) %>% 
      pull()
    selectInput("WQSDEQregionSelection", "Select DEQ Assessment Region", multiple = FALSE,
                choices= sort(op))  })
  
  output$WQSsubbasinSelection_ <- renderUI({
    req(input$WQSwaterbodyType, input$WQSDEQregionSelection)
    op <- filter(subbasinOptionsByWQStype, waterbodyType %in% input$WQSwaterbodyType) %>%
      filter(AssessmentRegion %in% input$WQSDEQregionSelection) %>%
      distinct(Basin_Code) %>% 
      pull() 
    selectInput("WQSsubbasinSelection", "Select Subbasin", multiple = FALSE,
                choices= sort(op))  })
  
  output$WQSbegin_ <- renderUI({
    req(input$WQSwaterbodyType, input$WQSDEQregionSelection, input$WQSsubbasinSelection)
    actionButton('WQSbegin', HTML("Begin Review With Subbasin Selection <br/>(Retrieves Last Saved Result)"),
                 class='btn-block')  })
  
  basinCodes <- reactive({
    req(input$WQSwaterbodyType, input$WQSDEQregionSelection, input$WQSsubbasinSelection)
    filter(subbasinOptionsByWQStype, waterbodyType %in% input$WQSwaterbodyType) %>%
      filter(AssessmentRegion %in% input$WQSDEQregionSelection) %>%
      filter(Basin_Code %in% input$WQSsubbasinSelection) %>%
      distinct(SubbasinOptions) %>% 
      pull() })
  
  WQSs <- eventReactive(input$WQSbegin, {
    req(input$WQSwaterbodyType, input$WQSDEQregionSelection, input$WQSsubbasinSelection)
    
    typeName <- filter(WQSlayerConversion, waterbodyType %in% input$WQSwaterbodyType) %>%
      distinct(WQS_ID) %>% 
      pull() 
    
    if(length(basinCodes()) > 1){
      WQSs <- withProgress(message = 'Reading in Large Spatial File',
                           st_zm(st_read(paste0('data/GIS/processedWQS/',typeName[1],'_', basinCodes()[1], '.shp') , 
                            fid_column_name = "OBJECTID")) %>%
                             rbind(st_zm(st_read(paste0('data/GIS/processedWQS/',typeName[1],'_', basinCodes()[2], '.shp') , 
                                                 fid_column_name = "OBJECTID"))) %>%
                             rbind(st_zm(st_read(paste0('data/GIS/processedWQS/',typeName[1],'_', basinCodes()[3], '.shp') , 
                                                 fid_column_name = "OBJECTID"))) )
    } else { WQSs <- withProgress(message = 'Reading in Large Spatial File',
                                  st_zm(st_read(paste0('data/GIS/processedWQS/',typeName[1],'_', basinCodes(), '.shp') ,
                                                          fid_column_name = "OBJECTID")) )   }
    WQSs <- WQSs %>%
    # withProgress(message = 'Reading in Large Spatial File',
    #              st_zm(
    #                st_read(paste0('data/GIS/processedWQS/',typeName[1],'_', basinCodes(), '.shp') , fid_column_name = "OBJECTID")) ) %>%
      st_transform(4326) %>%
      rename("GNIS_Name" = "GNIS_Nm",
             "WATER_NAME" = "WATER_N" ,
             "WQS_COMMENT" = "WQS_COM" ,
             "Basin_Code" = "Basn_Cd",
             "Edit_Date"  = "Edit_Dt",
             "Tier_III" = "Tir_III" ,
             "SECTION_DESCRIPTION" = 'SECTION',
             "created_user" = "crtd_sr",      
             "created_date" ="crtd_dt",
             "last_edited_user" = "lst_dtd_s",
             "last_edited_date" = "lst_dtd_d", "Shape_Length" = "Shp_Lng", 
             "BASIN_CODE" = "BASIN_C", "ASSESS_REG"="ASSESS_" , "Subbasin" = "Subbasn") %>%
      dplyr::select(WQS_ID, everything()) %>%
      {if(input$WQSwaterbodyType %in% c('Lacustrine', 'Estuarine'))
        rename(., "Shape_Area" = "Shap_Ar")
        else .}  })
  
  WQSsEL <- reactive({
    req(WQSs(), input$WQSwaterbodyType == "Estuarine")
    typeName <- filter(WQSlayerConversion, waterbodyType %in% input$WQSwaterbodyType) %>%
      distinct(WQS_ID) %>% 
      pull() 
    
    if(length(basinCodes()) > 1){
      WQSsEL <- withProgress(message = 'Reading in Additional Estuarine Spatial File',
                           st_zm(st_read(paste0('data/GIS/processedWQS/',typeName[1],'_', basinCodes()[1], '.shp') , 
                                         fid_column_name = "OBJECTID")) %>%
                             rbind(st_zm(st_read(paste0('data/GIS/processedWQS/',typeName[1],'_', basinCodes()[2], '.shp') , 
                                                 fid_column_name = "OBJECTID"))) %>%
                             rbind(st_zm(st_read(paste0('data/GIS/processedWQS/',typeName[1],'_', basinCodes()[3], '.shp') , 
                                                 fid_column_name = "OBJECTID"))) )
    } else { WQSsEL <- withProgress(message = 'Reading in Additional Estuarine Spatial File',
                                  st_zm(st_read(paste0('data/GIS/processedWQS/',typeName[1],'_', basinCodes(), '.shp') ,
                                                fid_column_name = "OBJECTID")) )   }
    WQSsEL <- WQSsEL %>%
    #withProgress(message = 'Reading in Additional Estuarine Spatial File',
    #             st_zm(
    #               st_read(paste0('data/GIS/processedWQS/',typeName[2],'_', basinCodes(), '.shp') , fid_column_name = "OBJECTID")) ) %>%
      st_transform(4326) %>%
      # match polygon structure
      rename("GNIS_Name" = "GNIS_Nm",
             "WATER_NAME" = "WATER_N" ,
             #"WQS_COMMENT" = "WQS_COMMEN" , 
             "WQS_COMMENT" = "WQS_COM" ,
             "Basin_Code" = "Basn_Cd",
             "Edit_Date"  = "Edit_Dt",
             "Tier_III" = "Tir_III" ,
             "SECTION_DESCRIPTION" = 'SECTION',
             "created_user" = "crtd_sr",      
             "created_date" ="crtd_dt",
             "last_edited_user" = "lst_dtd_s",
             "last_edited_date" = "lst_dtd_d", "Shape_Length" = "Shp_Lng", 
             "BASIN_CODE" = "BASIN_C", "ASSESS_REG"="ASSESS_" , "Subbasin" = "Subbasn") %>%  # match polygon structure
      mutate(Shape_Area = NA) %>%
      dplyr::select(WQS_ID, names(WQSs()))  })
  
  ## Map output of selected subbasin
  output$WQSVAmap <- renderLeaflet({
    req(input$WQSbegin, WQSs(), input$WQSDEQregionSelection, input$WQSsubbasinSelection)
    subbasins <- filter(subbasins, BASIN_CODE %in% as.character(basinCodes())) %>%
      filter(ASSESS_REG %in% input$WQSDEQregionSelection)
    
    m <- mapview( subbasins, label= subbasins$SUBBASIN, layer.name = 'Selected Subbasin',
                  popup= leafpop::popupTable(subbasins, zcol=c('BASIN_NAME', 'BASIN_CODE', 'SUBBASIN', 'ASSESS_REG', 'VAHU6_NOTE')))
    m@map %>% setView(sum(st_bbox(subbasins)$xmax, st_bbox(subbasins)$xmin)/2,
                      sum(st_bbox(subbasins)$ymax, st_bbox(subbasins)$ymin)/2,
                      zoom = 7)  })
  
  
  ## Make an object (once per Subbasin filter) that encompasses all WQS_ID options for said subbasin for manual WQS_ID adjustment modal, speeds rendering
  #WQS_ID_subbasinOptions <- reactive({req(WQSs())
  #  if(input$WQSwaterbodyType != "Estuarine"){
  #    as.character(WQSs()$WQS_ID)
  #  } else {  c(as.character(WQSs()$WQS_ID), as.character(WQSsEL()$WQS_ID))   }      })
  
  
  
  ### WQS reactive 
  observeEvent(input$WQSbegin, {
    
    # Weblink component based on input$WQSwaterbodyType
    if(input$WQSwaterbodyType == 'Riverine'){WQSreactive_objects$otherLayers <- "Streams/Rivers%20WQS;Public%20water%20supply;Trout;All%20other%20streams/rivers"}
    if(input$WQSwaterbodyType == 'Lacustrine'){WQSreactive_objects$otherLayers <- "Lakes/Reservoirs%20WQS;Public%20Water%20Supply;Trout;All%20other%20lakes/reservoirs"}
    if(input$WQSwaterbodyType == 'Estuarine'){WQSreactive_objects$otherLayers <- "Estuaries%20WQS;Estuarine%20waters;Tidal%20flow%20paths"}
    
    
    # Bring in existing WQS information
    WQSreactive_objects$WQSlookup <- loadData("WQSlookupTable")
    # limit conventionals_DWQS to just chosen subbasin
    WQSreactive_objects$conventionals_DWQS_Region <- st_intersection(conventionals_DWQS, 
                                                                     filter(subbasins, BASIN_CODE %in% basinCodes())) %>%
      mutate(`DEQ GIS Web App Link` =  paste0(webLinkpart1, StationID, webLinkpart2, WQSreactive_objects$otherLayers, webLinkpart3)) %>%
      dplyr::select(`DEQ GIS Web App Link`, everything())
    
    # All sites limited to waterbody type and subbasin
    WQSreactive_objects$snap_input <- readRDS('data/WQStable02032021.RDS') %>% # February 2021 update prior to official 2022 IR
      #  readRDS('data/WQStable.RDS') %>% # original effort
      filter(str_extract(WQS_ID, "^.{2}") %in% filter(WQSlayerConversion, waterbodyType %in% input$WQSwaterbodyType)$WQS_ID) %>%
      filter(gsub("_","",str_extract(WQS_ID, ".{3}_")) %in% 
               str_pad(unique(filter(subbasinOptionsByWQStype, SubbasinOptions %in% basinCodes())$SubbasinOptions), 
                       width = 2, side = 'left', pad = '0')) %>%
      # filter out any sites that happen to have existing WQS_ID
      filter(! StationID %in% WQSreactive_objects$WQSlookup$StationID) %>%
      group_by(StationID) %>%
      mutate(n = n()) %>% ungroup()
    # Sites limited to just region of interest
    WQSreactive_objects$snap_input_Region <- WQSreactive_objects$snap_input %>%
      left_join(WQSreactive_objects$conventionals_DWQS_Region, by = 'StationID') %>%
      filter(!is.na(Latitude) | !is.na(Longitude)) %>%
      st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
               remove = T, # don't remove these lat/lon cols from df
               crs = 4326) %>%
      st_intersection(filter(assessmentRegions, ASSESS_REG %in% input$WQSDEQregionSelection)) %>%
      st_drop_geometry() %>% # back to tibble
      rename('Buffer Distance' = 'Buffer.Distance') %>%
      dplyr::select(StationID, WQS_ID, `Buffer Distance`, n) %>%
      # Add back any missing sites that are dropped because they don't fall into assessment region boundaries
      bind_rows(
        filter(WQSreactive_objects$snap_input, StationID %in% 
                 filter(readRDS('data/missingSites.RDS'), 
                        ASSESS_REG %in% input$WQSDEQregionSelection)$FDT_STA_ID) %>%
          left_join(conventionals_DWQS, by = 'StationID') %>%
          dplyr::select(StationID, WQS_ID, `Buffer Distance`, n) )
    # Make dataset of all sites for highlighting purposes, preliminary list
    WQSreactive_objects$sitesUnique <- WQSreactive_objects$snap_input %>%
      full_join(WQSreactive_objects$conventionals_DWQS_Region, by = 'StationID') %>%
      # catch for sites outside a region
      {if(nrow(filter(.,is.na(FDT_STA_ID))) > 0)
        dplyr::select(., names(WQSreactive_objects$snap_input)) %>%
          left_join( conventionals_DWQS, by = 'StationID') %>%
          mutate(`DEQ GIS Web App Link` =  paste0(webLinkpart1, StationID, webLinkpart2, WQSreactive_objects$otherLayers, webLinkpart3)) %>%
          dplyr::select(StationID, WQS_ID, `Buffer Distance`, n, `DEQ GIS Web App Link`, everything())
        else .} %>%
      filter(!is.na(Latitude) | !is.na(Longitude)) %>%
      st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
               remove = F, # don't remove these lat/lon cols from df
               crs = 4326)
    # Make dataset of multiple segments snapped to single site IN REGION
    WQSreactive_objects$tooMany <- filter(WQSreactive_objects$snap_input_Region, n > 1) %>%
      group_by(StationID) %>% mutate(colorFac = row_number()) %>% ungroup() 
    # Make a dataset of actual segments for plotting
    WQSreactive_objects$tooMany_sf <- filter(WQSs(), WQS_ID %in% WQSreactive_objects$tooMany$WQS_ID) %>%
      left_join(WQSreactive_objects$tooMany, by = 'WQS_ID') %>%
      dplyr::select(StationID, WQS_ID, `Buffer Distance`, n, everything())
    if(input$WQSwaterbodyType == 'Estuarine'){
      WQSreactive_objects$tooMany_sf_EL <- filter(WQSsEL(), WQS_ID %in% WQSreactive_objects$tooMany$WQS_ID) %>%              # bonus polyline feature for Estuarine
        left_join(WQSreactive_objects$tooMany, by = 'WQS_ID') %>%
        dplyr::select(StationID, WQS_ID, `Buffer Distance`, n, everything())    
    } else {WQSreactive_objects$tooMany_sf_EL <- WQSs()[0,] %>%
      left_join(WQSreactive_objects$tooMany, by = 'WQS_ID') %>%
      dplyr::select(StationID, WQS_ID, `Buffer Distance`, n, everything())} # create dummy variable
    # Make dataset of sites associated with too many segments IN REGION
    WQSreactive_objects$tooMany_sites <- filter(WQSreactive_objects$sitesUnique, StationID %in% WQSreactive_objects$tooMany$StationID) %>%
      left_join(WQSs() %>% st_drop_geometry(), by = 'WQS_ID') %>%
      {if(input$WQSwaterbodyType == 'Estuarine')
        rbind(left_join(filter(WQSreactive_objects$sitesUnique, StationID %in% WQSreactive_objects$tooMany$StationID), 
                        WQSsEL() %>% st_drop_geometry(), by = 'WQS_ID'))
        else . } %>%
      distinct(StationID, .keep_all = T) %>%
      dplyr::select(-c(WQS_ID, `Buffer Distance`, n))
    # Make dataset of sites that snapped to a single WQS and join WQS info  IN REGION
    WQSreactive_objects$snapSingle <- filter(WQSreactive_objects$sitesUnique, n == 1 ) %>%
      {if(input$WQSwaterbodyType == 'Riverine')
        filter(., `Buffer Distance` != 'No connections within 80 m')
        else . } %>%
      filter(StationID %in% WQSreactive_objects$snap_input_Region$StationID) %>% # limit assignment to just what falls in a region
      left_join(WQSs() %>% st_drop_geometry(), by = 'WQS_ID') %>%
      {if(input$WQSwaterbodyType == 'Estuarine')
        filter(., str_extract(WQS_ID, "^.{2}") == 'EP') %>% # keep just polygon result from above
          rbind(filter(WQSreactive_objects$sitesUnique, n == 1) %>%
                  filter(StationID %in% WQSreactive_objects$snap_input_Region$StationID & str_extract(WQS_ID, "^.{2}") == 'EL') %>%
                  left_join(WQSsEL() %>% st_drop_geometry(), by = 'WQS_ID') )
        else . } %>%
      mutate(`Buffer Distance` = as.factor(`Buffer Distance`))
    # Make a dataset of actual segments that snapped to a single site for plotting
    WQSreactive_objects$snapSingle_sf <- filter(WQSs(), WQS_ID %in% WQSreactive_objects$snapSingle$WQS_ID) %>%
      left_join(dplyr::select( WQSreactive_objects$snapSingle, StationID, `Buffer Distance`, n, WQS_ID) %>% st_drop_geometry(), by = 'WQS_ID') %>%
      dplyr::select(StationID, WQS_ID, `Buffer Distance`, n, everything())
    if(input$WQSwaterbodyType == 'Estuarine'){
      WQSreactive_objects$snapSingle_sf_EL <- filter(WQSsEL(), WQS_ID %in% WQSreactive_objects$snapSingle$WQS_ID) %>% # bonus polyline feature for Estuarine
        left_join(dplyr::select( WQSreactive_objects$snapSingle, StationID, `Buffer Distance`, n, WQS_ID) %>% st_drop_geometry(), by = 'WQS_ID') %>%
        dplyr::select(StationID, WQS_ID, `Buffer Distance`, n, everything())    
    } else {WQSreactive_objects$snapSingle_sf_EL <- WQSs()[0,]  %>% # bonus polyline feature for Estuarine
      left_join(dplyr::select(WQSreactive_objects$snapSingle, StationID, `Buffer Distance`, n, WQS_ID) %>% st_drop_geometry(), by = 'WQS_ID') %>%
      dplyr::select(StationID, WQS_ID, `Buffer Distance`, n, everything()) } # create dummy variable
    # Make dataset of sites associated with no segments IN REGION
    WQSreactive_objects$snapNone <- filter(WQSreactive_objects$sitesUnique,  `Buffer Distance` == 'No connections within 80 m') %>% #is.na(WQS_ID)) %>%
      filter(StationID %in% WQSreactive_objects$snap_input_Region$StationID) %>% # limit assignment to just what falls in a region
      left_join(WQSs() %>% st_drop_geometry(), by = 'WQS_ID')
    # Make empty dataset of sites that assessors touched
    WQSreactive_objects$sitesAdjusted <-  WQSreactive_objects$sitesUnique[0,]  %>%
      dplyr::select(StationID, WQS_ID, `Buffer Distance`) %>%
      mutate(Comments = as.character())
    # Make dataset for user to download
    WQSreactive_objects$finalWQS <- WQSreactive_objects$WQSlookup
    # Make dataset of all selectable sites on map
    #WQSreactive_objects$sitesUniqueFin <- WQSreactive_objects$conventionals_DWQS_Region 
  })
  
  #  # Make dataset of all selectable sites on map
  #  observe({
  #    req(WQSreactive_objects$sitesUnique, WQSreactive_objects$conventionals_DWQS_Region)
  #    WQSreactive_objects$sitesUniqueFin <- WQSreactive_objects$conventionals_DWQS_Region })
  #    # for now, all sites are in conventionals pull, but this may not always be true
  #      #rbind(WQSreactive_objects$sitesUnique, mutate(WQSreactive_objects$conventionals_DWQS_Region, WQS_ID = NA, `Buffer Distance` = NA, n = NA) %>% dplyr::select(StationID, WQS_ID, `Buffer Distance`, n, FDT_STA_ID, everything()))  })
  
  
  # UI summaries of data pulled in to app, first and second tab
  output$singleSnapSummary1WQS <- renderPrint({ req(WQSreactive_objects$snap_input)
    cat(paste0('There are ', nrow(WQSreactive_objects$snapSingle), ' stations that snapped to 1 WQS segment in preprocessing.'))})
  output$singleSnapSummary2WQS <- renderPrint({ req(WQSreactive_objects$snap_input)
    cat(paste0('There are ', nrow(WQSreactive_objects$snapSingle), ' stations that snapped to 1 WQS segment in preprocessing.'))})
  output$snapTooManySummary1WQS <- renderPrint({ req(WQSreactive_objects$snap_input)
    cat(paste0('There are ', nrow(WQSreactive_objects$tooMany_sites), ' stations that snapped to > 1 WQS segment in preprocessing.'))})
  output$snapTooManySummary2WQS <- renderPrint({ req(WQSreactive_objects$snap_input)
    cat(paste0('There are ', nrow(WQSreactive_objects$tooMany_sites), ' stations that snapped to > 1 WQS segment in preprocessing.'))})
  output$noSnapSummary1WQS <- renderPrint({ req(WQSreactive_objects$snap_input)
    cat(paste0('There are ', nrow(WQSreactive_objects$snapNone), ' stations that snapped to 0 WQS segments in preprocessing.'))})
  output$noSnapSummary2WQS <- renderPrint({ req(WQSreactive_objects$snap_input)
    cat(paste0('There are ', nrow(WQSreactive_objects$snapNone), ' stations that snapped to 0 WQS segments in preprocessing.'))})
  
  
  
  ### WQS REVIEW TAB ##################################################################################
  
  output$test1 <- renderPrint({req(WQSs())
    #paste(
    #paste('WQSreactive_objects$tooMany_sites', WQSreactive_objects$tooMany_sites),
    #paste('nrow', nrow(WQSreactive_objects$tooMany_sf) ),
    #paste("sfc_MULTILINESTRING" %in% class(st_geometry(WQSreactive_objects$tooMany_sf))), sep ='<br>')
    #  if(#nrow(WQSreactive_objects$tooMany_sf) > 0 & 
    #    "sfc_MULTILINESTRING" %in% class(st_geometry(WQSreactive_objects$tooMany_sf)) 
    #     ){
    #    print('yes')}
    #WQSreactive_objects$conventionals_DWQS_Region
    
    filter( WQSreactive_objects$sitesUnique, `Buffer Distance` == 'No connections within 80 m')
  })
  
  
  # WQS Map
  output$WQSmap <- renderLeaflet({
    req(WQSreactive_objects$snap_input)
    CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE, 
                 options= leafletOptions(zoomControl = TRUE,minZoom = 3, maxZoom = 20,
                                         preferCanvas = TRUE)) %>%
      setView(-78, 37.5, zoom=7)  %>% 
      addCircleMarkers(data = WQSreactive_objects$conventionals_DWQS_Region, color='blue', fillColor='yellow', radius = 4,
                       fillOpacity = 0.5,opacity=0.8,weight = 1,stroke=T, group="Conventionals Stations in Basin",
                       label = ~FDT_STA_ID, layerId = ~FDT_STA_ID) %>% 
      #      {if("sfc_MULTIPOLYGON" %in% class(st_geometry(WQSs()))) 
      #        addPolygons(., data = WQSs(),
      #                    layerId = ~WQS_ID,
      #                    label=~WQS_ID, group="All WQS in selected Region/Basin", 
      #                    color = 'blue', #color = ~palTooMany(reactive_objects$tooMany$colorFac),
      #                    weight = 3,stroke=T,
      #                    popup=leafpop::popupTable(WQSs()),
      #                    popupOptions = popupOptions( maxHeight = 100 )) %>% 
      #          hideGroup("All WQS in selected Region/Basin") 
      #        else addPolylines(., data = WQSs(),
      #                          layerId = ~WQS_ID,
    #                          label=~WQS_ID, group="All WQS in selected Region/Basin", 
    #                          color = 'blue', #color = ~palTooMany(reactive_objects$tooMany$colorFac),
    #                          weight = 3,stroke=T,
    #                          popup=leafpop::popupTable(WQSs()),
    #                          popupOptions = popupOptions( maxHeight = 100 )) %>% 
    #          hideGroup("All WQS in selected Region/Basin")  } %>%
    addPolygons(data= assessmentRegions,  color = 'black', weight = 1,
                fillColor= ~pal(assessmentRegions$ASSESS_REG), fillOpacity = 0.5,stroke=0.1,
                group="Assessment Regions",
                popup=leafpop::popupTable(assessmentRegions, zcol=c('ASSESS_REG'))) %>% hideGroup('Assessment Regions') %>% #,'VAHU6','FedName'))) %>% hideGroup('Assessment Regions') %>%
      #      {if(input$WQSwaterbodyType == 'Estuarine')
      #        addPolylines(., data =WQSsEL(), # WQSs(),
      #                     layerId = ~WQS_ID,
      #                     label=~WQS_ID, group="All WQS in selected Region/Basin", 
      #                     color = 'orange',
      #                     weight = 3,stroke=T,
      #                     popup=leafpop::popupTable(WQSsEL()),#WQSs()),
      #                     popupOptions = popupOptions( maxHeight = 100 )) %>% 
      #          hideGroup("All WQS in selected Region/Basin") 
      #        else . } %>%
      inlmisc::AddHomeButton(raster::extent(-83.89, -74.80, 36.54, 39.98), position = "topleft") %>%
      inlmisc::AddSearchButton(group = "Conventionals Stations in Basin", zoom = 15,propertyName = "label",
                               textPlaceholder = "Search Conventionals Stations in Basin") %>%
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c('Conventionals Stations in Basin',
                                         #"All WQS in selected Region/Basin",
                                         'Assessment Regions'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')  %>%
      hideGroup("Conventionals Stations in Basin")    
  })
  
  WQSmap_proxy <- leafletProxy("WQSmap")
  
  # Add layers to map as requested- Single snapped sites
  observeEvent(input$plotSingleSnapSummaryWQS, {
    if (nrow(WQSreactive_objects$snapSingle) > 0 ){
      WQSmap_proxy %>%
        addCircleMarkers(data=WQSreactive_objects$snapSingle,
                         layerId = ~paste0(StationID,'_snapSingle'), # need unique layerID 
                         label=~StationID, group="Stations Snapped to 1 WQS Segment", 
                         radius = 5, fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T, color = 'black',
                         fillColor= ~palBufferDistance(WQSreactive_objects$snapSingle$`Buffer Distance`)) %>%
        {if(nrow(WQSreactive_objects$snapSingle_sf) > 0 & "sfc_POLYGON" %in% class(st_geometry(WQSreactive_objects$snapSingle_sf))) 
          addPolygons(., data=WQSreactive_objects$snapSingle_sf,
                      layerId = ~paste0(WQS_ID,'_snapSingle'),  # need unique layerID 
                      label=~WQS_ID, group="WQS Segments of Stations Snapped to 1 Segment", 
                      color = 'blue', weight = 3,stroke=T,
                      popup=leafpop::popupTable(WQSreactive_objects$snapSingle_sf),
                      popupOptions = popupOptions( maxHeight = 100 )) %>% 
            hideGroup("WQS Segments of Stations Snapped to 1 Segment")
          else . } %>%
        {if(nrow(WQSreactive_objects$snapSingle_sf) > 0 & "sfc_LINESTRING" %in% class(st_geometry(WQSreactive_objects$snapSingle_sf)))
          addPolylines(., data=WQSreactive_objects$snapSingle_sf,
                       layerId = ~paste0(WQS_ID,'_snapSingle'),  # need unique layerID 
                       label=~WQS_ID, group="WQS Segments of Stations Snapped to 1 Segment", 
                       color = 'blue', weight = 3,stroke=T,
                       popup=leafpop::popupTable(WQSreactive_objects$snapSingle_sf),
                       popupOptions = popupOptions( maxHeight = 100 )) %>%
            hideGroup("WQS Segments of Stations Snapped to 1 Segment")
          else . } %>%
        {if(nrow(WQSreactive_objects$snapSingle_sf_EL) > 0)
          addPolylines(., data=WQSreactive_objects$snapSingle_sf_EL,
                       layerId = ~paste0(WQS_ID,'_snapSingle'),  # need unique layerID 
                       label=~WQS_ID, group="WQS Segments of Stations Snapped to 1 Segment", 
                       color = 'blue', weight = 3,stroke=T,
                       popup=leafpop::popupTable(WQSreactive_objects$snapSingle_sf_EL),
                       popupOptions = popupOptions( maxHeight = 100 )) %>%
            hideGroup("WQS Segments of Stations Snapped to 1 Segment")  
          else . } %>%
        addLegend(position = 'topright', pal = palBufferDistance, values = WQSreactive_objects$snapSingle$`Buffer Distance`, 
                  group = 'Stations Snapped to 1 WQS Segment') %>%
        addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                         overlayGroups = c("Adjusted Sites",
                                           "Stations Snapped to 1 WQS Segment",
                                           "WQS Segments of Stations Snapped to 1 Segment",
                                           "Stations Snapped to > 1 WQS Segment",
                                           "WQS Segments of Stations Snapped to > 1 Segment",
                                           "Stations Snapped to 0 WQS Segments",
                                           'Conventionals Stations in Basin',
                                           #"All WQS in selected Region/Basin",
                                           'Assessment Regions'),
                         options=layersControlOptions(collapsed=T),
                         position='topleft')
    } else {
      showNotification("There are no sites that snapped to only 1 WQS segment in preprocessing steps. Nothing to plot.")
    }
  })
  
  # Add layers to map as requested- Too many snapped sites
  observeEvent(input$plotSnapTooManySummaryWQS, {
    
    if(nrow(WQSreactive_objects$tooMany_sites) > 0){
      palTooMany <- colorNumeric(c('green','yellow', 'blue','red', 'pink','purple'), domain = WQSreactive_objects$tooMany$colorFac)
      
      WQSmap_proxy %>%
        addCircleMarkers(data=WQSreactive_objects$tooMany_sites,
                         layerId = ~paste0(StationID,'_tooMany'),  # need unique layerID 
                         label=~StationID, group="Stations Snapped to > 1 WQS Segment", 
                         color='black', fillColor='red', radius = 5,
                         fillOpacity = 0.8,opacity=0.5,weight = 2,stroke=T) %>%
        {if(nrow(WQSreactive_objects$tooMany_sf) > 0 & "sfc_POLYGON" %in% class(st_geometry(WQSreactive_objects$tooMany_sf))) 
          addPolygons(., data=WQSreactive_objects$tooMany_sf,
                      layerId = ~paste0(WQS_ID,'_tooMany'),  # need unique layerID 
                      label=~WQS_ID, group="WQS Segments of Stations Snapped to > 1 Segment", 
                      color = ~palTooMany(WQSreactive_objects$tooMany_sf$colorFac),weight = 3,stroke=T,
                      popup=leafpop::popupTable(WQSreactive_objects$tooMany_sf),
                      popupOptions = popupOptions( maxHeight = 100 )) %>% 
            hideGroup("WQS Segments of Stations Snapped to > 1 Segment")
          else . } %>%
        {if(nrow(WQSreactive_objects$tooMany_sf) > 0 & "sfc_LINESTRING" %in% class(st_geometry(WQSreactive_objects$tooMany_sf)))
          addPolylines(., data=WQSreactive_objects$tooMany_sf,
                       layerId = ~paste0(WQS_ID,'_tooMany'),  # need unique layerID 
                       label=~WQS_ID, group="WQS Segments of Stations Snapped to > 1 Segment", 
                       color = ~palTooMany(WQSreactive_objects$tooMany_sf$colorFac),weight = 3,stroke=T,
                       popup=leafpop::popupTable(WQSreactive_objects$tooMany_sf),
                       popupOptions = popupOptions( maxHeight = 100 )) %>%
            hideGroup("WQS Segments of Stations Snapped to > 1 Segment")
          else . } %>%
        {if(nrow(WQSreactive_objects$tooMany_sf_EL) > 0)
          addPolylines(., data=WQSreactive_objects$tooMany_sf_EL,
                       layerId = ~paste0(WQS_ID,'_tooMany'),  # need unique layerID 
                       label=~WQS_ID, group="WQS Segments of Stations Snapped to > 1 Segment", 
                       color = ~palTooMany(WQSreactive_objects$tooMany_sf_EL$colorFac),weight = 3,stroke=T,
                       popup=leafpop::popupTable(WQSreactive_objects$tooMany_sf_EL),
                       popupOptions = popupOptions( maxHeight = 100 )) %>%
            hideGroup("WQS Segments of Stations Snapped to > 1 Segment")  
          else . } %>%
        addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                         overlayGroups = c("Adjusted Sites",
                                           "Stations Snapped to 1 WQS Segment",
                                           "WQS Segments of Stations Snapped to 1 Segment",
                                           "Stations Snapped to > 1 WQS Segment",
                                           "WQS Segments of Stations Snapped to > 1 Segment",
                                           "Stations Snapped to 0 WQS Segments",
                                           'Conventionals Stations in Basin',
                                           #"All WQS in selected Region/Basin",
                                           'Assessment Regions'),
                         options=layersControlOptions(collapsed=T),
                         position='topleft') 
    } else {
      showNotification("There are no sites that snapped to > 1 WQS segment in preprocessing steps. Nothing to plot.")
    }   })
  
  # Add layers to map as requested- 0 snapped sites
  observeEvent(input$plotNoSnapSummaryWQS, {
    if (nrow(WQSreactive_objects$snapNone) > 0 ){
      WQSmap_proxy %>%
        addCircleMarkers(data=WQSreactive_objects$snapNone,
                         layerId = ~paste0(StationID,'_snapNone'), # need unique layerID 
                         label=~StationID, 
                         group="Stations Snapped to 0 WQS Segments", 
                         color='black', fillColor='orange', radius = 5,
                         fillOpacity = 1,opacity=0.5,weight = 2,stroke=T) %>%
        addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                         overlayGroups = c("Adjusted Sites",
                                           "Stations Snapped to 1 WQS Segment",
                                           "WQS Segments of Stations Snapped to 1 Segment",
                                           "Stations Snapped to > 1 WQS Segment",
                                           "WQS Segments of Stations Snapped to > 1 Segment",
                                           "Stations Snapped to 0 WQS Segments",
                                           'Conventionals Stations in Basin',
                                           #"All WQS in selected Region/Basin",
                                           'Assessment Regions'),
                         options=layersControlOptions(collapsed=T),
                         position='topleft')
    } else {
      showNotification("There are no sites that snapped to 0 WQS segments in preprocessing steps. Nothing to plot.")
    }
  })
  
  # Map marker click (to identify selected sites
  observeEvent(input$WQSmap_marker_click, {
    site_click <- input$WQSmap_marker_click # this is all the info based on your click
    siteid <- strsplit(site_click$id, "_")[[1]][1] # this is just the layerID associated with your click
    # have to remove the unique layerID after _ to make sense of StationID
    
    if(!is.null(siteid)){ # if you clicked a point with info, find all Stations that match (with a round)
      # first find site matches from user input dataset, by lat and long
      siteMatches <- filter(WQSreactive_objects$sitesUnique, 
                            StationID %in% siteid) %>%
        st_drop_geometry() %>%
        pull(StationID)
      
      # and save all this info for later
      siteid_current <-  c(siteMatches)#, as.character(existingSiteMatches))
      
      # add the current site(s) to the selected list for highlighting and displaying in table
      if(is.null(WQSreactive_objects$namesToSmash)){
        WQSreactive_objects$namesToSmash <- siteid_current
      } else {
        WQSreactive_objects$namesToSmash <- append(siteid_current, WQSreactive_objects$namesToSmash)    }
    }
  })
  
  # Update map marker highlights
  observeEvent(WQSreactive_objects$namesToSmash, ignoreNULL=F, {
    if(!is.null(WQSreactive_objects$namesToSmash)){
      WQSmap_proxy %>%
        clearGroup(group='highlight') %>%
        addCircleMarkers(data=filter(WQSreactive_objects$sitesUnique, StationID %in% WQSreactive_objects$namesToSmash),
                         layerId = ~paste0(StationID,'_sitesHighlighted'),  # need unique layerID 
                         group='highlight', 
                         radius = 20, 
                         color='chartreuse', opacity = 0.75, fillOpacity = 0.4)  
    } else {
      WQSmap_proxy %>%
        clearGroup(group='highlight') }  })
  
  ## Clear all selected sites
  observeEvent(input$clear_allWQS, {
    WQSreactive_objects$namesToSmash=NULL
    WQSmap_proxy %>%
      clearGroup(group='highlight')  })
  
  ## Accept Snapped WQS Modal
  observeEvent(input$acceptWQS, {
    showModal(modalDialog(title = 'Accept Snapped WQS', size = 'l', easyClose = T,
                          DT::renderDataTable({
                            filter(WQSreactive_objects$sitesUnique, FDT_STA_ID %in% WQSreactive_objects$namesToSmash) %>%
                              dplyr::select(-`DEQ GIS Web App Link`) %>%
                              st_drop_geometry() %>%
                              datatable(rownames = F,  editable = 'cell',
                                        options = list(dom = 't', scrollX= TRUE, scrollY = '125px'))  }),
                          br(), br(),
                          textInput('acceptCommentWQS', 'Additional Comments and Documentation'),
                          actionButton('accept_okWQS', 'Accept', 
                                       style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', 
                                       icon=icon('check-circle')),
                          actionButton('accept_cancelWQS', 'Cancel', 
                                       style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', 
                                       icon=icon('window-close'))    ))
    jqui_draggable(selector = '.modal-content')  })
  
  # Do something with WQS Accept Modal
  observeEvent(input$accept_cancelWQS, {removeModal()})
  observeEvent(input$accept_okWQS, {
    # Get name and WQS_ID information from tooMany
    sitesUpdated <- filter(WQSreactive_objects$sitesUnique, StationID %in% WQSreactive_objects$namesToSmash) %>%
      #st_drop_geometry() %>%
      #distinct(FDT_STA_ID, .keep_all = T) %>%
      mutate(`Buffer Distance` = paste0('Manual Review | ', `Buffer Distance`),
             Comments = paste0('Manual Accept | ',input$acceptCommentWQS)) %>%
      dplyr::select(StationID, WQS_ID, `Buffer Distance`, Comments)
    
    
    # add the current site(s) to the adjusted list 
    #if(nrow(reactive_objects$sitesAdjusted) == 0){
    #  reactive_objects$sitesAdjusted
    #} else {
    WQSreactive_objects$sitesAdjusted <- rbind(WQSreactive_objects$sitesAdjusted, sitesUpdated) # rbind works better for sf objects
    #}
    
    dropMe <- unique(sitesUpdated$StationID)
    
    ## Remove Site from "to do' list
    # remove from snap to > 1 WQS sites and segments
    WQSreactive_objects$tooMany_sites <- filter(WQSreactive_objects$tooMany_sites, !(StationID %in% dropMe)) # drop sites
    WQSreactive_objects$tooMany_sf <- filter(WQSreactive_objects$tooMany_sf, !(StationID %in% dropMe)) # drop segments
    WQSreactive_objects$tooMany_sf_EL <- filter(WQSreactive_objects$tooMany_sf_EL, !(StationID %in% dropMe)) # drop segments
    
    
    # and if part of snap to 1 WQS, fix that data
    WQSreactive_objects$snapSingle <- filter(WQSreactive_objects$snapSingle, !(StationID%in% dropMe)) # drop sites
    WQSreactive_objects$snapSingle_sf <- filter(WQSreactive_objects$snapSingle_sf, !(StationID %in% dropMe)) # drop segments
    WQSreactive_objects$snapSingle_sf_EL <- filter(WQSreactive_objects$snapSingle_sf_EL, !(StationID %in% dropMe)) # drop segments
    
    # and if part of snap to 0 WQS, fix that data
    WQSreactive_objects$snapNone <- filter(WQSreactive_objects$snapNone, !(StationID %in% dropMe)) # drop sites
    
    # update output dataset
    WQSreactive_objects$finalWQS <- filter(WQSreactive_objects$finalWQS, !(StationID %in% dropMe)) %>%
      bind_rows(sitesUpdated %>% st_drop_geometry())
    
    
    # Empty map selection
    WQSreactive_objects$namesToSmash <- NULL
    
    ### Clear modal
    removeModal()
  })
  
  
  
  ## Manual WQS Adjustment Modal
  observeEvent(input$changeWQS, {
    showModal(modalDialog(title = 'Manually Adjust WQS', size = 'l', easyClose = T, 
                          DT::renderDataTable({
                            filter(WQSreactive_objects$sitesUnique, StationID %in% WQSreactive_objects$namesToSmash) %>%
                              st_drop_geometry() %>%
                              dplyr::select(-`DEQ GIS Web App Link`) %>%
                              datatable(rownames = F,  editable = 'cell',
                                        options = list(dom = 't', scrollX= TRUE, scrollY = '125px'))  }),
                          br(), br(),
                          textInput('mergeWQSID', "Manually input the WQS_ID you want connected to the selected station."),
                          helpText("Hint: Copy/Paste is your friend."),
                          # too computationally expensive and doesn't allow for other waterbody types to be manually input
                          #selectInput('mergeWQSID','Choose WQS to connect to station', 
                          #            choices = unique(c(as.character(filter(WQSreactive_objects$tooMany, 
                          #                                                   StationID %in% WQSreactive_objects$namesToSmash)$WQS_ID), # likely WQS
                          #                               WQS_ID_subbasinOptions()))), # less likely WQS but an option
                          #                               #as.character(allWQS_ID$WQS_ID)))), # less likely WQS but an option
                          textInput('adjustCommentWQS', 'Additional Comments and Documentation'),
                          actionButton('adjust_okWQS', 'Accept', 
                                       style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', 
                                       icon=icon('check-circle')),
                          actionButton('adjust_cancelWQS', 'Cancel', 
                                       style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', 
                                       icon=icon('window-close'))    ))  
    jqui_draggable(selector = '.modal-content')  })
  
  # Do something with WQS Adjustment Modal
  observeEvent(input$adjust_cancelWQS, {removeModal()})
  observeEvent(input$adjust_okWQS, {
    # Get name and location information from tooMany
    sitesUpdated <- filter(WQSreactive_objects$sitesUnique, StationID %in% WQSreactive_objects$namesToSmash) %>%
      #st_drop_geometry() %>%
      distinct(StationID, .keep_all = T) %>% # this time you do want to run a distinct to avoid duplicated rows
      mutate(WQS_ID = input$mergeWQSID,
             `Buffer Distance` = paste0('Manual Review | ', `Buffer Distance`),
             Comments = paste0('Manual Accept | ',input$adjustCommentWQS)) %>%
      dplyr::select(StationID, WQS_ID, `Buffer Distance`, Comments)
    
    # add the current site(s) to the adjusted list 
    WQSreactive_objects$sitesAdjusted <- rbind(WQSreactive_objects$sitesAdjusted, sitesUpdated) # rbind works better for sf objects
    
    dropMe <- unique(sitesUpdated$StationID)
    
    ## Remove Site from "to do' list
    # remove from snap to > 1 WQS sites and segments
    WQSreactive_objects$tooMany_sites <- filter(WQSreactive_objects$tooMany_sites, !(StationID %in% dropMe)) # drop sites
    WQSreactive_objects$tooMany_sf <- filter(WQSreactive_objects$tooMany_sf, !(StationID %in% dropMe)) # drop segments
    
    # and if part of snap to 1 WQS, fix that data
    WQSreactive_objects$snapSingle <- filter(WQSreactive_objects$snapSingle, !(StationID%in% dropMe)) # drop sites
    WQSreactive_objects$snapSingle_sf <- filter(WQSreactive_objects$snapSingle_sf, !(StationID %in% dropMe)) # drop segments
    WQSreactive_objects$snapSingle_sf_EL <- filter(WQSreactive_objects$snapSingle_sf_EL, !(StationID %in% dropMe)) # drop segments
    
    # and if part of snap to 0 WQS, fix that data
    WQSreactive_objects$snapNone <- filter(WQSreactive_objects$snapNone, !(StationID %in% dropMe)) # drop sites
    
    # update output dataset
    WQSreactive_objects$finalWQS <- filter(WQSreactive_objects$finalWQS, !(StationID %in% dropMe)) %>%
      bind_rows(sitesUpdated %>% st_drop_geometry())
    
    # Empty map selection
    WQSreactive_objects$namesToSmash <- NULL
    
    ### Clear modal
    removeModal()
  })
  
  
  
  # update WQSmap after WQS adjustment
  observe({
    req(WQSreactive_objects$sitesAdjusted)
    if(nrow(WQSreactive_objects$tooMany_sites)> 0){
      palTooMany <- colorNumeric(c('green','yellow', 'blue','red', 'pink','purple'), domain = WQSreactive_objects$tooMany$colorFac)
    } else {
      palTooMany <- colorNumeric(c('green','yellow', 'blue','red', 'pink','purple'), domain = 6)
    }
    
    ## Update proxy map
    if(nrow(WQSreactive_objects$sitesAdjusted) > 0){
      WQSmap_proxy %>% 
        # have to manually clear old sites to 'wipe' leaflet memory of joined sites
        clearGroup("Stations Snapped to 0 WQS Segments") %>%
        clearGroup("Stations Snapped to 1 WQS Segment") %>%
        clearGroup("WQS Segments of Stations Snapped to 1 Segment") %>%
        clearGroup("Stations Snapped to > 1 WQS Segment") %>%
        clearGroup("WQS Segments of Stations Snapped to > 1 Segment") %>%
        
        
        {if(nrow(WQSreactive_objects$tooMany_sites) > 0)
          addCircleMarkers(., data=WQSreactive_objects$tooMany_sites,
                           layerId = ~paste0(StationID,'_tooMany'),  # need unique layerID 
                           label=~StationID, group="Stations Snapped to > 1 WQS Segment", 
                           color='black', fillColor='red', radius = 5,
                           fillOpacity = 0.8,opacity=0.5,weight = 2,stroke=T) 
          else . } %>%
        {if(nrow(WQSreactive_objects$tooMany_sf) > 0) 
        {if("sfc_POLYGON" %in% class(st_geometry(WQSreactive_objects$tooMany_sf))) 
          addPolygons(., data=WQSreactive_objects$tooMany_sf,
                      layerId = ~paste0(WQS_ID,'_tooMany'),  # need unique layerID 
                      label=~WQS_ID, group="WQS Segments of Stations Snapped to > 1 Segment", 
                      color = ~palTooMany(WQSreactive_objects$tooMany_sf$colorFac),weight = 3,stroke=T,
                      popup=leafpop::popupTable(WQSreactive_objects$tooMany_sf),
                      popupOptions = popupOptions( maxHeight = 100 ))# %>% 
          #            hideGroup("WQS Segments of Stations Snapped to > 1 Segment")
          else
            addPolylines(., data=WQSreactive_objects$tooMany_sf,
                         layerId = ~paste0(WQS_ID,'_tooMany'),  # need unique layerID 
                         label=~WQS_ID, group="WQS Segments of Stations Snapped to > 1 Segment", 
                         color = ~palTooMany(WQSreactive_objects$tooMany_sf$colorFac),weight = 3,stroke=T,
                         popup=leafpop::popupTable(WQSreactive_objects$tooMany_sf),
                         popupOptions = popupOptions( maxHeight = 100 )) #%>%
          #            hideGroup("WQS Segments of Stations Snapped to > 1 Segment")
        }
          else . } %>%
        {if(nrow(WQSreactive_objects$tooMany_sf_EL) > 0)
          addPolylines(., data=WQSreactive_objects$tooMany_sf_EL,
                       layerId = ~paste0(WQS_ID,'_tooMany'),  # need unique layerID 
                       label=~WQS_ID, group="WQS Segments of Stations Snapped to > 1 Segment", 
                       color = ~palTooMany(WQSreactive_objects$tooMany_sf_EL$colorFac),weight = 3,stroke=T,
                       popup=leafpop::popupTable(WQSreactive_objects$tooMany_sf_EL),
                       popupOptions = popupOptions( maxHeight = 100 )) #%>%
          #            hideGroup("WQS Segments of Stations Snapped to > 1 Segment")  
          else . } %>%
        {if(nrow(WQSreactive_objects$snapSingle) > 0)
          addCircleMarkers(., data=WQSreactive_objects$snapSingle,
                           layerId = ~paste0(StationID,'_snapSingle'), # need unique layerID 
                           label=~StationID, group="Stations Snapped to 1 WQS Segment", 
                           radius = 5, fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T, color = 'black',
                           fillColor= ~palBufferDistance(WQSreactive_objects$snapSingle$`Buffer Distance`)) #%>%
          else .}  %>%
        {if(nrow(WQSreactive_objects$snapSingle_sf) > 0) 
        {if("sfc_MULTIPOLYGON" %in% class(st_geometry(WQSreactive_objects$snapSingle_sf))) 
          addPolygons(., data=WQSreactive_objects$snapSingle_sf,
                      layerId = ~paste0(WQS_ID,'_tooMany'),  # need unique layerID 
                      label=~WQS_ID, group="WQS Segments of Stations Snapped to 1 Segment", 
                      color = "blue",weight = 3,stroke=T,
                      popup=leafpop::popupTable(WQSreactive_objects$snapSingle_sf),
                      popupOptions = popupOptions( maxHeight = 100 )) #%>% 
          #            hideGroup("WQS Segments of Stations Snapped to 1 Segment")
          else
            addPolylines(., data=WQSreactive_objects$snapSingle_sf,
                         layerId = ~paste0(WQS_ID,'_tooMany'),  # need unique layerID 
                         label=~WQS_ID, group="WQS Segments of Stations Snapped to 1 Segment", 
                         color = "blue",weight = 3,stroke=T,
                         popup=leafpop::popupTable(WQSreactive_objects$snapSingle_sf),
                         popupOptions = popupOptions( maxHeight = 100 )) #%>%
          #            hideGroup("WQS Segments of Stations Snapped to 1 Segment")
        }
          else . } %>%
        {if(nrow(WQSreactive_objects$snapSingle_sf_EL) > 0)
          addPolylines(., data=WQSreactive_objects$snapSingle_sf_EL,
                       layerId = ~paste0(WQS_ID,'_tooMany'),  # need unique layerID 
                       label=~WQS_ID, group="WQS Segments of Stations Snapped to 1 Segment", 
                       color = "blue",weight = 3,stroke=T,
                       popup=leafpop::popupTable(WQSreactive_objects$snapSingle_sf_EL),
                       popupOptions = popupOptions( maxHeight = 100 ))# %>%
          #            hideGroup("WQS Segments of Stations Snapped to > 1 Segment")  
          else . } %>%
        {if(nrow(WQSreactive_objects$snapNone) > 0)
          addCircleMarkers(., data=WQSreactive_objects$snapNone,
                           layerId = ~paste0(StationID,'_snapNone'), # need unique layerID 
                           label=~StationID, 
                           group="Stations Snapped to 0 WQS Segments", 
                           color='black', fillColor='orange', radius = 5,
                           fillOpacity = 1,opacity=0.5,weight = 2,stroke=T)
          else . } %>%
        addCircleMarkers(data=WQSreactive_objects$sitesAdjusted,
                         layerId = ~paste0(StationID,'_sitesAdjusted'),  # need unique layerID 
                         label=~StationID, group="Adjusted Sites", 
                         color='black', fillColor='purple', radius = 5,
                         fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T) %>% 
        addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                         overlayGroups = c("Adjusted Sites",
                                           "Stations Snapped to 1 WQS Segment",
                                           "WQS Segments of Stations Snapped to 1 Segment",
                                           "Stations Snapped to > 1 WQS Segment",
                                           "WQS Segments of Stations Snapped to > 1 Segment",
                                           "Stations Snapped to 0 WQS Segments",
                                           'Conventionals Stations in Basin',
                                           #"All WQS in selected Region/Basin",
                                           'Assessment Regions'),
                         options=layersControlOptions(collapsed=T),
                         position='topleft') 
    }    })
  
  
  
  
  
  ### Stations Data and Spatially Joined WQS Tab
  output$selectedSiteTableWQS <- DT::renderDataTable({
    req(WQSreactive_objects$namesToSmash)
    filter(WQSreactive_objects$sitesUnique, FDT_STA_ID %in% WQSreactive_objects$namesToSmash) %>%
      st_drop_geometry() %>%
      datatable(rownames = F, escape= F, options = list(dom = 't', scrollX= TRUE, scrollY = '100px'))  })
  
  output$associatedWQSTableWQS <- DT::renderDataTable({
    req(WQSreactive_objects$namesToSmash)
    filter(WQSs(), WQS_ID %in% filter(WQSreactive_objects$snap_input, StationID %in% WQSreactive_objects$namesToSmash)$WQS_ID) %>%
      {if(input$WQSwaterbodyType == 'Estuarine')
        rbind(.,
              filter(WQSsEL(), WQS_ID %in%
                       filter(WQSreactive_objects$snap_input, StationID %in% WQSreactive_objects$namesToSmash)$WQS_ID) ) 
        else . } %>%
      st_drop_geometry() %>%
      dplyr::select(WQS_ID, everything()) %>%
      distinct(WQS_ID, .keep_all = T) %>% # for some reason this is duplicated in the app but cannot recreate on local testing
      datatable(rownames = F, options = list(dom = 't', scrollX= TRUE, scrollY = '200px'))  })
  
  ### Updated Stations Data and Manually QAed WQS Tab
  output$adjustedStationsTableWQS <- DT::renderDataTable({
    req(WQSreactive_objects$namesToSmash, WQSreactive_objects$sitesAdjusted)
    filter(WQSreactive_objects$sitesAdjusted, StationID %in% WQSreactive_objects$namesToSmash) %>%
      st_drop_geometry() %>%
      datatable(rownames = F, options = list(dom = 't', scrollX= TRUE, scrollY = '100px'))  })
  
  ## User adjusted WQS table, WQS details
  output$associatedWQSTableWQSQA <- DT::renderDataTable({
    req(WQSreactive_objects$namesToSmash, WQSreactive_objects$sitesAdjusted)
    filter(WQSs() %>% st_drop_geometry(), WQS_ID %in% filter(WQSreactive_objects$sitesAdjusted, StationID %in% WQSreactive_objects$namesToSmash)$WQS_ID) %>%
      {if(input$WQSwaterbodyType == 'Estuarine')
        rbind(.,
              filter(WQSsEL() %>% st_drop_geometry(), WQS_ID %in% 
                       filter(WQSreactive_objects$sitesAdjusted, StationID %in% WQSreactive_objects$namesToSmash)$WQS_ID) )
        else . } %>%
      #   st_drop_geometry() %>%
      dplyr::select(WQS_ID, everything()) %>%
      distinct(WQS_ID, .keep_all = T) %>% # for some reason this is duplicated in the app but cannot recreate on local testing
      datatable(rownames = F, options = list(dom = 't', scrollX= TRUE, scrollY = '200px'))  })
  
  #  ## Download WQS Information
  #  export_file=reactive(paste0('WQSlookupTable.csv'))#, region(), '_', basin(),'_',input$assessmentType, '_', Sys.Date(),'.csv'))
  #  output$downloadWQS <- downloadHandler(
  #    filename=function(){export_file()},
  #    content = function(file) {
  #      write.csv(WQSreactive_objects$finalWQS %>%
  #                  # get rid of geometry if needed
  #                  {if('geometry' %in% names(WQSreactive_objects$finalWQS))
  #                    dplyr::select(., -geometry) 
  #                    else . } %>%
  #                  as.data.frame(), file, row.names = F) }) 
  
  observeEvent(input$saveWQS, {
    saveData(WQSreactive_objects$finalWQS %>%
               # get rid of geometry if needed
               {if('geometry' %in% names(WQSreactive_objects$finalWQS))
                 dplyr::select(., -geometry) 
                 else . } %>%
               as.data.frame(), "WQSlookupTable")
    showNotification("WQS information saved on Connect server.")
    
  })  
  
})