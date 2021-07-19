source('global.R')

assessmentRegions <- st_read( 'data/GIS/AssessmentRegions_simple.shp')
ecoregion <- st_read('data/GIS/vaECOREGIONlevel3__proj84.shp')
ecoregionLevel4 <- st_read('data/GIS/vaECOREGIONlevel4__proj84.shp')
county <- st_read('data/GIS/VACountyBoundaries.shp')
assessmentLayer <- st_read('data/GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326))
subbasins <- st_read('data/GIS/DEQ_VAHUSB_subbasins_EVJ.shp') %>%
  rename('SUBBASIN' = 'SUBBASIN_1') %>%
  mutate(SUBBASIN = ifelse(is.na(SUBBASIN), as.character(BASIN_NAME), as.character(SUBBASIN))) %>%
  mutate(ProbBasin = case_when(SUBBASIN == 'Big Sandy River' ~ 'Big Sandy',
                               SUBBASIN == 'Chowan River' ~ 'Chowan',
                               SUBBASIN %in% c('James River - Lower', "James River - Middle", "James River - Upper") ~ 'James',
                               SUBBASIN == 'New River' ~ 'New',
                               SUBBASIN == 'Potomac River' ~ 'Potomac',
                               SUBBASIN == 'Shenandoah River' ~ 'Shenandoah',
                               SUBBASIN == 'Rappahannock River' ~ 'Rappahannock',
                               SUBBASIN == 'Roanoke River' ~ 'Roanoke',
                               SUBBASIN == 'Clinch and Powell Rivers' ~ 'Clinch',
                               SUBBASIN == 'Holston River' ~ 'Holston',
                               SUBBASIN == 'York River' ~ 'York',
                               TRUE ~ as.character(NA)),
         ProbSuperBasin = case_when(SUBBASIN %in% c('Big Sandy River','Holston River','Clinch and Powell Rivers') ~ 'Tennessee',
                                    SUBBASIN %in% c('Potomac River', 'Shenandoah River') ~ 'Potomac-Shenandoah',
                                    SUBBASIN %in% c('Rappahannock River', 'York River') ~ 'Rappahannock-York',
                                    TRUE ~ as.character(NA)))

subbasinVAHU6crosswalk <- read_csv('data/basinAssessmentReg_clb_EVJ.csv') %>%
  filter(!is.na(SubbasinVAHU6code)) %>%
  mutate(SUBBASIN = ifelse(is.na(SUBBASIN), BASIN_NAME, SUBBASIN)) #%>%
#dplyr::select(SUBBASIN, SubbasinVAHU6code)

# labCommentCodes <- pool %>% tbl( "Wqm_Comment_Cds_Codes_Wqm_View") %>%
#   as_tibble()
# pin(labCommentCodes, description = 'Lab Comment Codes', board = 'rsconnect')
labCommentCodes <- pin_get("labCommentCodes", board = 'rsconnect')

WQSlookup <- pin_get("WQSlookup-withStandards",  board = "rsconnect")
WQM_Stations_Spatial <- pin_get("ejones/WQM-Stations-Spatial", board = "rsconnect") %>%
  rename("Basin_Name" = "Basin_Code") # can't have same name different case when using sqldf
WQM_Stations_Full <- st_as_sf(pin_get('ejones/WQM-Station-Full', board = 'rsconnect'))


# analyte options
Wqm_Parameter_Grp_Cds_Codes_Wqm_View <- pool %>% tbl(in_schema("wqm", 'Wqm_Parameter_Grp_Cds_Codes_Wqm_View')) %>%
  filter(Pg_Parm_Name != "STORET STORAGE TRANSACTION DATE YR/MO/DAY") %>%
  distinct(Pg_Parm_Name) %>% arrange(Pg_Parm_Name) %>% as_tibble() %>% drop_na()




shinyServer(function(input, output, session) {
  
  # empty reactive objects list
  reactive_objects = reactiveValues() 
  
  ### -------------------------------Multistation Query-------------------------------------------------------------------------------------------------------
  # Query by spatial filters
  output$spatialFilters_assessmentRegion <- renderUI({req(input$queryType == 'Spatial Filters')
    list(helpText('Interactive cross validation between filters applies in this section.'),
         selectInput('assessmentRegionFilter','Assessment Region',
                     choices = sort(unique(WQM_Stations_Spatial$ASSESS_REG)), multiple = T)) })
  
  output$spatialFilters_subbasin <- renderUI({req(input$queryType == 'Spatial Filters')
    if(is.null(input$assessmentRegionFilter)){
      choices <- distinct(WQM_Stations_Spatial, Basin_Name) %>% drop_na() %>% arrange(Basin_Name) %>% pull()
    }else{
      choices <- filter(WQM_Stations_Spatial, ASSESS_REG %in% input$assessmentRegionFilter) %>%
        distinct(Basin_Name) %>% arrange(Basin_Name) %>%  pull()  }
    selectInput('subbasinFilter','Basin', choices = choices, multiple = T) })
  
  output$spatialFilters_VAHU6 <- renderUI({  req(input$queryType == 'Spatial Filters')
    if(is.null(input$assessmentRegionFilter) & is.null(input$subbasinFilter)){choices <- sort(unique(assessmentLayer$VAHU6))}
    if(is.null(input$assessmentRegionFilter) & !is.null(input$subbasinFilter)){
      choices <-  filter(WQM_Stations_Spatial, Basin_Name %in% input$subbasinFilter) %>%
        distinct(VAHU6) %>% arrange(VAHU6) %>% pull() }
    if(!is.null(input$assessmentRegionFilter) & is.null(input$subbasinFilter)){
      choices <- filter(WQM_Stations_Spatial, ASSESS_REG %in% input$assessmentRegionFilter) %>%
        distinct(VAHU6) %>% arrange(VAHU6) %>% pull() }
    if(!is.null(input$assessmentRegionFilter) & !is.null(input$subbasinFilter)){
      choices <- filter(WQM_Stations_Spatial, ASSESS_REG %in% input$assessmentRegionFilter) %>%
        filter(Basin_Name %in% input$subbasinFilter) %>%
        distinct(VAHU6) %>% arrange(VAHU6) %>%  pull()  }
    selectInput('VAHU6Filter','VAHU6', choices = choices, multiple = T) })
  
  output$spatialFilters_Ecoregion <- renderUI({#req(input$queryType == 'Spatial Filters')
    selectInput('ecoregionFilter','Level 3 Ecoregion', choices = sort(unique(ecoregion$US_L3NAME)), multiple = T) })
  
  output$spatialFilters_EcoregionLevel4 <- renderUI({#req(input$queryType == 'Spatial Filters')
    selectInput('ecoregionFilterLevel4','Level 4 Ecoregion', choices = sort(unique(ecoregionLevel4$US_L4NAME)), multiple = T) })
  
  output$spatialFilters_County <- renderUI({#req(input$queryType == 'Spatial Filters')
    selectInput('countyFilter','County/City', choices = sort(unique(county$NAME)), multiple = T) })
  
  output$dateRange_multistationUI <- renderUI({#req(input$queryType == 'Spatial Filters')
    dateRangeInput('dateRange_multistation',
                   label = 'Filter Stations By Sample Date Range (YYYY-MM-DD)',
                   start = as.Date("2020-01-01"),
                   end = as.Date("2020-12-31"))   })
  # Program Code Filter
  output$programCode_FilterUI <- renderUI({#req(input$queryType == 'Spatial Filters')
    selectInput('programCode_Filter', 'Filter Stations By Program Code',
                choices = sort(unique(programCodes$Spg_Code)), multiple = TRUE)})
  
  # Program Code table
  observeEvent(input$showProgramCodeTable,{
    showModal(modalDialog(
      title="Program Code Descriptions",
      DT::dataTableOutput('programCodeTable'),
      easyClose = TRUE))  })
  
  output$programCodeTable <- renderDataTable({req(input$showProgramCodeTable)
    datatable(programCodes %>% arrange(Spg_Code), rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bift', scrollX = TRUE, scrollY = '350px',
                             pageLength = nrow(programCodes),
                             buttons=list('copy',list(extend='excel',filename=paste0('CEDSprogramCodes')),
                                          'colvis')), selection = 'none')   })
  # Lab Media Code
  output$labMediaCodeUI <- renderUI({
    selectInput('labMediaCode', 'Filter Stations By Lab Media Codes', choices = sort(unique(labMediaCodes$Act_Media_Desc)), multiple = TRUE) })
  
  # Sample Group Code filter
  output$sampleGroupCode_FilterUI <- renderUI({
    choices <- labMediaCodes %>%
      {if(!is.null(input$labMediaCode))
        filter(., Act_Media_Desc %in% input$labMediaCode)
        else .} %>%
      pull(Lc_Parm_Group_Code)
    selectInput('sampleGroupCode_Filter', 'Filter Stations By Sample Group Codes (Based on Selected Lab Media Codes)',
                choices = sort(choices), multiple = TRUE)})
  
  # Sample Group Code table
  observeEvent(input$showSampleGroupCodeTable,{
    showModal(modalDialog(size = "l",
                          title="Sample Group Code Descriptions",
                          DT::dataTableOutput('sampleGroupCodeTable'),
                          easyClose = TRUE))  })
  
  output$sampleGroupCodeTable <- renderDataTable({req(input$showSampleGroupCodeTable)
    datatable(labMediaCodes %>% arrange(Lc_Parm_Group_Code), rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bift', scrollX = TRUE, scrollY = '500px',
                             pageLength = nrow(labMediaCodes),
                             buttons=list('copy',list(extend='excel',filename=paste0('CEDSlabGroupCodes')),
                                          'colvis')), selection = 'none')   })
  
  
  output$analyte_FilterUI <- renderUI({
    selectInput('analyte_Filter', 'Filter Stations By Analytes Collected',
                choices = Wqm_Parameter_Grp_Cds_Codes_Wqm_View, multiple = TRUE) })
  
  
  # Query by spatial filter selection
  observeEvent(input$begin_multistation_spatial,{
    show_modal_spinner(spin = 'flower')
    
    #print(input$wildcardRunIDText)
    
    
    reactive_objects$WQM_Stations_Filter <- WQM_Stations_Filter_function('Spatial Filters', pool, WQM_Stations_Spatial, input$VAHU6Filter, input$subbasinFilter,
                                                                         input$assessmentRegionFilter, input$ecoregionFilter, input$ecoregionFilterLevel4, input$countyFilter, input$dateRange_multistation,
                                                                         input$analyte_Filter, programCodeFilter = input$programCode_Filter, labGroupCodeFilter = input$sampleGroupCode_Filter,
                                                                         runIDfilter = input$wildcardRunIDText, manualSelection = NULL, wildcardSelection = NULL)
    remove_modal_spinner()
    #print( reactive_objects$WQM_Stations_Filter)
    if(nrow(reactive_objects$WQM_Stations_Filter) == 0){
      showNotification("No stations returned for selected criteria.", duration = 10, type = 'error') }
  })
  
  # Query by wildcard selection
  output$wildcardSelection <- renderUI({req(input$queryType == 'Wildcard Selection')
    list(
      helpText('Remember, use % as your wildcard, not *'),
      textInput('wildcardText', 'Filter by StationID LIKE', value = NULL, placeholder = '2A%') )      })
  
  observeEvent(input$begin_multistation_wildcard,{
    show_modal_spinner(spin = 'flower')
    
    reactive_objects$WQM_Stations_Filter <- WQM_Stations_Filter_function('Wildcard Selection',
                                                                         pool, WQM_Stations_Spatial, VAHU6Filter = NULL, subbasinFilter = NULL, assessmentRegionFilter = NULL,
                                                                         ecoregionFilter = input$ecoregionFilter, ecoregionLevel4Filter = input$ecoregionFilterLevel4,
                                                                         countyFilter = input$countyFilter,
                                                                         dateRange_multistation = input$dateRange_multistation,
                                                                         analyte_Filter = input$analyte_Filter,
                                                                         programCodeFilter = input$programCode_Filter,
                                                                         labGroupCodeFilter =  input$sampleGroupCode_Filter,
                                                                         runIDfilter = input$wildcardRunIDText, manualSelection = NULL,
                                                                         wildcardSelection = as.character(toupper(input$wildcardText)))
    remove_modal_spinner()
    #print( reactive_objects$WQM_Stations_Filter)
    if(nrow(reactive_objects$WQM_Stations_Filter) == 0){
      showNotification("No stations returned for selected criteria.", duration = 10, type = 'error') }
  })
  
  
  
  
  
  
  # Query by manual selection
  output$manualSelectionUI <- renderUI({req(input$queryType == 'Manually Specify Stations (requires a few seconds for the station text box to appear)')
    list(helpText('Begin typing station names and the app will filter available data by input text. Multiple stations are allowed.'),
         selectInput('manualSelection','Station ID', choices = sort(unique(WQM_Stations_Spatial$StationID)), multiple = T)) })
  
  observeEvent(input$begin_multistation_manual, {
    show_modal_spinner(spin = 'flower')
    
    reactive_objects$WQM_Stations_Filter <- WQM_Stations_Filter_function('Manually Specify Stations (requires a few seconds for the station text box to appear)',
                                                                         pool, WQM_Stations_Spatial, VAHU6Filter = NULL, subbasinFilter = NULL, assessmentRegionFilter = NULL,
                                                                         ecoregionFilter = input$ecoregionFilter, ecoregionLevel4Filter = input$ecoregionFilterLevel4,
                                                                         countyFilter = input$countyFilter,
                                                                         dateRange_multistation = input$dateRange_multistation,
                                                                         analyte_Filter = input$analyte_Filter,
                                                                         programCodeFilter = input$programCode_Filter,
                                                                         labGroupCodeFilter =  input$sampleGroupCode_Filter,
                                                                         runIDfilter = input$wildcardRunIDText, manualSelection = as.character(input$manualSelection),
                                                                         wildcardSelection = NULL)
    remove_modal_spinner()
    #print( reactive_objects$WQM_Stations_Filter)
    if(nrow(reactive_objects$WQM_Stations_Filter) == 0){
      showNotification("No stations returned for selected criteria.", duration = 10, type = 'error') }
  })
  
  
  
  
  
  
  observe({ req(nrow(reactive_objects$WQM_Stations_Filter) > 0)
    ## Basic Station Info
    reactive_objects$multistationInfoFin <- left_join(Wqm_Stations_View %>%  # need to repull data instead of calling stationInfo bc app crashes
                                                        filter(Sta_Id %in% reactive_objects$WQM_Stations_Filter$StationID) %>%
                                                        as_tibble() %>%
                                                        # add link to data and add link to internal GIS web app with WQS layer on there
                                                        mutate(`CEDS Station View Link` = paste0("<b><a href='https://ceds.deq.virginia.gov/ui#wqmStations/",
                                                                                                 Sta_Id,"'",
                                                                                                 " target= '_blank'> View Monitoring Station in CEDS</a></b>"),
                                                               `DEQ GIS Web App Link` =  paste0("<b><a href='https://gis.deq.virginia.gov/GISStaffApplication/?query=WQM%20Stations%20(All%20stations%20with%20full%20attributes),STATION_ID,",
                                                                                                Sta_Id,
                                                                                                "&showLayers=DEQInternalDataViewer_1723;WATER%20LAYERS;WQM%20Stations%20(All%20stations%20with%20full%20attributes);",
                                                                                                ";2020%20Draft%20ADB%20WQA%20Layers;2020%20Rivers%20(Any%20Use)&level=14' target='_blank'>View Monitoring Station in DEQ Staff App</a></b>" )) %>%
                                                        dplyr::select(Sta_Id, Sta_Desc, `CEDS Station View Link`, `DEQ GIS Web App Link`, everything()),
                                                      ########filter(WQM_Station_View, Sta_Id %in% toupper(input$station)), # need to filter instead of calling stationInfo bc app crashes
                                                      dplyr::select(WQM_Station_Full,
                                                                    STATION_ID, Latitude, Longitude, WQM_STA_STRAHER_ORDER, EPA_ECO_US_L3CODE,
                                                                    EPA_ECO_US_L3NAME, EPA_ECO_US_L4CODE, EPA_ECO_US_L4NAME, BASINS_HUC_8_NAME,
                                                                    BASINS_VAHU6, WQS_WATER_NAME, WQS_SEC, WQS_CLASS,
                                                                    WQS_SPSTDS, WQS_PWS, WQS_TROUT, WQS_TIER_III, WQM_YRS_YEAR, WQM_YRS_SPG_CODE),
                                                      by = c('Sta_Id' = 'STATION_ID')) %>%
      left_join(dplyr::select(WQM_Stations_Spatial, StationID, ASSESS_REG, CountyCityName), by = c('Sta_Id' = 'StationID')) %>%
      dplyr::select(Sta_Id, Sta_Desc, `CEDS Station View Link`, `DEQ GIS Web App Link`, Latitude, Longitude, WQM_STA_STRAHER_ORDER,
                    ASSESS_REG, CountyCityName, EPA_ECO_US_L3CODE, EPA_ECO_US_L3NAME, EPA_ECO_US_L4CODE, EPA_ECO_US_L4NAME,
                    BASINS_HUC_8_NAME, BASINS_VAHU6, WQS_WATER_NAME, WQS_SEC, WQS_CLASS,
                    WQS_SPSTDS, WQS_PWS, WQS_TROUT, WQS_TIER_III, everything())
    
    # Empty station user selection to start with
    reactive_objects$selectedSites <- NULL  })
  
  output$multistationMap <- renderLeaflet({#req(reactive_objects$WQM_Stations_Filter)
    # color palette for assessment polygons
    pal <- colorFactor(
      palette = topo.colors(7),
      domain = assessmentRegions$ASSESS_REG)
    pal2 <- colorFactor(
      palette = rainbow(7),
      domain = ecoregion$US_L3NAME)
    
    CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE,
                 options= leafletOptions(zoomControl = TRUE,minZoom = 3, maxZoom = 20,
                                         preferCanvas = TRUE)) %>%
      setView(-79.1, 37.7, zoom=7)  %>%
      addPolygons(data= ecoregion,  color = 'gray', weight = 1,
                  fillColor= ~pal2(ecoregion$US_L3NAME), fillOpacity = 0.5,stroke=0.1,
                  group="Level III Ecoregions",label = ~US_L3NAME) %>% hideGroup('Level III Ecoregions') %>%
      addPolygons(data= assessmentRegions,  color = 'black', weight = 1,
                  fillColor= ~pal(assessmentRegions$ASSESS_REG), fillOpacity = 0.5,stroke=0.1,
                  group="Assessment Regions", label = ~ASSESS_REG) %>% hideGroup('Assessment Regions') %>%
      # addPolygons(data= county,  color = 'black', weight = 1,
      #             fillColor= 'gray', fillOpacity = 0.5,stroke=0.1,
      #             group="County", label = ~NAME) %>% hideGroup("County") %>%
      inlmisc::AddHomeButton(raster::extent(-83.89, -74.80, 36.54, 39.98), position = "topleft") %>%
      addDrawToolbar(
        targetGroup='Selected',
        polylineOptions=FALSE,
        markerOptions = FALSE,
        polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0, color = 'white', weight = 3)),
        rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0, color = 'white', weight = 3)),
        circleOptions = FALSE,
        circleMarkerOptions = FALSE,
        editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions())) %>%
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c("Level III Ecoregions", 'Assessment Regions'),
                       #overlayGroups = c("Level III Ecoregions", "County", 'Assessment Regions'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')        })
  
  map_proxy_multi <- leafletProxy("multistationMap")
  
  # # Add layers to map as requested
  assessmentLayerFilter <- reactive({req(nrow(reactive_objects$WQM_Stations_Filter) > 0)
    filter(assessmentLayer, VAHU6 %in% reactive_objects$WQM_Stations_Filter$VAHU6) })
  
  observe({req(nrow(reactive_objects$WQM_Stations_Filter) > 0)
    map_proxy_multi %>%
      clearMarkers() %>%
      # flyTo(lng= mean(reactive_objects$WQM_Stations_Filter$Longitude, na.rm=T),
      #       lat = mean(reactive_objects$WQM_Stations_Filter$Latitude, na.rm=T),
      #       zoom = 8) %>%
      flyToBounds(lng1 = min(reactive_objects$WQM_Stations_Filter$Longitude)+0.01,
                  lat1 = min(reactive_objects$WQM_Stations_Filter$Latitude)+0.01,
                  lng2 = max(reactive_objects$WQM_Stations_Filter$Longitude)+0.01,
                  lat2 = max(reactive_objects$WQM_Stations_Filter$Latitude)+0.01) %>%
      addCircleMarkers(data = reactive_objects$WQM_Stations_Filter,
                       color='blue', fillColor='gray', radius = 4,
                       fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="Spatial Filter Station(s)",
                       label = ~StationID, layerId = ~StationID,
                       popup = leafpop::popupTable(reactive_objects$WQM_Stations_Filter, zcol=c('StationID'))) %>%
      {if(nrow(assessmentLayerFilter()) > 0)
        addPolygons(., data= assessmentLayerFilter(),  color = 'black', weight = 1,
                    fillColor= 'gray', fillOpacity = 0.5,stroke=0.1,
                    group="VAHU6", label = ~VAHU6) %>% hideGroup('VAHU6')
        else . } %>%
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c("Spatial Filter Station(s)", "VAHU6","Level III Ecoregions", 'Assessment Regions'),
                       #overlayGroups = c("Spatial Filter Station(s)", "VAHU6","Level III Ecoregions", "County", 'Assessment Regions'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')  })
  
  
  
  ## User polygon selection feature
  observeEvent(input$multistationMap_draw_new_feature,{
    
    shape = input$multistationMap_draw_new_feature
    
    # derive polygon coordinates and feature_type from shape input
    polygon_coordinates <- shape$geometry$coordinates
    feature_type <- shape$properties$feature_type
    
    if(feature_type %in% c("rectangle","polygon")) {
      # change user coordinates into sf multipolygon
      poly <- st_sf(what = 'user selected polygon',
                    geom = st_sfc(st_cast(st_polygon(list(do.call(rbind,lapply(polygon_coordinates[[1]],function(x){c(x[[1]][1],x[[2]][1])})))), 'MULTIPOLYGON') ))
      st_crs(poly) <- 4326 # set crs (can't do in one step???)
      
      # select sites inside polygon
      if(is.null(reactive_objects$selectedSites)){
        reactive_objects$selectedSites <- reactive_objects$WQM_Stations_Filter %>%
          st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
                   remove = F, # don't remove these lat/lon cols from df
                   crs = 4326) %>%
          st_intersection(poly)
      } else {
        addedSites <- reactive_objects$WQM_Stations_Filter %>%
          st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
                   remove = F, # don't remove these lat/lon cols from df
                   crs = 4326) %>%
          st_intersection(poly)
        reactive_objects$selectedSites <- rbind(reactive_objects$selectedSites, addedSites)
      }
    } })
  
  # Highlight selected sites from polygon
  observe({req(nrow(reactive_objects$selectedSites) > 0)
    map_proxy_multi %>%
      addCircleMarkers(data = reactive_objects$selectedSites,
                       color='blue', fillColor='yellow', radius = 4,
                       fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="User Selected Station(s)",
                       label = ~StationID, layerId = ~StationID,
                       popup = leafpop::popupTable(reactive_objects$selectedSites, zcol=c('StationID'))) %>%
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c("User Selected Station(s)","Spatial Filter Station(s)","VAHU6", "Level III Ecoregions", 'Assessment Regions'),
                       #overlayGroups = c("User Selected Station(s)","Spatial Filter Station(s)","VAHU6", "Level III Ecoregions", "County", 'Assessment Regions'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')  })
  
  # redraw all sites if user selection deleted
  observeEvent(input$multistationMap_draw_deleted_features,{
    reactive_objects$selectedSites <- NULL
    
    map_proxy_multi %>%
      clearGroup(group="User Selected Station(s)") %>%
      addCircleMarkers(data = reactive_objects$WQM_Stations_Filter,
                       color='blue', fillColor='gray', radius = 4,
                       fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="Spatial Filter Station(s)",
                       label = ~StationID, layerId = ~StationID,
                       popup = leafpop::popupTable(reactive_objects$selectedSites, zcol=c('StationID'))) %>%
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c("Spatial Filter Station(s)","VAHU6","Level III Ecoregions",'Assessment Regions'),
                       #overlayGroups = c("Spatial Filter Station(s)","VAHU6","Level III Ecoregions", "County", 'Assessment Regions'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')    })
  
  # Update "final" site selection after user input
  observe({req(reactive_objects$multistationInfoFin)
    # "final sites"
    reactive_objects$multistationSelection <- reactive_objects$multistationInfoFin %>%
      {if(!is.null(reactive_objects$selectedSites))
        filter(., Sta_Id %in% reactive_objects$selectedSites$StationID)
        else . } })
  
  
  
  # Update "final" site selection after user input
  observe({req(reactive_objects$multistationSelection)
    ## Station Sampling Information
    reactive_objects$multistationInfoSampleMetrics <- reactive_objects$multistationSelection %>%
      group_by(Sta_Id) %>%
      mutate(`Years Sampled` = paste0(year(WQM_YRS_YEAR))) %>%
      dplyr::select(Sta_Id, WQM_YRS_SPG_CODE,WQM_YRS_YEAR,`Years Sampled`) %>%
      group_by(Sta_Id, `Years Sampled`) %>%
      summarise(`Sample Codes` = paste0(WQM_YRS_SPG_CODE, collapse = ' | '))
    
    ## Field Data Information
    reactive_objects$multistationFieldData <- pool %>% tbl(in_schema("wqm", "Wqm_Field_Data_View")) %>%
      filter(Fdt_Sta_Id %in% !! reactive_objects$multistationSelection$Sta_Id &  #reactive_objects$WQM_Stations_Filter$StationID &
               between(as.Date(Fdt_Date_Time), !! input$dateRange_multistation[1], !! input$dateRange_multistation[2]) ) %>% #& # x >= left & x <= right
      #Ssc_Description != "INVALID DATA SET QUALITY ASSURANCE FAILURE") %>%  # don't drop QA failure on SQL part bc also drops any is.na(Ssc_Description)
      as_tibble() %>%
      filter(Ssc_Description != "INVALID DATA SET QUALITY ASSURANCE FAILURE")
    
    ### Analyte information
    if(nrow(reactive_objects$multistationFieldData) == 0){
      showNotification("No data for selected window.", duration = 10, type = 'error')
    } else {
      reactive_objects$multistationAnalyteData <- pool %>% tbl(in_schema("wqm", "Wqm_Analytes_View")) %>%
        filter(Ana_Sam_Fdt_Id %in% !! reactive_objects$multistationFieldData$Fdt_Id &
                 #between(as.Date(Ana_Received_Date), !! input$dateRange_multistation[1], !! input$dateRange_multistation[2]) & # x >= left & x <= right
                 Pg_Parm_Name != "STORET STORAGE TRANSACTION DATE YR/MO/DAY") %>%
        as_tibble() %>%
        left_join(dplyr::select(reactive_objects$multistationFieldData, Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time), by = c("Ana_Sam_Fdt_Id" = "Fdt_Id")) }    })
  
  ## Display Station Information
  output$multistationInfoTable <- DT::renderDataTable({
    req(reactive_objects$multistationSelection)
    datatable(reactive_objects$multistationSelection %>% distinct(Sta_Id, .keep_all = T) %>% arrange(Sta_Id),
              rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                             pageLength = nrow(reactive_objects$multistationSelection %>% distinct(Sta_Id, .keep_all = T)),
                             buttons=list('copy','colvis')))  })
  
  output$multistationInfoSampleMetrics <- DT::renderDataTable({req(reactive_objects$multistationInfoSampleMetrics)
    datatable(reactive_objects$multistationInfoSampleMetrics,
              rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                             pageLength = nrow(reactive_objects$multistationInfoSampleMetrics),
                             buttons=list('copy','colvis')) ) })
  
  
  ### Water Quality Data Tab----------------------------------------------------------------------------------------------------------------
  
  output$multistationDateRangeFilter_ <- renderUI({ req(nrow(reactive_objects$multistationFieldData) > 0)
    dateRangeInput('multistationDateRangeFilter',
                   label = 'Filter Available Data Further By User Selected Date Range (YYYY-MM-DD)',
                   start = min(as.Date(reactive_objects$multistationFieldData$Fdt_Date_Time)),
                   end = max(as.Date(reactive_objects$multistationFieldData$Fdt_Date_Time)))  })
  
  ## Lab codes based on original dataset brought back from ODS
  output$multistationLabCodesDropped_ <- renderUI({ req(nrow(reactive_objects$multistationAnalyteData) > 0)
    codeOptions <- sort(unique(reactive_objects$multistationAnalyteData$Ana_Com_Code))
    list(helpText('Below are the unique lab codes retrieved based on the initial query parameters of StationID and Date Range filters on
                    the Station Data tab. Please check any lab codes you do not want included in any further analyses. For assistance with
                    lab code descriptions, please click the button below.'),
         actionButton('multistationReviewLabCodes',"Lab Code Table",class='btn-block'),
         checkboxGroupInput('multistationLabCodesDropped', 'Lab Codes Revoved From Futher Analyses',
                            choices = codeOptions, inline = TRUE, selected = c('QF'))   ) })
  
  ## Depth filter
  output$multistationDepthFilter_ <- renderUI({ req(nrow(reactive_objects$multistationFieldData) > 0)
    checkboxInput('multistationDepthFilter', 'Only Analyze Surface Measurements (Depth <= 0.3 m)') })
  
  ## Lab Code Module
  observeEvent(input$multistationReviewLabCodes,{
    showModal(modalDialog(
      title="Lab Code Descriptions",
      DT::dataTableOutput('multistationLabCodeTable'),
      easyClose = TRUE))  })
  
  output$multistationLabCodeTable <- renderDataTable({req(input$multistationReviewLabCodes)
    datatable(labCommentCodes %>% arrange(Com_Code), rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bift', scrollX = TRUE, scrollY = '350px',
                             pageLength = nrow(labCommentCodes),
                             buttons=list('copy',list(extend='excel',filename=paste0('CEDSlabCodes')),
                                          'colvis')), selection = 'none')   })
  
  # Drop any unwanted Analyte codes
  observe({req(nrow(reactive_objects$multistationFieldData) > 0, input$multistationDateRangeFilter)
    reactive_objects$multistationFieldDataUserFilter <- filter(reactive_objects$multistationFieldData, between(as.Date(Fdt_Date_Time), input$multistationDateRangeFilter[1], input$multistationDateRangeFilter[2]) ) %>%
      {if(input$multistationDepthFilter == TRUE)
        filter(., Fdt_Depth <= 0.3)
        else . }
    reactive_objects$multistationAnalyteDataUserFilter <- filter(reactive_objects$multistationAnalyteData, between(as.Date(Fdt_Date_Time), input$multistationDateRangeFilter[1], input$multistationDateRangeFilter[2]) ) %>%
      filter(Ana_Sam_Mrs_Container_Id_Desc %in% input$multistationRepFilter) %>%
      filter(! Ana_Com_Code %in% input$multistationLabCodesDropped)
    reactive_objects$VSCIresults <- pin_get("VSCIresults", board = "rsconnect") %>%
      filter( between(as.Date(`Collection Date`), input$multistationDateRangeFilter[1], input$multistationDateRangeFilter[2]) )
    
  })
  
  
  
  
  ##### Assessment Part ############ ----------------------------------------------------------------------------------------------------------------------------------------
  

    callModule(assessment,'assessmentModule',  multistationFieldDataUserFilter = reactive(reactive_objects$multistationFieldDataUserFilter), 
               multistationAnalyteDataUserFilter = reactive(reactive_objects$multistationAnalyteDataUserFilter),
               WQM_Stations_Filter = reactive(reactive_objects$WQM_Stations_Filter), 
               multistationSelection = reactive(reactive_objects$multistationInfoFin),
               VSCIresults = reactive(reactive_objects$VSCIresults),
               WQSlookup = reactive(WQSlookup))
    
  
  
  # observe({req(nrow(reactive_objects$multistationFieldDataUserFilter) > 0)
  #   reactive_objects$multistationInfo <- pool %>% tbl(in_schema("wqm",  "Wqm_Stations_View")) %>%
  #     filter(Sta_Id %in% !! WQM_Stations_Filter$StationID) %>%
  #     as_tibble()
  #   
  #   reactive_objects$multistationGIS_View <-  pool %>% tbl(in_schema("wqm",  "Wqm_Sta_GIS_View")) %>%
  #     filter(Station_Id %in% !! WQM_Stations_Filter$StationID) %>%
  #     as_tibble()
  #   
    
   #  reactive_objects$stationTable <- left_join(tibble(STATION_ID = unique(reactive_objects$multistationSelection$Sta_Id)),
   #                            WQSlookup, by = c('STATION_ID'='StationID')) %>%
   #    mutate(CLASS_BASIN = paste(CLASS,substr(BASIN, 1,1), sep="_")) %>%
   #    mutate(CLASS_BASIN = ifelse(CLASS_BASIN == 'II_7', "II_7", as.character(CLASS))) %>%
   #    # Fix for Class II Tidal Waters in Chesapeake (bc complicated DO/temp/etc standard)
   #    left_join(WQSvalues, by = 'CLASS_BASIN') %>%
   #    dplyr::select(-c(CLASS.y,CLASS_BASIN)) %>%
   #    rename('CLASS' = 'CLASS.x') %>%
   #    left_join(WQMstationSpatial %>% distinct(StationID, .keep_all = TRUE), by = c('STATION_ID' = 'StationID')) %>%
   #    # last cycle had code to fix Class II Tidal Waters in Chesapeake (bc complicated DO/temp/etc standard) but not sure if necessary
   #    lakeNameStandardization() %>% # standardize lake names
   #    
   #    
   #    # extra special step
   #    mutate(Lake_Name = case_when(STATION_ID %in% c('2-TRH000.40') ~ 'Thrashers Creek Reservoir',
   #                                 STATION_ID %in% c('2-LSL000.16') ~ 'Lone Star Lake F (Crystal Lake)',
   #                                 STATION_ID %in% c('2-LSL000.04') ~ 'Lone Star Lake G (Crane Lake)',
   #                                 STATION_ID %in% c('2-LSL000.20') ~ 'Lone Star Lake I (Butler Lake)',
   #                                 STATION_ID %in% c('2-NWB002.93','2-NWB004.67', '2-NWB006.06') ~ 'Western Branch Reservoir',
   #                                 STATION_ID %in% c('2-LDJ000.60') ~ 'Lake Nottoway (Lee Lake)',
   #                                 TRUE ~ as.character(Lake_Name))) %>%
   #    left_join(lakeNutStandards %>% 
   #                mutate(Lakes_187B = 'y'),  # special step to make sure the WQS designation for 187 are correct even when not
   #              by = c('Lake_Name')) %>%
   #    # lake drummond special standards
   #    mutate(Lakes_187B = ifelse(is.na(Lakes_187B.y ), Lakes_187B.x, Lakes_187B.y), 
   #           `Chlorophyll a (ug/L)` = case_when(Lake_Name %in% c('Lake Drummond') ~ 35,
   #                                              TRUE ~ as.numeric(`Chlorophyll a (ug/L)`)),
   #           `Total Phosphorus (ug/L)` = case_when(Lake_Name %in% c('Lake Drummond') ~ 40,
   #                                                 TRUE ~ as.numeric(`Total Phosphorus (ug/L)`))) %>% 
   #    dplyr::select(STATION_ID:StreamType, Lakes_187B, `Description Of Waters`:`Total Phosphorus (ug/L)`) })
   #  
   # output$test <- renderPrint({reactive_objects$stationTable})
     #left_join(tibble(STATION_ID = unique(reactive_objects$multistationSelection$Sta_Id)),
  #                                       WQSlookup, by = c('STATION_ID'='StationID')) %>%
  #     mutate(CLASS_BASIN = paste(CLASS,substr(BASIN, 1,1), sep="_")) %>%
  #     mutate(CLASS_BASIN = ifelse(CLASS_BASIN == 'II_7', "II_7", as.character(CLASS))) %>%
  #     # Fix for Class II Tidal Waters in Chesapeake (bc complicated DO/temp/etc standard)
  #     left_join(WQSvalues, by = 'CLASS_BASIN') %>%
  #     dplyr::select(-c(CLASS.y,CLASS_BASIN)) %>%
  #     rename('CLASS' = 'CLASS.x') %>%
  #     left_join(WQMstationSpatial %>% distinct(StationID, .keep_all = TRUE), by = c('STATION_ID' = 'StationID')) %>%
  #     # last cycle had code to fix Class II Tidal Waters in Chesapeake (bc complicated DO/temp/etc standard) but not sure if necessary
  #     lakeNameStandardization() %>% # standardize lake names
  #     
  #     
  #     # extra special step
  #     mutate(Lake_Name = case_when(STATION_ID %in% c('2-TRH000.40') ~ 'Thrashers Creek Reservoir',
  #                                  STATION_ID %in% c('2-LSL000.16') ~ 'Lone Star Lake F (Crystal Lake)',
  #                                  STATION_ID %in% c('2-LSL000.04') ~ 'Lone Star Lake G (Crane Lake)',
  #                                  STATION_ID %in% c('2-LSL000.20') ~ 'Lone Star Lake I (Butler Lake)',
  #                                  STATION_ID %in% c('2-NWB002.93','2-NWB004.67', '2-NWB006.06') ~ 'Western Branch Reservoir',
  #                                  STATION_ID %in% c('2-LDJ000.60') ~ 'Lake Nottoway (Lee Lake)',
  #                                  TRUE ~ as.character(Lake_Name))) %>%
  #     left_join(lakeNutStandards %>% 
  #                 mutate(Lakes_187B = 'y'),  # special step to make sure the WQS designation for 187 are correct even when not
  #               by = c('Lake_Name')) %>%
  #     # lake drummond special standards
  #     mutate(Lakes_187B = ifelse(is.na(Lakes_187B.y ), Lakes_187B.x, Lakes_187B.y), 
  #            `Chlorophyll a (ug/L)` = case_when(Lake_Name %in% c('Lake Drummond') ~ 35,
  #                                               TRUE ~ as.numeric(`Chlorophyll a (ug/L)`)),
  #            `Total Phosphorus (ug/L)` = case_when(Lake_Name %in% c('Lake Drummond') ~ 40,
  #                                                  TRUE ~ as.numeric(`Total Phosphorus (ug/L)`))) %>% 
  #     dplyr::select(STATION_ID:StreamType, Lakes_187B, `Description Of Waters`:`Total Phosphorus (ug/L)`) })#
  # 
   
  # conventionals <- conventionalsSummary(conventionals= conventionalsTemplate,
  #                                       stationFieldDataUserFilter= reactive_objects$multistationFieldDataUserFilter,
  #                                       stationAnalyteDataUserFilter = reactive_objects$multistationAnalyteDataUserFilter,
  #                                       reactive_objects$multistationInfo,
  #                                       reactive_objects$multistationGIS_View,
  #                                       dropCodes = c('QF')) %>% 
  #   arrange(FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH) 
  
})