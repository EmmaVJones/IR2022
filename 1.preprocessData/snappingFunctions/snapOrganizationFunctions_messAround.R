# function to find sites with +1 segment
snapCheck <- function(successDataFrame){
  successDataFrame %>%
    group_by(`Point Unique Identifier`) %>%
    filter(n()>1)
}


AUselecter <- function(sfLines, sfPoint, i, nObjects) {
  ui <- miniPage(
    gadgetTitleBar("AU Selection", right = miniTitleBarButton("done", "Accept", primary = TRUE)),
    miniTabstripPanel(
      miniTabPanel("Site", icon = icon("mouse-pointer"),#sliders
                   miniContentPanel(
                     uiOutput('stationCounter'), br(),hr(),
                     uiOutput('StationID'), br(),
                     radioButtons("auChosen", "Choose the correct assessment unit for the site", 
                                  choices = sfLines$ID305B, selected = NULL)#,
                     #miniButtonBlock(
                     #  actionButton("chooseAU", "Choose AU"))
                   )),
      miniTabPanel("Map", icon = icon("map-o"),
                   miniContentPanel(padding = 0,
                                    leafletOutput("map", height = "100%")
                   )),
      miniTabPanel('Table', icon = icon("table"),
                   miniContentPanel(
                     DT::dataTableOutput("table")))
    )
  )
  
  
  server <- function(input, output, session) {
    
    output$stationCounter <- renderUI({paste('Station Counter:',i, 'of',nObjects)  })
    
    output$StationID <- renderUI({strong(paste('StationID:',unique(sfLines$`Point Unique Identifier`)))})
    
    output$map <- renderLeaflet({
      m <- mapview(sfLines, label= sfLines$ID305B, layer.name = c('AUs snapped to selected site'), 
                   zcol = "ID305B", legend=FALSE,
                   popup= leafpop::popupTable(sfLines, zcol=c("ID305B","MILES","CYCLE","WATER_NAME","LOCATION" )),
                   map.types = c("OpenStreetMap","Esri.WorldImagery")) +
        mapview(sfPoint, color = 'yellow',lwd= 5, label= sfPoint$FDT_STA_ID, 
                layer.name = c('Selected Site'),
                popup= leafpop::popupTable(sfPoint, zcol=c('FDT_STA_ID','STA_DESC')),
                map.types = c("OpenStreetMap","Esri.WorldImagery"))
      m@map 
      
    })
    
    output$table <- DT::renderDataTable({
      z <- sfLines %>% st_set_geometry(NULL)
      DT::datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollY = "290px", dom='t'))
    })
    
    #observeEvent(input$chooseAU, {
    #   userValue <- data.frame(StationID = unique(sfPoint$FDT_STA_ID), ID305B = input$auChosen)
    #})
    
    observeEvent(input$done, {
      userValue <- data.frame(StationID = as.character(unique(sfPoint$FDT_STA_ID)), 
                              ID305B = as.character(input$auChosen))
      stopApp(userValue)
    })
  }
  
  runGadget(shinyApp(ui, server))
  
}



WQSselecter <- function(sfLines, sfPoint, i, nObjects) {
  ui <- miniPage(
    gadgetTitleBar("WQS Selection", right = miniTitleBarButton("done", "Accept", primary = TRUE)),
    miniTabstripPanel(
      miniTabPanel("Site", icon = icon("mouse-pointer"),#sliders
                   miniContentPanel(
                     uiOutput('stationCounter'), br(),hr(),
                     uiOutput('StationID'), br(),
                     radioButtons("wqsChosen", "Choose the correct WQS segment for the site", 
                                  choices = sfLines$OBJECTID, selected = NULL))),
      miniTabPanel("Map", icon = icon("map-o"),
                   miniContentPanel(padding = 0,
                                    leafletOutput("map", height = "100%")
                   )),
      miniTabPanel('Table', icon = icon("table"),
                   miniContentPanel(
                     DT::dataTableOutput("table")))
    )
  )
  
  
  server <- function(input, output, session) {
    
    output$stationCounter <- renderUI({paste('Station Counter:',i, 'of',nObjects)  })
    
    output$StationID <- renderUI({strong(paste('StationID:',unique(sfLines$`Point Unique Identifier`)))})
    
    output$map <- renderLeaflet({
      sfLines <- mutate(sfLines, OBJID2 = as.character(OBJECTID)) # have to duplicate bc mapview doesnt want to lable using sfLines$OBJECTID
      m <- mapview(sfLines, label= sfLines$OBJID2, layer.name = c('WQS segmentss snapped to selected site'), 
                   zcol = "OBJECTID", legend=FALSE,
                   popup= leafpop::popupTable(sfLines, zcol=c("OBJECTID","WQS_COMMEN","WATER_NAME","SEC","CLASS",'SPSTDS','SECTION_DE',
                                                     'PWS','Trout','StreamType','Tier_III')),
                   map.types = c("OpenStreetMap","Esri.WorldImagery")) +
        mapview(sfPoint, color = 'yellow',lwd= 5, label= sfPoint$FDT_STA_ID, 
                layer.name = c('Selected Site'),
                popup= leafpop::popupTable(sfPoint, zcol=c('FDT_STA_ID','STA_DESC')),
                map.types = c("OpenStreetMap","Esri.WorldImagery"))
      m@map 
      
    })
    
    output$table <- DT::renderDataTable({
      z <- sfLines %>% st_set_geometry(NULL)
      DT::datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollY = "290px", dom='t'))
    })
    
    observeEvent(input$done, {
      userValue <- data.frame(StationID = as.character(unique(sfPoint$FDT_STA_ID)), 
                              OBJECTID = as.character(input$wqsChosen))
      stopApp(userValue)
    })
  }
  
  runGadget(shinyApp(ui, server))
  
}



basinNameSwitcher <- function(conventionalsName){
  if(conventionalsName == "James River Basin"){return('James')}
  if(conventionalsName == "Roanoke River Basin"){return('Roanoke')}
  if(conventionalsName == "Chowan and Dismal Swamp River Basin"){return('ChowanDismalSwamp')}
  if(conventionalsName == "New River Basin"){return('New')}
  if(conventionalsName == "Shenandoah River Basin"){return('PotomacShenandoah')}
  if(conventionalsName == "Potomac River Basin"){return('PotomacShenandoah')}
  if(conventionalsName == "Ches. Bay and Small Coastal Basin"){return('ChesBayCoastal')}
  if(conventionalsName == "Rappahannock River Basin"){return('Rappahannock')}
  if(conventionalsName == "Tennessee and Big Sandy River Basin"){return('TennesseeBigSandy')}
  if(conventionalsName == "York River Basin"){return('York')}
  if(is.na(conventionalsName)){return(NA)}
}



snapAndOrganizeAU_noUI <- function(Regional_Sites, previousCycleAU, bufferDistances){
  
  # Join unique sites from current cycle to last cycle's station table to see what already has AU info
  #Regional_Sites_sf <- mutate(Regional_Sites, STATION_ID = FDT_STA_ID) %>% # make joining column
  #  left_join(previousCycleStationTable, by='STATION_ID') %>% # join to get ID305B info
  #  st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer while at it
  #           remove = F, # don't remove these lat/lon cols from df
  #           crs = 4326) %>% # add projection, needs to be geographic for now bc entering lat/lng, 
  #  st_transform( st_crs(previousCycleAU)) # now change crs to Albers to make snapping work
  
  Regional_Sites_sf <- st_transform(Regional_Sites, st_crs(previousCycleAU)) # now change crs to Albers to make snapping work
  
  # Only work with sites that don't already have AU info
  Regional_Sites_sf_noAU <- filter(Regional_Sites_sf, is.na(ID305B_1))
  
  if(nrow(Regional_Sites_sf_noAU) > 0){
    # snapping logic
    print(paste('Snapping sites to AUs by:',min(bufferDistances),'to', max(bufferDistances), 'meters', sep=' '))
    snapList_AU <- snap_Points_to_Feature_List(Regional_Sites_sf_noAU,'FDT_STA_ID',
                                               previousCycleAU, bufferDistances)
    return(snapList_AU)
    
  } else {
    Regional_Sites_sf_AU <- Regional_Sites_sf %>% # start with sites regardless of AU's
      dplyr::select(UID:ID305B_1, -c(ID305B_1.x,ID305B_1.y),-geometry,geometry) 
    # dplyr::select(FDT_STA_ID:STATION_ID, ID305B_1,ID305B_2:geometry) # edit EVJ 2/19/2020
    
    return(Regional_Sites_sf_AU)
  }
}
    

snapAndOrganizeAU_newOutput <- function(Regional_Sites, previousCycleAU, 
                                        bufferDistances, outDir){
  # Join unique sites from current cycle to last cycle's station table to see what already has AU info
  #Regional_Sites_sf <- mutate(Regional_Sites, STATION_ID = FDT_STA_ID) %>% # make joining column
  #  left_join(previousCycleStationTable, by='STATION_ID') %>% # join to get ID305B info
  #  st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer while at it
  #           remove = F, # don't remove these lat/lon cols from df
  #           crs = 4326) %>% # add projection, needs to be geographic for now bc entering lat/lng, 
  #  st_transform( st_crs(previousCycleAU)) # now change crs to Albers to make snapping work
  
  Regional_Sites_sf <- st_transform(Regional_Sites, st_crs(previousCycleAU)) # now change crs to Albers to make snapping work
  
  # Analyze by Major Basin
  uniqueBasins <- unique(Regional_Sites_sf$Basin)
  
  for(i in 1:length(uniqueBasins)){
    #print(i)}
    Regional_Sites_sf_AU_basin <- filter(Regional_Sites_sf, Basin == uniqueBasins[i])
    
    # Only work with sites that don't already have AU info
    Regional_Sites_sf_noAU <- filter(Regional_Sites_sf_AU_basin, is.na(ID305B_1))
    
    if(nrow(Regional_Sites_sf_noAU) > 0){
      # snapping logic
      print(paste('Snapping sites to AUs by:',min(bufferDistances),'to', max(bufferDistances), 
                  'meters in basin', i, 'of', length(uniqueBasins), sep=' '))
      snapList_AU <- snap_Points_to_Feature_List(Regional_Sites_sf_noAU,'FDT_STA_ID',
                                                 previousCycleAU, bufferDistances)
      # to EPSG4326 (WGS84 for web mapping)
      snapList_AU[['sf_output']] <- st_transform(snapList_AU[['sf_output']], 4326)
    } else {
      snapList_AU[['sf_output']] <- mutate(previousCycleAU[0,], 
                                           `Point Unique Identifier` = NA, 
                                           `Buffer Distance` = NA) %>%
        dplyr::select(`Point Unique Identifier`, `Buffer Distance`, everything()) %>%
        st_transform( 4326)
    }
    
    snapList_AU[['inputSites']] <- st_transform(Regional_Sites_sf_AU_basin, 4326)
    
    # Save Things to outDir
    saveRDS(snapList_AU, paste0(outDir,uniqueBasins[i],'.RDS') )
    # final thing to give assessors for WQS snapping
  }
  return('All Done!')
  #return(snapList_AU) # this is still a sf object for WQS snapping
}

snapAndOrganizeAU <- function(Regional_Sites, previousCycleAU, bufferDistances){
  
  # Join unique sites from current cycle to last cycle's station table to see what already has AU info
  #Regional_Sites_sf <- mutate(Regional_Sites, STATION_ID = FDT_STA_ID) %>% # make joining column
  #  left_join(previousCycleStationTable, by='STATION_ID') %>% # join to get ID305B info
  #  st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer while at it
  #           remove = F, # don't remove these lat/lon cols from df
  #           crs = 4326) %>% # add projection, needs to be geographic for now bc entering lat/lng, 
  #  st_transform( st_crs(previousCycleAU)) # now change crs to Albers to make snapping work
  
  Regional_Sites_sf <- st_transform(Regional_Sites, st_crs(previousCycleAU)) # now change crs to Albers to make snapping work
  
  # Only work with sites that don't already have AU info
  Regional_Sites_sf_noAU <- filter(Regional_Sites_sf, is.na(ID305B_1))
  
  if(nrow(Regional_Sites_sf_noAU) > 0){
    # snapping logic
    print(paste('Snapping sites to AUs by:',min(bufferDistances),'to', max(bufferDistances), 'meters', sep=' '))
    snapList_AU <- snap_Points_to_Feature_List(Regional_Sites_sf_noAU,'FDT_STA_ID',
                                               previousCycleAU, bufferDistances)
    #snapList_AU <- readRDS('data/allBRRO_snapList_AU.RDS') #prerun results
    
    if(nrow(snapList_AU[['sf_output']]) > 0){
      # Process sites that snapped to too many segments
      tooMany <- snapCheck(snapList_AU[['sf_output']])
      
      # perfect sites
      sites <- filter(snapList_AU[['sf_output']], 
                      !(`Point Unique Identifier` %in% tooMany$`Point Unique Identifier`)) %>%
        st_set_geometry(NULL) %>%
        mutate(FDT_STA_ID=`Point Unique Identifier`)
      
      
      if(nrow(tooMany) > 0){
        tooMany <- tooMany %>%  st_transform(4326)# project to WGS84 for plotting
        
        siteWithTooMany <- filter(Regional_Sites_sf_noAU, FDT_STA_ID %in% 
                                    unique(tooMany$`Point Unique Identifier`)) %>%
          st_transform(4326)# project to WGS84 for plotting
        print(paste(length(unique(tooMany$`Point Unique Identifier`)),
                    'sites that snapped to too many segments. These need manual review in pane to the right.'))
        
        # empty place to put results, save time on processing and dont put in dataframe immediately bc
        # looping is already slow enough
        StationID <- as.character(nrow(siteWithTooMany))
        ID305B <- as.character(nrow(siteWithTooMany))
        
        for (i in 1:nrow(siteWithTooMany)){
          zz <- AUselecter(filter(tooMany, `Point Unique Identifier` %in% siteWithTooMany[i,]),
                           siteWithTooMany[i,], i , nrow(siteWithTooMany)) 
          StationID[i] <- as.character(zz[1][[1]])
          ID305B[i] <- as.character(zz[2][[1]])
        }
        
        results  <- data.frame(StationID, ID305B, stringsAsFactors = FALSE)
        
        # Combine sites that snapped to a segement perfectly the first time
        results_AU <- left_join(results, previousCycleAU, by='ID305B') %>%
          mutate(`Buffer Distance` = 'User Selected') %>%
          dplyr::rename(`Point Unique Identifier` = 'StationID') %>%
          dplyr::select(`Point Unique Identifier`, `Buffer Distance`, ID305B, OBJECTID, everything(), -geometry) %>%
          bind_rows(sites) %>% # add perfect sites
          mutate(FDT_STA_ID=`Point Unique Identifier`)
      } else {
        results_AU <- sites
      }
      #Make a dataset that didnt have AU's when joined to stationTable but now does have AU info thanks to
      # auto snapping or manual choice process
      Regional_Sites_noAU_AU <- filter(Regional_Sites_sf_noAU, FDT_STA_ID %in% 
                                         results_AU$`Point Unique Identifier`) %>%
        st_set_geometry(NULL) %>%
        left_join(results_AU, by = 'FDT_STA_ID') %>%
        mutate(ID305B_1 = ID305B) %>%
        select(FDT_STA_ID, ID305B_1)
      
      Regional_Sites_sf_AU <- Regional_Sites_sf %>% # start with sites regardless of AU's
        left_join(Regional_Sites_noAU_AU, by = 'FDT_STA_ID') %>%
        mutate(ID305B_1 = ifelse(is.na(ID305B_1.x), 
                                 as.character(ID305B_1.y), ID305B_1.x)) %>% # replace with snapped if NA
        dplyr::select(UID:ID305B_1, -c(ID305B_1.x,ID305B_1.y),-geometry,geometry) 
        #dplyr::select(FDT_STA_ID:STATION_ID, ID305B_1,ID305B_2:geometry, -c(ID305B_1.x,ID305B_1.y))  # edit EVJ 2/19/2020
    
  } 
  
  
  } else {
    Regional_Sites_sf_AU <- Regional_Sites_sf %>% # start with sites regardless of AU's
      dplyr::select(UID:ID305B_1, -c(ID305B_1.x,ID305B_1.y),-geometry,geometry) 
      # dplyr::select(FDT_STA_ID:STATION_ID, ID305B_1,ID305B_2:geometry) # edit EVJ 2/19/2020
  }

  Regional_Sites_needVeryManualAU <- filter(Regional_Sites_sf_AU, is.na(ID305B_1))
  print(paste(nrow(Regional_Sites_needVeryManualAU), 'sites did not snap to any AU segments within', 
              max(bufferDistances),'meters. These will all need to be manually filled in in the output .csv 
              prior to using the assessment app. All sites sent to the AU snapping tool are returned
              for snapping to WQS layer regardless of AU information.',sep=" "))
  
  # final thing to give assessors for WQS snapping
  return(Regional_Sites_sf_AU) # this is still a sf object for WQS snapping
}


snapAndOrganizeWQS <- function(AUsnappedSites, WQSfileLocation, basinName, bufferDistances){
  # Bring in WQS for basin
  print(paste('Bringing in appropriate WQS file for',basinName))
  WQS <- st_read(paste(WQSfileLocation,'updated',basinName,'.shp', sep='')) %>%
    st_transform(crs = 102003) # convert to Albers Equal Area just for snapping
  # Make sure AUsnappedSites in Albers
  AUsnappedSites <- AUsnappedSites %>% st_transform(crs = 102003) # convert to Albers Equal Area just for snapping
  
  # snapping logic
  print(paste('Snapping sites to WQS by:',min(bufferDistances),'to', max(bufferDistances), 'meters', sep=' '))
  snapList_WQS <- snap_Points_to_Feature_List(AUsnappedSites,'FDT_STA_ID',WQS, bufferDistances)
  
  if(nrow(snapList_WQS[['sf_output']]) > 0){
    tooMany <- snapCheck(snapList_WQS[['sf_output']])
    
    # perfect sites
    sites <- filter(snapList_WQS[['sf_output']], !(`Point Unique Identifier` %in% tooMany$`Point Unique Identifier`)) %>%
      st_set_geometry(NULL) %>%
      mutate(FDT_STA_ID=`Point Unique Identifier`)
    
    
    # deal with any sites that snapped to too many segments
    if(nrow(tooMany) > 0){
      tooMany <- tooMany %>%  st_transform(4326)# project to WGS84 for plotting
      
      # User fix sites that snapped to too many segments
      siteWithTooMany <- filter(AUsnappedSites, FDT_STA_ID %in% unique(tooMany$`Point Unique Identifier`)) %>%
        st_transform(4326)# project to WGS84 for plotting
      
      # Loop to build gadget for each site that snapped to too many sites
      # empty place to put results, save time on processing and dont put in dataframe immediately bc
      #   looping is already slow enough
      StationID <- as.character(nrow(siteWithTooMany))
      OBJECTID <- as.character(nrow(siteWithTooMany))
      for (i in 1:nrow(siteWithTooMany)){
        zz <- WQSselecter(filter(tooMany, `Point Unique Identifier` %in% siteWithTooMany[i,]), 
                          siteWithTooMany[i,], i , nrow(siteWithTooMany)) 
        StationID[i] <- as.character(zz[1][[1]])
        OBJECTID[i] <- as.character(zz[2][[1]])   }
      
      results  <- data.frame(StationID, OBJECTID, stringsAsFactors = FALSE)
      
      # Combine sites that snapped to a segement perfectly the first time
      results$OBJECTID <- as.integer(results$OBJECTID) # force to interger so join can happen on that column
      results_WQS <- left_join(results, WQS, by='OBJECTID') %>%
        mutate(`Buffer Distance` = 'User Selected', FDT_STA_ID=StationID) %>%
        dplyr::rename(`Point Unique Identifier` = 'StationID') %>%
        dplyr::select(`Point Unique Identifier`, `Buffer Distance`, everything(), -geometry) %>%
        bind_rows(sites)  # add perfect sites
      
      
    } else {
      results_WQS <- sites
    }
  }
  
  # Didn't snap to any WQS in max buffered distance
  noWQS <- snapList_WQS[['tbl_output']]
  print(paste(nrow(noWQS), 'sites did not snap to any segments within',max(bufferDistances),'meters in
              the',basinName,'basin. These will all need to be manually filled in in the output .csv 
              prior to using the assessment app. All sites sent to the WQS snapping tool are returned
              in this .csv regardless of WQS information.',sep=" "))
 
  # Use original basin AU dataset to include those sites that didnt snap to WQS segment within given buffer distances
  Sites_AU_WQS <- left_join(AUsnappedSites, results_WQS, by = 'FDT_STA_ID') %>%
    st_set_geometry(NULL)
  
  # final thing to give assessors for app 
  return(Sites_AU_WQS) # this is essentially a basin specific version of 'data/BRRO_Sites_AU_WQS.csv')
}



#AUsnappedSites <- tidalFreshLines
#WQSfile <- estuaryLinesWQS
#bufferDistances <- seq(10,200,by=10)

#MULTIPOINT <- AUsnappedSites # sf MULTIPOINT file
#POINT_UID_colname <- 'FDT_STA_ID' # as.character(name of unique identifier in POINT file)
#MULTILINESTRING <- WQS # stream network
#bufferDistances <- bufferDistances 

snapAndOrganizeWQSestuary <- function(AUsnappedSites, WQSfile, bufferDistances){
  
  WQS <- WQSfile %>%
    st_transform(crs = 102003) # convert to Albers Equal Area just for snapping
  # Make sure AUsnappedSites in Albers
  AUsnappedSites <- AUsnappedSites %>% st_transform(crs = 102003) # convert to Albers Equal Area just for snapping
  
  # snapping logic
  print(paste('Snapping sites to WQS by:',min(bufferDistances),'to', max(bufferDistances), 'meters', sep=' '))
  snapList_WQS <- snap_Points_to_Feature_List(AUsnappedSites,'FDT_STA_ID',WQS, bufferDistances)
  
  if(nrow(snapList_WQS[['sf_output']]) > 0){
    tooMany <- snapCheck(snapList_WQS[['sf_output']])
    
    # perfect sites
    sites <- filter(snapList_WQS[['sf_output']], !(`Point Unique Identifier` %in% tooMany$`Point Unique Identifier`)) %>%
      st_set_geometry(NULL) %>%
      mutate(FDT_STA_ID=`Point Unique Identifier`)
    
    
    # deal with any sites that snapped to too many segments
    if(nrow(tooMany) > 0){
      tooMany <- tooMany %>%  st_transform(4326)# project to WGS84 for plotting
      
      # User fix sites that snapped to too many segments
      siteWithTooMany <- filter(AUsnappedSites, FDT_STA_ID %in% unique(tooMany$`Point Unique Identifier`)) %>%
        st_transform(4326)# project to WGS84 for plotting
      
      # Loop to build gadget for each site that snapped to too many sites
      # empty place to put results, save time on processing and dont put in dataframe immediately bc
      #   looping is already slow enough
      StationID <- as.character(nrow(siteWithTooMany))
      OBJECTID <- as.character(nrow(siteWithTooMany))
      for (i in 1:nrow(siteWithTooMany)){
        zz <- WQSselecter(filter(tooMany, `Point Unique Identifier` %in% siteWithTooMany[i,]), 
                          siteWithTooMany[i,], i , nrow(siteWithTooMany)) 
        StationID[i] <- as.character(zz[1][[1]])
        OBJECTID[i] <- as.character(zz[2][[1]])   }
      
      results  <- data.frame(StationID, OBJECTID, stringsAsFactors = FALSE)
      
      # Combine sites that snapped to a segement perfectly the first time
      results$OBJECTID <- as.integer(results$OBJECTID) # force to interger so join can happen on that column
      results_WQS <- left_join(results, WQS, by='OBJECTID') %>%
        mutate(`Buffer Distance` = 'User Selected', FDT_STA_ID=StationID) %>%
        dplyr::rename(`Point Unique Identifier` = 'StationID') %>%
        dplyr::select(`Point Unique Identifier`, `Buffer Distance`, everything(), -geometry) %>%
        bind_rows(sites)  # add perfect sites
      
      
    } else {
      results_WQS <- sites
    }
  }
  
  # Didn't snap to any WQS in max buffered distance
  noWQS <- snapList_WQS[['tbl_output']]
  print(paste(nrow(noWQS), 'sites did not snap to any segments within',max(bufferDistances),'meters in
              the estuary Line layer. These will all need to be manually filled in in the output .csv 
              prior to using the assessment app. All sites sent to the WQS snapping tool are returned
              in this .csv regardless of WQS information.',sep=" "))
  
  # Use original basin AU dataset to include those sites that didnt snap to WQS segment within given buffer distances
  Sites_AU_WQS <- left_join(AUsnappedSites, results_WQS, by = 'FDT_STA_ID') %>%
    st_set_geometry(NULL)
  
  # final thing to give assessors for app 
  return(Sites_AU_WQS) # this is essentially a basin specific version of 'data/BRRO_Sites_AU_WQS.csv')
}
