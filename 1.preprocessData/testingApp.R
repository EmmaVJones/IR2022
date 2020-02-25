ui <- fluidPage(
  headerPanel(
    title='Citmon organization'),
  
  mainPanel(width=11,
            bsCollapsePanel(list(icon('exchange'),"Establish Variables"), value=2,
                            fluidRow(
                              column(2, uiOutput('IDfield1')),
                              column(2, uiOutput('IDfield2')),
                              column(2, uiOutput('IDfield3')),
                              column(2, actionButton('adjustInput', icon=icon("refresh"), label='', style = "margin-top: 25px;"))
                            ),
                            dataTableOutput('updatedTable')
            ),
            bsCollapsePanel(list(icon('map-marked-alt'), 'Review Sites'), value = 3,
                            fluidRow(actionButton('plotInputSites', icon=icon("map-pin"), label='Plot User Data On Map', style = "margin-top: 25px;")),
                            br(),
                            # Map
                            fluidRow(shinycssloaders::withSpinner(leaflet::leafletOutput("map", height="600px"),size=2, color="#0080b7"))
            ),
            
            bsCollapsePanel(list(icon('cog'), 'Review Selection'), value = 4,
                            # Reviewer actions
                            fluidRow(
                              actionButton('clear_all', 'Clear all', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('backspace')),
                              actionButton('accept', 'Accept', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('check-circle')),
                              actionButton('reject', 'Reject', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('minus-circle')),
                              actionButton('merge', 'Merge', style='color: #fff; background-color: #337ab7; border-color: #2e6da4', icon=icon('object-group')),
                              downloadButton('exp_rev', label = "Export reviews",style='color: #fff; background-color: #228b22; border-color: #134e13'),
                              actionButton('checkOut', 'checkMeOut', style='color: #fff; background-color: #337ab7; border-color: #2e6da4', icon=icon('object-group'))),
                            br(),
                            tabsetPanel(
                              tabPanel(strong('Original User Uploaded and Existing DEQ Stations Data'),
                                       br(),
                                       fluidRow(
                                         h5(strong('Original User Entered Station Data')),
                                         div(DT::dataTableOutput("selected_sites_table"), style = "font-size:80%")),
                                       br(),
                                       fluidRow(
                                         h5(strong('Existing DEQ Station Data')),
                                         div(DT::dataTableOutput("existing_selected_sites_table"), style = "font-size:80%")) ),
                              tabPanel(strong('Updated User Uploaded Stations Data'),
                                       br(),
                                       fluidRow(
                                         h5(strong('Accepted Sites Station Data')),
                                         div(DT::dataTableOutput("accepted_sites_table"), style = "font-size:80%")),
                                       br(),
                                       fluidRow(
                                         h5(strong('Rejected Sites Station Data')),
                                         div(DT::dataTableOutput("rejected_sites_table"), style = "font-size:80%")),
                                       br(),
                                       fluidRow(
                                         h5(strong('Merged Sites Station Data')),
                                         div(DT::dataTableOutput("merged_sites_table"), style = "font-size:80%")))),
                            
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            verbatimTextOutput('test'))
  )
)

server <- function(input, output, session){
  
  # color palette for assessment polygons
  pal <- colorFactor(
    palette = topo.colors(7),
    domain = assessmentRegions$ASSESS_REG)
  
  # empty reactive objects list
  reactive_objects=reactiveValues()
  
  
  
  observe(reactive_objects$sites_input <- cit )
  observe(reactive_objects$reviewer <- 'evj')

######  
  # Empty accepted sites 
  observeEvent(input$adjustInput, { reactive_objects$sites_Accepted <- reactive_objects$sites_Adjusted[0,]})
  # Empty rejected sites 
  observeEvent(input$adjustInput, { reactive_objects$sites_Rejected <- reactive_objects$sites_Adjusted[0,]})
  # Empty merged sites 
  observeEvent(input$adjustInput, { reactive_objects$sites_Merged = reactive_objects$sites_Adjusted[0,]})
#####
  
  # Renamed columns into reproducible format for app, this is also the original raw data holder
  observeEvent(input$adjustInput, {
    reactive_objects$sites_Adjusted <- reassignColumns(reactive_objects$sites_input, Group_Station_ID, Latitude, Longitude)%>%
      # add UID now so it trickles through continuously to all other variables
      group_by(originalStationID, Latitude, Longitude) %>%
      mutate(UID = group_indices()) %>%#row_number()) %>%
      dplyr::select(UID, everything()) %>%
      ungroup()})
  
  # Sites without spatial data to be given back to user as separate sheet upon download
  observeEvent(input$adjustInput, {
    reactive_objects$notEnoughInfo <- filter(reactive_objects$sites_Adjusted, is.na(originalStationID), is.na(Latitude)|is.na(Longitude)) })# separate sites without location information or identifier)
  
 
  
  # Unique sites spatial dataset for map and table
  observeEvent(input$adjustInput, {
    reactive_objects$sitesUnique <- filter(reactive_objects$sites_Adjusted, !is.na(originalStationID), !is.na(Latitude)|!is.na(Longitude))  %>% # drop sites without location information
      distinct(originalStationID, Latitude, Longitude, .keep_all =T)  %>% #distinct by location and name
      st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
               remove = F, # don't remove these lat/lon cols from df
               crs = 4326) # add coordinate reference system, needs to be geographic for now bc entering lat/lng
  })
  
  # Make dataset of all sites for highlighting purposes
  observeEvent(input$adjustInput, {
    reactive_objects$allSites <- suppressWarnings(
      dplyr::select(reactive_objects$sitesUnique, originalStationID, Latitude, Longitude) %>%
      dplyr::rename('uniqueID' = 'originalStationID') %>%
      # sf objects use rbind for quieter results than dplyr::bind_rows()
        rbind(dplyr::select(existingStations, FDT_STA_ID, Latitude, Longitude) %>%
                  dplyr::rename('uniqueID' = 'FDT_STA_ID')) )  })
  
#  # Data associated with each unique site combo (orginalStationID, Lat, Long) for joining back on download
#  observeEvent(input$adjustInput, {
#    reactive_objects$sitesData <- reactive_objects$sites_Adjusted %>%
#      group_by(originalStationID, Latitude, Longitude) %>%
#      mutate(UID = group_indices()) %>%#row_number()) %>% #group_indices()) %>%
#      dplyr::select(UID, everything()) %>%
#      ungroup()    })
  
# for testing, keeps things smaller  
#  output$updatedTable <- DT::renderDataTable({
#    req(reactive_objects$sitesUnique)
#    datatable(reactive_objects$sitesUnique, rownames = F, options=list(scrollX = TRUE, scrollY = "300px"))  })
  
  

#### Review Map ####
  
  # Map output
  #session$onFlushed(once = T, function() {
  output$map <- renderLeaflet({

    CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE) %>%
      setView(-78, 37.5, zoom=6) %>%
      addCircleMarkers(data = existingStations, color='orange', fillColor='black', radius = 3,
                       fillOpacity = 0.5,opacity=0.5,weight = 1,stroke=T,group="Existing Stations",
                       label = ~FDT_STA_ID, layerId = ~FDT_STA_ID#, 
                       #popup = leafpop::popupTable(existingStations, 
                      #                             zcol=c( "FDT_STA_ID", "STA_DESC", "Deq_Region",
                      #                                     "Huc6_Vahu6","ID305B_1", "ID305B_2",  "ID305B_3"  ))
                      ) %>% 
#      addPolygons(data= assessmentRegions,  color = 'black', weight = 1,
#                  fillColor= ~pal(assessmentRegions$ASSESS_REG), fillOpacity = 0.5,stroke=0.1,
#                  group="Assessment Regions",
#                  popup=leafpop::popupTable(assessmentRegions, zcol=c('ASSESS_REG','VAHU6','FedName'))) %>% hideGroup('Assessment Regions') %>%
      inlmisc::AddHomeButton(raster::extent(-83.89, -74.80, 36.54, 39.98), position = "topleft") %>%
      inlmisc::AddSearchButton(group = "Existing Stations", zoom = 15,propertyName = "label",
                               textPlaceholder = "Search Existing Stations") %>%
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c('Existing Stations','Assessment Regions'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft') %>%
      hideGroup("Existing Stations")  }) # })
  
  map_proxy=leafletProxy("map")
  
  
  # Add sites via proxy on site_types change
  # need to add extra button bc user will not intuitively load map before pressing input$adjustInput
  # and a proxy layer needs the map loaded first
  observeEvent(input$plotInputSites, {
    map_proxy %>%
      addCircleMarkers(data=reactive_objects$sitesUnique,
                       layerId = ~originalStationID,
                       label=~originalStationID, group="Sites", 
                       color='yellow', fillColor='red', radius = 5,
                       fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T#,
                       #popup=leafpop::popupTable(reactive_objects$sitesUnique, 
                       #                          zcol=c('UID','originalStationID','finalStationID'))
                       ) %>%
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c('Sites','Existing Stations','Assessment Regions'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft') })
  
  
  # Map marker click (to identify selected sites, will also select sites w/ identical (round(lat/long, 4) but different names
  observeEvent(input$map_marker_click, {
    site_click <- input$map_marker_click # this is all the info based on your click
    siteid <- site_click$id # this is just the layerID associated with your click

    if(!is.null(siteid)){ # if you clicked a point with info, find all Stations that match (with a round)
      # first find site matches from user input dataset, by lat and long
      siteMatches <- filter(reactive_objects$sitesUnique, 
                            originalStationID %in% siteid |
                              Latitude %in% round(site_click$lat, 4) & 
                              Longitude %in% round(site_click$lng, 4)) %>%
        mutate(sites = originalStationID) %>%
        dplyr::select(sites) %>% 
        st_drop_geometry() %>%
        pull()
      # then existing sites
      existingSiteMatches <- filter(existingStations, 
                                    FDT_STA_ID %in% siteid |
                                      Latitude %in% round(site_click$lat, 4) & 
                                      Longitude %in% round(site_click$lng, 4)) %>%
        mutate(sites = FDT_STA_ID) %>%
        dplyr::select(sites) %>% 
        st_drop_geometry() %>%
        pull()
      
      # and save all this info for later
      siteid_current <-  c(siteMatches, existingSiteMatches)
      
      # now for accepted sites
      if(!is.null(reactive_objects$sites_Accepted)){
        acceptedSiteMatches <- filter(reactive_objects$sites_Accepted, finalStationID %in% siteid |
                                        Latitude %in% round(site_click$lat, 4) & 
                                        Longitude %in% round(site_click$lng, 4)) %>%
          mutate(sites = finalStationID) %>%
          dplyr::select(sites) %>% 
          pull()
        
        # and save all this info for later
        siteid_current <-  c(siteid_current, acceptedSiteMatches)
      }
      
      # now for rejected sites
      if(!is.null(reactive_objects$sites_Rejected)){
        rejectedSiteMatches <- filter(reactive_objects$sites_Rejected, originalStationID %in% siteid |
                                        Latitude %in% round(site_click$lat, 4) & 
                                        Longitude %in% round(site_click$lng, 4)) %>%
          mutate(sites = originalStationID) %>%
          dplyr::select(sites) %>% 
          pull()
        
        # and save all this info for later
        siteid_current <-  c(siteid_current, rejectedSiteMatches)
      }
      
      # now for merged sites
      if(!is.null(reactive_objects$sites_Merged)){
        mergedSiteMatches <- filter(reactive_objects$sites_Merged, finalStationID %in% siteid |
                                        Latitude %in% round(site_click$lat, 4) & 
                                        Longitude %in% round(site_click$lng, 4)) %>%
          mutate(sites = finalStationID) %>%
          dplyr::select(sites) %>% 
          pull()
        
        # and save all this info for later
        siteid_current <-  c(siteid_current, mergedSiteMatches)
      }
      
      # add the current site(s) to the selected list for highlighting and displaying in table
      if(is.null(reactive_objects$namesToSmash)){
        reactive_objects$namesToSmash <- siteid_current
      } else {
        reactive_objects$namesToSmash <- append(siteid_current, reactive_objects$namesToSmash)
      }
    } 
  })
  
  # reactive to hold selected (user uploaded) data for tables
  userEnteredStationDataTable <- reactive({
    req(reactive_objects$namesToSmash, reactive_objects$sitesUnique)
      filter(reactive_objects$sitesUnique, originalStationID %in% reactive_objects$namesToSmash) })
  
  
  # Update map marker highlights
  observeEvent(reactive_objects$namesToSmash, ignoreNULL=F, {
    req(reactive_objects$namesToSmash)
    map_proxy %>%
      clearGroup(group='highlight') %>%
      addCircleMarkers(data=filter(reactive_objects$allSites, uniqueID %in% reactive_objects$namesToSmash),
                       group='highlight', #options = pathOptions(pane = "highlight"), 
                       radius = 20, 
                       color='chartreuse', opacity = 0.75, fillOpacity = 0.4)  })
  
  # Accept selected site(s) Modal
  observeEvent(input$accept, {
    showModal(modalDialog(title = 'Confirm Site Acceptance', size = 'l',
                          #datatable(userEnteredStationDataTable(), 
                          #          selection='none', rownames=FALSE, 
                          #          options = list(scrollY = '125px', paging = FALSE, scrollX=TRUE, dom="t"))}),
                          dataTableOutput('acceptTable_editable'),
                          br(), br(),
                          textInput('acceptComment', 'Additional Comments and Documentation'),
                          actionButton('accept_ok', 'Accept', 
                                       style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', 
                                       icon=icon('check-circle')),
                          actionButton('accept_cancel', 'Cancel', 
                                       style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', 
                                       icon=icon('window-close'))    ))  })
  forAcceptModalTable <- reactive({
    req(userEnteredStationDataTable())
    userEnteredStationDataTable() %>% st_drop_geometry() %>% as.data.frame()})
  
  # Accept Modal table
  output$acceptTable_editable <- DT::renderDataTable({
    datatable(forAcceptModalTable(), 
              selection='none', rownames=FALSE, editable = T, 
              options = list(scrollY = '125px', paging = FALSE, scrollX=TRUE, dom="t"))})
  
  # Observe User changes to modal table 
  observeEvent(input$acceptTable_editable_cell_edit, {
    #dat = userEnteredStationDataTable() %>% st_drop_geometry() %>% as.data.frame()
    info = input$acceptTable_editable_cell_edit
    str(info)
    i = info$row
    j = info$col + 1  # column index offset by 1
    v = info$value
    forAcceptModalTable()[i, j] <<- DT::coerceValue(v, forAcceptModalTable()[i, j])
    replaceData(dataTableProxy('acceptTable_editable_cell_edit'), forAcceptModalTable(), resetPaging = FALSE, rownames = FALSE)
  })
  
  # Do something with Accepted Site(s)
  observeEvent(input$accept_cancel, {removeModal()})
  observeEvent(input$accept_ok, {
    # update data with finalStationID, reviewer, and reviewer comments
    updatedData <- userEnteredStationDataTable() %>% #filter(reactive_objects$sitesUnique, originalStationID %in% reactive_objects$namesToSmash) %>%  # Fix sitesUnique for table to populate accepted information #
      st_drop_geometry() %>%
      mutate(finalStationID = originalStationID,
             Reviewer = reactive_objects$reviewer, 
             ReviewComment = input$acceptComment)
    
    # add the current site(s) to the accepted list 
    reactive_objects$sites_Accepted <- bind_rows(reactive_objects$sites_Accepted, updatedData)
    

    ## Remove Site from "to do' list
    reactive_objects$sitesUnique <- filter(reactive_objects$sitesUnique, !(originalStationID %in% reactive_objects$sites_Accepted))
    
    ### Clear modal
    removeModal()
  })
  
  # add accepted sites to map if they exist?
  observe({
    req(reactive_objects$sites_Accepted)
    ## Update proxy map
    if(nrow(reactive_objects$sites_Accepted) > 0){
      map_proxy %>%
        addCircleMarkers(data=reactive_objects$sites_Accepted,
                         layerId = ~finalStationID,
                         label=~finalStationID, group="Accepted Sites", 
                         color='black', fillColor='green', radius = 5,
                         fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T) %>%
        addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                         overlayGroups = c('Sites','Accepted Sites','Rejected Sites',
                                           'Merged Sites','Existing Stations','Assessment Regions'),
                         options=layersControlOptions(collapsed=T),
                         position='topleft') }  })
  
  
  
  # Reject selected site(s) Modal
  observeEvent(input$reject, {
    showModal(modalDialog(title = 'Confirm Site Rejection', size = 'l',
                          DT::renderDataTable({
                            datatable(userEnteredStationDataTable(),
                                      selection='none', rownames=FALSE, 
                                      options = list(scrollY = '125px', paging = FALSE, scrollX=TRUE, dom="t"))}),
                          br(), br(),
                          textInput('rejectComment', 'Additional Comments and Documentation'),
                          actionButton('reject_ok', 'Reject', 
                                       style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', 
                                       icon=icon('check-circle')),
                          actionButton('reject_cancel', 'Cancel', 
                                       style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', 
                                       icon=icon('window-close'))    ))  })
  
  # Do something with Rejected Site(s)
  observeEvent(input$reject_cancel, {removeModal()})
  observeEvent(input$reject_ok, {
    # update data with finalStationID, reviewer, and reviewer comments
    updatedData <- userEnteredStationDataTable() %>% #filter(reactive_objects$sitesUnique, originalStationID %in% reactive_objects$namesToSmash) %>%  # Fix sitesUnique for table to populate accepted information #
      st_drop_geometry() %>%
      mutate(Reviewer = reactive_objects$reviewer, 
             ReviewComment = input$rejectComment)
    
    # add the current site(s) to the rejected list 
    reactive_objects$sites_Rejected <- bind_rows(reactive_objects$sites_Rejected, updatedData) 
    
    
    ## Remove Site from "to do' list
    reactive_objects$sitesUnique <- filter(reactive_objects$sitesUnique, !(originalStationID %in% reactive_objects$sites_Rejected))
    
    ### Clear modal
    removeModal()
  })
  
  # add rejected sites to map if they exist?
  observe({
    req(reactive_objects$sites_Rejected)
    ## Update proxy map
    if(nrow(reactive_objects$sites_Rejected) > 0){
      map_proxy %>%
        addCircleMarkers(data=reactive_objects$sites_Rejected,
                         layerId = ~originalStationID,
                         label=~originalStationID, group="Rejected Sites", 
                         color='black', fillColor='red', radius = 5,
                         fillOpacity = 0.7,opacity=0.5,weight = 2,stroke=T) %>%
        addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                         overlayGroups = c('Sites','Accepted Sites','Rejected Sites',
                                           'Merged Sites','Existing Stations','Assessment Regions'),
                         options=layersControlOptions(collapsed=T),
                         position='topleft') }  })
    
  
  
  # Merge selected site(s) Modal
  observeEvent(input$merge, {
    req(reactive_objects$namesToSmash)
    showModal(modalDialog(title = 'Confirm Site Merge', size = 'l',
                          DT::renderDataTable({
                            datatable(userEnteredStationDataTable(),
                                      selection='none', rownames=FALSE, 
                                      options = list(scrollY = '200px', paging = FALSE, scrollX=TRUE, dom="t"))}),
                          br(), br(),
                          selectInput('mergeSiteName','Choose site to merge name and location information to all selected sites',
                                      choices = reactive_objects$namesToSmash),
                          textInput('mergeComment', 'Additional Comments and Documentation'),
                          actionButton('merge_ok', 'Merge', 
                                       style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', 
                                       icon=icon('check-circle')),
                          actionButton('merge_cancel', 'Cancel', 
                                       style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', 
                                       icon=icon('window-close'))    ))  })
  
  # Do something with Accepted Site(s)
  observeEvent(input$merge_cancel, {removeModal()})
  observeEvent(input$merge_ok, {
    # Get name and location information from all site data
    finalName <- input$mergeSiteName
    finalLat <- filter(reactive_objects$allSites, uniqueID %in% finalName) %>%
      st_drop_geometry() %>%
      dplyr::select(Latitude) %>% 
      pull()
    finalLong <- filter(reactive_objects$allSites, uniqueID %in% finalName) %>%
      st_drop_geometry() %>%
      dplyr::select(Longitude) %>% 
      pull()
    
    
    # update data with finalStationID, reviewer, and reviewer comments
    sites_Merged <- userEnteredStationDataTable() %>% 
      st_drop_geometry() %>%
      mutate(finalStationID = finalName,
             Reviewer = reactive_objects$reviewer, 
             ReviewComment = input$mergeComment,
             Latitude = finalLat,
             Longitude = finalLong)

    # add the current site(s) to the merged list 
    reactive_objects$sites_Merged <- bind_rows(reactive_objects$sites_Merged, sites_Merged)
    
    dropMe <- reactive_objects$sites_Merged['originalStationID'] %>% pull()
    
    ## Remove Site from "to do' list
    reactive_objects$sitesUnique <- filter(reactive_objects$sitesUnique, !(originalStationID %in% dropMe))
    

    ### Clear modal
    removeModal()
  })
  
  # add merged sites to map if they exist?
  observe({
    req(reactive_objects$sites_Merged)
    ## Update proxy map
    if(nrow(reactive_objects$sites_Merged) > 0){
      map_proxy %>% 
        # have to manually clear old sites to 'wipe' leaflet memory of joined sites
        clearGroup("Sites") %>%
        addCircleMarkers(data=reactive_objects$sites_Merged,
                         layerId = ~finalStationID,
                         label=~finalStationID, group="Merged Sites", 
                         color='black', fillColor='purple', radius = 5,
                         fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T) %>%
        addCircleMarkers(data=reactive_objects$sitesUnique,
                         layerId = ~originalStationID,
                         label=~originalStationID, group="Sites", 
                         color='yellow', fillColor='red', radius = 5,
                         fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T) %>%
        addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                         overlayGroups = c('Sites','Accepted Sites','Rejected Sites',
                                           'Merged Sites','Existing Stations','Assessment Regions'),
                         options=layersControlOptions(collapsed=T),
                         position='topleft') }  })
  
  ## Clear all selected sites
  observeEvent(input$clear_all, {
    reactive_objects$namesToSmash=NULL
    map_proxy %>%
      clearGroup(group='highlight')
  })
  
  
  
#### Review Selected Sites ####
  
  output$selected_sites_table <- DT::renderDataTable({
    req(reactive_objects$namesToSmash)
    datatable(userEnteredStationDataTable(),
              rownames = F, options = list(dom = 't', scrollX= TRUE, scrollY = '100px')) 
  })
  
  output$existing_selected_sites_table <- DT::renderDataTable({
    req(reactive_objects$namesToSmash)
    datatable(filter(existingStations, FDT_STA_ID %in% reactive_objects$namesToSmash),
              # neat trick to avoid text wrapping of super long columns, keeps size of table consistent at single row
              class = 'nowrap display',
              rownames = F, options = list(dom = 't', scrollX= TRUE, scrollY = '75px')) 
  })
  
  output$accepted_sites_table <- DT::renderDataTable({
    req(reactive_objects$sites_Accepted)
    datatable(filter(reactive_objects$sites_Accepted, finalStationID %in% reactive_objects$namesToSmash),
              # neat trick to avoid text wrapping of super long columns, keeps size of table consistent at single row
              class = 'nowrap display',
              rownames = F, options = list(dom = 't', scrollX= TRUE, scrollY = '75px'))  })
  
  output$rejected_sites_table <- DT::renderDataTable({
    req(reactive_objects$sites_Rejected)
    datatable(filter(reactive_objects$sites_Rejected, originalStationID %in% reactive_objects$namesToSmash),
              # neat trick to avoid text wrapping of super long columns, keeps size of table consistent at single row
              class = 'nowrap display',
              rownames = F, options = list(dom = 't', scrollX= TRUE, scrollY = '75px')) })
  
  output$merged_sites_table <- DT::renderDataTable({
    req(reactive_objects$sites_Merged)
    datatable(filter(reactive_objects$sites_Merged, finalStationID %in% reactive_objects$namesToSmash),
              # neat trick to avoid text wrapping of super long columns, keeps size of table consistent at single row
              class = 'nowrap display',
              rownames = F, options = list(dom = 't', scrollX= TRUE, scrollY = '75px')) })
  

### Data manipulation and export process ####
  
  
  observeEvent(input$checkOut, {
    showModal(modalDialog(title = 'reactive check out', size = 'l', easyClose = TRUE,
                         # print(reactive_objects$sites_Rejected),
                          p('reject'),
                          DT::renderDataTable({
                            datatable(as.data.frame(reactive_objects$sites_Rejected), 
                                      rownames=FALSE, options = list(scrollY = '400px', paging = FALSE, scrollX=TRUE))}),#, dom="t"))}),
                          p('reactive rejected sites'),
                          DT::renderDataTable({
                            datatable(as.data.frame(rejected_Sites()), 
                                      rownames=FALSE, options = list(scrollY = '400px', paging = FALSE, scrollX=TRUE))}),
                          p('together'),
                          DT::renderDataTable({
                            datatable(as.data.frame(rbind(Accepted_Data(), Merged_Data())), 
                                      rownames=FALSE, options = list(scrollY = '400px', paging = FALSE, scrollX=TRUE))})
    ))})

  # Have to do merged and accepted separately in case one doesnt exist even though same manipulation steps
  Accepted_Data <- reactive({
    if(!is.null(reactive_objects$sites_Accepted)){
      # avoid doubling all the data, just get the things we messed with
      dplyr::select(reactive_objects$sites_Accepted, UID, originalStationID, finalStationID, Latitude, Longitude) %>%
        right_join( # but first drop finalStationID and original lat/long
          dplyr::select(reactive_objects$sites_Adjusted, -c(finalStationID, Latitude, Longitude)),
          by = c('UID','originalStationID')) %>%
        filter(!is.na(finalStationID))
    } else {
      z <- reactive_objects$sites_Adjusted[1,]
      z[,1:length(z)] <- NA
      return(z)  }  })
  
  Merged_Data <- reactive({
    if(!is.null(reactive_objects$sites_Merged)){
      # avoid doubling all the data, just get the things we messed with
      dplyr::select(reactive_objects$sites_Merged, UID, originalStationID, finalStationID, Latitude, Longitude) %>%
        right_join( # but first drop finalStationID and original lat/long
          dplyr::select(reactive_objects$sites_Adjusted, -c(finalStationID, Latitude, Longitude)),
          by = c('UID','originalStationID')) %>%
        filter(!is.na(finalStationID))
    } else {
      z <- reactive_objects$sites_Adjusted[1,]
      z[,1:length(z)] <- NA
      return(z)  } })
  
  Rejected_Data <- reactive({
    if(!is.null(reactive_objects$sites_Rejected)){
      mutate(reactive_objects$sites_Rejected, type = 'Reject') %>% # add a rejection marker since the finalID isnt changed so you can find it later
        dplyr::select(UID, originalStationID, finalStationID, Latitude, Longitude, type) %>%
        right_join( # but first drop finalStationID and original lat/long
          dplyr::select(reactive_objects$sites_Adjusted, -c(finalStationID, Latitude, Longitude)),
          by = c('UID','originalStationID')) %>%
        filter(!is.na(type)) %>% # filter by rejection
        dplyr::select(-type) # drop marker from user
    } else {
      z <- reactive_objects$sites_Adjusted[1,]
      z[,1:length(z)] <- NA
      return(z)  }  })
  

  
  # Export reviews
  export_file=reactive(paste0('site-reviews-', input$reviewer,'-', Sys.Date(),'.xlsx'))
  output$exp_rev <- downloadHandler(
    filename=function(){export_file()},
    content = function(file) {writexl::write_xlsx(
      list(Accepted_and_Merged_Sites = as.data.frame(bind_rows(reactive_objects$sites_Accepted,
                                                               reactive_objects$sites_Merged)),
           Accepted_and_Merged_Sites_Data = as.data.frame(
             bind_rows(Accepted_Data(), Merged_Data())),
           Rejected_Sites = as.data.frame(reactive_objects$sites_Rejected),
           Rejected_Sites_Data = as.data.frame(bind_rows(Rejected_Data())),
           Missing_Station_Info = as.data.frame(reactive_objects$notEnoughInfo),
           inputData = as.data.frame(reactive_objects$sites_input)),
      path = file, format_headers=F, col_names=T) }) 
  
  
}


shinyApp(ui, server)

