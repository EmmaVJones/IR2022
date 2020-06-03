cit <- read_csv('James Beckley/csv/2019VASOS_Data for DEQ_approvedMuddy.csv')

sites_input <- cit
sites_Adjusted <- reassignColumns(sites_input, `Station ID`, Latitude, Longitude) %>%#`Station Name`, `Latitude DD`, `Longitude DD`) %>%#Group_Station_ID, Latitude, Longitude)%>%
  # add UID now so it trickles through continuously to all other variables
  group_by(originalStationID, Latitude, Longitude) %>%
  mutate(UID = group_indices()) %>%#row_number()) %>%
  dplyr::select(UID, everything()) %>%
  ungroup()
notEnoughInfo <- filter(sites_Adjusted, is.na(originalStationID) | is.na(Latitude) | is.na(Longitude)) # separate sites without location information or identifier)
sitesUnique <- filter(sites_Adjusted, !is.na(originalStationID) & !is.na(Latitude) & !is.na(Longitude))  %>% # drop sites without location information
  distinct(originalStationID, Latitude, Longitude, .keep_all =T)  %>% #distinct by location and name
  st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
           remove = F, # don't remove these lat/lon cols from df
           crs = 4326)
allSites <- suppressWarnings(
  dplyr::select(sitesUnique, originalStationID, Latitude, Longitude) %>%
    dplyr::rename('uniqueID' = 'originalStationID') %>%
    # sf objects use rbind for quieter results than dplyr::bind_rows()
    rbind(dplyr::select(existingStations, STATION_ID, Latitude, Longitude) %>%
            #rbind(dplyr::select(existingStations, FDT_STA_ID, Latitude, Longitude) %>%
            dplyr::rename('uniqueID' = 'STATION_ID')) )

# Empty accepted sites 
sites_Accepted <- sites_Adjusted[0,]
# Empty rejected sites 
sites_Rejected <- sites_Adjusted[0,]
# Empty merged sites 
sites_Merged = sites_Adjusted[0,]


namesToSmash <- "16-PL28-Quander-QC1" #"JB1"                 "09-PL30-Acc-ACC2"

userEnteredStationDataTable <- filter(sitesUnique, originalStationID %in% namesToSmash)




observeEvent(input$merge_ok, {
  # Get name and location information from all site data
  finalName <- "16-PL28-Quander-QC1"#input$mergeSiteName
  finalLat <- filter(allSites, uniqueID %in% finalName) %>%
    st_drop_geometry() %>%
    dplyr::select(Latitude) %>% 
    pull()
  finalLong <- filter(allSites, uniqueID %in% finalName) %>%
    st_drop_geometry() %>%
    dplyr::select(Longitude) %>% 
    pull()
  
  
  # update data with finalStationID, reviewer, and reviewer comments
  sites_Merged <- userEnteredStationDataTable %>% 
    st_drop_geometry() %>%
    mutate(finalStationID = finalName,
           Reviewer = 'evj', 
           ReviewComment = 'merge',
           Latitude = finalLat,
           Longitude = finalLong)
  
  # add the current site(s) to the merged list 
  sites_Merged <- bind_rows(sites_Merged, sites_Merged)
  
  dropMe <- sites_Merged['originalStationID'] %>% pull()
  
  ## Remove Site from "to do' list
  sitesUnique <- filter(sitesUnique, !(originalStationID %in% dropMe))
  


  namesToSmash <- "JB1" 
  userEnteredStationDataTable <- filter(sitesUnique, originalStationID %in% namesToSmash)
  

observeEvent(input$accept_ok, {
  
  # update data with finalStationID, reviewer, and reviewer comments
  updatedData <- userEnteredStationDataTable %>% #filter(reactive_objects$sitesUnique, originalStationID %in% reactive_objects$namesToSmash) %>%  # Fix sitesUnique for table to populate accepted information #
    st_drop_geometry() %>%
    mutate(finalStationID = originalStationID,
           Reviewer = 'evj', 
           ReviewComment = 'accept')
  
  # add the current site(s) to the accepted list 
  sites_Accepted <- bind_rows(sites_Accepted, updatedData)
  
  