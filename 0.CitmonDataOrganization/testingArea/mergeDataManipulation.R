shinyApp(ui, server)

cit <- read_csv('James Beckley/csv/2019VASOS_Data for DEQ_approvedRocky.csv') %>%
  reassignColumns(`Station ID`,Latitude, Longitude)

sites_Adjusted <- cit %>%
#  reassignColumns(RCAStationID, LAT, LON) %>%
  group_by(originalStationID, Latitude, Longitude) %>%
  mutate(UID = group_indices()) %>%#row_number()) %>%
  dplyr::select(UID, everything()) %>%
  ungroup()

notEnoughInfo <- filter(sites_Adjusted, is.na(originalStationID) | is.na(Latitude) | is.na(Longitude)) 

sitesUnique <- filter(sites_Adjusted, !is.na(originalStationID) & !is.na(Latitude) & !is.na(Longitude))  %>% # drop sites without location information
  distinct(originalStationID, Latitude, Longitude, .keep_all =T)  %>% #distinct by location and name
  #mutate(UID = row_number()) %>%
  #dplyr::select(UID, everything()) %>%
  st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
           remove = F, # don't remove these lat/lon cols from df
           crs = 4326) # add coordinate reference system, needs to be geographic for now bc entering lat/lng
#sitesData <- sites_Adjusted %>%
#  group_by(originalStationID, Latitude, Longitude) %>%
#  mutate(UID = group_indices()) %>%
#  dplyr::select(UID, everything()) %>% 
#  ungroup()


allSites <- suppressWarnings(
  dplyr::select(sitesUnique, originalStationID, Latitude, Longitude) %>%
    dplyr::rename('uniqueID' = 'originalStationID') %>%
    # sf objects use rbind for quieter results than dplyr::bind_rows()
    rbind(dplyr::select(existingStations, FDT_STA_ID, Latitude, Longitude) %>%
            dplyr::rename('uniqueID' = 'FDT_STA_ID')) ) 

## Merge sites just in user dataset

# working on merge feature with originalStationID %in% c('R07','R7','4AROA-R07-FC')

siteid <- c('R07','R7','4AROA-R07-FC')

namesToSmash <- filter(sitesUnique, originalStationID %in% siteid) %>%
  st_drop_geometry() %>%
  mutate(sites = originalStationID) %>%
  bind_rows(existingSiteMatches <- filter(existingStations, 
                                          FDT_STA_ID %in% siteid) %>%
              mutate(sites = FDT_STA_ID) %>%
              dplyr::select(sites) %>% 
              st_drop_geometry() ) %>%
  dplyr::select(sites) %>% 
  pull() 

# Give optionsas a renderUI selectInput
finalName <- unique(namesToSmash)[3] # This could include DEQ names???
finalLat <- filter(allSites, uniqueID %in% finalName) %>%
  st_drop_geometry() %>%
  dplyr::select(Latitude) %>% 
  pull()
finalLong <- filter(allSites, uniqueID %in% finalName) %>%
  st_drop_geometry() %>%
  dplyr::select(Longitude) %>% 
  pull()

sites_Merged <- filter(sitesUnique, originalStationID %in% siteid) %>% 
  st_drop_geometry() %>%
  mutate(finalStationID = finalName,
         Reviewer = 'evj',#reactive_objects$reviewer, 
         ReviewComment = 'merged', #input$acceptComment)
         # merge correct lat/long
         Latitude = finalLat,
         Longitude = finalLong)
  
  


sites_Accepted <- filter(sitesUnique, originalStationID %in% '1.1') %>% 
  st_drop_geometry() %>%
  mutate(finalStationID = originalStationID,
         Reviewer = 'evj',#reactive_objects$reviewer, 
         ReviewComment = 'accept as is')#input$acceptComment)


sites_Rejected <-  filter(sitesUnique, originalStationID %in% '1.2') %>% 
  st_drop_geometry() %>%
  mutate(#finalStationID = originalStationID,
         Reviewer = 'evj',#reactive_objects$reviewer, 
         ReviewComment = 'reject', #input$acceptComment)
         type = 'Reject') # add a rejection marker since the finalID isnt changed so you can find it later

# make one clean dataset for data reorganization
acceptAndMerge <- bind_rows(sites_Accepted, sites_Merged) %>%
  # avoid doubling all the data, just get the things we messed with
  dplyr::select(UID, originalStationID, finalStationID, Latitude, Longitude) %>%
  right_join( # but first drop finalStationID and original lat/long
    dplyr::select(sites_Adjusted, -c(finalStationID, Latitude, Longitude)),
    by = c('UID','originalStationID')) %>%
  filter(!is.na(finalStationID))
  



sites_Rejected_data <- sites_Rejected %>%
  dplyr::select(UID, originalStationID, finalStationID, Latitude, Longitude, type) %>%
  right_join( # but first drop finalStationID and original lat/long
    dplyr::select(sites_Adjusted, -c(finalStationID, Latitude, Longitude)),
    by = c('UID','originalStationID')) %>%
  filter(!is.na(type)) %>% # filter by rejection
  dplyr::select(-type) # drop marker from user




bind_rows(sites_Accepted, sites_Merged) %>%
  dplyr::select(UID, originalStationID, finalStationID)

distinct(dplyr::select(sitesData, -c(finalStationID, Latitude, Longitude)),UID, .keep_all = T) %>%
  dplyr::select(UID, originalStationID)

dplyr::select(sitesUnique, UID, originalStationID)







### work on replacing certain columns from chosen site sites to merged sites and data


# Give optionsas a renderUI selectInput
finalName <- unique(namesToSmash)[3] # This could include DEQ names???
finalLat <- filter(allSites, uniqueID %in% finalName) %>%
  st_drop_geometry() %>%
  dplyr::select(Latitude) %>% 
  pull()
finalLong <- filter(allSites, uniqueID %in% finalName) %>%
  st_drop_geometry() %>%
  dplyr::select(Longitude) %>% 
  pull()

sites_Merged <- filter(sitesUnique, originalStationID %in% siteid) %>% 
  st_drop_geometry() %>%
  mutate(finalStationID = finalName,
         Reviewer = 'evj',#reactive_objects$reviewer, 
         ReviewComment = 'merged', #input$acceptComment)
         # merge correct lat/long
         Latitude = finalLat,
         Longitude = finalLong)

# find common columns between existing stations and user input
matchingColumns <- names(existingStations)[names(existingStations) %in% names(sites_Merged)]
# Remove columns that I need from that list to avoid accidentally overwriting
columnsToMerge <- matchingColumns[!(matchingColumns %in% c('Group_Station_ID','Latitude','Longitude'))] # input$IDfield1_UI, input$IDfield2_UI, input$IDfield3_UI

z <- bind_rows(sitesUnique %>% st_drop_geometry(), existingStations %>% st_drop_geometry()) 

z <- list(sitesUnique %>% st_drop_geometry(), existingStations %>% st_drop_geometry()) %>%
  dplyr::bind_rows() %>% 
  readr::type_convert()

z <- vec_rbind(sitesUnique %>% st_drop_geometry(), existingStations %>% st_drop_geometry(), .ptype = sitesUnique %>% st_drop_geometry()) %>%
  as.tibble()


