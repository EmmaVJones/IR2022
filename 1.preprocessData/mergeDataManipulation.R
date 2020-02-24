shinyApp(ui, server)



sites_Adjusted <- cit
sitesUnique <- filter(sites_Adjusted, !is.na(originalStationID), !is.na(Latitude)|!is.na(Longitude))  %>% # drop sites without location information
  distinct(originalStationID, Latitude, Longitude, .keep_all =T)  %>% #distinct by location and name
  mutate(UID = row_number()) %>%
  dplyr::select(UID, everything()) %>%
  st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
           remove = F, # don't remove these lat/lon cols from df
           crs = 4326) # add coordinate reference system, needs to be geographic for now bc entering lat/lng
sitesData <- sites_Adjusted %>%
  group_by(originalStationID, Latitude, Longitude) %>%
  mutate(UID = group_indices()) %>%
  dplyr::select(UID, everything()) %>% 
  ungroup()


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


# make one clean dataset for data reorganization
acceptAndMerge <- bind_rows(sites_Accepted, sites_Merged) %>%
  # avoid doubling all the data, just get the things we messed with
  dplyr::select(UID, originalStationID, finalStationID, Latitude, Longitude) %>%
  right_join( # but first drop finalStationID and original lat/long
    dplyr::select(sitesData, -c(finalStationID, Latitude, Longitude)),
    by = c('UID','originalStationID')) %>%
  filter(!is.na(finalStationID))
  
bind_rows(sites_Accepted, sites_Merged) %>%
  dplyr::select(UID, originalStationID, finalStationID)

distinct(dplyr::select(sitesData, -c(finalStationID, Latitude, Longitude)),UID, .keep_all = T) %>%
  dplyr::select(UID, originalStationID)
