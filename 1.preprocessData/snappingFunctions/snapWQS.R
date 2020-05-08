
# function to find sites with +1 segment
snapCheck <- function(successDataFrame){
  successDataFrame %>%
    group_by(`Point Unique Identifier`) %>%
    filter(n()>1)
}


# snap a point to multiline at a given distance and return the multilinestring feature

snap_bufferMethod <- function(POINT, MULTILINESTRING, distance){
  step1 <- st_buffer(POINT,dist = distance)
  st_zm(MULTILINESTRING) %>% 
    filter(st_intersects(., st_zm(step1), sparse = FALSE))
}


# snap single point to multiline features from input sequence of buffer distances and return whether 
# a feature was found within the given buffer distances

snap_Point_to_Feature <- function(POINT, # sf POINT file
                                  POINT_UID_colname, # as.character(name of unique identifier in POINT file)
                                  MULTILINESTRING, # stream network
                                  bufferDistances # numeric sequence of distances to run buffer, these will be in
){              # the unit of the POINT and MULTILINESTRING files
  
  x <- 0
  repeat {
    x <- x + 1
    b <- snap_bufferMethod(POINT,MULTILINESTRING,bufferDistances[x])
    if (nrow(b) > 0 | x == length(bufferDistances)) break   }
  
  cn <- as.character(unique(st_set_geometry(POINT,NULL) %>% select_(POINT_UID_colname)))
  
  if( nrow(b) == 0 ){
    b <- tibble(`Point Unique Identifier` = cn,
                `Buffer Distance` = paste('No connections within', max(bufferDistances),
                                          st_crs(POINT)$units, sep = ' '))
  } else {
    b <- mutate(b,`Point Unique Identifier` = cn,
                `Buffer Distance` = paste(bufferDistances[x], 
                                          st_crs(POINT)$units, sep = ' ')) %>%
      dplyr::select(`Point Unique Identifier`, `Buffer Distance`, WQS_ID) %>%
      st_drop_geometry()}
  return(b)
}


# Snap multiple points to multiline feature and return list of outputs 

snap_Points_to_Feature <- function(MULTIPOINT, # sf MULTIPOINT file
                                   POINT_UID_colname, # as.character(name of unique identifier in POINT file)
                                   MULTILINESTRING, # stream network
                                   bufferDistances # numeric sequence of distances to run buffer, these will be in
                                   # the unit of the MULTIPOINT and MULTILINESTRING files)
                                   ){              
  
  # place to store stuff
  tbl_output <- tibble(`Point Unique Identifier`= character(), `Buffer Distance` = character(), WQS_ID = character())
  
  # Don't love using a loop here but can't figure out better way at present
  for(i in 1:nrow(MULTIPOINT)) {
    print(paste('Snapping Point ',i,' of ',nrow(MULTIPOINT), sep = ''))
    z <- snap_Point_to_Feature(MULTIPOINT[i,], POINT_UID_colname, MULTILINESTRING, bufferDistances)
    tbl_output <-  suppressWarnings( tbl_output %>% bind_rows(z))
  }
  return(tbl_output)
}




snapAndOrganizeWQS <- function(sites, # sf MULTIPOINT file
                               POINT_UID_colname,# as.character(name of unique identifier in POINT file)
                               WQSlayer,# stream network
                               bufferDistances, # numeric sequence of distances to run buffer, these will be in
                               # the unit of the MULTIPOINT and MULTILINESTRING files
                               WQStable ){
  

  # transform to Albers for spatial intersection, do this inside function to not change layer used in other analyses
  sites <- sites %>% st_transform(102003)  
  WQSlayer <- WQSlayer %>% st_transform(102003) 
  
  snapList_WQS <- snap_Points_to_Feature(sites, POINT_UID_colname,
                                         WQSlayer, bufferDistances)
  WQStable <- bind_rows(WQStable,
                        rename(snapList_WQS, 'StationID'= 'Point Unique Identifier') %>%
                          dplyr::select(StationID, WQS_ID, `Buffer Distance`))
  
  return(WQStable)
}
  