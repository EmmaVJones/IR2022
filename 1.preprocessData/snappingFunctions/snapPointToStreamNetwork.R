
# function to find sites with +1 segment
snapCheck <- function(successDataFrame){
  successDataFrame %>%
    group_by(StationID) %>%
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
                                  bufferDistances, # numeric sequence of distances to run buffer, these will be in
                                  # the unit of the POINT and MULTILINESTRING files
                                  fieldDesired # name of field you want from the spatial dataset
){
  
  POINT_UID_colnamequo <- enquo(POINT_UID_colname)
  fieldDesiredquo <- enquo(fieldDesired)
  
  x <- 0
  repeat {
    x <- x + 1
    b <- snap_bufferMethod(POINT,MULTILINESTRING,bufferDistances[x])
    if (nrow(b) > 0 | x == length(bufferDistances)) break   }
  
  cn <- as.character(unique(st_set_geometry(POINT,NULL) %>% select(!!POINT_UID_colnamequo)))
  
  if( nrow(b) == 0 ){
    b <- tibble(`Point Unique Identifier` = cn,
                `Buffer Distance` = paste('No connections within', max(bufferDistances),
                                          st_crs(POINT)$units, sep = ' '))
  } else {
    b <- mutate(b,`Point Unique Identifier` = cn,
                `Buffer Distance` = paste(bufferDistances[x], 
                                          st_crs(POINT)$units, sep = ' ')) %>%
      dplyr::select(`Point Unique Identifier`, `Buffer Distance`, !!fieldDesiredquo) %>%
      st_drop_geometry()}
  return(b)
}


# Snap multiple points to multiline feature and return list of outputs 

snap_Points_to_Feature <- function(MULTIPOINT, # sf MULTIPOINT file
                                   POINT_UID_colname, # as.character(name of unique identifier in POINT file)
                                   MULTILINESTRING, # stream network
                                   bufferDistances, # numeric sequence of distances to run buffer, these will be in
                                   # the unit of the MULTIPOINT and MULTILINESTRING files)
                                   fieldDesired # name of field you want from the spatial dataset
                                   ){              
  
  # place to store stuff
  #tbl_output <- tibble(`Point Unique Identifier`= character(), `Buffer Distance` = character(), ID305B = character())
  tbl_output <- tibble(`Point Unique Identifier`= character(), `Buffer Distance` = character())#, {{fieldDesired}} := character()) # rlang trick
  
  
  # Don't love using a loop here but can't figure out better way at present
  for(i in 1:nrow(MULTIPOINT)) {
    print(paste('Snapping Point ',i,' of ',nrow(MULTIPOINT), sep = ''))
    z <- snap_Point_to_Feature(MULTIPOINT[i,], POINT_UID_colname, MULTILINESTRING, bufferDistances, fieldDesired)
    tbl_output <-  suppressWarnings( tbl_output %>% bind_rows(z))
  }
  return(tbl_output)
}


#sites <- distinctSites_AUtoDo[1:5,] 
#POINT_UID_colname <- 'FDT_STA_ID'
#streamNetwork <- riverineAU 
#bufferDistances = seq(20,80,by=20)  # buffering by 20m from 20 - 80 meters
#tableToAppendTo <- distinctSites_AU
#fieldDesired <- "ID305B"

snapAndOrganize <- function(sites, # sf MULTIPOINT file
                               POINT_UID_colname,# as.character(name of unique identifier in POINT file)
                               streamNetwork,# stream network spatial layer
                               bufferDistances, # numeric sequence of distances to run buffer, these will be in
                               # the unit of the MULTIPOINT and MULTILINESTRING files
                               tableToAppendTo,
                              fieldDesired # name of field you want from the spatial dataset
                              ){
  
  fieldDesiredquo <- enquo(fieldDesired)

  # transform to Albers for spatial intersection, do this inside function to not change layer used in other analyses
  sites <- sites %>% st_transform(102003)  
  streamNetwork <- streamNetwork %>% st_transform(102003) 
  
  snapList <- snap_Points_to_Feature(sites, POINT_UID_colname,
                                     streamNetwork, bufferDistances, fieldDesired)
  tableToAppendTo <- bind_rows(tableToAppendTo,
                        rename(snapList, 'StationID'= 'Point Unique Identifier') %>%
                          dplyr::select(StationID, !!fieldDesiredquo, `Buffer Distance`))
  
  return(tableToAppendTo)
}
  