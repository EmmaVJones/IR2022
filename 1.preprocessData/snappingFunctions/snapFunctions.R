# R 3.5.1

# These functions are detailed in StreamCatTools/snapFunctions.Rmd


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
      dplyr::select(`Point Unique Identifier`, `Buffer Distance`, everything())  }
  return(b)
}


# Snap multiple points to multiline feature and return list of outputs 

snap_Points_to_Feature_List <- function(MULTIPOINT, # sf MULTIPOINT file
                                        POINT_UID_colname, # as.character(name of unique identifier in POINT file)
                                        MULTILINESTRING, # stream network
                                        bufferDistances # numeric sequence of distances to run buffer, these will be in
){              # the unit of the MULTIPOINT and MULTILINESTRING files)
  # Make a list to store the two types of results
  out_list <- list(sf_output = list(), tbl_output = list() )
  
  # Don't love using a loop here but can't figure out better way at present
  for(i in 1:nrow(MULTIPOINT)) {
    print(paste('Snapping Point ',i,' of ',nrow(MULTIPOINT), sep = ''))
    z <- snap_Point_to_Feature(MULTIPOINT[i,], POINT_UID_colname, MULTILINESTRING, bufferDistances)
    if("sf" %in% class(z)){
      if( length(out_list$sf_output) == 0 ){
        out_list$sf_output <- z
      } else { 
        out_list$sf_output <- suppressWarnings( out_list$sf_output <- rbind(out_list$sf_output,z) ) }
    } else {
    #  # new 2/19/2020, one last shot at finding line
    #  print(paste('Snapping Point ',i,' of ',nrow(MULTIPOINT),' BONUS BUFFER', sep = ''))
    #  z <- snap_Point_to_Feature(MULTIPOINT[i,], POINT_UID_colname, MULTILINESTRING, 500)
    #  
    #  if("sf" %in% class(z)){
    #    if( length(out_list$sf_output) == 0 ){
    #      out_list$sf_output <- rbind(out_list$sf_output,z)
    #    } else {
          out_list$tbl_output <-  suppressWarnings( out_list$tbl_output %>% bind_rows(z) ) }
    #  }
    #}
  }
  
  # Report Results
  print('Use objectName$sf_output to view successful connections.')
  print('Use objectName$tbl_output to view sites that did not connect to any segments within the input buffer distances.')
  
  return(out_list)
}

