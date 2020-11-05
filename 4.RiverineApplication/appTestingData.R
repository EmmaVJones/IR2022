# app testing data
DEQregionSelection <- 'BRRO'
basinSelection <- "James-Upper"
HUC6Selection <- 'JU11'


conventionals <- pin_get("conventionals2022IRdraft", board = "rsconnect")
vahu6 <- st_as_sf(pin_get("vahu6", board = "rsconnect")) # bring in as sf object


stationTable <- read_csv('userDataToUpload/processedStationData/stationsTable2022begin.csv')

regionalAUs <- st_zm(st_as_sf(pin_get(paste0(DEQregionSelection, 'workingAUriverine'), board = 'rsconnect'))) 
regionalAUsForTesting <- regionalAUs

the_data <- filter(vahu6, ASSESS_REG %in% DEQregionSelection) %>%
    left_join(dplyr::select(subbasinToVAHU6, VAHU6, Basin, BASIN_CODE, Basin_Code))
basin_filter <- filter(the_data, Basin_Code %in% basinSelection)
huc6_filter <- filter(basin_filter, VAHU6 %in% HUC6Selection)

AUs <- suppressWarnings(st_intersection(regionalAUs,  huc6_filter)) #filter(vahu6, VAHU6 %in% huc6_filter()$VAHU6)))})

stationSummary <- filter(conventionals, Huc6_Vahu6 %in% huc6_filter$VAHU6) %>%
    distinct(FDT_STA_ID, .keep_all = TRUE) %>%
    select(FDT_STA_ID:FDT_SPG_CODE, STA_LV2_CODE:STA_CBP_NAME) %>% 
    mutate(`Analyzed By App` = ifelse(FDT_STA_ID %in% unique(stationTable$STATION_ID), 'yes','no'))
  