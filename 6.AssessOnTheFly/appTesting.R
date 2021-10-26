source('global.R')

assessmentLayer <- st_read('data/GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326))


regionResults <- statewideResults[['TRO']]  

stationTableResults <- left_join(regionResults$`Assessment Results`$stationTableResults,
                                 dplyr::select(regionResults$stationGIS_View,
                                               STATION_ID = Station_Id, LATITUDE = Latitude, LONGITUDE = Longitude),
                                 by = 'STATION_ID')
runSummary <- summarizeRuns(regionResults$stationFieldData)
assessmentSummary <- stationSummary(stationTableResults, parameterEXCcrosswalk) 

View(
runSummary %>% 
  #filter(Fdt_Collector_Id == 'RJS' & Fdt_Run_Id %in% c('W21A1','W21A2', 'W21D1')) %>% 
  group_by(Fdt_Collector_Id) %>% 
  mutate(`Station Count` = `Stations Per Run` * `Times Completed`) %>% 
  summarise(`Unique Runs Completed` = length(unique(Fdt_Run_Id)),
            `Total Runs Completed` = sum(`Times Completed`),
            `Unique Stations Monitored` = sum(`Stations Per Run`),
            `Total Stations Monitored` = sum(`Station Count`))
)


# View map of regional overview
indStatusMap('Overall Status', assessmentSummary)


# summarize monitoring by run or station month
stationFieldData <- regionResults$stationFieldData
byWhat <- 'Run ID'
monthlyBreakdown <- function(stationFieldData, byWhat){
  if(byWhat == 'Run ID'){
    z <- stationFieldData %>% 
      mutate(SampleMonth = month(Fdt_Date_Time, label = T, abbr = F)) %>% 
      dplyr::select(SampleMonth, Fdt_Run_Id, Fdt_Collector_Id) %>% 
      arrange(SampleMonth, Fdt_Run_Id) %>% 
      group_by(Fdt_Run_Id, SampleMonth) %>% distinct() 
  } else {
    z <- stationFieldData %>% 
      mutate(SampleMonth = month(Fdt_Date_Time, label = T, abbr = F)) %>% 
      dplyr::select(SampleMonth, Fdt_Sta_Id, Fdt_Collector_Id) %>% 
      arrange(SampleMonth, Fdt_Sta_Id) %>% 
      group_by(Fdt_Sta_Id, SampleMonth) %>% distinct()  }
  
  return(z %>% pivot_wider(names_from = SampleMonth, values_from = Fdt_Collector_Id, names_sep = ', ') )

}

z <- monthlyBreakdown(regionResults$stationFieldData, 'StationID')

datatable(z, rownames = F, escape= F, extensions = 'Buttons',
          options = list(dom = 'Bit', scrollX = TRUE, scrollY = '350px',selection = 'none',
                         pageLength = nrow(z) ))

 
 
# calculate QA percentage by program code
left_join(
  regionResults$stationFieldData %>% 
    distinct(Fdt_Collector_Id) %>% 
    arrange(Fdt_Collector_Id),
  regionResults$stationFieldData %>% 
    filter(Fdt_Spg_Code == 'QA') %>% 
    group_by(Fdt_Collector_Id) %>% 
    summarise(`QA Samples` = length(unique(Fdt_Date_Time))) %>% 
    left_join(
      regionResults$stationFieldData %>% 
        group_by(Fdt_Collector_Id) %>% 
        summarise(`Total Samples` = length(unique(Fdt_Date_Time))),
      by = 'Fdt_Collector_Id'  ) %>% 
    mutate(`QA Sample Percentage` = format(`QA Samples` / `Total Samples` * 100, digits = 2)),
  by = 'Fdt_Collector_Id'  ) 
  
write.csv(
regionResults$stationFieldData %>% 
  filter(Fdt_Collector_Id %in% c('RJS', 'SMH')), 'FieldData_Jan-July2021.csv', row.names = F)

  

 
 