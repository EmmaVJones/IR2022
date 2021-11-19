source('global.R')

assessmentLayer <- st_read('data/GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326))


regionResults <- statewideResults[['PRO']]  

stationTableResults <- left_join(regionResults$`Assessment Results`$stationTableResults,
                                 dplyr::select(regionResults$stationGIS_View,
                                               STATION_ID = Station_Id, LATITUDE = Latitude, LONGITUDE = Longitude),
                                 by = 'STATION_ID') %>% 
  dplyr::select(-contains("_STAT")) %>% # don't give user status info
  rename(`Bacteria STV Stats` = BACTERIASTATS, `Preliminary Bacteria Decision` = BACTERIADECISION) %>% 
  left_join(regionResults$Conventionals %>% 
              group_by(FDT_STA_ID) %>% 
              summarise(SPGsummary = paste0(unique(FDT_SPG_CODE, collapse = ' | '))) %>% 
              summarise(SPGsummary = paste0(SPGsummary, collapse = ' | ')),
            by = c('STATION_ID' = 'FDT_STA_ID')) %>% 
  dplyr::select(STATION_ID, Sta_Desc, SPGsummary, TEMP_EXC:LONGITUDE, everything())  %>% 
  filter(!is.na(LATITUDE) | !is.na(LONGITUDE))

stationTableResultsYTD <- left_join(regionResults$`Assessment Results YTD`$stationTableResults,
                                 dplyr::select(regionResults$stationGIS_ViewYTD,
                                               STATION_ID = Station_Id, LATITUDE = Latitude, LONGITUDE = Longitude),
                                 by = 'STATION_ID') %>% 
  dplyr::select(-contains("_STAT")) %>% # don't give user status info
  rename(`Bacteria STV Stats` = BACTERIASTATS, `Preliminary Bacteria Decision` = BACTERIADECISION) %>% 
  left_join(regionResults$ConventionalsYTD %>% 
              group_by(FDT_STA_ID) %>% 
              summarise(SPGsummary = paste0(unique(FDT_SPG_CODE, collapse = ' | '))) %>% 
              summarise(SPGsummary = paste0(SPGsummary, collapse = ' | ')),
            by = c('STATION_ID' = 'FDT_STA_ID')) %>% 
  dplyr::select(STATION_ID, Sta_Desc, SPGsummary, TEMP_EXC:LONGITUDE, everything())  %>% 
  filter(!is.na(LATITUDE) | !is.na(LONGITUDE))


#filter(stationTableResults, str_detect(SPGsummary, 'AW'))

runSummary <- summarizeRuns(regionResults$stationFieldData)
assessmentSummary <- stationSummary(stationTableResults, parameterEXCcrosswalk) 
stationSummary_ <- stationSummary(stationTableResultsYTD, parameterEXCcrosswalk) 


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
indStatusMap('Overall Status', assessmentSummary, 'All Stations')
indStatusMap('E.coli Geomean', assessmentSummary, 'HF')

z <- filter(stationTableResults, STATION_ID %in% '2-JKS023.61') %>% #input$regionalMap_marker_click$id) %>% 
  #filter_at(vars(ends_with("_EXC")), any_vars( . > 0)) %>% 
  dplyr::select(STATION_ID, Sta_Desc, TEMP_EXC:LONGITUDE, everything())
datatable(z, rownames = F, escape= F, extensions = 'Buttons',
          options = list(dom = 'Bit', scrollX = TRUE, scrollY = '350px',selection = 'none',
                         pageLength = nrow(z),
                         columDefs = list(list(visible=FALSE, targets= c(2)))))

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

  




z <- stationTableResults %>% #filter(stationTableResults, STATION_ID %in% input$regionalMap_marker_click$id) %>% 
  #filter_at(vars(ends_with("_EXC")), any_vars( . > 0)) %>% 
  dplyr::select(STATION_ID, Sta_Desc, TEMP_EXC:LONGITUDE, everything()) %>% 
  dplyr::select(-contains("_STAT"))
  arrange(desc(TEMP_EXC))
datatable(z, rownames = F, escape= F, extensions = c('Buttons','FixedColumns'),
          options = list(dom = 'Bift', scrollX = TRUE, scrollY = '350px',selection = 'none',
                         pageLength = nrow(z), fixedColumns = list(leftColumns = 1),
                         columnDefs = list(list(visible=FALSE, targets=c("TEMP_STAT" ,    "DO_STAT" ,      "PH_STAT"))), #columnsToHide)),
                         buttons=list('copy')) ) %>% #,
                                      #list(extend='excel',filename=paste0('ExceedanceSummary',input$regionChoice)))) ) %>% 
  formatStyle(c('TEMP_EXC','TEMP_SAMP'), 'TEMP_EXC', backgroundColor = styleInterval(0, c(NA, 'yellow'))) # highlight all exceedances yellow
  
  
  
  formatStyle(c('TEMP_EXC','TEMP_SAMP','TEMP_STAT'), 'TEMP_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow', 'yellow'))) %>%
  formatStyle(c('DO_EXC','DO_SAMP','DO_STAT'), 'DO_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow', 'yellow'))) %>%
  formatStyle(c('PH_EXC','PH_SAMP','PH_STAT'), 'PH_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow', 'yellow'))) %>%
  formatStyle(c('ECOLI_EXC','ECOLI_SAMP','ECOLI_GM_EXC','ECOLI_GM_SAMP','ECOLI_STAT'), 'ECOLI_STAT', backgroundColor = styleEqual(c('IM'), c('yellow'))) %>%
  formatStyle(c('ENTER_SAMP','ENTER_EXC',"ENTER_GM_EXC","ENTER_GM_SAMP",'ENTER_STAT'), 'ENTER_STAT', backgroundColor = styleEqual(c('IM'), c('yellow'))) %>%
  formatStyle(c('AMMONIA_EXC','AMMONIA_STAT'), 'AMMONIA_STAT', backgroundColor = styleEqual(c('Review', 'IM'), c('yellow'))) %>%
  formatStyle(c('NUT_TP_EXC','NUT_TP_SAMP'), 'NUT_TP_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow'))) %>% 
  formatStyle(c('NUT_CHLA_EXC','NUT_CHLA_SAMP'), 'NUT_CHLA_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow'))) 




 
 