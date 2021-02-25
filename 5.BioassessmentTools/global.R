library(tidyverse)
library(shinydashboard)
library(sf)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(inlmisc)
library(DT)
library(plotly)
library(lubridate)
library(pool)
library(pins)
library(config)
library(readxl)
library(rmarkdown)

#####################################   UPDATE EACH NEW TOOL REBUILD #############################################
# Establish Assessment Period 
assessmentPeriod <- as.POSIXct(c("2015-01-01 00:00:00 UTC","2020-12-31 23:59:59 UTC"),tz='UTC')
assessmentCycle <- '2022'
##################################################################################################################

source('helperFunctions/VSCI_metrics_GENUS.R')
source('helperFunctions/VCPMI_metrics_GENUS.R')

# Register RStudio Connect, don't need to do multiple times
#board_register("rsconnect", server = "http://deq-rstudio-prod.cov.virginia.gov:3939")

# get configuration settings
conn <- config::get("connectionSettings")

board_register_rsconnect(key = conn$CONNECT_API_KEY,  #Sys.getenv("CONNECT_API_KEY"),
                         server = conn$CONNECT_SERVER)#Sys.getenv("CONNECT_SERVER"))



# Retrieve Pins
#WQM_Station_Full <- pin_get("ejones/WQM-Station-Full", board = "rsconnect")
#Wqm_Stations_View <- pin_get("ejones/WQM-Stations-View", board = "rsconnect")
#WQM_Stations <- pin_get("ejones/WQM-Sta-GIS-View", board = "rsconnect")

# limit data to assessment window From the start
benSamps <- pin_get("ejones/benSamps", board = "rsconnect") %>%
  filter(between(`Collection Date`, assessmentPeriod[1], assessmentPeriod[2])) %>%# limit data to assessment window
  filter(RepNum %in% c('1', '2')) %>% # drop QA and wonky rep numbers
  filter(`Target Count` == 110) # only assess rarified data
habSamps <- pin_get("ejones/habSamps", board = "rsconnect") %>%
  filter(between(`Collection Date`, assessmentPeriod[1], assessmentPeriod[2]))# limit data to assessment window
Wqm_Stations_View <- pin_get("ejones/WQM-Stations-View", board = "rsconnect") %>%
  filter(Sta_Id %in% benSamps$StationID)
WQM_Stations <- pin_get("ejones/WQM-Sta-GIS-View", board = "rsconnect") %>%
  filter(Station_Id %in% benSamps$StationID)
#WQM_Station_Full <- pin_get("ejones/WQM-Station-Full", board = "rsconnect") %>%
#  filter(WQM_STA_ID %in% benSamps$StationID) %>%
#  distinct(WQM_STA_ID, .keep_all = T) %>%
benSampsStations <- st_as_sf(pin_get("ejones/benSampsStations", board = "rsconnect")) %>%
  filter(StationID %in% benSamps$StationID)
benSamps <- left_join(benSamps, benSampsStations, by = 'StationID') %>% # update with spatial, assess reg, vahu6, basin/subbasin, & ecoregion info
  dplyr::select(StationID, Sta_Desc, everything()) %>% 
  arrange(StationID)


VSCIresults <- pin_get("ejones/VSCIresults", board = "rsconnect") %>%
  filter(BenSampID %in% benSamps$BenSampID)
VCPMI63results <- pin_get("ejones/VCPMI63results", board = "rsconnect") %>%
  filter(BenSampID %in% benSamps$BenSampID)
VCPMI65results <- pin_get("ejones/VCPMI65results", board = "rsconnect") %>%
  filter(BenSampID %in% benSamps$BenSampID)
benthics <- pin_get("ejones/benthics", board = "rsconnect") %>%
  filter(BenSampID %in% benSamps$BenSampID)
habValues <- pin_get("ejones/habValues", board = "rsconnect") %>%
  filter(HabSampID %in% habSamps$HabSampID)
habObs <- pin_get("ejones/habObs", board = "rsconnect") %>%
  filter(HabSampID %in% habSamps$HabSampID)
#masterTaxaGenus <- pin_get("ejones/masterTaxaGenus", board = "rsconnect")



# Template to standardize variables for DT habitat heatmap across high and low gradients
habitatTemplate <- tibble(StationID = NA, HabSampID = NA, `Collection Date` = NA, `HabSample Comment` = NA, `Total Habitat Score` = NA, `Bank Stability` = NA, 
                          `Channel Alteration` = NA, `Channel Flow Status` = NA, `Channel Sinuosity` = NA, Embeddedness = NA, 
                          `Epifaunal Substrate / Available Cover` = NA, `Pool Substrate Characterization` = NA, `Pool Variability` = NA, 
                          `Frequency of riffles (or bends)` = NA, `Riparian Vegetative Zone Width` = NA, `Sediment Deposition` = NA, 
                          `Vegetative Protection` = NA, `Velocity / Depth Regime` = NA)



totalHabScore <- function(habValues){
  habValues %>%
    group_by(HabSampID) %>%
    summarise(`Total Habitat Score` = sum(HabValue, na.rm = T))
}

averageTotHab_windows <- function(habValues_totHab){
  habValues_totHab %>%
    group_by(StationID) %>%
    summarise(`Total Habitat Average` = format(mean(`Total Habitat Score`, na.rm = T), digits = 3),
              `n Samples` = n()) %>% 
    mutate(Window = paste0('IR ', assessmentCycle, ' (6 year Average)')) %>%
    dplyr::select(StationID, Window, `Total Habitat Average`, `n Samples`) %>%
    # Two Year Average
    bind_rows(habValues_totHab %>%
                filter(year(`Collection Date`) %in% c(2019, 2020)) %>%
                group_by(StationID) %>%
                summarise(`Total Habitat Average` = format(mean(`Total Habitat Score`, na.rm = T), digits = 3),
                          `n Samples` = n()) %>% ungroup() %>%
                mutate(Window = as.character('2019-2020 Average')) %>% 
                dplyr::select(StationID, Window, everything())) %>%
    bind_rows(habValues_totHab %>%
                group_by(StationID, Season) %>%
                summarise(`Total Habitat Average` = format(mean(`Total Habitat Score`, na.rm = T), digits = 3),
                          `n Samples` = n()) %>%
                rename('Window' = 'Season') %>% ungroup()) %>%
    bind_rows(habValues_totHab %>%
                mutate(Window = year(`Collection Date`)) %>%
                group_by(StationID, Window) %>%
                summarise(`Total Habitat Average` = format(mean(`Total Habitat Score`, na.rm = T), digits=3),
                          `n Samples` = n()) %>% ungroup() %>%
                mutate(Window = as.character(Window))) %>% 
    mutate(Window = factor(Window, levels = c('IR 2022 (6 year Average)', '2019-2020 Average',
                                              'Spring', 'Fall', '2020', '2019', '2018', '2017', '2016', '2015'))) %>%
    arrange(StationID, Window)
}


averageSCI_windows <- function(benSamps_Filter_fin, SCI_filter, assessmentCycle){
  dat <- left_join(benSamps_Filter_fin, dplyr::select(SCI_filter, StationID, BenSampID, SCI, `SCI Score`),
                   by = c('StationID', 'BenSampID')) 
  dat %>%
    # IR window Average
    group_by(StationID, SCI) %>%
    summarise(`SCI Average` = format(mean(`SCI Score`, na.rm = T), digits = 3),
              `n Samples` = n()) %>% 
    mutate(Window = paste0('IR ', assessmentCycle, ' (6 year Average)'))  %>%
    dplyr::select(SCI, Window, `SCI Average`, `n Samples`) %>%
    # Two Year Average
    bind_rows(dat %>%
                filter(year(`Collection Date`) %in% c(2019, 2020)) %>%
                group_by(StationID, SCI) %>%
                summarise(`SCI Average` = format(mean(`SCI Score`, na.rm = T), digits=3),
                          `n Samples` = n()) %>% ungroup() %>%
                mutate(Window = as.character('2019-2020 Average')) %>% 
                dplyr::select(StationID, SCI, Window, everything())) %>%
    # Add Yearly Averages
    bind_rows(dat %>%
                mutate(Window = year(`Collection Date`)) %>%
                group_by(StationID, SCI, Window) %>%
                summarise(`SCI Average` = format(mean(`SCI Score`, na.rm = T), digits=3),
                          `n Samples` = n()) %>% ungroup() %>%
                mutate(Window = as.character(Window))) %>%
    # Add seasonal averages
    bind_rows(dat %>%
                group_by(StationID, SCI, Season) %>%
                summarise(`SCI Average` = format(mean(`SCI Score`, na.rm = T), digits = 3),
                          `n Samples` = n()) %>%
                rename('Window' = 'Season') %>% ungroup()) %>%
    mutate(Window = factor(Window, levels = c('IR 2022 (6 year Average)', '2019-2020 Average',
                                              'Spring', 'Fall', '2020', '2019', '2018', '2017', '2016', '2015'))) %>%
    arrange(StationID, SCI, Window)
}





### Functions for fact sheet generation

# Check user uploaded data against pinned data
pinCheck <- function(pinName, userUpload){
  pinnedDecisions <- pin_get(pinName, board = 'rsconnect')
  removeOlderDecisions <- filter(pinnedDecisions, StationID %in% userUpload$StationID)
  overwriteOld <- bind_rows(filter(pinnedDecisions, ! StationID %in% removeOlderDecisions$StationID),
                            userUpload) %>%
    group_by(StationID) %>%
    mutate(n = n())
  
  # double check no duplicates
  if(nrow(filter(overwriteOld, n > 1)) == 0 ){
    # pin back to server
    pin(dplyr::select(overwriteOld, -n), 
        name = 'IR2022bioassessmentDecisions_test', 
        description = paste0('Test dataset for developing IR2022 bioassessment fact sheet tool ', Sys.time()), 
        board = 'rsconnect')
    return('pin updated on server')
  } else {
    return(filter(filter(overwriteOld, n > 1)))  }
}
#pinCheck('IR2022bioassessmentDecisions_test', userUpload)

# Do all habitat things efficiently

habitatConsolidation <- function( userStationChoice, habSamps, habValues){
  habSampsUserSelection <- filter(habSamps, StationID %in% userStationChoice) 
  habValuesUserSelection <- filter(habValues, HabSampID %in% habSampsUserSelection$HabSampID)
  totalHabitat <- habSampsUserSelection %>%
    group_by(HabSampID) %>%
    # get total habitat values
    left_join(totalHabScore(habValuesUserSelection), by = 'HabSampID') %>%
    mutate(Season = factor(Season,levels=c("Spring","Outside Sample Window","Fall"))) %>%
    dplyr::select(StationID, HabSampID, everything()) %>%
    arrange(`Collection Date`) %>%
    ungroup() 
  
  habitatCrosstab <- bind_rows(habitatTemplate,
                               left_join(habValuesUserSelection, 
                                         dplyr::select(habSampsUserSelection, HabSampID, StationID, `Collection Date`),
                                         by = 'HabSampID') %>%
                                 group_by(StationID, HabSampID, `Collection Date`) %>%
                                 arrange(HabParameterDescription) %>% ungroup() %>%
                                 pivot_wider(id_cols = c('StationID','HabSampID','Collection Date'), names_from = HabParameterDescription, values_from = HabValue) %>%
                                 left_join(dplyr::select(totalHabitat, HabSampID, `HabSample Comment`, `Total Habitat Score`), by = 'HabSampID') %>%
                                 dplyr::select(StationID, HabSampID, `Collection Date`, `HabSample Comment`, `Total Habitat Score`, everything()) ) %>%
    drop_na(StationID) %>%
    arrange(StationID, `Collection Date`) 
  return(habitatCrosstab)
}

# Raw Bug data results for Report
rawBugData <- function(SCI){
  bioResultsTableTemplate <- tibble(StationID = NA, `Collection Date` = NA, `Replicate Number` = NA, SCI = NA, 
                                    `Spring SCI Score` = NA, `Fall SCI Score` = NA)
  bind_rows(bioResultsTableTemplate, 
            SCI %>%
              group_by(StationID, `Collection Date`, SCI, RepNum) %>%
              dplyr::select(StationID, `Collection Date`, `Replicate Number` = RepNum, SCI, Season, `SCI Score`) %>%
              mutate(`Collection Date` = as.Date(`Collection Date`),#, format = '%M-%D-%Y'),
                     Season = paste0(Season, ' SCI Score')) %>%
              pivot_wider(names_from = Season, values_from = `SCI Score`) ) %>%
    drop_na(StationID) %>%
    arrange(`Collection Date`, `Replicate Number`)
}

# SCI Statistics for report
SCIstatistics <- function(SCI1){
  suppressMessages(suppressWarnings(
    SCI1 %>%
      # IR window Average
      group_by(StationID, SCI) %>%
      summarise(`SCI Average` = format(mean(`SCI Score`, na.rm = T), digits = 3),
                `n Samples` = n()) %>% 
      mutate(Window = paste0('IR ', assessmentCycle, ' (6 year) Average'))  %>%
      dplyr::select(SCI, Window, `SCI Average`, `n Samples`) %>%
      # Two Year Average
      bind_rows(SCI1 %>%
                  filter(year(`Collection Date`) %in% c(2019, 2020)) %>%
                  group_by(StationID, SCI) %>%
                  summarise(`SCI Average` = format(mean(`SCI Score`, na.rm = T), digits=3),
                            `n Samples` = n()) %>% ungroup() %>%
                  mutate(Window = as.character('2019-2020 Average')) %>% 
                  dplyr::select(StationID, SCI, Window, everything())) %>%
      # Two Year Spring Average
      bind_rows(SCI1 %>%
                  filter(year(`Collection Date`) %in% c(2019, 2020) & Season == 'Spring') %>%
                  group_by(StationID, SCI) %>%
                  summarise(`SCI Average` = format(mean(`SCI Score`, na.rm = T), digits=3),
                            `n Samples` = n()) %>% ungroup() %>%
                  mutate(Window = as.character('2019-2020 Spring Average')) %>% 
                  dplyr::select(StationID, SCI, Window, everything())) %>%
      # Two Year Fall Average
      bind_rows(SCI1 %>%
                  filter(year(`Collection Date`) %in% c(2019, 2020) & Season == 'Fall') %>%
                  group_by(StationID, SCI) %>%
                  summarise(`SCI Average` = format(mean(`SCI Score`, na.rm = T), digits=3),
                            `n Samples` = n()) %>% ungroup() %>%
                  mutate(Window = as.character('2019-2020 Fall Average')) %>% 
                  dplyr::select(StationID, SCI, Window, everything())) %>%
      # Add seasonal averages
      bind_rows(SCI1 %>%
                  group_by(StationID, SCI, Season) %>%
                  mutate(Season = paste0('IR ', assessmentCycle, ' (6 year) ', Season,' Average')) %>%
                  summarise(`SCI Average` = format(mean(`SCI Score`, na.rm = T), digits = 3),
                            `n Samples` = n()) %>%
                  rename('Window' = 'Season') %>% ungroup()) %>%
      mutate(Window = factor(Window, levels = c('2019-2020 Average', '2019-2020 Spring Average', '2019-2020 Fall Average', 'IR 2022 (6 year) Average', 
                                                'IR 2022 (6 year) Spring Average', 'IR 2022 (6 year) Fall Average'))) %>%
      arrange(StationID, SCI, Window)  %>% ungroup() ) )
}


# SCI plot for report
SCIresultsPlot <- function(SCI, assessmentDecision){
  if(unique(assessmentDecision$AssessmentMethod) == 'VSCI'){
    mutate(SCI, `Collection Date` = as.Date(`Collection Date`)) %>% 
      ggplot(aes(x = `Collection Date`, y = `SCI Score`, fill=Season)) +
      geom_col()+
      scale_fill_manual("Season", values = c("Fall" = "black", "Spring" = "dark grey"))+
      labs(x="Collection Year", y="VSCI Score") +
      scale_y_continuous(#name="VSCI", 
        breaks=seq(0, 100, 10),limits=c(0,100)) +
      scale_x_date(date_labels = '%Y') +
      geom_hline(yintercept=60, color="red", size=1)+
      theme(axis.text.x=element_text(angle=45,hjust=1))
  } else {
    mutate(SCI, `Collection Date` = as.Date(`Collection Date`)) %>% 
      ggplot(aes(x = `Collection Date`, y = `SCI Score`, fill=Season)) +
      geom_col()+
      scale_fill_manual("Season", values = c("Fall" = "black", "Spring" = "dark grey"))+
      labs(x="Collection Year", y="VCPMI Score") +
      scale_y_continuous(#name="VSCI", 
        breaks=seq(0, 100, 10),limits=c(0,100)) +
      scale_x_date(date_labels = '%Y') +
      geom_hline(yintercept=60, color="red", size=1)+
      theme(axis.text.x=element_text(angle=45,hjust=1))
  }
}

# SCI metrics table for report
SCImetricsTable <- function(SCI){
  SCI %>%
    mutate(`Collection Date` = as.Date(`Collection Date`)) %>% 
    group_by(StationID, `Collection Date`, SCI, RepNum) %>%
    dplyr::select(StationID, `Collection Date`, `Replicate Number` = RepNum, Season, SCI, `SCI Score`,`Family Total Taxa`:`Fam %MFBI Score`, 
                  `Family %5 Dominant`:PctIntol) %>%
    #clean up empty columns with a quick pivot longer (with drop na) and then back to wide
    pivot_longer(cols = `Family Total Taxa`:PctIntol, names_to = 'metric', values_to = 'metricVal', values_drop_na = TRUE) %>%
    pivot_wider(names_from = metric, values_from = metricVal) %>% ungroup() %>% 
    arrange(`Collection Date`, `Replicate Number`)
}


## Habitat plot for report
habitatPlot <- function(habitat){
  if(nrow(habitat) > 0){
    minDate <- as.Date(as.character("2015-01-01") , origin ="%Y-%m-%d")
    maxDate <- as.Date(as.character("2020-12-31"), origin ="%Y-%m-%d")# add min and max dates to make rectagle plotting easier, starting at 6 month buffer by can play with
    
    habitat %>%
      mutate(`Collection Date` = as.Date(`Collection Date`)) %>% 
      ggplot(aes(x = `Collection Date`, y = `Total Habitat Score`))+
      #geom_bar(stat="identity")
      annotate("rect", xmin=minDate, xmax=maxDate, ymin=150 ,  ymax=Inf, alpha=1, fill="#0072B2")+ 
      annotate("rect",xmin=minDate, xmax=maxDate, ymin=130, ymax=150, alpha=1, fill="#009E73" ) +
      annotate("rect",xmin=minDate, xmax=maxDate, ymin=100, ymax=130, alpha=1, fill="#F0E442") +
      annotate("rect",xmin=minDate, xmax=maxDate, ymin=-Inf, ymax=100, alpha=1, fill="firebrick" ) +
      geom_bar(stat="identity", width = 75)+
      theme(axis.text=element_text(size=14, face="bold"),
            axis.title=element_text(size=14, face="bold"),
            legend.position = "none") +
      scale_y_continuous(name="Total Habitat Score", breaks=seq(0, 200, 25),limits=c(0,200)) +
      scale_x_date(date_breaks='1 year', date_labels =  "%Y")+
      theme(axis.text.x=element_text(angle=45,hjust=1))  }
}

# Habitat Table for final Report
habitatDTcoloredTable <- function(habitat){
  if(nrow(habitat) > 0){
    habitatTable <- habitat %>%
      mutate(`Collection Date` = as.Date(`Collection Date`)) %>% 
      dplyr::select(-HabSampID) %>%
      #clean up empty columns with a quick pivot longer (with drop na) and then back to wide
      pivot_longer(cols = `Bank Stability`:`Velocity / Depth Regime`, names_to = 'metric', values_to = 'metricVal', values_drop_na = TRUE) %>%
      pivot_wider(names_from = metric, values_from = metricVal) %>% ungroup() %>% 
      arrange(`Collection Date`)
    
    habBreaks<-seq(0,20, 1)
    habClrs<-c('firebrick', 'firebrick','firebrick','firebrick','firebrick','firebrick', "#F0E442","#F0E442","#F0E442","#F0E442","#F0E442", 
               "#009E73","#009E73","#009E73","#009E73","#009E73", "#0072B2","#0072B2","#0072B2","#0072B2","#0072B2")
    
    DT::datatable(habitatTable, escape=F, rownames = F, options=list(pageLength=nrow(habitatTable),dom= 'Bt', scrollX=TRUE)) %>% 
      formatStyle('Total Habitat Score', backgroundColor = "lightgray") %>%
      formatStyle(names(habitatTable)[5:length(habitatTable)],  backgroundColor = styleEqual(habBreaks, habClrs), alpha=0.1,
                  textAlign = 'center')  }
}