

#=============== Fish Tissue ==========================


#======================================================
Fish_PCB_fun=function(x){
  
  stopifnot(exprs = {is.numeric(x)})
  
  case_when(
    x >= 18 & x < 20 ~ "DEQ_18",
    x >= 20 & x < 100 ~ "DEQ_20",
    x >= 100 & x < 500 ~ "VDH_100",
    x >= 500 ~ "VDH_500",
    TRUE ~ NA_character_
  )}


#===============  PCBs Fish Tissue =======================================

#bold font indicates analyte concentration greater than or equal to "new" DEQ's screening value of 18 ppb
#purple font indicates analyte concentration greater than or equal to DEQ's screening value of 20 ppb
#blue font indicates analyte concentration greater than or equal to  VDH "lower" level of concern of 100 ppb
#red font indicates analyte concentration greater than or equal to VDH "upper" level of concern of 500 ppb

PCBs_FISH_2018_19 = read_csv("Fish_Tissue/PCBs_FISH_2018_19.csv")%>%
  janitor::clean_names(.,"screaming_snake")%>%
  mutate_at(vars("FSF_COLL_DATETIME"),~lubridate::mdy(.)) %>%
  mutate(Exceedance_Type=Fish_PCB_fun(TSR_CORR_RECOV_WET),
         Non_Detect=ifelse(TSR_CORR_RECOV_WET>0,"NO","YES"))


match_fish_stations=left_join(PCBs_FISH_2018_19,FishMetals_Cleaned,by=c("FSF_STA_ID"="Station_ID2"))


#=========== Metals screening values===========================

Metal_SV =read_csv("Fish_Tissue/Metal_SV.csv")

#==============================================================


Fish_Metals_Cleaned = read_csv("Fish_Tissue/Fish_Metals_Cleaned.csv") %>%
  select_at(vars(-contains("QA")))%>%
  mutate_at(vars("Collection_Date"),~lubridate::mdy(.))%>%
  mutate(Year=year(Collection_Date)) %>%
  pivot_longer("Be":"Pb", names_to = "Abv", values_to = "Result") %>%
  left_join(Metal_SV,by="Abv") %>%
  mutate(Year=year(Collection_Date),
         DEQ_Old=ifelse(Result>DEQ_Old_SV,"YES","NO"),
         DEQ_New=ifelse(Result>DEQ_New_SV,"YES","NO"),
         VDH=ifelse(Result>VDH_SV,"YES","NO"),
         PQLim=ifelse(Result>PQL,"YES","NO")) %>%
  pivot_longer("DEQ_Old":"PQLim", names_to = "Screening_Value", values_to = "Exceedance") %>%
  filter(Exceedance=="YES",Screening_Value %in% c("DEQ_New","VDH"))%>%
  group_by(Year,Metal,Screening_Value)%>%
  tally(name="No_Samples") %>%
  mutate(Percent=No_Samples/240*100)



##==== Fish tissue: PCBs in sediment==============

Sediment_PCBs = read_csv("Fish_Tissue/Sediment_PCBs.csv")%>%
  select(1:9) %>%
  mutate(ERM=180,PEC=676) %>% # ERM is supposed to be saltwater PEC Tidal Fresh
  mutate(Exceedance=case_when(
    Total_PCB_ppb >ERM ~  "ERM Exceedance",
    Total_PCB_ppb >PEC ~  "PEC Exceedance",
    TRUE~NA_character_))


Sediment_PCBs %>%
  group_by(Year) %>%
  summarise(Samples=n_distinct(DEQ_ID,Date),
            Stations=n_distinct(DEQ_ID))
