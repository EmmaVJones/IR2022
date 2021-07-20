z <- #suppressWarnings(
  stationAnalyteDataUserFilter0 %>% 
  
  filter(Ana_Sam_Fdt_Id %in% '2880155') %>%#  c('3000675', "3000674")) %>% ##'2986075') %>%  
  
    dplyr::select(Ana_Sam_Fdt_Id, Ana_Sam_Mrs_Container_Id_Desc,  ParameterName, Ana_Uncensored_Value) %>% 
  group_by(Ana_Sam_Fdt_Id, ParameterName) %>% 
  mutate(n = as.factor(n())) %>% #filter(Ana_Sam_Fdt_Id == '3001273') 
  arrange(desc(n), ParameterName) %>% 
  ungroup() %>% group_by(Ana_Sam_Fdt_Id) %>% 
  summarise(uniqueN = length(unique(n)))
  

test <- left_join(stationFieldDataUserFilter1, z,
                  by = c("FDT_ID" = "Ana_Sam_Fdt_Id"))

if(any(z$uniqueN > 1)){
  zz <- filter(z, uniqueN > 1)
  
  # clean up Ana_Sam_Fdt_Id that have multiple n's
  for( i in unique(zz$Ana_Sam_Fdt_Id)){
    zzID <- filter(z, Ana_Sam_Fdt_Id == i)
    #if(length(unique(zzID$n)) > 1){
      zzIDfine <- filter(zzID, n == max(unique(zzID$n)))
      zzIDfix <- filter(zzID, n != max(unique(zzID$n)))
      
      zzIDfinalFix <- zzIDfine[0,]
      for(k in unique(zzIDfix$ParameterName)){
        #print(k)
        zzIDfix1 <- filter(zzIDfix, ParameterName == k) %>% 
          mutate(dataSource = 'real')
        zzIDPlaceholder <- mutate(zzIDfix1, dataSource = 'fake',
                                  Ana_Uncensored_Value = as.numeric(NA)) %>%
          slice(1)
        
        zzIDfixed <- bind_rows(zzIDfix1,
                               zzIDPlaceholder %>% slice(rep(row_number(), max(unique(zzID$n)) - nrow(zzIDfix1)   ))) %>% 
          group_by(ParameterName) %>% 
          mutate(n = n()) 
        zzIDfinalFix <- bind_rows(zzfinalIDFix, zzfixed)
      }
      zzfinalFix <- bind_rows(zzfine, zzfinalFix)
    #} 
  }
  
}



  
zzz <- zz %>% # zzfinalFix %>% #zz[1:15,] %>% 
    pivot_wider(id_cols = Ana_Sam_Fdt_Id, 
                names_from = ParameterName, values_from = Ana_Uncensored_Value, values_fn = list()) %>% 
    unnest(-Ana_Sam_Fdt_Id)#)
rm(zzz)

zz <- #suppressWarnings(
  stationAnalyteDataUserFilter0 %>% 
  dplyr::select(Ana_Sam_Fdt_Id, Ana_Sam_Mrs_Container_Id_Desc,  ParameterName, Ana_Uncensored_Value) %>% 
  group_by(Ana_Sam_Fdt_Id, ParameterName) %>% 
  mutate(n = n()) %>% 
  filter(n>1) %>% 
  arrange(Ana_Sam_Fdt_Id, ParameterName)


xx <- stationAnalyteDataUserFilter0 %>%
  
  filter(Ana_Sam_Fdt_Id %in% unique( stationAnalyteDataUserFilter0$Ana_Sam_Fdt_Id)[1:59]) %>% 
  
  dplyr::select(Ana_Sam_Fdt_Id, Ana_Sam_Mrs_Container_Id_Desc,  ParameterName, Ana_Uncensored_Value) %>% 
  pivot_wider(id_cols = Ana_Sam_Fdt_Id, 
              names_from = ParameterName, values_from = Ana_Uncensored_Value, values_fn = list()) %>% 
  unnest(-Ana_Sam_Fdt_Id)
