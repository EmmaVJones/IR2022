# These are the metric functions that build the VSCI

# R version 3.5.1
# Adapted from code by Lou Reynolds and Leah Ettema (USEPA)

#x <- bugTraits

#metrich = taxa richness of each metric by sample
# Drop exclued taxa for richness metrics
metrich <- function(x){ x %>%
    #filter(metric_val > 0) %>%
    filter(metric_val > 0 & `Final Family Level Taxa` > 0) %>%
    group_by(BenSampID,metric)%>%
    summarise(metrich=n_distinct(`Final VA Family ID`))}


#metsum = number of individuals per metric by sample 
# Keep Excluded Taxa for this metric
#x <- bugTraits
metsum <- function(x){ x %>%
    filter(metric_val > 0) %>%
    #filter(metric_val > 0 & `Excluded Taxa` == 0) %>%
    group_by(BenSampID,metric)%>%
    summarise(metsum=sum(`Family Level Count`))}

#x <- bugdatrare
bugtotals_function <- function(x){ x %>% 
    group_by(BenSampID) %>% 
    summarise(bugtotal=sum(`Family Level Count`)) }
 

#bugtotals <- bugtotals_function(bugdatrare)
  
#metprop=proprotion of individuals in the sample with that metric  
metprop_function <- function(x,bugtotals){
  mets <- full_join(metrich(x),metsum(x),by=c('BenSampID','metric'))
  left_join(mets, bugtotals, by="BenSampID") %>% 
    mutate(metprop=metsum/bugtotal)}
#metprop <- metprop_function(bugTraits, bugtotals)


#IBI Metric Calculations----
#Because these are Family based, the bugdata must be joined to the EDAS list and converted to Family before calculations

#y <- bugdatrare

#The first 5 metrics decrease with stress
#**Total Richness----
rich <- function(y, specialName){ 
  z <- y %>%
    group_by(BenSampID) %>%
    filter(`Family Level Excluded Taxa` >= 0) %>%
    summarise(totTaxa = n_distinct(`Final VA Family ID`))
  names(z) <- c('BenSampID',specialName)
  return(z)} # Richness
#rich(bugdatrare, 'Family Total Taxa')

#metricName <- 'e'
#percent <- FALSE
#specialName <- 'Family EPT Taxa'

summaryStress <- function(metprop, metricName, percent, specialName){
  z <- filter_(metprop, lazyeval::interp(~metric == metricName)) %>%
    group_by(BenSampID) 
  if(percent == FALSE){
    justSummary <- select(z, BenSampID, metrich)
    names(justSummary) <- c('BenSampID',ifelse(is.na(specialName),metricName,specialName))
    return(justSummary)
  }else{
    percent <- mutate(z, percent = metprop*100) %>%
      select(BenSampID, percent)
    names(percent) <- c('BenSampID',paste('%',specialName,sep = ''))
    return(percent)
  }
}
#summaryStress(metprop,'ept', percent = F, NA)
#summaryStress(metprop,'ept', percent = F, 'Family EPT Taxa')
#summaryStress(metprop,'e', percent = T, 'Ephem')
#summaryStress(metprop, 'ptmin', T, 'PT - Hydro')
#summaryStress(metprop,'scraper', T, 'FamilyScraper')
#summaryStress(metprop,'chiro', T, 'Chironomid')


  
# ** % Dominant 2 Taxa---- *** note this is programmed differently than % Dom 5 metric (on purpose)
pickTopTwoDom <- function(y){
  #y %>%
  filter(y, `Family Level Excluded Taxa` >= 0 ) %>%
  group_by(BenSampID,`Final VA Family ID`) %>% 
  summarize(twodtot=sum(`Family Level Count`)) %>%
  top_n(n=2, wt=twodtot) %>% #returns top n rows.  but if 2nd taxa is tied, returns all rows
  ungroup() %>% 
  group_by(BenSampID) %>%
  arrange(BenSampID, desc(twodtot)) %>% #These two arrange lines put the family in descending order
  slice(1:2)} #incase there is tie drop extra taxa, dom 5 metrics keep everyone!

pDom2 <- function(y, bugtotals, specialName){
  top2 <- pickTopTwoDom(y)
  z <- left_join(top2, bugtotals, by ="BenSampID") %>% 
    mutate(pdom=(twodtot/bugtotal)*100) %>%   
    select(BenSampID,pdom) %>%
    group_by(BenSampID) %>% 
    summarize(pdom2=sum(pdom))
  names(z) <- c('BenSampID',specialName)
  return(z)}
#pDom2(bugdatrare, bugtotals,'Family %2 Dominant')

# **Hisenhoff Index----

#Hilsenhoff Index taxa info
#hiltax <- filter(vmast, metric == 'TolVal') %>%
#  group_by(`Final VA Family ID`) %>%
#  summarise(TolVal = mean(metric_val))


#Raw Calculations for Hilsenhoff Index and Shannon Diversity  
hilsindex <- function(y, hiltax, specialName){
  z <- left_join(y, hiltax, by="Final VA Family ID") %>%
    group_by(BenSampID)%>% 
    mutate(nxa=`Family Level Count`*TolVal, sumn=sum(`Family Level Count`)) %>% 
    filter(!is.na(TolVal) & `Final Family Level Taxa` > 0) %>% 
    group_by(BenSampID) %>% 
    summarise(hilsindex=sum(nxa)/sum(`Family Level Count`))#summarise(hilsindex=sum(nxa)/mean(sumn))
  names(z) <- c('BenSampID',specialName)
  return(z)
  }

#hilsindex(y,hiltax,'Family HBI')

#This function truncates scores to fall between 0-100
truncfxn <- function(x){ifelse(x>=100,100,x)}



# VSCI, all put together

VSCIcalculation <- function(bugTraits,bugdatrare,vmast){
  bugtotals <- bugtotals_function(bugdatrare)
  metprop <- metprop_function(bugTraits, bugtotals)
  hiltax <- filter(vmast, metric == 'TolVal') %>%
    group_by(`Final VA Family ID`) %>%
    summarise(TolVal = mean(metric_val))
  
  # IBI Calculations
  IBI <- suppressMessages(
    rich(bugdatrare, 'Family Total Taxa') %>%
      left_join(summaryStress(metprop,'ept', percent = F, 'Family EPT Taxa')) %>%
      left_join(summaryStress(metprop,'e', percent = T, 'Ephem')) %>%
      left_join(summaryStress(metprop, 'ptmin', T, 'PT - Hydropsychidae')) %>% 
      left_join(summaryStress(metprop,'scraper', T, 'FamilyScraper')) %>% 
      left_join(summaryStress(metprop,'chiro', T, 'Chiro')) %>%
      left_join(pDom2(bugdatrare, bugtotals,'Family %2 Dominant')) %>%
      left_join(hilsindex(bugdatrare,hiltax,'Family HBI')) %>% 
      replace(is.na(.), 0) %>%
      mutate(`%Ephem Score` = truncfxn((.[[4]]/61.3) * 100),
             `%PT-H Score` = truncfxn((.[[5]]/35.6) * 100),
             `Fam Richness Score` = truncfxn((.[[2]]/22)*100),
             `%Chironomidae Score` = truncfxn(100-.[[7]]),
             `Fam EPT Score` = truncfxn((.[[3]]/11) * 100),
             `Fam %Scraper Score` = truncfxn((.[[6]]/51.6) * 100),
             `Fam %2Dom Score` = truncfxn(((100-.[[8]])/69.2) * 100),
             `Fam %MFBI Score` = truncfxn(((10-.[[9]])/6.8) * 100),
             `Fam SCI` = (`Fam Richness Score` + `Fam EPT Score` + `%Ephem Score` + `%PT-H Score` +
               `Fam %Scraper Score` + `%Chironomidae Score` + `Fam %2Dom Score` + `Fam %MFBI Score`)/8))
  return(IBI)
}



