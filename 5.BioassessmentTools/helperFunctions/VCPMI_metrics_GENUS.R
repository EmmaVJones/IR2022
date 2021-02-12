

#sites <- c('CXB7221R110')


##benthicsAll <-benthics
#benthics <- filter(benthicsAll, BenSampID %in% sites)


## make small datasets for testing
##bugTraitsAll <- bugTraits
#bugTraits <- filter(bugTraitsAll, BenSampID %in% sites)  

##exclusionMathAll <- exclusionMath
#exclusionMath <- filter(exclusionMathAll, BenSampID %in% sites)  

#bugdatrare <- exclusionMath
#y <- bugdatrare







# ** % Dominant 5 Taxa---- *** note this is programmed differently than 2 dom metrics! (on purpose)
#y <- bugdatrare

pickTopFiveDom <- function(y){
  #y %>%
  filter(y, `Family Level Excluded Taxa` >= 0 ) %>%
    group_by(BenSampID,`Final VA Family ID`) %>% 
    summarize(fivedtot=sum(`Family Level Count`)) %>%
    top_n(n=5, wt=fivedtot) %>% #returns top n rows.  but if 2nd taxa is tied, returns all rows
    ungroup() %>% 
    group_by(BenSampID) %>%
    arrange(BenSampID, desc(fivedtot))# %>% #These two arrange lines put the family in descending order
    #slice(1:5)} #incase there is tie we don't drop them per EDAS history, this is on purpose
}

pDom5 <- function(y, bugtotals, specialName){
  top5 <- pickTopFiveDom(y)
  z <- left_join(top5, bugtotals, by ="BenSampID") %>% 
    mutate(pdom=(fivedtot/bugtotal)*100) %>%   
    select(BenSampID,pdom) %>%
    group_by(BenSampID) %>% 
    summarize(pdom5=sum(pdom))
  names(z) <- c('BenSampID',specialName)
  return(z)}
#pDom5(bugdatrare, bugtotals,'Family %5 Dominant')



# % Intolerant metric

intolInd <- function(x, bugtotals){
  x %>%
    group_by(BenSampID) %>%
    filter(metric == 'TolVal' & metric_val %in% c(0,1,2,3)) %>%
    filter(`Family Level Excluded Taxa`  >= 0) %>% # drop any excluded taxa 
    summarize(`Intolerant Individuals` = sum(`Family Level Count`)) %>%
    left_join(bugtotals, by = 'BenSampID') %>%
    group_by(BenSampID) %>%
    summarize(`%Intoler` = (`Intolerant Individuals` / bugtotal) * 100)
} # x <- bugTraits





VCPMI63calculation <- function(bugTraits,bugdatrare,vmast){
  bugtotals <- bugtotals_function(bugdatrare)
  metprop <- metprop_function(bugTraits, bugtotals)
  hiltax <- filter(vmast, metric == 'TolVal') %>%
    group_by(`Final VA Family ID`) %>%
    summarise(TolVal = mean(metric_val))
  
  # IBI Calculations
  IBI <- suppressMessages(
    rich(bugdatrare, 'Family Total Taxa') %>%
      left_join(hilsindex(bugdatrare,hiltax,'Family HBI')) %>%
      left_join(summaryStress(metprop,'ept', percent = F, 'Family EPT Taxa')) %>%
      left_join(summaryStress(metprop,'e', percent = T, 'Ephem')) %>%
      left_join(summaryStress(metprop, 'ptmin', T, 'PT - Hydropsychidae')) %>% 
      left_join(pDom5(bugdatrare, bugtotals,'Family %5 Dominant')) %>%
      left_join(summaryStress(metprop,'clinger-HS', percent = T, 'ClngP-HS')) %>% 
      replace(is.na(.), 0) %>%
      rowwise() %>%
      mutate(
        `Richness Score` = ifelse(`Family Total Taxa`/21 > 1, 100, 100 * (`Family Total Taxa` - 6.1)/(21-6.1)),
        `Richness Final` = ifelse(`Richness Score` < 0, 0, `Richness Score`),
        `HBI Score` = truncfxn((7.4 - `Family HBI`)/(7.4- 4.9) * 100),
        `HBI Final` = ifelse(`HBI Score` < 0, 0, `HBI Score`),
        `EPT Score` = ifelse( `Family EPT Taxa`/8 > 1, 100, 100 * (`Family EPT Taxa` /8 )),
        `EPT Final` = ifelse(`EPT Score` < 0, 0, `EPT Score`),
        EPHEM = truncfxn(`%Ephem`/25.4 * 100),
        `PT-H` = truncfxn(`%PT - Hydropsychidae`/8.2 * 100),
        Pct5DOM = truncfxn((100 - `Family %5 Dominant`)/(100-69.2) * 100),
        `PctClng-HS` = truncfxn(`%ClngP-HS`/18.7 * 100),
        `CPMI63+CHOWAN` = sum(`Richness Final`, `HBI Final`, `EPT Final`, EPHEM, `PT-H`, Pct5DOM, `PctClng-HS`) / 7  ) )     
  return(IBI)
}










VCPMI65calculation <- function(bugTraits,bugdatrare,vmast){
  bugtotals <- bugtotals_function(bugdatrare)
  metprop <- metprop_function(bugTraits, bugtotals)
  hiltax <- filter(vmast, metric == 'TolVal') %>%
    group_by(`Final VA Family ID`) %>%
    summarise(TolVal = mean(metric_val))
    
  # IBI Calculations
  IBI <- suppressMessages(
    rich(bugdatrare, 'Family Total Taxa') %>%
      left_join(hilsindex(bugdatrare,hiltax,'Family HBI')) %>%
      left_join(summaryStress(metprop,'ept', percent = F, 'Family EPT Taxa')) %>%
      left_join(summaryStress(metprop,'e', percent = T, 'Ephem')) %>%
      left_join(summaryStress(metprop, 'ptmin', T, 'PT - Hydropsychidae')) %>% 
      left_join(summaryStress(metprop,'clinger-HS', percent = T, 'ClngP-HS')) %>% 
      left_join(summaryStress(metprop,'scraper', T, 'Scrap')) %>% 
      left_join(intolInd(bugTraits, bugtotals)) %>%
      replace(is.na(.), 0) %>%
      rowwise() %>%
      mutate(
        `Richness Score` = ifelse(`Family Total Taxa`/22 > 1, 100, 100 * (`Family Total Taxa` - 7)/(22-7)),
        `Richness Final` = ifelse(`Richness Score` < 0, 0, `Richness Score`),
        `HBI Score` = truncfxn((7.2 - `Family HBI`)/(7.2- 4.2) * 100),
        `HBI Final` = ifelse(`HBI Score` < 0, 0, `HBI Score`),
        `EPT Score` = ifelse( `Family EPT Taxa`/9.1 > 1, 100, 100 * (`Family EPT Taxa` /9.1 )),
        `EPT Final` = ifelse(`EPT Score` < 0, 0, `EPT Score`),
        EPHEM = truncfxn(`%Ephem`/34.6 * 100),
        `PT-H` = truncfxn(`%PT - Hydropsychidae`/27.8 * 100),
        `PctScrap` = truncfxn(`%Scrap`/35.5 * 100),
        `PctClng-HS` = truncfxn(`%ClngP-HS`/46.6 * 100),
        PctIntol = truncfxn(`%Intoler`/31.3 * 100),
        `CPMI65-CHOWAN` = sum(`Richness Final`, `HBI Final`, `EPT Final`, EPHEM, `PT-H`, PctScrap, `PctClng-HS`, PctIntol) / 8  ) )     
  return(IBI)
}

# IIf([TotTaxa]/22>1,100,100*([TotTaxa]-7)/(22-7)) AS [Richness Score], 
# IIf([Richness Score]<0,0,[Richness Score]) AS RichnessFinal, 
# IIf((7.2-[HBI])/(7.2-4.2)*100>100,100,(7.2-[HBI])/(7.2-4.2)*100) AS HBIScore, 
# IIf([HBIScore]<0,0,[HBIScore]) AS HBIFinal,
# IIf([EPTTax]>0,IIf([EPTTax]/9.1>1,100,100*([EPTTax]-0)/(9.1-0)),0) AS [EPT Score], 
# IIf([EPT Score]<0,0,[EPT Score]) AS EPTFinal, 
# IIf([%Ephem]>0,IIf([%Ephem]/34.6*100>100,100,([%Ephem]-0)/(34.6-0)*100),0) AS EPHEM,
# IIf([%PT - Hydropsychidae]>0,IIf([%PT - Hydropsychidae]/27.8*100>100,100,([%PT - Hydropsychidae]-0)/(27.8-0)*100),0) AS [PT-H],
#IIf([%Scrap]/35.5*100>100,100,([%Scrap]-0)/(35.5-0)*100) AS PctScrap,
#IIf([%ClngP-HS]>0,IIf([%ClngP-HS]/46.6*100>100,100,([%ClngP-HS]-0)/(46.6-0)*100),0) AS [PctClng-HS], 
#IIf([%Intoler]>0,IIf([%Intoler]/31.3*100>100,100,([%Intoler]-0)/(31.3-0)*100),0) AS PctIntol,
#([RichnessFinal]+[HBIFinal]+[EPTFinal]+[EPHEM]+[PT-H]+[PctScrap]+[PctClng-HS]+[PctIntol])/8 AS [CPMI65-CHOWAN]
