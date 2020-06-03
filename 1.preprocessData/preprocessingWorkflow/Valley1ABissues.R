stations1A <- filter(conventionals_DWQS, gsub("_","",str_extract(FDT_STA_ID, ".{2}")) %in% 
                        str_pad(unique(filter(subbasinOptionsByWQStype, SubbasinOptions %in% '1A')$SubbasinOptions), 
                                width = 2, side = 'left', pad = '0'))
stations1B <- filter(conventionals_DWQS, gsub("_","",str_extract(FDT_STA_ID, ".{2}")) %in% 
                       str_pad(unique(filter(subbasinOptionsByWQStype, SubbasinOptions %in% '1B')$SubbasinOptions), 
                               width = 2, side = 'left', pad = '0'))

mapview(stations1A, color = 'red') + mapview(stations1B, color = 'black')+mapview(assessmentRegions) + mapview(basin7) + mapview(assessmentLayer)
