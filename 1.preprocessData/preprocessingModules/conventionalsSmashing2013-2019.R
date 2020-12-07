# Bring in 2020 IR data pull (2013-2018 data), will still need to bring in 2017&2018 sites
# Goal: filter out all BRRO sites to get a list of which sites were sampled each year
#  and frequency if possible
conventionals <- read_csv('C:/HardDriveBackup/R/GitHub/Rivers-StreamsAssessment/R&S_app_v4/data/final2020data/CEDSWQM_2020_IR_DATA-CONVENTIONALS_20190305.csv') 
conventionals$FDT_DATE_TIME2 <- as.POSIXct(conventionals$FDT_DATE_TIME, format="%m/%d/%Y %H:%M")

#summary(conventionals$FDT_DATE_TIME2)

# Now add recent data (2018-Nov 2019- the day Roger made the data pull)
# already limited to BRRO (SCRO and WCRO)
conventionals2 <- read_excel('C:/HardDriveBackup/R/GitHub/AmbientNetworkPlanning/for2020/data/CEDSWQM_CONVENTIONALS_2018+.xlsx')
conventionals2$FDT_DATE_TIME2 <- as.POSIXct(conventionals2$FDT_DATE_TIME, format="%m/%d/%Y %H:%M")
conventionals2$FDT_DATE_TIME <- as.character(conventionals2$FDT_DATE_TIME) # for smashing with original conventionals
summary(conventionals2$FDT_DATE_TIME2)

# filter data to just 2019 to not duplicate data from 2018
conventionals2019 <- filter(conventionals2, FDT_DATE_TIME2 > '2018-12-31 23:59:00')
summary(conventionals2019$FDT_DATE_TIME2)
# cool.
#glimpse(conventionals2019)

# what is in conventionals that isn't in conventionals2019??
names(conventionals)[!names(conventionals) %in% names(conventionals2019)]

conventionalsAll <- bind_rows(conventionals,conventionals2019) %>%
  # get groundwater sites out of here
  filter(FDT_SPG_CODE != 'GW')

conventionals <- filter(conventionalsAll, !is.na(Latitude)|!is.na(Longitude)) # remove sites without coordinates

write_csv(conventionals, 'data/final2020data/CEDSWQM_2020_IR_DATA-CONVENTIONALS_20190305.csv')

rm(conventionalsAll);rm(conventionals2);rm(conventionals2019)
