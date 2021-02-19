


library(readxl)

# for Fish Tissue use TSR_CORR_RECOV_WET
# for sediment use TSR_CORR_RECOV_DRY

#===================== Fish==================
To_DEQ_19NF_samples = readxl::read_excel("C:/HardDriveBackup/Assessment/IR2022/Fish Tissue/VIMSdata/To_DEQ_19NF samples.xlsx") %>%
rename(FSS_LAB_NUM=FSS_LAB_NUM...1)%>%
group_by(FSS_LAB_NUM,FSF_STA_ID, FSF_COLL_DATETIME, SPD_SPECIES_LENGTH,SPD_SPECIES_WEIGHT)%>%
summarise(Total_PCBs=sum(TSR_CORR_RECOV_WET,na.rm = T)) %>%
  dplyr::select(`VIMS ID` = FSS_LAB_NUM, FDT_STA_ID = FSF_STA_ID, FDT_DATE_TIME = FSF_COLL_DATETIME,
                 `Species Length` = SPD_SPECIES_LENGTH, `Species Weight` = SPD_SPECIES_WEIGHT, `Total PCBs` = Total_PCBs)


#====================Sed======================
path="C:/HardDriveBackup/Assessment/IR2022/Fish Tissue/VIMSdata/2012_2018_VIMS_DATA/2012_2018_VIMS_fish_sediment_data.xlsx"

sheets=c("qry_DEQ_Final_PCB_MSpec_2018all","qry_DEQ_Final_PCB_MSpec_2017sed",
         "qry_DEQ_Final_PCB_MSpec_2016sed","2015_Seds+Blanks",
         "qry_DEQ_Final_PCB_MSpec_2014fis","qry_DEQ_Final_PCB_MSpec_2013sed")


Cleaned_PCB_Sed=map2_dfr(path,sheets,~read_excel(.x,sheet=.y)%>%
           rename(FSS_LAB_NUM=FSS_LAB_NUM...1)%>%
           filter(PSP_SPE_CODE_ALPHA=="SED")%>%
           group_by(FSS_LAB_NUM,FSF_COLL_DATETIME,FSF_STA_ID)%>%
           summarise(Total_sed_PCBs=sum(TSR_CORR_RECOV_DRY,na.rm=T)))%>%
  rename(FDT_STA_ID = FSF_STA_ID)

