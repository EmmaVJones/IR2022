VIMS2014fish <- read_excel('C:/HardDriveBackup/Assessment/IR2022/Fish Tissue/VIMSdata/2012_2018_VIMS_DATA/2012_2018_VIMS_fish_sediment_data.xlsx',
                          sheet = 'qry_DEQ_Final_PCB_MSpec_2014fis')
VIMS2015fish <- read_excel('C:/HardDriveBackup/Assessment/IR2022/Fish Tissue/VIMSdata/2012_2018_VIMS_DATA/2012_2018_VIMS_fish_sediment_data.xlsx',
                          sheet = '2015_Fish')
VIMS2016fish <- read_excel('C:/HardDriveBackup/Assessment/IR2022/Fish Tissue/VIMSdata/2012_2018_VIMS_DATA/2012_2018_VIMS_fish_sediment_data.xlsx',
                          sheet = 'qry_DEQ_Final_PCB_MSpec_2016fis')
VIMS2016sed <- read_excel('C:/HardDriveBackup/Assessment/IR2022/Fish Tissue/VIMSdata/2012_2018_VIMS_DATA/2012_2018_VIMS_fish_sediment_data.xlsx',
                       sheet = 'qry_DEQ_Final_PCB_MSpec_2016sed')
VIMS2017fish <- read_excel('C:/HardDriveBackup/Assessment/IR2022/Fish Tissue/VIMSdata/2012_2018_VIMS_DATA/2012_2018_VIMS_fish_sediment_data.xlsx',
                       sheet = 'qry_DEQ_Final_PCB_MSpec_2017fis')
VIMS2017sed <- read_excel('C:/HardDriveBackup/Assessment/IR2022/Fish Tissue/VIMSdata/2012_2018_VIMS_DATA/2012_2018_VIMS_fish_sediment_data.xlsx',
                       sheet = 'qry_DEQ_Final_PCB_MSpec_2017sed')
VIMS2018 <- read_excel('C:/HardDriveBackup/Assessment/IR2022/Fish Tissue/VIMSdata/2012_2018_VIMS_DATA/2012_2018_VIMS_fish_sediment_data.xlsx',
                       sheet = 'qry_DEQ_Final_PCB_MSpec_2018all')
VIMS2019 <- read_excel('C:/HardDriveBackup/Assessment/IR2022/Fish Tissue/VIMSdata/To_DEQ_19NF samples.xlsx',
                       sheet = 'qry_DEQ_Final_PCB_MSpec_2014')

#do data structures match? 
names(VIMS2019) == names(VIMS2014fish)
names(VIMS2019) == names(VIMS2015fish)
names(VIMS2019) == names(VIMS2016fish)
names(VIMS2019) == names(VIMS2016sed)
names(VIMS2019) == names(VIMS2017fish)
names(VIMS2019) == names(VIMS2017sed)
names(VIMS2019) == names(VIMS2018)

# so everything matches but some years have fish and sediment separate and some together. what is going on?