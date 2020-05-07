st_layers('GIS/WQS_layers_04282020.gdb')
st_layers('GIS/WQS_layers_05072020.gdb')
st_layers('GIS/WQS_layers_05062020_EVJ.gdb')

riverine <- st_read('GIS/WQS_layers_05062020.gdb', layer = 'riverine_05062020' , fid_column_name = "OBJECTID")


riverine <- st_read('GIS/WQS_layers_04282020.gdb', layer = 'riverine_04282020' , fid_column_name = "OBJECTID")

estuarineLines <- st_read('GIS/WQS_layers_04282020.gdb', layer = 'estuarinelines_04282020' , fid_column_name = "OBJECTID")

estuarinePoly <- st_read('GIS/WQS_layers_04282020.gdb', layer = 'estuarinepolygons_04282020' , fid_column_name = "OBJECTID")


# basin conversion
basinCodesConversion <- riverine %>%
  st_drop_geometry() %>%
  distinct(BASIN, .keep_all = T) %>%
  dplyr::select(BASIN, Basin_Code) %>%
  arrange(BASIN) %>%
  bind_rows(estuarineLines %>%
              st_drop_geometry() %>%
              distinct(BASIN, .keep_all = T) %>%
              dplyr::select(BASIN, Basin_Code) %>%
              arrange(BASIN)) %>%
  bind_rows(estuarinePoly %>%
              st_drop_geometry() %>%
              distinct(BASIN, .keep_all = T) %>%
              dplyr::select(BASIN, Basin_Code) %>%
              arrange(BASIN) %>%
              filter(BASIN == 7) # fix error in layer
            ) %>%
  distinct(BASIN, .keep_all = T) %>%
  arrange(BASIN) %>%
  filter(!is.na(BASIN))


# make a table from this info

#write.csv(basinCodesConversion, 'data/basinCodeConversion.csv', row.names = F)



# play with sending SQL query into st_read to limit amount of data pulled in

estuarineLinesSQL <- st_read('GIS/WQS_layers_04282020.gdb', layer = 'estuarinelines_04282020' , fid_column_name = "OBJECTID",
                             query = "select * from estuarinelines_04282020 WHERE BASIN = '2D'")
                        # query = "select * from estuarinelines_04282020 limit 3") # works
                             

time1 <- Sys.time()
riverine <- st_read('GIS/WQS_layers_04282020.gdb', layer = 'riverine_04282020' , fid_column_name = "OBJECTID")
record1 <- Sys.time()-time1

time2 <- Sys.time()
riverineSQL <- st_read('GIS/WQS_layers_04282020.gdb', layer = 'riverine_04282020' , fid_column_name = "OBJECTID",
                             query = "select * from riverine_04282020 WHERE BASIN = '2A'")
record2 <- Sys.time()-time2

time2.2 <- Sys.time()
riverineSQL <- st_read('GIS/WQS_layers_04282020.gdb', layer = 'riverine_04282020' , fid_column_name = "OBJECTID") %>%
  filter(BASIN == '2A')
record2.2 <- Sys.time()-time2.2


time3 <- Sys.time()
riverine9SQL <- st_read('GIS/WQS_layers_04282020.gdb', layer = 'riverine_04282020' , fid_column_name = "OBJECTID",
                         query = "select * from riverine_04282020 WHERE BASIN = '9'")
record3 <- Sys.time()-time3

time3.3 <- Sys.time()
riverine9 <- st_read('GIS/WQS_layers_04282020.gdb', layer = 'riverine_04282020' , fid_column_name = "OBJECTID") %>%
  filter(BASIN == '9')
record3.3 <- Sys.time()-time3.3



time4 <- Sys.time()
riverine2BSQL <- st_read('GIS/WQS_layers_04282020.gdb', layer = 'riverine_04282020' , fid_column_name = "OBJECTID",
                       query = "select * from riverine_04282020 WHERE BASIN = '2B'")
record4 <- Sys.time()-time4


## so faster to pull in whole dataset and then use dplyr to reduce data instead of sending in SQL initially
# This shouldn't be the case but it is.