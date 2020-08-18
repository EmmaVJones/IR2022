

```{r monitored vs assessed}
SorIM <- function(x){
  if(any(as.character(x) %in% c('S','IM'))){'Assessed'
  } else {'Not'}
}

FindIt <- function(l) {
  purrr::some(l, ~str_detect(., 'Assessed'))
}

SorIM1 <- function(x){
  purrr::some(x, ~str_detect(., c('S', 'IM')))
}

final2020withData1 <- final2020withData %>% #mutate(MONITOR_STATUS = TRUE) %>%
  st_drop_geometry() %>%
  group_by(FDT_STA_ID) %>%
  #dplyr::select(FDT_STA_ID, contains("_STAT")) %>%
  mutate(ASSESSMENT_STATUS = across(contains("_STAT"), ~ SorIM1(.x) )) 

mutate(final2020withData1, ASSESSMENT_STATUS = map_lgl(ASSESSMENT_STATUS_tibble, FindIt))

```

```{r}
df <- final2020withData[c(1,3,20), ] %>% st_drop_geometry() %>%
  dplyr::select(FDT_STA_ID, contains(c("TEMP_STAT", "DO_STAT", 'PH_STAT')) )


SorIM <- function(x){
  if(any(x %in% c('S','IM'))){'Assessed'
  } else {'Not'}
}

df %>%
  group_by(FDT_STA_ID) %>%
  mutate()
mutate(ASSESSMENT_STATUS = SorIM(TEMP_STAT:PH_STAT))#SorIM(contains("_STAT")))

```

```{r}
SorIM <- function(x){
  assess_test <- "\\b(S|IM)\\b"
  #str_detect(x, assess_test) | str_detect(DO_STAT, assess_test) |str_detect(PH_STAT, assess_test)
  any(str_detect(x, assess_test))
}

SorIM(c(as.character(df$TEMP_STAT, as.character(df$DO_STAT))))

df %>%
  group_by(FDT_STA_ID) %>%
  mutate(across(contains("_STAT"),  as.character )) %>%
  #mutate(Assess = across(where(is.character), paste0))
  #mutate(across(c(x,ends_with("_STAT")),paste0))
  mutate(Assess = SorIM(TEMP_STAT,PH_STAT))
#mutate(Assess = SorIM(contains("_STAT")))
#mutate(Assess = str_detect(TEMP_STAT, assess_test) | str_detect(DO_STAT, assess_test) |str_detect(PH_STAT, assess_test))
```


```{r}
df1 <- tribble(~UserName, ~Bio,
               "apple", "tv",
               "radio", "television",
               "orange", "blue")

Bio_pat <- "\\b(news|reporter|journalist|radio|tv|television)\\b"
UserName_pat <- "\\b(news|tv)\\b"

mutate(df1, ismedia = str_detect(UserName, UserName_pat) | str_detect(Bio, Bio_pat))

```


