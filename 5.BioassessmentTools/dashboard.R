# source('global.R')
# 
# assessmentRegions <- st_read( 'data/GIS/AssessmentRegions_simple.shp')
# ecoregion <- st_read('data/GIS/vaECOREGIONlevel3__proj84.shp') 

ui <- dashboardPage(
  
  dashboardHeader(title = "IR 2022 Bioassessment Dashboard", titleWidth = '400px'),
  dashboardSidebar(
    sidebarMenu(
      helpText('Pro Tip: You can use each of the drop', br(),
               ' down boxes independently; however, ', br(),
               'if you start with the Collector Filter,', br(),
               ' the cross validation features narrow',br(), 
               ' down options in subsequent selection ', br(),
               ' fields.'),
      uiOutput('filters'),
      checkboxGroupInput("repFilter", "Rep Filter (if none are selected then all are included)", choices = c('1', '2'), selected = NULL) )) ,
  dashboardBody(
    tabsetPanel(
      tabPanel(title = span(tagList(icon("globe", lib = "glyphicon"), " Map")),
               leafletOutput('map'),
               h4('Selected Station Information'),
               dataTableOutput('stationInfoTable')),
      tabPanel(title = span(tagList(icon("stats", lib = "glyphicon"), " SCI Scores")),
               plotlyOutput('SCIplot'),
               dataTableOutput('SCITable')),
      tabPanel(title = span(tagList(icon("stats", lib = "glyphicon"), " Habitat Scores"))),
      tabPanel(title = span(tagList(icon("calculator"), " Station Summary"))),
      tabPanel(title = span(tagList(icon("balance-scale"), " Assessment Decision")))
      
      
    )
  )
)
#verbatimTextOutput('table1'), verbatimTextOutput('table2')


server <- function(input, output, session) {
  
  # original filters
  output$filters <- renderUI({
    list(
      selectInput("collectorFilter", "Collector Filter", choices = sort(unique(benSamps$`Collected By`)), multiple = TRUE),
      selectInput("basinFilter", "Basin Filter", choices = sort(unique(benSamps$Basin_Code)), selected = NULL, multiple = TRUE),
      selectInput("stationFilter", "StationID Filter", choices = sort(unique(benSamps$StationID)), selected = NULL, multiple = TRUE)#,
      #selectInput("repFilter", "Rep Filter", choices = sort(unique(benSampsFilter$RepNum)), selected = NULL, multiple = TRUE)    )
      ) })
  
  # update filters if user uses collector first
  observe({ updateSelectInput(session, "basinFilter", "Basin Filter",
                              #choices = sort(unique(benSampsFilter()$Basin_Code))) })
                              choices = if(!is.null(input$collectorFilter)){
                                filter(benSamps, `Collected By` %in% input$collectorFilter) %>%
                                  distinct(Basin_Code) %>% arrange(Basin_Code) %>% pull()
                              } else {distinct(benSamps, Basin_Code) %>% arrange(Basin_Code) %>% pull()} ) })
  
  observe({ updateSelectInput(session, "stationFilter", "StationID Filter",
                              choices = if(!is.null(input$collectorFilter)){
                                filter(benSamps, `Collected By` %in% input$collectorFilter) %>%
                                  filter( Basin_Code %in% input$basinFilter) %>%
                                  distinct(StationID) %>% arrange(StationID) %>% pull()
                              } else {distinct(benSamps, StationID) %>% arrange(StationID) %>% pull()} ) })
  
  # observe({ updateSelectInput(session, "repFilter", "Rep Filter",
  #                             choices = if(!is.null(input$collectorFilter)){
  #                               filter(benSamps, `Collected By` %in% input$collectorFilter) %>%
  #                                 filter( Basin_Code %in% input$basinFilter) %>%
  #                                 filter(RepNum %in% input$repFilter) %>%
  #                                 distinct(RepNum) %>% arrange(RepNum) %>% pull()
  #                             } else {distinct(benSamps, RepNum) %>% arrange(RepNum) %>% pull()} ) })
  
  # Filter by user input
  benSampsFilter <- reactive({
    benSamps %>%
      {if(!is.null(input$collectorFilter))
        filter(., `Collected By` %in% input$collectorFilter)
        else . } %>%
      {if(!is.null(input$basinFilter))
        filter(., Basin_Code %in% input$basinFilter)
        else . } %>%
      {if(!is.null(input$stationFilter))
        filter(., StationID %in% input$stationFilter)
        else . } %>%
      {if(!is.null(input$repFilter))
        filter(., RepNum %in% input$repFilter)
        else .} })
  benSampsFilterStations <- reactive({req(benSampsFilter())
    filter(benSampsStations, StationID %in% benSampsFilter()$StationID) })
  
  
  ### Map Tab
  
  output$map <- renderLeaflet({req(benSampsStations)
    # color palette for assessment polygons
    pal <- colorFactor(palette = topo.colors(7), domain = assessmentRegions$ASSESS_REG)
    pal2 <- colorFactor(palette = rainbow(7), domain = ecoregion$US_L3NAME)

    CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE) %>%
      setView(-78, 37.5, zoom=6) %>%
      addPolygons(data= ecoregion,  color = 'gray', weight = 1,
                  fillColor= ~pal2(ecoregion$US_L3NAME), fillOpacity = 0.5,stroke=0.1,
                  group="Level III Ecoregions",label = ~US_L3NAME) %>% hideGroup('Level III Ecoregions') %>%
      addPolygons(data= assessmentRegions,  color = 'black', weight = 1,
                  fillColor= ~pal(assessmentRegions$ASSESS_REG), fillOpacity = 0.5,stroke=0.1,
                  group="Assessment Regions", label = ~ASSESS_REG) %>% hideGroup('Assessment Regions') %>%
      inlmisc::AddHomeButton(raster::extent(-83.89, -74.80, 36.54, 39.98), position = "topleft") %>%
      # inlmisc::AddSearchButton(group = "Biomonitoring Stations", zoom = 15,propertyName = "label",
      #                          textPlaceholder = "Search stations") %>%
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c(#"Biomonitoring Stations",
                         'Level III Ecoregions', 'Assessment Regions'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')    })

  map_proxy <- leafletProxy("map")

  observe({
    map_proxy %>%
      clearGroup('Biomonitoring Stations') %>%
      {if(nrow(benSampsFilterStations()) > 0)
        addCircleMarkers(., data=benSampsFilterStations(),color='yellow', fillColor='blue', radius = 5,
                         fillOpacity = 0.5,opacity=1,weight = 2,stroke=T,group="Biomonitoring Stations", label = ~StationID,
                         popup=~paste(sep='<br>',
                                      paste(strong('StationID : '), StationID),
                                      paste(strong('Total Station Visits (Not Sample Reps) :'), `Total Station Visits (Not Sample Reps)`),
                                      paste(strong('Ecoregion : '), US_L3CODE),
                                      paste(strong('Ecoregion Name : '), US_L3NAME),
                                      paste(strong('Basin : '), Basin_Code),
                                      paste(strong('HUC12 Name : '), HU_12_NAME),
                                      paste(strong('HUC12 : '), HUC_12),
                                      paste(strong('DEQ Region : '), ASSESS_REG),
                                      paste(strong('VAHU6 : '), VAHU6))) %>%
          addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                           overlayGroups = c('Biomonitoring Stations', #"All Biomonitoring Stations",
                                             'Level III Ecoregions', 'Assessment Regions'),
                           options=layersControlOptions(collapsed=T), position='topleft')
        else . }       })

  output$stationInfoTable <- renderDataTable({req(benSampsFilterStations())
    z <- benSampsFilterStations() %>% arrange(StationID)
    datatable(z, rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                             pageLength = nrow(z), buttons=list('copy','colvis'))) })      
  
  
  
  ## SCI Scores Tab
  
  SCI_filter <- reactive({ req(benSampsFilter())
    # choose appropriate SCI based on Ecoregion
    SCI_filter <- filter(VSCIresults, BenSampID %in% filter(benSampsFilter(), ! US_L3CODE %in% c(63,65))$BenSampID) %>%
      bind_rows(
        filter(VCPMI63results, BenSampID %in% filter(benSampsFilter(),  US_L3CODE %in% c(63))$BenSampID)  ) %>%
      bind_rows(
        filter(VCPMI65results, BenSampID %in% filter(benSampsFilter(),  US_L3CODE %in% c(65))$BenSampID)  ) %>%
      mutate(SeasonGradient = as.factor(paste0(Season, " (",Gradient,")")),
             SeasonGradientColor = case_when(SeasonGradient == "Spring (Riffle)" ~  "#66C2A5",
                                             SeasonGradient == "Spring (Boatable)" ~  "#66C2A5",
                                             SeasonGradient == "Spring (MACS)" ~  "#66C2A5",
                                             SeasonGradient == "Outside Sample Window (Riffle)" ~ "#FC8D62",
                                             SeasonGradient == "Outside Sample Window (Boatable)" ~ "#FC8D62",
                                             SeasonGradient == "Outside Sample Window (MACS)" ~ "#FC8D62",
                                             SeasonGradient == "Fall (Riffle)" ~ "#8DA0CB",
                                             SeasonGradient == "Fall (Boatable)" ~ "#8DA0CB",
                                             SeasonGradient == "Fall (MACS)" ~ "#8DA0CB",
                                             TRUE ~ as.character(NA)) ) %>%
      left_join(dplyr::select(benSampsFilter(), StationID, Sta_Desc)) %>%
      dplyr::select(StationID, Sta_Desc, BenSampID, `Collection Date`, RepNum, SCI, `SCI Score`, `SCI Threshold`,
                    `Sample Comments`:Season, everything())
      SCI_filter$SeasonGradient <- factor(SCI_filter$SeasonGradient,levels=c("Spring (Riffle)", "Spring (Boatable)", "Spring (MACS)",
                                                                           "Outside Sample Window (Riffle)",
                                                                           "Outside Sample Window (Boatable)",
                                                                           "Outside Sample Window (MACS)",
                                                                           "Fall (Riffle)", "Fall (Boatable)", "Fall (MACS)")) %>% 
      droplevels()
  return(SCI_filter)  })
  
  output$SCIplot <- renderPlotly({ req(SCI_filter())
    plot_ly(SCI_filter(), x = ~`Collection Date`, y = ~`SCI Score`, type = 'bar', 
            color = ~SeasonGradient,  #marker = list(color = ~SeasonGradientColor,width = 0.5), # throws off color for some reason
            stroke = list(color = 'rgb(0, 0, 0)', width = 3),
            hoverinfo="text", text=~paste(sep="<br>",
                                          paste("StationID: ", StationID),
                                          paste("Collection Date: ", as.Date(`Collection Date`)),
                                          paste('Replicate: ', RepNum),
                                          paste("Collector ID: ",`Collected By`),
                                          paste("BenSampID: ", BenSampID),
                                          paste("SCI Score: ", format(`SCI Score`, digits=2)),
                                          paste("Gradient: ", Gradient)),
            name = ~paste('Rep ',RepNum, SeasonGradient)) %>%
      layout(showlegend=TRUE,
        yaxis=list(title="SCI"),
        xaxis=list(title="Sample Date",tickfont = list(size = 10),
                   type = 'date',tickformat = "%Y"))   })
  
  output$SCITable <- renderDataTable({req(SCI_filter())
    z <- mutate(SCI_filter(), `Collection Date` = as.Date(`Collection Date`)) %>%
      arrange(StationID, `Collection Date`, RepNum)
    datatable(z, rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                             pageLength = nrow(z), buttons=list('copy','colvis'))) })
  
  #output$table1 <- renderPrint({ sort(unique(benSampsFilter()$RepNum)) })
  #output$table2 <- renderPrint({ benSampsFilter() })
}

shinyApp(ui, server)

