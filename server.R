shinyServer(function(input, output, session){
  
  # UBER --------------------------------------------------------------------
  
  
  output$anioSel <- renderUI({
    selectizeInput('anioElg', 'Tiempo', c('2016-4', '2017-1', '2017-2', '2017-3'))
  })
  
  output$selcbase <- renderUI({
    varN <- as.list(setNames(c('hod', 'dow', 'month'), c('Horas', 'Semanas', 'Meses')))
    selectizeInput('tiempoElg', 'Viajes por ', varN)
  })
  
  
  output$selBarrio <- renderUI({
    x <- input$tiempoElg
    if (is.null(x)) return()
    elg <- gsub('-', '_', input$anioElg)
    data <- read_csv(paste0('data/barrios_', x, '.csv'))
    data <- data %>% filter(year == elg)
    selcVec <- sort(unique(data$neighbourhood_origin))
    selectizeInput('barrioElg', 'Barrio', selcVec)
  })
  
  
  
  baseUber <- reactive({
    x <- input$tiempoElg
    y <- gsub('-', '_', input$anioElg)
    barrio <- input$barrioElg
    if (is.null(x)) return()
    
    db <- src_sqlite(paste0("data/", x, ".sqlite3"))
    if (x == 'hod') time <- 'hourly'
    if (x == 'dow') time <- 'weekly'
    if (x == 'month') time <- 'monthly'
    
     d <- paste0('SELECT * FROM uber_', time, '_', y,'_data')
     df <- tbl(db, sql(d))
     df <- df %>% filter(neighbourhood_origin %in% barrio)
     df
  })
  

  output$boxOpc <- renderUI({
    radioButtons('optCom', '', c('Total viajes', 'Tiempo promedio'), inline = FALSE)
  })
  
  
  
  uberdtaP <- reactive({
    elg <- input$tiempoElg
    opTe <- input$optCom
    
    if (opTe == 'Total viajes') {
      df <- baseUber() %>% group_by_(elg) %>% summarise(total = n())
    } else {
      df <- baseUber() %>% group_by_(elg) %>% summarise(total = round((mean(mean_travel_time))/60, 2))
    }
    df %>% collect()
  })
  
  


  output$vizUber <- renderHighchart({
    myClickFunc <-  JS("function(event) {Shiny.onInputChange('hcClickedUber',  {id:event.point.category.name, timestamp: new Date().getTime()});}")
    df <- uberdtaP() %>% collect()
    h <- highchart() %>%  hc_chart(type = "column") %>%
      hc_xAxis(
        categories =  map(df[[input$tiempoElg]], function(x) {
          as.character(x)
        })
      ) %>%
      hc_series(list(
        data=  map(df[['total']], function(x) {
          as.numeric(x)
        }),
        color= '#538083',
        allowPointSelect= FALSE,
        cursor = 'pointer',
        dataLabels = list(
          enabled = FALSE,
          fontFamily= 'Open Sans'),
        showInLegend = FALSE,
        events = list(
          click = myClickFunc
        )
      )) %>%
      hc_plotOptions(
        series = list(
          states = list(
            hover = list (
              color = '#509f27'
            ))
        )
      ) %>%
      hc_tooltip(headerFormat = 'Clikea para ver los destinos del barrio seleccionado<br/>',
                 pointFormat = paste0("<b>{point.category}:</b> {point.y}")) %>%
      hc_exporting(enabled = TRUE, buttons= list(
        contextButton= list(
          symbol= 'url(https://cdn1.iconfinder.com/data/icons/feather-2/24/download-32.png)',
          height= 30,
          width= 33,
          symbolSize= 24,
          symbolX= 30,
          symbolY= 30,
          menuItems = list('printChart', 'downloadJPEG', 'downloadPNG', 'downloadSVG', 'downloadPDF')
        )
      ))
    h
  })





  uberMapaData <- reactive({
    timSel <-  input$hcClickedUber$id
    sd <- input$tiempoElg

    if (is.null(timSel)) return()


    df <- baseUber() %>% select_(var = sd,   "sourceid", "dstid", "mean_travel_time", "standard_deviation_travel_time",
                                 "geometric_mean_travel_time", "geometric_standard_deviation_travel_time", "year",
                                 "neighbourhood_origin", "lat_orig", "lon_orig",  "neighbourhood_dest", "lat_dest", "lon_dest")
    df %>% filter(var %in% timSel) %>% collect()

  })

  output$nRutas <-  renderUI({
    sliderInput("rangeRutas",
                "",
                min = 1,
                max = nrow(uberMapaData()),
                value = 15,
                step = 1,
                animate=TRUE)
  })

  output$vizUberUi <- renderUI({
    timSel <-  input$hcClickedUber$id

    if (is.null(timSel)) {
      g <- list(
        highchartOutput('vizUber',  width = "97%", height = "403px"),
        div(style = "margin-left: 3%;margin-top: 6%;",
            downloadButton('idDownUber', 'Descarga los datos')))
    } else {
      g <- list(
        highchartOutput('vizUber',  width = "97%", height = "403px"),
        div(style = "margin-left: 3%;margin-top: 6%;",
            downloadButton('idDownUber', 'Descarga los datos'),
            uiOutput('nRutas'))
      )}
    g
  })

  output$mapUber <- renderLeaflet({
    mp <-  leaflet(data= topojson_read("data/delimMapa/Localidad_data.topojson")) %>%
      addProviderTiles(providers$Wikimedia) %>%
      setView(lng = -74.09729, lat = 4.530901, zoom = 10) %>%
      addPolygons(weight = 1.4,
                  color = "#000",
                  opacity = 1,
                  fillColor = "transparent")
    mp
  })



  observe({

    timSel <-  input$hcClickedUber$id
    sd <- input$tiempoElg
    nRt <- input$rangeRutas

    if (is.null(nRt)) nRt <- 15

    if (is.null(timSel)) return()
    m <- leafletProxy("mapUber", data = uberMapaData()) %>%
      clearShapes()

    for (i in 1:nRt)
      m <- m %>%
      addPolylines(lat = c(uberMapaData()[i,]$lat_orig, uberMapaData()[i,]$lat_dest),
                   lng = c(uberMapaData()[i,]$lon_orig, uberMapaData()[i,]$lon_dest),
                   label =  sprintf(
                     paste0('<p><b>Año del viaje: </b>', uberMapaData()[i,]$year ,'</br><b>Destino: ', uberMapaData()[i,]$neighbourhood_dest, '</b></br>','<b>Duración promedio del viaje:</b> ', round(uberMapaData()[i,]$mean_travel_time/60, 2), ' minutos</br> <b>Duración promedio de viaje: </b>', uberMapaData()[i,]$mean_travel_time, ' segundos<br/><b>Desviación Estándar: </b>', uberMapaData()[i,]$standard_deviation_travel_time ,' segundos</p>'
                     )) %>% lapply(htmltools::HTML),
                   color = '#509f27',
                   opacity = 0.8,
                   dashArray ="10,7"
      )


    m
  })
  
  
  output$idDownUber <- downloadHandler(
    "all_data_uber.zip",
    content = function(file) {
      
      dir.create(tmp <- tempfile())
      df <- baseUber() %>% collect()
      write_csv(df, file.path(tmp, "data_all.csv"), na = '')
      zip(file, tmp)
      
    })
  


  

  
})