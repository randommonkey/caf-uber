library(shiny)
library(shinyjs)
library(tidyverse)
library(leaflet)
library(geojsonio)
library(hgchmagic)
library(lazyeval)
library(zip)
library(RSQLite)
library(datafringe)
library(leaflet.extras)
library(DT)

hcoptslang <- getOption("highcharter.lang")
hcoptslang$contextButtonTitle <- 'Descargar Imagen'
hcoptslang$printChart <- "Imprimir Gráfico"
hcoptslang$downloadJPEG <- "Descarga en JPEG"
hcoptslang$downloadPNG <- "Descarga en PNG"
hcoptslang$downloadPDF <- "Descarga en PDF"
hcoptslang$downloadSVG <- "Descarga en SVG"
hcoptslang$thousandsSep <- ","

options(highcharter.lang = hcoptslang)


caf_theme <- hc_theme(
  colors = c('#2A7F62','#C3ACCE', '#538083', '#89909F', '#DFD9E2', '#2c6444'),#c('#0b356D', '#3F8909', '#ACA9A9','#CD7031','#1670D2'),
  chart = list(
    backgroundColor = "transparent"
  ),
  title = list(
    style = list(
      color = '#333333',
      fontFamily = "Open Sans",
      textDecoration= 'none'
    )
  ),
  legend = list(
    itemStyle = list(
      fontFamily = '',
      color = 'black'
    ),
    itemHoverStyle = list(
      color = 'gray'
    )
  )
)



