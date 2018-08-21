shinyUI(
  fluidPage(
    useShinyjs(),
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$img(src = 'Cargando.gif', class="loadmessage")),
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="style.css"),
      includeScript("js/iframeSizer.contentWindow.min.js")
    ),
    div(class = "oneLine",
        uiOutput('selcbase'),
        uiOutput('anioSel'),
        uiOutput('selBarrio'),
        uiOutput('boxOpc')),
    div(class = 'vizPar',
        uiOutput('vizUberUi'),
        leafletOutput('mapUber', width = "97%", height = "470px"))
  )
)