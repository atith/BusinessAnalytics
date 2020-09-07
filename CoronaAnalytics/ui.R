library(shiny)

#Install of package if required
if (!require(magrittr))
  install.packages("magrittr", repos = "http://cran.us.r-project.org")
if (!require(rvest))
  install.packages("rvest", repos = "http://cran.us.r-project.org")
if (!require(readxl))
  install.packages("readxl", repos = "http://cran.us.r-project.org")
if (!require(dplyr))
  install.packages("dplyr", repos = "http://cran.us.r-project.org")
if (!require(maps))
  install.packages("maps", repos = "http://cran.us.r-project.org")
if (!require(ggplot2))
  install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if (!require(reshape2))
  install.packages("reshape2", repos = "http://cran.us.r-project.org")
if (!require(ggiraph))
  install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if (!require(RColorBrewer))
  install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if (!require(leaflet))
  install.packages("leaflet", repos = "http://cran.us.r-project.org")
if (!require(plotly))
  install.packages("plotly", repos = "http://cran.us.r-project.org")
if (!require(geojsonio))
  install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if (!require(shiny))
  install.packages("shiny", repos = "http://cran.us.r-project.org")
if (!require(shinyWidgets))
  install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if (!require(shinydashboard))
  install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if (!require(shinythemes))
  install.packages("shinythemes", repos = "http://cran.us.r-project.org")


# Wir definieren eine
shinyUI(
  fluidPage(
    # Application title
    titlePanel("Auswirkung einer Pandemie auf die Wirtschaft"),
    
    #Navigation Bar - wird alle verfügbaren Seiten anzeigen
    navbarPage(
      theme = shinytheme("superhero"),
      collapsible = TRUE,
      "",
      id = "nav",
      
      tabPanel(
        "COVID-19 Ausbreitung",
        dashboardPage(
          dashboardHeader(disable = TRUE),
          dashboardSidebar(
            uiOutput("outslider"),
            h1(textOutput("tag"))
          ),
          dashboardBody(
            fluidRow(
              valueBoxOutput("valueBox_confirmed"),
              valueBoxOutput("valueBox_deceased"),
              valueBoxOutput("valueBox_recovered")
            ),
            fluidRow(
              column(width = 7, leafletOutput("weltkarte")),
              column(width = 5, DT::dataTableOutput("summary"))
            )
          )
        )
      ),
      
      #Seite 2 Darstellung der Weltwirtschaft
      tabPanel(
        "Bruttoinlandsprodukt",
        div(
          class = "outer",
          tags$head(includeCSS("style.css")),
          
          #leafletOutput ist zur Darstellung einer Landkarte
          leafletOutput("weltkarte2", width = "100%", height = "100%"),
          
          absolutePanel(
            id = "controls2",
            class = "panel panel-default",
            top = 75,
            left = 55,
            width = 250,
            fixed = TRUE,
            draggable = TRUE,
            height = "auto",
            
            span(tags$i(
              h6("Die Weltwirschaft ist abhängig vom Bruttoinlandsprodukt")
            ), style = "color:#045a8d"),
            
            h3(textOutput("reactive_wirtschaft"), align = "right"),
            plotOutput("wachstum_wirtschaft", height =
                         "130px", width = "100%"),
            
            sliderInput(
              "plot_year",
              label = h5("Select mapping date"),
              min = as.Date('1980', "%Y"),
              max = as.Date('2021', "%Y"),
              value = as.Date('2019', "%Y"),
              timeFormat = "%Y",
            ),
            
            h1(textOutput("Jahr"))
          )
        )
      ),
      
      #Panel 3 für Zusammenhang von Covid19 und Weltwirtschaft
      tabPanel(
        "Korrelation zwischen Wirtschaft und Covid19",
        
        # Show a plot of the generated distribution
        fluidRow(
          column(width = 6, plotlyOutput("economy")),
          # column(width = 5, plotOutput("correlation")),
          column(width = 5, plotOutput("rlm"))
        )
      ),
      
      tabPanel(
        "Datenblatt in Tabelle",
        div(
          class = "outer",
          tags$head(includeCSS("style.css")),
          tags$style(includeCSS("style.css")),
          
          #wir brauchen eine Tabellen Vorschau, dies dient auch zum Verständnis der Daten
          
          numericInput("maxrows", "Zeilen die angezeigt werden", 25),
          verbatimTextOutput("vision"),
          downloadButton("downloadCsv", "Download die CSV"),
          tags$br(),
          tags$br(),
          "Die Links von den Datenquellen können wir hier einfügen",
        )
      )
    )
  ))  