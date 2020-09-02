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
      
      #1 Seite wird Sars-Cov-2 und davon die die Ausbreitung weltweit zeigen
      tabPanel(
        "COVID-19 Ausbreitung",
        div(
          class = "outer",
          tags$head(includeCSS("style.css")),
          tags$style(includeCSS("style.css")),
          
          #leafletOutput ist zur Darstellung einer Weltkarte
          leafletOutput("weltkarte", width = "100%", height = "100%"),
          
          absolutePanel(
            id = "controls",
            class = "panel panel-default",
            top = 75,
            left = 55,
            width = 250,
            fixed = TRUE,
            draggable = TRUE,
            height = "auto",
            
            #das sind die Überschriften aus dem Panel
            span(tags$i(h6("Fälle weltweit"))),
            h3(textOutput("reactive_case_count"), align = "right"),
            h4(textOutput("reactive_death_count"), align = "right"),
            span(h4(
              textOutput("reactive_recovered_count"), align = "right"
            )),
            span(h4(
              textOutput("reactive_active_count"), align = "right"
            )),
            h6(textOutput("clean_date_reactive"), align = "right"),
            h6(textOutput("reactive_country_count"), align = "right"),
            plotOutput("epi_curve", height = "130px", width = "100%"),
            plotOutput("cumulative_plot", height = "130px", width =
                         "100%"),
            
            #Slider für das Datum
            uiOutput("outslider"),
            
            h1(textOutput("tag"))
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
                #Sollte aus der CVS datei eingelesen werden
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
          fluidRow(
            div(
              class = "outer",
              tags$head(includeCSS("style.css")),
              
              # Show a plot of the generated distribution
              plotOutput("economy", width ="50%", height ="50%"),
              
              #column(12, plotOutput("corona")),
              
              #leafletOutput ist zur Darstellung einer Landkarte - Es funktioniert nicht ich habe zwar ein Output Objekt
              # leafletOutput("landkarte", width =
              #                   "50%", height = "50%"),
              
              #absolutePanel(
              #  id = "controls3",
              #  class = "panel panel-default",
              #  top = 90,
              #  left = 0,
              #  width = 250,
              #  fixed = TRUE,
              #  draggable = FALSE,
              #  height = "auto",
                
              #  span(tags$i(
              #    p("Diese Graphik soll die Korrelation der Prognose für das BIP 2020 und den Corona Fällen zeigen")
              #  ), style = "black"),
              #)
            )
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
      ),
        
      #Panel für Zusammenhang von Covid19 und Weltwirtschaft
      tabPanel(
          "TEST",
          div(
            class = "outer",
            tags$head(includeCSS("style.css")),
            
            # Show a plot of the generated distribution
            plotOutput("Verteilung", width =
                         "100%", height = "100%"),
            
            # Sidebar with a slider input for number of bins
            
            absolutePanel(
              id = "testcontrols",
              class = "panel panel-default",
              top = 75,
              left = 55,
              width = 250,
              fixed = TRUE,
              draggable = TRUE,
              height = "auto",
              
              
              sliderInput(
                "erwartungswert",
                "Erwartungswert:",
                min = 1,
                max = 50,
                value = 30
              ),
              sliderInput(
                "ziehungen",
                "Ziehungen:",
                min = 1,
                max = 10000,
                value = 100,
                step = 100
              ),
              sliderInput(
                "standardabweichung",
                "Standardabweichung:",
                min = 1,
                max = 10,
                value = 1
              ),
              
              textOutput("ari_mittel")
            )
            
          )
        
      )
    )
  ))  