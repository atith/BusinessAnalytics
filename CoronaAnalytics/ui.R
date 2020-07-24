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

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Application title
    titlePanel("COVID-19 vs. economics"),
    
    #Navigation Bar - wird alle verfügbaren Seiten anzeigen
    navbarPage(
        theme = shinytheme("flatly"),
        collapsible = TRUE,
        "COVID-19 tracker",
        id = "nav",
        
        #erste Seite wird Sars-Cov-2 und davon die die Ausbreitung weltweit zeigen
        tabPanel(
            "COVID-19 Ausbreitung",
            div(
                class = "outer",
                tags$head(includeCSS("style.css")),
                
                #leafletOutput ist zur Darstellung einer Weltkarte
                leafletOutput("weltkarte", width = "100%", height =
                                  "100%"),
                
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
                    #plotOutput("epi_curve", height="130px", width="100%"),
                    #plotOutput("cumulative_plot", height="130px", width="100%"),
                    
                    #Slider für das Datum
                    sliderInput(
                        "plot_date",
                        label = h5("Wähle ein Datum für die jeweilige Ausbreitung"),
                        
                        #Sollte aus der CSV datei eingelesen werden
                        min = as.Date('2020-02-24', "%Y-%m-%d"),
                        max = as.Date('2020-07-21', "%Y-%m-%d"),
                        value = as.Date('2020-07-21'),
                        timeFormat = "%d %b",
                        animate = animationOptions(interval = 3000, loop = FALSE)
                    )
                )
            )
        ),
        tabPanel(
            "COVID-19 in den Ländern",
            div(
                class = "outer",
                tags$head(includeCSS("style.css")),
                
                #leafletOutput ist zur Darstellung einer Landkarte
                leafletOutput("landkarte", width =
                                  "50%", height = "50%"),
                
                absolutePanel(
                    id = "controls",
                    class = "panel panel-default",
                    top = 75,
                    left = 55,
                    width = 250,
                    fixed = TRUE,
                    draggable = TRUE,
                    height = "auto",
                    
                    span(tags$i(
                        h6(
                            "Reported cases are subject to significant variation in testing policy and capacity between countries."
                        )
                    ), style = "color:#045a8d"),
                    h3(textOutput("reactive_case_count"), align = "right"),
                    h4(textOutput("reactive_death_count"), align = "right"),
                    span(h4(
                        textOutput("reactive_recovered_count"), align = "right"
                    ), style = "color:#006d2c"),
                    span(h4(
                        textOutput("reactive_active_count"), align = "right"
                    ), style = "color:#cc4c02"),
                    h6(textOutput("clean_date_reactive"), align = "right"),
                    h6(textOutput("reactive_country_count"), align = "right"),
                    plotOutput("epi_curve", height =
                                   "130px", width = "100%"),
                    plotOutput("cumulative_plot", height =
                                   "130px", width = "100%"),
                    
                    sliderInput(
                        "plot_date",
                        label = h5("Select mapping date"),
                        #Sollte aus der CVS datei eingelesen werden
                        min = as.Date('2020-02-24', "%Y-%m-%d"),
                        max = as.Date('2020-07-21', "%Y-%m-%d"),
                        value = as.Date('2020-07-21'),
                        timeFormat = "%d %b",
                        animate =
                            animationOptions(interval = 3000, loop = FALSE)
                    )
                )
                
                
                
            )
            
            
            
            
        )
    )
))     