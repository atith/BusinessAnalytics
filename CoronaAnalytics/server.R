## Atith Songtham, Fabien Böckle
## COVID-19 + financial World Data interactive mapping tool 

# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# load required packages
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")

# Define server logic required to draw a worldmap

shinyServer(function(input, output) {
    
    # import data
    # hier müssen wir die Daten definieren die geladen werden sollen
    corona_cases = read.csv("input_data/coronacasesworlwide.csv")
    laender = read.csv("input_data/countries_codes_and_coordinates.csv")
    worldcountry = geojson_read("input_data/countries.geojson", what = "sp")
    country_geoms = read.csv("input_data/country_geoms.csv")
    
    #daten müssen verarbeietet werden
    
    # extrahieren vom Datum der Corona Cases Tabelle
    if (any(grepl("/", corona_cases$date))) { 
        corona_cases$date = format(as.Date(corona_cases$date, format="%d/%m/%Y"),"%Y-%m-%d") 
    } else { corona_cases$date = as.Date(corona_cases$date, format="%Y-%m-%d") }
    corona_cases$date = as.Date(corona_cases$date)
    cv_min_date = as.Date(min(corona_cases$date),"%Y-%m-%d")
    current_date = as.Date(max(corona_cases$date),"%Y-%m-%d")
    cv_max_date_clean = format(as.POSIXct(current_date),"%d %B %Y")   
    
    output$weltkarte<-renderLeaflet({
        leaflet(worldcountry) %>%
            addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE))
    })
    
    url <- reactive({
        glue("https://api.abalin.net/get/namedays?day={day_()}&month={month_()}")
    })
    
    
    #zum Testen der Panels
    nv<-reactive(rnorm(input$ziehungen,
                       input$erwartungswert,
                       input$standardabweichung))
    
    output$Verteilung <- renderPlot({
        
        ggplot()+geom_histogram(aes(nv()), bins = 40)+
            geom_vline(xintercept = mean(nv()), col="red", show.legend = TRUE)
    })
    
    output$ari_mittel <- renderText(paste("Aritmethisches Mittel:", round(mean(nv()),2)))
    
})