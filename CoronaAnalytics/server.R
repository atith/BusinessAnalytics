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
library(readxl)

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
if(!require(readr)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")

# Define server logic required to draw a worldmap

shinyServer(function(input, output) {
    
    # import data
    # hier müssen wir die Daten definieren die geladen werden sollen
    corona_cases = read_excel("input_data/corona_cases_v2.xlsx")
    laender = read.csv("input_data/countries_codes_and_coordinates.csv", header=TRUE, sep=",")
    worldcountry = geojson_read("input_data/countries.geojson", what = "sp")
    country_geoms = read.csv("input_data/country_geoms.csv")
<<<<<<< HEAD
    economy = read_excel("input_data/GDP_World.xlsx")
=======
    bip_daten <- read_excel("input_data/imf-dm-export-20200806.xls")
 
    #corona_cases <- read_csv("input_data/corona_cases.csv")
    
    View(corona_cases)
    View(bip_daten) 
>>>>>>> 20e5ee33a6b4ae418f2b7621d6644c0c9215aad8
    
    #daten müssen verarbeietet werden

    # extrahieren vom Datum der Corona Cases Tabelle
    # if (any(grepl(corona_cases$date))) {
    #     corona_cases$date = format(as.Date(corona_cases$date, format="%d/%m/%Y"),"%Y-%m-%d")
    # } else { corona_cases$date = as.Date(corona_cases$date, format="%Y-%m-%d") }
    # corona_cases$date = as.Date(corona_cases$date)
    # cv_min_date = as.Date(min(corona_cases$date),"%Y-%m-%d")
    # current_date = as.Date(max(corona_cases$date),"%Y-%m-%d")
    # cv_max_date_clean = format(as.POSIXct(current_date),"%d %B %Y")
    
    meinemap=leaflet(worldcountry) %>% 
        addTiles() %>% 
        addLayersControl(
            position = "bottomright",
            overlayGroups = c("2019-COVID (new)", "2019-COVID (active)", "2019-COVID (cumulative)"),
            options = layersControlOptions(collapsed = FALSE)) %>% 
        hideGroup(c("2019-COVID (active)", "2019-COVID (cumulative)"))  %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(0, 30, zoom = 2)
    
    output$weltkarte<-renderLeaflet({
        meinemap
    })
    
    url <- reactive({
        glue("https://api.abalin.net/get/namedays?day={day_()}&month={month_()}")
    })
    
    
    #Output für die Tabelle
    
    output$vision <- renderPrint({
        print(tail(corona_cases %>% select(c(dateRep,month, year,cases, countriesAndTerritories)), input$maxrows), row.names = FALSE)
    })
    
    #zum Testen der Panels
    nv<-reactive(rnorm(input$ziehungen,
                       input$erwartungswert,
                       input$standardabweichung))
        
    output$Verteilung <- renderPlot({
        
        ggplot()+geom_histogram(aes(nv()), bins = 40)+
            geom_vline(xintercept = mean(nv()), col="red", show.legend = TRUE)
    })
    
    str(economy)
    
    output$economy <- renderPlot({
        
        ggplot(data=economy, aes(Year, Data, group = 1)) + 
            geom_line() +
            labs(x = "Year", y = "World")
    })
    
    str(corona_cases)
    
    output$corona <- renderPlot({
        
        ggplot(data=corona_cases, aes(Date, Cases, group = 1)) + 
            geom_line() +
            labs(x = "Date", y = "Cases")
    })
    
    output$ari_mittel <- renderText(paste("Aritmethisches Mittel:", round(mean(nv()),2)))
    
})