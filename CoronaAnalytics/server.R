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
library(matrixStats)
library(plyr)
library(ggpubr)

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
# if(!require(devtools)) install.packages("devtools"), devtools::install_github("kassambara/ggpubr")

# Define server logic required to draw a worldmap

shinyServer(function(input, output) {
    
    # import data
    # hier müssen wir die Daten definieren die geladen werden sollen
    corona_cases = read.csv("input_data/coronavirus.csv", header=TRUE, sep= ",")
    laender = read.csv("input_data/countries_codes_and_coordinates.csv", header=TRUE, sep=",")
    worldcountry = geojson_read("input_data/countries.geojson", what = "sp")
    country_geoms = read.csv("input_data/country_geoms.csv")
    economy = read_excel("input_data/GDP_World.xlsx")
    bip_daten <- read_excel("input_data/GDP.xls")
    
    # corona_cases$total <- rowSums( corona_cases[,5:ncol(corona_cases)] )
    corona_cases$total <- apply( corona_cases[,5:ncol(corona_cases)], 1, max)
    corona_cases <- ddply(corona_cases,"Country.Region",numcolwise(sum))
    
    cv_gdp <- merge(bip_daten, corona_cases, by.x="Country", by.y="Country.Region")
    cv_gdp <- subset(cv_gdp, select=c("Country","total","2020"))
    cv_gdp[ cv_gdp == "no data" ] <- 0
    cv_gdp["2020"] = lapply(cv_gdp["2020"], FUN = as.numeric)
    
    names(cv_gdp)[3] <- "BIP"
    
    View(cv_gdp)

    #daten müssen verarbeietet werden

    # extrahieren vom Datum der Corona Cases Tabelle
    # if (any(grepl(corona_cases$date))) {
    #     corona_cases$date = format(as.Date(corona_cases$date, format="%d/%m/%Y"),"%Y-%m-%d")
    # } else { corona_cases$date = as.Date(corona_cases$date, format="%Y-%m-%d") }
    # corona_cases$date = as.Date(corona_cases$date)
    # cv_min_date = as.Date(min(corona_cases$date),"%Y-%m-%d")
    # current_date = as.Date(max(corona_cases$date),"%Y-%m-%d")
    # cv_max_date_clean = format(as.POSIXct(current_date),"%d %B %Y")
    
    
    #Karte für Covid 19
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
    
    #Karte für BIP 
    
    bp<-reactive(input$plot_year)
    
    formating <- function(x) {
      plot_year <- format(x,'%Y')
      return(plot_year)
    }
    
    output$Jahr <- renderText({ 
      paste("Ausgewähltes Jahr:", formating(bp()))
    })
    
    # Plotting Parameter kreieren für das BIP
    #plot_year <- formating(bp())
    #BIP_pal <- colorNumeric(palette = "Blues", domain = bip_daten$'2020')
        
    output$weltkarte2<-renderLeaflet({
      
      bip_daten[ bip_daten == "no data" ] <- NA
      plot_year <- formating(bp())
      bip_daten[plot_year] = lapply(bip_daten[plot_year], FUN = as.numeric)
      round(bip_daten[plot_year], 3)
      #bip_daten[plot_year] = round(as.integer(bip_daten[plot_year]), digits = 0)
      bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
      BIP_pal <- colorBin("YlOrRd", domain = bip_daten[plot_year], bins = bins)
      
      View(bip_daten[plot_year])
      
      
     labels <- sprintf(
        "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
        bip_daten$Country, unlist(bip_daten[plot_year])
      ) %>% lapply(htmltools::HTML)
      
      
      leaflet(worldcountry) %>% 
        addTiles() %>% 
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(0, 30, zoom = 2) %>%
        addPolygons(fillColor = ~BIP_pal, 
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    label = labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))
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
    
    output$economy <- renderPlot({
      
      corr <- cor(cv_gdp$total, cv_gdp["BIP"], method="spearman")
      
      View(test)
      
      # cv_gdp["2020"] <- unlist(cv_gdp["2020"])
      # cv_gdp["total"] <- unlist(cv_gdp["total"])
      
      ggplot(data=cv_gdp, aes(x=BIP, y=total)) +
        geom_point(size=2, shape="square filled", fill="blue", col="red") +
        ggtitle("title") +
        theme(plot.title = element_text(hjust=0,5)) +
        xlim (-10, 10) +
        geom_smooth(method = "lm")

      # sp + stat_cor(aes(color = cyl), label.x = 3)
      # #> `geom_smooth()` using formula 'y ~ x'
      # 
      # ggplot(cv_gdp, aes(displ, hwy)) +
      #   geom_point() +
      #   geom_smooth(method = "lm")
    })
    
    # output$corona <- renderPlot({
    #     
    #     ggplot(data=corona_cases, aes(Date, Cases, group = 1)) + 
    #         geom_line() +
    #         labs(x = "Date", y = "Cases")
    # })
    
    output$ari_mittel <- renderText(paste("Aritmethisches Mittel:", round(mean(nv()),2)))
    
})