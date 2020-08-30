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
if(!require(sp)) install.packages("sp")
if(!require(raster)) install.packages("raster")



# Define server logic required to draw a worldmap

shinyServer(function(input, output) {
    
    # import data
    # hier müssen wir die Daten definieren die geladen werden sollen
    corona_cases = read.csv("input_data/coronavirus.csv", header=TRUE, sep= ",")
    worldcountry = geojson_read("input_data/50m.geojson", what = "sp")
    country_geoms = read.csv("input_data/countries_codes_and_coordinates.csv", sep=",")
    economy = read_excel("input_data/GDP_World.xlsx")
    bip_daten <- read_excel("input_data/GDP.xls")
    
    #Manche Länder heißen der Countrygeoms anders und müssen angepasst werden
      #China, People's Republic of - Mainland China
      #Hong Kong SAR - Hong Kong
      #Korea, Republic of -Republic of Korea
      #Slovak Republic -Slovakia
      #South Sudan, Republic of -	South Sudan
      #Taiwan Province of China - Taiwan
      #United States - USA
      #United Kingdom - UK
      #Iran  - Iran (Islamic Republic of)
      #Congo, Dem. Rep. of the - Democratic Republic of the Congo
      #Congo, Republic of - Republic of the Congo
      #Macao SAR - Macao
      #Namibia
      #Tanzania
      #Lao P.D.R
      #Kyrgyz Republic
      #Côte d'Ivoire
      #Eritrea
    
    bip_daten$Country[bip_daten$Country =="China, People's Republic of"] <- "Mainland China"
    bip_daten$Country[bip_daten$Country =="Hong Kong SAR"] <- "Hong Kong"
    bip_daten$Country[bip_daten$Country =="Korea, Republic of"] <- "Republic of Korea"
    bip_daten$Country[bip_daten$Country =="Slovak Republic"] <- "Slovakia"
    bip_daten$Country[bip_daten$Country =="South Sudan, Republic of"] <- "South Sudan"
    bip_daten$Country[bip_daten$Country =="United States"] <- "USA"
    bip_daten$Country[bip_daten$Country =="United Kingdom"] <- "UK"
    bip_daten$Country[bip_daten$Country =="Congo, Dem. Rep. of the"] <- "Democratic Republic of the Congo"
    bip_daten$Country[bip_daten$Country =="Congo, Republic of"] <- "Republic of the Congo"
    bip_daten$Country[bip_daten$Country =="Macao SAR"] <- "Macao"
    bip_daten$Country[bip_daten$Country =="Iran"] <- "Iran (Islamic Republic of)"
    bip_daten$Country[bip_daten$Country =="Lao P.D.R."] <- "Lao People's Democratic Republic"
    bip_daten$Country[bip_daten$Country =="Tanzania"] <- "Tanzania, United Republic of"
    bip_daten$Country[bip_daten$Country =="Kyrgyz Republic"] <- "Kyrgyzstan"
    bip_daten$Country[bip_daten$Country =="Côte d'Ivoire"] <- "Cote d'Ivoire"
    bip_daten$Country[bip_daten$Country =="Taiwan Province of China"] <- "Taiwan"
    
    country_geoms$alpha3[country_geoms$alpha3 =="SSD"] <- "SDS"
    country_geoms$alpha3[country_geoms$alpha3 =="ERI"] <- "ERI"
    
    bip_daten[bip_daten == "no data" ] <- NA
    #country_geoms <- country_geoms[complete.cases(country_geoms), ]
    
    # corona_cases$total <- rowSums( corona_cases[,5:ncol(corona_cases)] )
    corona_cases$total <- apply( corona_cases[,5:ncol(corona_cases)], 1, max)
    corona_cases <- ddply(corona_cases,"Country.Region",numcolwise(sum))
    
    cv_gdp <- merge(bip_daten, corona_cases, by.x="Country", by.y="Country.Region")
    cv_gdp <- subset(cv_gdp, select=c("Country","total","2020"))
    cv_gdp[ cv_gdp == "no data" ] <- 0
    cv_gdp["2020"] = lapply(cv_gdp["2020"], FUN = as.numeric)
    
    names(cv_gdp)[3] <- "BIP"
    
    #Bip-Daten werden mit Standortdaten angereichert -> zum test nehme ich nur ein Jahr und zwar 2020
    #bip_daten[2:nrow(bip_daten),5:ncol(bip_daten)] = lapply(bip_daten[2:nrow(bip_daten),5:ncol(bip_daten)], FUN = as.numeric)
    
    #Karte für Covid 19
    meinemap=leaflet(worldcountry) %>% 
        addTiles() %>% 
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(0, 30, zoom = 2) %>%
        addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                  fillColor ="red")
    
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
        
    output$weltkarte2<-renderLeaflet({
      
      
      #keine Fehlerhaften daten und eindeutiges Datum
      plot_year <- formating(bp())
      
      bip_daten = subset(bip_daten, select=c("Country",plot_year))
      bip_daten[plot_year] <- sapply(bip_daten[plot_year], as.numeric)
      round(bip_daten[plot_year], 3)
      names(bip_daten)[names(bip_daten)==plot_year] <- "bip_wert"
      bip_daten <- merge(country_geoms, bip_daten, by.x="Country", by.y="Country")
      bip_daten$longitude <- as.numeric(bip_daten$longitude)
      bip_daten$latitude <- as.numeric(bip_daten$latitude)
      
      bip_daten = filter(bip_daten, bip_daten$alpha3 %in% worldcountry$ADM0_A3)
      if (all(bip_daten$alpha3 %in% worldcountry$ADM0_A3)==FALSE) { print("Error: inconsistent country names")}
      
      bip_daten = bip_daten[order(bip_daten$alpha3),]
      
      bip_daten.SP <- SpatialPointsDataFrame(bip_daten[ ,c(6, 7)], bip_daten[,-c(6, 7)])
      
                            
      plot_map <- worldcountry[worldcountry$ADM0_A3 %in% bip_daten$alpha3, ]
      
      View(plot_map)
      View(bip_daten)
      View(worldcountry)
      
      #Hier definieren wir die Farben für die BIP Werte des Jahres
      bins <- c(Inf,10,7,5,4,2,1 ,0,-1,-2,-4,-5,-7,-10,-Inf)
      BIP_pal <- colorBin("RdYlGn", domain = bip_daten$bip_wert, bins = bins)
      
      leaflet(plot_map) %>% 
        addTiles() %>% 
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(0, 30, zoom = 2) %>%
        #addMarkers(data = bip_daten, lng = ~longitude, lat = ~latitude, popup = as.character(bip_daten$Country))
        addPolygons(fillColor = ~BIP_pal(bip_daten$bip_wert),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    label = sprintf("<strong>%s</strong><br/>BIP: %g", bip_daten$Country, bip_daten$bip_wert) %>% lapply(htmltools::HTML),
                    labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")
                   )
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
      
      # cv_gdp["2020"] <- unlist(cv_gdp["2020"])
      # cv_gdp["total"] <- unlist(cv_gdp["total"])
      
      #linerarer Zusammenhang -> Advanced Analytics, Zukunfsaussage. Wie ändert sich ungefähr das BIP bei steigenden Cororna Zahlen
      #Robost Linar Modell
      #https://stat.ethz.ch/R-manual/R-devel/library/MASS/html/rlm.html
      
      ggplot(data=cv_gdp, aes(x=total, y=BIP)) +
        geom_point(size=2, shape="square filled", fill="blue", col="red") +
        theme(plot.title = element_text(hjust=0,5, size=16,  family="New Times Roman" )) +
        ggtitle("Abendland") +
        xlim (10, 500000) +
        geom_smooth(method = "lm")

    })
    
    
    output$ari_mittel <- renderText(paste("Aritmethisches Mittel:", round(mean(nv()),2)))
    
})