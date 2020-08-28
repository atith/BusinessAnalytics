## Atith Songtham, Fabien Böckle
## COVID-19 + financial World Data interactive mapping tool 

# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
source("getData/getData.R")
# load required packages
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(plyr)) install.packages("plyr", repos = "http://cran.us.r-project.org")
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
if(!require(DT)) install.packages("DT")
if(!require(ggpubr)) install.packes("ggpubr")
if(!require(shiny)) install.packages("shiny")
if(!require(matrixStats)) install.packages("matrixStats")
if(!require(janitor)) install.packages("janitor")
if(!require(MASS)) install.packages("MASS")

# Define server logic required to draw a worldmap

shinyServer(function(input, output) {
    
    # import data
    # hier müssen wir die Daten definieren die geladen werden sollen
    corona_cases = read.csv("input_data/corona_cases.csv", header=TRUE, sep= ",")
    laender = read.csv("input_data/countries_codes_and_coordinates.csv", header=TRUE, sep=",")
    worldcountry = geojson_read("input_data/countries.geojson", what = "sp")
    country_geoms = read.csv("input_data/countries_codes_and_coordinates.csv")
    economy = read_excel("input_data/GDP_World.xlsx")
    bip_daten <- read_excel("input_data/GDP.xls")
    data_confirmed <- read_csv("input_data/corona_cases.csv")
    data_deceased  <- read_csv("input_data/corona_deaths.csv")
    data_recovered <- read_csv("input_data/corona_recovered.csv")
    GDP <- read_csv("input_data/GDP_per_Capita.csv")
    
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
    
    #Absolutberechnung der GDP per capita pro Land
    bip_daten_2020 = subset(bip_daten, select=c("Country","2020"))
    bip_daten_2020["2020"] <- sapply(bip_daten_2020["2020"], as.numeric)
    round(bip_daten_2020["2020"], 3)
    bip_daten_2020 <- merge(bip_daten_2020, country_geoms, by.x="Country", by.y="Country")
    names(bip_daten_2020)[names(bip_daten_2020)=="2020"] <- "bip20"
    
    TotalGDP <- merge(GDP, bip_daten_2020, by.x="Country Code", by.y="alpha3")
    TotalGDP <- subset(TotalGDP, select=c("Country Code","Country Name","2019","population", "bip20"))
    TotalGDP["2019"] <- sapply(TotalGDP["2019"], as.numeric)
    TotalGDP["population"] <- sapply(TotalGDP["population"], as.numeric)
    TotalGDP["calc"] <- (TotalGDP["2019"] * TotalGDP$population)
    TotalGDP["absolut"] <- TotalGDP$calc * ((TotalGDP$bip20 + 100) / 100)

    bip_daten[bip_daten == "no data" ] <- NA
    #country_geoms <- country_geoms[complete.cases(country_geoms), ]
    
    #sum corona cases in new row for each column (needed for Dashboard)
    data_confirmed <- adorn_totals(data_confirmed,"row")
    
    #sum corona cases in new row for each column (needed for Dashboard)
    data_deceased <- adorn_totals(data_deceased,"row")
    
    #sum corona cases in new row for each column (needed for Dashboard)
    data_recovered <- adorn_totals(data_recovered,"row")
    
    #set last row number for corona cases
    y = nrow(data_confirmed)
    #get cell value from last row and cell for total corona cases
    total_cases <- apply( data_confirmed[y,ncol(data_confirmed)], 1, max)
    as.numeric(total_cases)
    
    
    #set last row number for corona deaths
    y = nrow(data_deceased)
    #get cell value from last row and cell for total corona deaths
    total_deaths <- apply( data_deceased[y,ncol(data_deceased)], 1, max)
    as.numeric(total_deaths)
    
    
    #set last row number for corona recovered
    y = nrow(data_recovered)
    #get cell value from last row and cell for total corona recovered
    total_recovered <- apply( data_recovered[y,ncol(data_recovered)], 1, max)
    as.numeric(total_recovered)

    # corona_cases$total <- rowSums( corona_cases[,5:ncol(corona_cases)] )
    # corona_cases$total <- apply( corona_cases[,5:ncol(corona_cases)], 1, max)
    # x = corona_cases[,ncol(corona_cases)]
    corona_cases$total <- apply(corona_cases[, 5:ncol(corona_cases)], 1, max)
    corona_cases <- ddply(corona_cases,"Country.Region",numcolwise(sum))
    
    TotalGDP["Country Name"][TotalGDP["Country Name"] =="United States"] <- "US"
    
    cv_gdp <- merge(TotalGDP, corona_cases, by.x="Country Name", by.y="Country.Region")
    cv_gdp <- subset(cv_gdp, select=c("Country Name","absolut","total", "bip20", "population"))
    cv_gdp[ cv_gdp == "no data" ] <- 0
    cv_gdp$affected <- (cv_gdp$total / cv_gdp$population) * 100
    names(cv_gdp)[names(cv_gdp)=="Country Name"] <- "Country"
    
    # cv_gdp["2020"] = lapply(cv_gdp["2020"], FUN = as.numeric)
    
    # names(cv_gdp)[3] <- "BIP"
    
    #Bip-Daten werden mit Standortdaten angereichert -> zum test nehme ich nur ein Jahr und zwar 2020
    #bip_daten[2:nrow(bip_daten),5:ncol(bip_daten)] = lapply(bip_daten[2:nrow(bip_daten),5:ncol(bip_daten)], FUN = as.numeric)
    
    bip_daten[ bip_daten == "no data" ] <- NA
    bip_daten = subset(bip_daten, select=c("Country","2020"))
    bip_daten["2020"] <- sapply(bip_daten["2020"], as.numeric)
    round(bip_daten["2020"], 3)
    bip_daten <- merge(bip_daten, country_geoms, by.x="Country", by.y="Country")
    bip_daten$longitude <- as.numeric(bip_daten$longitude)
    bip_daten$latitude <- as.numeric(bip_daten$latitude)

    
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
      #Diese Funktion steht zum Test oben
      #bip_daten["2020"] = lapply(bip_daten["2020"], FUN = as.numeric)
     
      
      #wir müssen die Länder für die Karte selektieren
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
      
      #coordinates(bip_daten) <- c("longitude", "latitude")
      #crs.geo1 = CRS("+proj=longlat")  
      #proj4string(bip_daten) = crs.geo1
      #proj4string(worldcountry) = crs.geo1
      
      
      plot_map <- worldcountry[worldcountry$ADM0_A3 %in% bip_daten$alpha3, ]
      
      #Hier definieren wir die Farben für die BIP Werte des Jahres
      bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
      BIP_pal <- colorBin("RdYlGn", domain = bip_daten$bip_wert, bins = bins)
      
      
     #labels <- sprintf(
      #  "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
      #  bip_daten.SP$Country,
      #) %>% lapply(htmltools::HTML)
      
      
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
    
    output$economy <- renderPlotly({
      
      g <- ggplot(data=cv_gdp, aes(x=total, y=absolut, label=Country)) +
          geom_point(aes(size = total, color = "red")) +
          # size=2, shape="square filled", fill="blue", col="red") +
        theme(plot.title = element_text(hjust=0,5, size=16,  family="New Times Roman" )) +
        # ggtitle("Abendland") +
        xlim (10, 500000) +
        geom_smooth(method = "lm")
      
      gg <- ggplotly(g)
    })
    
    # output$correlation({
      # corr <- cor(cv_gdp$total, cv_gdp$absolut, method="spearman")
    # })
    # Robust linear regression
    robustfit <- rlm(cv_gdp$affected ~ cv_gdp$bip20)
    
    
    output$rlm <- renderPlot({
      # 
      # ggplot2(data = cv_gdp, aes(x=total, y=absolut)) +
      # geom_point() +
      #   stat_smooth(method=function(formula,data,weights=weight) rlm(formula,
      #                                                                data,
      #                                                                weights=weight,
      #                                                                method="MM"),
      #               fullrange=TRUE) +
      #   xlim(0,160)
      # Scatter plot without outliers with robust linear regression line
      plot(cv_gdp$bip20 ~ cv_gdp$affected, main="Corona vs. Economy\n[ no outliers, robust linear regression ]",
           xlab="corona cases in %", ylab="bip in %") +
      # xlim(100,100000) +
      abline(robustfit, col="red", lwd=3)
    })
      
      
      # cv_gdp["2020"] <- unlist(cv_gdp["2020"])
      # cv_gdp["total"] <- unlist(cv_gdp["total"])
      
      #linerarer Zusammenhang -> Advanced Analytics, Zukunfsaussage. Wie ändert sich ungefähr das BIP bei steigenden Cororna Zahlen
      #Robost Linar Modell
      #https://stat.ethz.ch/R-manual/R-devel/library/MASS/html/rlm.html
      #Dashboard: https://www.youtube.com/watch?v=YneLLtGcCus
      
      
      #To-Dos:
      #1) Dashboard auf Startseite
      #2) Layout in Korrelation
      #3) Modell in Korrelation Tab hinzufügen
      #4) Verweis auf abschgeschnittene Daten
    
    output$valueBox_confirmed <- renderValueBox({
      valueBox(
        paste0(total_cases),
        subtitle = "Confirmed",
        icon     = icon("file-medical"),
        color    = "light-blue",
        width    = NULL
      )
    })
    
    output$valueBox_deceased <- renderValueBox({
      valueBox(
        paste0(total_deaths),
        subtitle = "Deceased",
        icon     = icon("heartbeat"),
        color    = "light-blue"
      )
    })
    
    output$valueBox_recovered <- renderValueBox({
      valueBox(
        paste0(total_recovered),
        subtitle = "Recovered",
        icon     = icon("heart"),
        color    = "light-blue"
      )
    })
    
    corona_table <- corona_cases[, c("Country.Region", "total")]
    
    output$summary <- DT::renderDataTable(
      datatable(corona_table, options = list(
        pageLength = 10, autowidth=TRUE)
      )
    )
    
    output$ari_mittel <- renderText(paste("Aritmethisches Mittel:", round(mean(nv()),2)))
    
})