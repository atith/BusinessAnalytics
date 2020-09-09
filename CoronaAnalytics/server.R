##Verantwortlichkeiten 
## Atith Songtham - Erste Seite für Corona, erste Analyse mit absoluten Fallzahlen, Dashboard Design, automatisierter Abzug von Daten 
## Fabien Böckle - Zweite Seite für BIP, zweite Analyse mit relativen Fallzahlen, Leaflet Design, Input Parameter wählen
## COVID-19 + financial World Data interactive mapping tool 

# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(install.load)

source("getData.R")

# load required packages
if(!require(magrittr)) install_load("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install_load("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install_load("readxl", repos = "http://cran.us.r-project.org")
if(!require(maps)) install_load("maps")
if(!require(ggplot2)) install_load("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install_load("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install_load("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install_load("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install_load("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install_load("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install_load("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(readr)) install_load("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(sp)) install_load("sp")
if(!require(matrixStats)) install_load("matrixStats")
if(!require(plyr)) install_load("plyr")
if(!require(ggpubr)) install_load("ggpubr")
if(!require(raster)) install_load("raster")
if(!require(DT)) install_load("DT")
if(!require(ggpubr)) install_load("ggpubr")
if(!require(shiny)) install_load("shiny")
if(!require(matrixStats)) install_load("matrixStats")
if(!require(janitor)) install_load("janitor")
if(!require(MASS)) install_load("MASS")
if(!require(stringr)) install_load("stringr")
if(!require(dplyr)) install_load("dplyr", repos = "http://cran.us.r-project.org")


# Define server logic required to draw a worldmap

shinyServer(function(input, output) {
    
    # import data
    # hier müssen wir die Daten definieren die geladen werden sollen
    corona_cases = read.csv("input_data/corona_cases.csv", header=TRUE, sep= ",")
    worldcountry = geojson_read("input_data/50m.geojson", what = "sp")
    country_geoms = read.csv("input_data/countries_codes_and_coordinates.csv", sep=",")
    economy = read_excel("input_data/GDP_World.xlsx")
    bip_daten <- read_excel("input_data/GDP.xls")										
    data_confirmed <- read_csv("input_data/corona_cases.csv")
    data_deceased  <- read_csv("input_data/corona_deaths.csv")
    data_recovered <- read_csv("input_data/corona_recovered.csv")
    GDP <- read_csv("input_data/GDP_per_Capita.csv")
    
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
    bip_daten$Country[bip_daten$Country =="Tanzania"] <- "Tanzania United Republic of"
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
    names(bip_daten_2020)[names(bip_daten_2020)=="2020"] <- "BIP_Wachstum"
    
    TotalGDP <- merge(GDP, bip_daten_2020, by.x="Country Code", by.y="alpha3")
    TotalGDP <- subset(TotalGDP, select=c("Country Code","Country Name","2019","population", "BIP_Wachstum"))
    TotalGDP["2019"] <- sapply(TotalGDP["2019"], as.numeric)
    TotalGDP["population"] <- sapply(TotalGDP["population"], as.numeric)
    TotalGDP["calc"] <- (TotalGDP["2019"] * TotalGDP$population)
    TotalGDP["absolut"] <- TotalGDP$calc * ((TotalGDP$BIP_Wachstum + 100) / 100)
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
	
    corona_cases$total <- apply(corona_cases[, 5:ncol(corona_cases)], 1, max)
	  corona_cases <- ddply(corona_cases,"Country.Region",numcolwise(sum))
	
    TotalGDP["Country Name"][TotalGDP["Country Name"] =="United States"] <- "US"
    
    cv_gdp <- merge(TotalGDP, corona_cases, by.x="Country Name", by.y="Country.Region")
    cv_gdp <- subset(cv_gdp, select=c("Country Name","absolut","total", "BIP_Wachstum", "population"))
    cv_gdp[ cv_gdp == "no data" ] <- 0
    cv_gdp$Corona_Anteilig <- (cv_gdp$total / cv_gdp$population) * 100
    names(cv_gdp)[names(cv_gdp)=="Country Name"] <- "Country"
    
    #bip_daten[2:nrow(bip_daten),5:ncol(bip_daten)] = lapply(bip_daten[2:nrow(bip_daten),5:ncol(bip_daten)], FUN = as.numeric)
    bip_daten[ bip_daten == "no data" ] <- NA
    bip_daten <- merge(bip_daten, country_geoms, by.x="Country", by.y="Country")
    bip_daten$longitude <- as.numeric(bip_daten$longitude)
    bip_daten$latitude <- as.numeric(bip_daten$latitude)	
    
    #-----------------------------------------------------------------------------------------------------------
    #Arbeiten an der Corona Map
  
    corona = read_csv("input_data/corona_cases.csv")
    corona_tot = read_csv("input_data/corona_deaths.csv")
    corona_gesund = read_csv("input_data/corona_recovered.csv")
    
    #man braucht das minDate und das maxDate für den zeitlichen Verlauf, diese stehen in der Spaltenüberschrift
    
    corona_formating <- function(cv_daten,tag) {
      names(cv_daten)[1:2] = c("Province", "Country")
      #cv_daten$Country = cv_daten$Country %>% str_replace_all(., " ", "")
      Datum = names(cv_daten)[which(names(cv_daten)=="1/22/20"):ncol(cv_daten)]
      cv_daten = cv_daten %>% 
        dplyr::select(-c(Province, Lat, Long)) %>% 
        #select(-c(Province, Lat, Long)) %>% 
        group_by(Country) %>% 
        summarise_each(funs(sum)) #%>%
        #data.frame()
      
      return(cv_daten)
    }
    
    corona = corona_formating(corona, "cases")
    corona_tot = corona_formating(corona_tot, "tot")
    corona_gesund = corona_formating(corona_gesund, "genesen")
    #die Datei muss verbunden werden mit der Countries_geom 
    
    Datum = names(corona)[which(names(corona)=="1/22/20"):ncol(corona)]
    Datum  = format(as.Date(Datum,"%m/%d/%y"))
    corona_min_date = head(Datum, n = 1)
    corona_max_date = tail(Datum, n = 1)
    
    t_corona = t(corona)
    
    output$outslider<- renderUI({
      sliderInput("inslider", 
                  label = HTML("Um nachzuvollziehen, wie sich der Coronavirus seit Ausbruch verbreitet hat, nutzen Sie den Regler.
                  <br> <br> Wähle einen Tag: "),
                  min=as.Date(corona_min_date),
                  max=as.Date(corona_max_date),
                  value= as.Date('2020-07-21'),
                  timeFormat = "%d %b",
                  animate = animationOptions(interval = 1000, loop = FALSE, playButton=icon("google-play"))
      )
    })

    #Karte für Covid 19

    gt<-reactive({
      input$inslider
    })
    
    # Karte für Covid 19
    output$weltkarte<-renderLeaflet({
      plot_date = gt()
      plot_date = toString(plot_date)

      Datum = names(corona)[which(names(corona)=="1/22/20"):ncol(corona)]
      Datum  = format(as.Date(Datum,"%m/%d/%y"))
      colnames(corona)[which(names(corona)=="1/22/20"):ncol(corona)] <- Datum

      colnames(corona_tot)[which(names(corona_tot)=="1/22/20"):ncol(corona_tot)] <- Datum
      colnames(corona_gesund)[which(names(corona_gesund)=="1/22/20"):ncol(corona_gesund)] <- Datum
      corona = subset(corona, select=c("Country",plot_date))

      corona_tot = subset(corona_tot, select=c("Country",plot_date))
      corona_gesund = subset(corona_gesund, select=c("Country",plot_date))

      names(corona)[names(corona)==plot_date] <- "Coronafaelle"
      names(corona_tot)[names(corona_tot)==plot_date] <- "Tote"
      names(corona_gesund)[names(corona_gesund)==plot_date] <- "Genesen"

      corona["Coronafaelle"] <- sapply(corona["Coronafaelle"], as.numeric)
      corona_tot["Tote"] <- sapply(corona_tot["Tote"], as.numeric)
      corona_gesund["Genesen"] <- sapply(corona_gesund["Genesen"], as.numeric)


      corona <- merge(corona, corona_tot, by.x="Country", by.y="Country")
      corona <- merge(corona, corona_gesund, by.x="Country", by.y="Country")

      corona$Country[corona$Country =="China"] <- "Mainland China"
      corona$Country[corona$Country =="Iran"] <- "Iran (Islamic Republic of)"
      corona$Country[corona$Country =="Czechia"] <-"Czech Republic"
      corona$Country[corona$Country =="Congo (Kinshasa)"] <- "Democratic Republic of the Congo"
      corona$Country[corona$Country =="Congo (Brazzaville)"] <- "Republic of the Congo"
      corona$Country[corona$Country =="Korea, South"] <-"Republic of Korea"
      corona$Country[corona$Country =="Laos"] <- "Lao People's Democratic Republic"
      corona$Country[corona$Country =="Moldova"] <- "Moldova, Republic of"
      corona$Country[corona$Country =="Taiwan*"] <- "Taiwan"
      corona$Country[corona$Country =="Tanzania"] <-"Tanzania United Republic of"
      corona$Country[corona$Country =="US"] <- "USA"
      corona$Country[corona$Country =="United Kingdom"] <- "UK"
      corona$Country[corona$Country =="Gambia"] <- "The Gambia"
      corona$Country[corona$Country =="Bahamas"] <- "The Bahamas"

      mytest <- anti_join(corona, country_geoms, by.x="Country", by.y="Country")

      corona <- merge(corona, country_geoms, by.x="Country", by.y="Country")

      corona$longitude <- as.numeric(corona$longitude)
      corona$latitude <- as.numeric(corona$latitude)


      corona = filter(corona, corona$alpha3 %in% worldcountry$ADM0_A3)
      #corona_countries = corona %>% filter(alpha3 %in% worldcountry$ADM0_A3)
      if (all(corona$alpha3 %in% worldcountry$ADM0_A3)==FALSE) { print("Error: inconsistent country names")}
      corona = corona[order(corona$alpha3),]
      corona_map <- worldcountry[worldcountry$ADM0_A3 %in% corona$alpha3, ]

      #Hier definieren wir die Farben für die Corona Werte des Jahres
      bins <- c(Inf,1000000,100000,10000,1000,100,0)
      corona_pal <- colorBin("YlOrRd", domain = corona$Coronafaelle, bins = bins)

      leaflet(corona_map) %>%
        addTiles() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(0, 30, zoom = 2) %>%
        #addMarkers(data = bip_daten, lng = ~longitude, lat = ~latitude, popup = as.character(bip_daten$Country))
        addPolygons(fillColor = ~corona_pal(corona$Coronafaelle),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    label = sprintf("<strong>%s</strong><br/>Fälle insgesamt: %s <br/>Tote: %s <br/> Gesund: %s", corona$Country, format(corona$Coronafaelle, big.mark=".", scientific=FALSE), format(corona$Tote, big.mark=".", scientific=FALSE), format(corona$Genesen, big.mark=".", scientific=FALSE) ) %>% lapply(htmltools::HTML),
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")
        )

    })
    
    #-------------------------------------------------------------------------------------------------------------
    
    
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
                    label = paste0(sprintf("<strong>%s</strong><br/>BIP: %g", bip_daten$Country, bip_daten$bip_wert),"%") %>% lapply(htmltools::HTML),
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
    
        
    output$Verteilung <- renderPlot({
        
        ggplot()+geom_histogram(aes(nv()), bins = 40)+
            geom_vline(xintercept = mean(nv()), col="red", show.legend = TRUE)
    })

    output$economy <- renderPlotly({
      g <- ggplot(data = cv_gdp, aes(x = total, y = BIP_Wachstum, label = Country)) +
        geom_point() +
        # geom_point(aes(size = total, color = "red"), show.legend = FALSE) +
        # size=2, shape="square filled", fill="blue", col="red") +
        theme(plot.title = element_text(
          hjust = 0, 5,
          size = 16,
          family = "New Times Roman"
        )) +
        # ggtitle("Abendland") +
        xlim (10, 500000) +
        geom_smooth(method = "lm") +
        
        scale_x_continuous(name="Anzahl der totalen Corona Fälle", labels= function(n){format(n, scientific = FALSE)}) + 
        
        scale_y_continuous(name="BIP in %", labels= function(n){format(n, scientific = FALSE)})
        
      gg <- ggplotly(g)
    })
    
    
    robustfit <- rlm(cv_gdp$BIP_Wachstum ~ cv_gdp$total)
    cor <- coef(robustfit)
  
    output$slant1 <- renderText(paste0("Der Graph neigt sich mit dem Wert ", format(round(cor[2],7), big.mark = ".", scientific = FALSE), ". Daher ist davon auszugehen, dass es keine Korrelation gibt."))
    
    robustfit2 <- rlm(cv_gdp$BIP_Wachstum ~ cv_gdp$Corona_Anteilig)
    cor2 <- coef(robustfit2)
    
    output$slant2 <- renderText(paste0("Der Graph neigt sich mit dem Wert ", round(cor2[2],3), ". Daher ist davon auszugehen, dass es aufgrund der relativ niedrigen Zahlen im Graphen eine Korrelation gibt."))
    
    output$rlm <- renderPlotly({
      g1 <- ggplot(data = cv_gdp, aes(x = Corona_Anteilig, y = BIP_Wachstum, label = Country)) +
        geom_point() +
        # geom_point(aes(size = total, color = "red"), show.legend = FALSE) +
        # size=2, shape="square filled", fill="blue", col="red") +
        theme(plot.title = element_text(
          hjust = 0, 5,
          size = 16,
          family = "New Times Roman"
        )) +
        # ggtitle("Abendland") +
        xlim (10, 500000) +
        geom_smooth(method = "lm") +
        
        scale_x_continuous(name="Corona Fälle in % zur Bevölkerung", labels= function(n){format(n, scientific = FALSE)}) + 
        
        scale_y_continuous(name="BIP in %", labels= function(n){format(n, scientific = FALSE)})
      
      gg1 <- ggplotly(g1)
    })
	
    output$valueBox_confirmed <- renderValueBox({
      valueBox(
        paste0(prettyNum(total_cases,big.mark=".",decimal.mark = ",",scientific=FALSE)),
        subtitle = "Bestätigte Fälle",
        icon     = icon("file-medical"),
        color    = "light-blue",
        width    = NULL
      )
    })
	
	    output$valueBox_deceased <- renderValueBox({
      valueBox(
        paste0(prettyNum(total_deaths,big.mark=".",decimal.mark = ",",scientific=FALSE)),
        subtitle = "Verstorben",
        icon     = icon("heartbeat"),
        color    = "light-blue"
      )
    })
    
    output$valueBox_recovered <- renderValueBox({
      valueBox(
        paste0(prettyNum(total_recovered,big.mark=".",decimal.mark = ",",scientific=FALSE)),
        subtitle = "Genesen",
        icon     = icon("heart"),
        color    = "light-blue"
      )
    })

############-BIP Champions-#############################    
    output$valueBox_maxbip <- renderValueBox({
      
      BIP_row_max <- bip_daten_2020[which.max(bip_daten_2020$BIP_Wachstum),]
      
      BIPmax = BIP_row_max$BIP_Wachstum
      BIP_land_max = BIP_row_max$Country
      
      valueBox(
        paste0(prettyNum(BIPmax,big.mark=".",decimal.mark = ",",scientific=FALSE), "%"),
        subtitle = paste0("Gewinner von 2020: ", BIP_land_max),
        icon     = icon("chart-line"),
        color    = "green"
      )
    })
    
    output$valueBox_lowbip <- renderValueBox({
      
      BIP_row_min <- bip_daten_2020[which.min(bip_daten_2020$BIP_Wachstum),]
      
      BIPmin = BIP_row_min$BIP_Wachstum
      BIP_land_min = BIP_row_min$Country
      
      valueBox(
        paste0(prettyNum(BIPmin,big.mark=".",decimal.mark = ",",scientific=FALSE), "%"),
        subtitle = paste0("Verlierer von 2020: ", BIP_land_min),
        icon     = icon("chart-line"),
        color    = "red"
      )
    })
    
    output$valueBox_bipworld <- renderValueBox({
      
      bip_daten_2020[bip_daten_2020 == "no data" ] <- NA
      Bip_row <-nrow(bip_daten_2020)
      Bip_sum <-sum(bip_daten_2020[, 'BIP_Wachstum'], na.rm = TRUE)
      
      BIP_avg_world <- round(Bip_sum/Bip_row, 2)
      
      
      
      valueBox(
        paste0(prettyNum(BIP_avg_world,big.mark=".",decimal.mark = ",",scientific=FALSE), "%"),
        subtitle = paste0("Durchschnittliches Wachstum 2020"),
        icon     = icon("chart-line"),
        color    = "light-blue"
      )
    })
    
    names(bip_daten_2020)[names(bip_daten_2020)=="Country"] <- "Länder"
    BIP_table <- bip_daten_2020[, c("Länder", "BIP_Wachstum")]
    names(BIP_table)[names(BIP_table)=="BIP_Wachstum"] <- "Prognose für 2020"
    
    output$bip_sum <- DT::renderDataTable(
      datatable(BIP_table, options = list(
        pageLength = 10, autowidth=TRUE)
      )
    )
    
    names(corona_cases)[names(corona_cases)=="Country.Region"] <- "Länder"
    names(corona_cases)[names(corona_cases)=="total"] <- "Coronafälle"
    corona_table <- corona_cases[, c("Länder", "Coronafälle")]
    
    output$summary <- DT::renderDataTable(
      datatable(corona_table, options = list(
        pageLength = 10, autowidth=TRUE)
      )
    )
})