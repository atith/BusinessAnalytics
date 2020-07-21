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
require(ggplot2)

# import data
# hier müssen wir die Daten definieren die geladen werden sollen

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        
        # covid tab 
        output$clean_date_reactive <- renderText({
            format(as.POSIXct(input$plot_date),"%d %B %Y")
        })
        
        reactive_db = reactive({
            cv_cases %>% filter(date == input$plot_date)
            # reactive = cv_cases %>% filter(date == "2020-04-25")
        })
        
        reactive_db_last24h = reactive({
            cv_cases %>% filter(date == input$plot_date & new_cases>0)
        })
        
        reactive_db_large = reactive({
            large_countries = reactive_db() %>% filter(alpha3 %in% worldcountry$ADM0_A3)
            #large_countries = reactive %>% filter(alpha3 %in% worldcountry$ADM0_A3)
            worldcountry_subset = worldcountry[worldcountry$ADM0_A3 %in% large_countries$alpha3, ]
            large_countries = large_countries[match(worldcountry_subset$ADM0_A3, large_countries$alpha3),]
            large_countries
        })
        
        reactive_db_large_last24h = reactive({
            large_countries = reactive_db_last24h() %>% filter(alpha3 %in% worldcountry$ADM0_A3)
            large_countries = large_countries[order(large_countries$alpha3),]
            large_countries
        })
        
        reactive_polygons = reactive({
            worldcountry[worldcountry$ADM0_A3 %in% reactive_db_large()$alpha3, ]
        })
        
        reactive_polygons_last24h = reactive({
            worldcountry[worldcountry$ADM0_A3 %in% reactive_db_large_last24h()$alpha3, ]
        })
        
        output$reactive_case_count <- renderText({
            paste0(prettyNum(sum(reactive_db()$cases), big.mark=","), " cases")
        })
        
        output$reactive_death_count <- renderText({
            paste0(prettyNum(sum(reactive_db()$deaths), big.mark=","), " deaths")
        })
        
        output$reactive_recovered_count <- renderText({
            paste0(prettyNum(sum(reactive_db()$recovered), big.mark=","), " recovered")
        })
        
        output$reactive_active_count <- renderText({
            paste0(prettyNum(sum(reactive_db()$active_cases), big.mark=","), " active cases")
        })
        
        output$reactive_case_count_China <- renderText({
            paste0("Mainland China: ", prettyNum(sum(subset(reactive_db(), country=="Mainland China")$cases), big.mark=",")," (",
                   prettyNum((cv_aggregated %>% filter(date == input$plot_date & region=="Mainland China"))$new, big.mark=",")," new)")
        })
        
        output$reactive_case_count_row <- renderText({
            paste0("Other: ", prettyNum(sum(subset(reactive_db(), country!="Mainland China")$cases), big.mark=",")," (",
                   prettyNum((cv_aggregated %>% filter(date == input$plot_date & region=="Other"))$new, big.mark=",")," new)")
        })
        
        output$reactive_country_count <- renderText({
            paste0(nrow(subset(reactive_db(), country!="Diamond Princess Cruise Ship")), " countries/regions affected")
        })
        
        output$reactive_new_cases_24h <- renderText({
            paste0((cv_aggregated %>% filter(date == input$plot_date & region=="Global"))$new, " new in last 24h")
        })
        
        output$mymap <- renderLeaflet({ 
            basemap
        })
        
        observeEvent(input$plot_date, {
            leafletProxy("map") %>% 
                clearMarkers() %>%
                clearShapes() %>%
                addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = ~cv_pal(reactive_db_large()$deathsper100k)) %>%
                
                addCircleMarkers(data = reactive_db_last24h(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(new_cases)^(1/5), 
                                 fillOpacity = 0.1, color = covid_col, group = "2019-COVID (new)",
                                 label = sprintf("<strong>%s (past 24h)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Recovered: %d<br/>Cases per 100,000: %g<br/>Deaths per 100,000: %g", reactive_db_last24h()$country, reactive_db_last24h()$new_cases, reactive_db_last24h()$new_deaths, reactive_db_last24h()$new_recovered, reactive_db_last24h()$newper100k, reactive_db_last24h()$newdeathsper100k) %>% lapply(htmltools::HTML),
                                 labelOptions = labelOptions(
                                     style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                                     textsize = "15px", direction = "auto")) %>%
                
                addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/5), 
                                 fillOpacity = 0.1, color = covid_col, group = "2019-COVID (cumulative)",
                                 label = sprintf("<strong>%s (cumulative)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Recovered: %d<br/>Cases per 100,000: %g<br/>Deaths per 100,000: %g", reactive_db()$country, reactive_db()$cases, reactive_db()$deaths,reactive_db()$recovered, reactive_db()$per100k, reactive_db()$deathsper100k) %>% lapply(htmltools::HTML),
                                 labelOptions = labelOptions(
                                     style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                                     textsize = "15px", direction = "auto")) %>%
                
                addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(active_cases)^(1/5), 
                                 fillOpacity = 0.1, color = covid_col, group = "2019-COVID (active)",
                                 label = sprintf("<strong>%s (active)</strong><br/>Confirmed cases: %g<br/>Cases per 100,000: %g<br/><i><small>Excludes individuals known to have<br/>recovered (%g) or died (%g).</small></i>", reactive_db()$country, reactive_db()$active_cases, reactive_db()$activeper100k, reactive_db()$recovered, reactive_db()$deaths) %>% lapply(htmltools::HTML),
                                 labelOptions = labelOptions(
                                     style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                                     textsize = "15px", direction = "auto"))  %>%
                
                addCircleMarkers(data = sars_final, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4), 
                                 fillOpacity = 0.2, color = sars_col, group = "2003-SARS",
                                 label = sprintf("<strong>%s</strong><br/>SARS cases: %g<br/>Deaths: %d<br/>Cases per 100,000: %g", sars_final$country, sars_final$cases, sars_final$deaths, sars_final$per100k) %>% lapply(htmltools::HTML),
                                 labelOptions = labelOptions(
                                     style = list("font-weight" = "normal", padding = "3px 8px", "color" = sars_col),
                                     textsize = "15px", direction = "auto")) %>%
                
                addCircleMarkers(data = h1n1_cases, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(projected_deaths)^(1/4), 
                                 fillOpacity = 0.2, color = h1n1_col, group = "2009-H1N1 (swine flu)",
                                 label = sprintf("<strong>%s</strong><br/>H1N1 deaths (confirmed): %g<br/>H1N1 deaths (estimated): %g", h1n1_cases$region, h1n1_cases$deaths, h1n1_cases$projected_deaths) %>% lapply(htmltools::HTML),
                                 labelOptions = labelOptions(
                                     style = list("font-weight" = "normal", padding = "3px 8px", "color" = h1n1_col),
                                     textsize = "15px", direction = "auto")) %>%
                
                addCircleMarkers(data = ebola_cases, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4), 
                                 fillOpacity = 0.2, color = ebola_col, group = "2014-Ebola",
                                 label = sprintf("<strong>%s</strong><br/>Ebola cases: %g<br/>Deaths: %d", ebola_cases$country, ebola_cases$cases, ebola_cases$deaths) %>% lapply(htmltools::HTML),
                                 labelOptions = labelOptions(
                                     style = list("font-weight" = "normal", padding = "3px 8px", "color" = ebola_col),
                                     textsize = "15px", direction = "auto"))
        })
        
        output$cumulative_plot <- renderPlot({
            cumulative_plot(cv_aggregated, input$plot_date)
        })
        
        output$epi_curve <- renderPlot({
            new_cases_plot(cv_aggregated, input$plot_date)
        })
        

    }
    
