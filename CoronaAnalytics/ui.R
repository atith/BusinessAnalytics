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



sidebar <- dashboardSidebar(width = 300,
  sidebarMenu(id="sidebarid", 
    menuItem("COVID-19 Ausbreitung", icon = icon("globe-europe"), tabName = "COV"),
    conditionalPanel(
      'input.sidebarid == "COV"',
      uiOutput("outslider")
    ),
    menuItem("Bruttoinlandsprodukt", icon = icon("euro-sign"), tabName = "BIP"),
    
    conditionalPanel(
      'input.sidebarid == "BIP"',
      sliderInput(
      "plot_year",
      label = HTML('Wähle ein Datum: '),
      min = as.Date('1980', "%Y"),
      max = as.Date('2021', "%Y"),
      value = as.Date('2020', "%Y"),
      timeFormat = "%Y",
      )
    ),
    
    menuItem("Korrelation", icon = icon("chart-bar"), tabName = "KOR")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "COV",
      fluidRow(
        valueBoxOutput("valueBox_confirmed"),
        valueBoxOutput("valueBox_deceased"),
        valueBoxOutput("valueBox_recovered")
      ),
      
      #fluidRow(
      #column(width = 12,uiOutput("outslider"))
      #),
      
      fluidRow(
        column(width = 7, leafletOutput("weltkarte")),
        column(width = 5, DT::dataTableOutput("summary"))
      )
    ),
    
    tabItem(tabName = "BIP",
      fluidRow(
        valueBoxOutput("valueBox_maxbip"), #BIP BIP Gewinner
        valueBoxOutput("valueBox_lowbip"), #BIP Verlierer
        valueBoxOutput("valueBox_bipworld") #Wachstum
      ),
            
      
      fluidRow(
        column(width = 7, leafletOutput("weltkarte2")),
        column(width = 5, DT::dataTableOutput("bip_sum"))
      )
    ),
    
    tabItem(tabName = "KOR",
      fluidRow(
        column(width = 6, plotlyOutput("economy")),
        # column(width = 5, plotOutput("correlation")),
        column(width = 5, plotOutput("rlm"))
      ),      
    )
    
  )
)

shinyUI(
  dashboardPage(skin="blue",
    dashboardHeader(title = "Auswirkung einer Pandemie auf die Wirtschaft", titleWidth = 300 ),
    sidebar,
    body
  )
)
