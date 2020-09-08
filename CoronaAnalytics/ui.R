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
      label = HTML("Um nachzuvollziehen, wie sich das Bruttoinlandsprodukt über die Jahre verändert, nutzen Sie den Regler. <br> <br>
                   Wählen Sie ein Jahr aus: "),
      min = as.Date('1980', "%Y"),
      max = as.Date('2021', "%Y"),
      value = as.Date('2020', "%Y"),
      timeFormat = "%Y",
      animate = animationOptions(interval = 1000, loop = FALSE, playButton=icon("google-play"))
      )
    ),
    
    menuItem("Korrelation", icon = icon("chart-bar"), tabName = "KOR",
      menuSubItem("BIP zu Corona in absolut", tabName ="KOR1"),
      menuSubItem("BIP zu Corona in %", tabName="KOR2")
    ),
    menuItem("Ergebnis & Daten", icon = icon("bullseye"), tabName = "RES")
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
    
    tabItem(tabName = "KOR1",
      fluidRow(
          box(
          title="Verhältnis des BIP Wachstum zu den absoluten Coronafällen",
          footer="Mit der Grafik wird versucht, einen Zusammenhang zwischen dem BIP Wachstum und dem Ausbruch der Corona Pandemie darzustellen.
          Hierbei fällt auf, dass es keinen direkten Zusammenhang zwischen den BIP Zahlen und den absoluten Coronafällen gibt.",
          status="primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,
          plotlyOutput("economy"))
        ),
      fluidRow(
        box(title="Neigung",
        textOutput("slant1")
        )
      )
        # column(width = 5, plotOutput("correlation")),
    ),
      tabItem(tabName ="KOR2",
        fluidRow(
          box(
            title="Zusammenhang des BIP Wachstums zu den prozentualen Coronafällen",
            footer="Mit der Grafik wird versucht, einen Zusammenhang zwischen dem BIP Wachstum und dem prozentualen Anteil der Corona Infektionen, bezogen auf die jeweilige Landesbevölkerung, darzustellen. 
            Hierbei fällt auf, dass es einen direkten Zusammenhang zwischen dem Wachstum und den prozentualen Coronafällen gibt.",
            status="primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            plotlyOutput("rlm")
          )
        ),
        fluidRow(
          box(title="Neigung",
              textOutput("slant2")
          )
        )
     ),
    tabItem(tabName="RES",
            fluidRow(
              box(
                title = "Ergebnis",
                HTML("<p>Mit dieser Arbeit wurde versucht, eine Korrelation zwischen der Prognose des BIP Wachstums und der derzeitigen Corona Pandemie herzustellen.<br>
                     Eine der Erkenntnisse dieser Arbeit ist, dass eine zuverlässige Aussage zum BIP Wachstum nicht möglich ist, da die Prognose des BIP nicht aktuell gehalten wird.<br>
                     Dies könnte dazu führen, dass eine gewisse Art von Panik ohne tatsächtliche Grundlage entsteht (zumindest bezogen auf die Wirtschaftszahlen).<br><br>
                     Anhand unseres linearen Regressionsmodells lässt sich veranschaulichen, dass bei einer Durchseuchung der Landesbevölkerung die prognostizierten Wachstumszahlen dennoch beeinflusst wurden (siehe Länder wie Qatar, Panama, Brasilien, USA).<br>
                     </p>"),
                background = "light-blue",
                width=12
              )
            ),
            fluidRow(
              box(
                title= "Daten",
                HTML("<p>Für die hier vorliegende App wurden folgende Quellen verwendet:</p><br>
                      <ul>
                      <li>Für Corona: John Hopkins Institut (https://github.com/CSSEGISandData/COVID-19)</li>
                      <li>Für BIP: International Monetary Fund (https://www.imf.org/external/datamapper/NGDP_RPCH@WEO/OEMDC/ADVEC/WEOWORLD)</li>
                     </ul><br>
                     <p>Um die Aktualität zu gewährleisten, werden die Corona-Daten bei jedem Start der App neu heruntergeladen.</p>"),
                background = "light-blue",
                width=12
              )
            )
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
