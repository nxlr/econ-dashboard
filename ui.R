library(shiny)
library(plotly)
library(bslib)
library(thematic)
library(showtext)
library(highcharter)

inactivity <- "function idleTimer() {
  var t = setTimeout(logout, 5000);
  window.onmousemove = resetTimer; // catches mouse movements
  window.onmousedown = resetTimer; // catches mouse movements
  window.onclick = resetTimer;     // catches mouse clicks
  window.onscroll = resetTimer;    // catches scrolling
  window.onkeypress = resetTimer;  //catches keyboard actions

  function logout() {
    window.location.reload();  //reload the window
  }

  function resetTimer() {
    clearTimeout(t);
    t = setTimeout(logout, 5000);  // time is in milliseconds (1000 is 1 second)
  }
}
idleTimer();"

# Setup the bslib theme object
my_theme <- bs_theme(bootswatch = "materia")

# Let thematic know to update the fonts, too
thematic_shiny(font = "auto")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  #tags$script(inactivity),
  theme = my_theme,
  
  
    # Application title
    #titlePanel(h3("Department of Economics, GJUS&T (Hisar)", align="center")),
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Lato&display=swap');
      @import url('https://fonts.googleapis.com/css2?family=Roboto&display=swap');
      @import url('https://fonts.googleapis.com/css2?family=Nunito:wght@300&family=Playfair+Display+SC:ital,wght@0,400;0,700;1,400&display=swap');
      body {
        background-color: white;
        color: white;
        font-family: 'Roboto', sans-serif;
      },
      h2 {font-family: 'Lato', sans-serif;},
    ")),
    tags$style("
    .navbar-header{
      padding-right: 120px;
      font-family: 'Playfair Display SC';
      
    }")
    
    
  ),
  
    navbarPage(collapsible = TRUE,
      title="Haryana Economy",
      
      tabPanel("GDP", icon = icon("chart-line"),
               
               sidebarLayout( fluid = TRUE,
                 sidebarPanel(
                   selectInput(inputId="plotType", label="Select Data",
                               c("State GDP (Year-wise)"="GSDP_bar", 
                                 "GDP Distribution (District-wise)" = "GDPtree", 
                                 "Sectoral GDP (Year-wise)" = "sectorGDP", 
                                 "District GDP (Year-wise)" = "distBar")
                   ),
                   
                   conditionalPanel(
                     condition = "input.plotType == 'GDPtree'",
                     selectInput(inputId = "yearGDP", label = "Year", c("2017-18", "2016-17", "2015-16", "2014-15", "2013-14", "2012-13", "2011-12"))
                     
                   ),
                   
                   conditionalPanel(
                     condition = "input.plotType == 'sectorGDP'",
                     selectInput(inputId = "sectorYear", label = "Year", c("2020-21","2019-20","2018-19","2017-18", "2016-17", "2015-16", "2014-15", "2013-14", "2012-13", "2011-12","2010-11","2009-10","2008-09","2007-08","2006-07","2005-06","2004-05","2003-04","2002-03","2001-02","2000-01"))
                     
                   ),
                   
                   conditionalPanel(
                     condition = "input.plotType == 'distBar'",
                     selectInput(inputId = "district", 
                                 label = "District", 
                                 c("Panchkula","Ambala","Yamunanagar","Kurushetra","Kaithal",
                                   "Karnal","Sirsa","Jind","Fatehabad","Hisar","Panipat",
                                   "Sonipat","Rohtak","Bhiwani","Jhajjar","Gurugram",
                                   "Faridabad","Rewari","Mahendragarh","Mewat","Palwal")
                     )
                   )
                   
                   
                 ),
                 
                 mainPanel(
                   conditionalPanel(
                     condition = "input.plotType == 'GDPtree'",
                     uiOutput('treeTabs')
                     ),
                   conditionalPanel(
                     condition = "input.plotType == 'distBar'",
                     uiOutput('barTabs')
                     ),
                   conditionalPanel(
                     condition = "input.plotType == 'GSDP_bar'",
                     plotlyOutput("gsdpBars")
                     ),
                   conditionalPanel(
                     condition = "input.plotType == 'sectorGDP'",
                     highchartOutput("sectGDP")
                     )
                   )
                 
                 
               )
      ),
               
      tabPanel("Labour Statistics", icon = icon("users"),
               sidebarLayout(fluid = TRUE,
                             sidebarPanel(
                               selectInput(inputId = "yearLabor", label = " PLFS Year", c("2019-20","2018-19","2017-18")),
                               selectInput(inputId = "ageLabor", label = " Age Group", c("15-29","15 and above","All ages"))
                             ),
                             
                             mainPanel(
                               tabsetPanel(
                                 tabPanel("Overall Distribution", plotlyOutput("allLaborPlot")),
                                 tabPanel("Education-wise Distribution", textOutput("edLaborPlot")),
                                 tabPanel("Sectoral Distribution", plotlyOutput("sectoralLaborPlot"))
                               )
                             )
               )),
      tabPanel("Industry", icon = icon("industry"),
               sidebarLayout(fluid = TRUE,
                  sidebarPanel(
                    selectInput(inputId = "indusYear", 
                                label = "Year", 
                                c("2019-20","2018-19","2017-18", "2016-17", "2015-16", "2014-15", "2013-14", "2012-13", "2011-12", "2010-11"))
                  ),
                  
                  mainPanel(
                    plotlyOutput("scatterFact")
                  )
                )),
      tabPanel("State Finance", icon = icon("rupee-sign")),
      tabPanel("Health", icon = icon("heartbeat")),
      tabPanel("Education", icon = icon("user-graduate")),
      navbarMenu("About",
                 tabPanel("About Us", icon = icon("info-circle"),
                          sidebarPanel(
                            tags$h5("Coming Soon..."),
                          )),
                 tabPanel("Contact", icon = icon("envelope"),
                          sidebarPanel(
                            tags$h5("Coming Soon..."),
                          )),
                 tabPanel("GDP (DFD)", icon = icon("sitemap"),
                          mainPanel( width = 12,
                                     tags$h5("Data Flow Diagram: State GDP calculation of Haryana"),
                                     includeHTML("./datasets/gdp/SGDP DFD Layout (sample).drawio.html")))
                 )
      
                
      
    )

    
))
