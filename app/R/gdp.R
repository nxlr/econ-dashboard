gdpUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tabBox(
      id = ns("gdp"),
      title = "",
      type = "tabs",
      width = 12,
      status = "olive",
      closable = FALSE,
      solidHeader = TRUE,
      headerBorder = TRUE,
      collapsible = FALSE,
      maximizable = TRUE,
      elevation = 4,
      sidebar = NULL,
      tabPanel(paste(state, "GDP", sep = " "),
               sidebarLayout(
                 sidebarPanel(
                   width = 2,
                   selectInput(
                     inputId = ns("gdpSeries"),
                     label = "GDP Series",
                     c('GDP at 2011-12 Prices','GDP at Current Prices')
                   )
                 ),
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Plot",
                              highchartOutput(ns("gdpBars"))
                     ),
                     tabPanel("Data", HTML("</br>"),
                              DTOutput(ns("gdpTable"))
                     )
                 )
               )
              )
      ),
      tabPanel("District GDP",
               sidebarLayout(
                 sidebarPanel(
                   width = 2,
                   selectInput(
                     inputId = ns("district"),
                     label = "District",
                     unique(districtGDP$District)
                   )
                 ),
                 mainPanel(
                   width = 10,
                   tabsetPanel(
                     tabPanel("Trend",
                              highchartOutput(ns("districtGDP"))
                     ),
                     tabPanel("Data", HTML("</br>"),
                              DTOutput(ns("districtGDPTable"))
                     )
                   )
                 )
               )
      ),
      tabPanel("Districts GDP (Treemap)",
               sidebarLayout(
                 sidebarPanel(
                   width = 2,
                   selectInput(
                     inputId = ns("yearGDP"),
                     label = "Year",
                     rev(unique(districtGDP$Year))
                   )
                 ),
                 mainPanel(
                   width = 10,
                   tabsetPanel(
                     tabPanel("Plot",
                              highchartOutput(ns("gdpTreeMap"))
                     ),
                     tabPanel("Data", HTML("</br>"),
                              DTOutput(ns("gdpTreeMapTable"))
                     )
                   )
                 )
               )
      ),
      tabPanel("Sector GVA (Trend)",
               
               sidebarLayout(
                 sidebarPanel(
                   width = 2,
                   varSelectInput(
                     inputId = ns("gsvaVar"),
                     label = "Sector",
                     sectoralAll %>% select(-one_of("Year"))
                   )
                 ),
                 mainPanel(
                   width = 10,
                   tabsetPanel(
                     tabPanel("Plot", 
                              highchartOutput(ns("gsvaPlot"))
                     ),
                     tabPanel("Data", HTML("</br>"),
                              DTOutput(ns("gsvaTable"))
                     )
                   )
                 )
               )
      ),
      tabPanel("GVA (Pie)",
               sidebarLayout(
                 sidebarPanel(
                   width = 2,
                   selectInput(inputId = ns("sectorYear"), 
                               label = "Year",
                               rev(unique(sectoralGDP$Year))
                   )
                 ),
                 mainPanel(
                   width = 10,
                   tabsetPanel(
                     tabPanel("Plot", 
                              highchartOutput(ns("sectorGDP"))
                     ),
                     tabPanel("Data", HTML("</br>"),
                              DTOutput(ns("sectorGDPTable"))
                     )
                   )
                 )
               )
      ),
      
      tabPanel("GVA (Stacked)",
               tabsetPanel(
                 tabPanel("Plot",
                          highchartOutput(ns("sectorGDPtrend"))
                 ),
                 tabPanel("Data",
                   DTOutput(ns("sectorAll"))
                 )
               )
      )
    )
    
  )
}


gdpServer <- function(id, stateGDP, districtGDP, sectoralGDP) {
  moduleServer(id, function(input, output, session) {
    
    # User Input for GDP Series
    dtaSeries <- reactive({
      req(input$gdpSeries)
      df2 <- gdp_melt %>% filter(gdp_melt$Variable==input$gdpSeries)
    })
    
    
    # GDP Table
    output$gdpTable <- renderDT({
      
      #'arg' should be one of “default”, “bootstrap”, “bootstrap4”, “bootstrap5”, 
      # “bulma”, “dataTables”, “foundation”, “jqueryui”, “semanticui”
      datatable(dtaSeries(), rownames = FALSE,
                extensions = 'Responsive',
                style = "bootstrap5",
                caption = "State GDP in Rs. Crores",
                options = list(
                  columnDefs = list(list(className = 'dt-center', targets = c(1))),
                  autoWidht = TRUE,
                  pageLength = 5,
                  lengthMenu = c(5, 10)
                ),
                class = 'cell-border stripe')
    })
    
    # State GDP Bar Plot with Trend
    
    output$gdpBars <- renderHighchart({
      hc <- highchart() %>% 
        hc_add_series(data = dtaSeries(), type = 'column', 
                      hcaes(x = Year, y = Value, color = Value)) %>%
        hc_title(text = input$gdpSeries,
                 align = "center") %>%
        hc_yAxis(title = list(text = paste("GDP (in ₹ Crores)"))) %>%
        hc_xAxis(title = list(text = paste("Year")), categories = stateGDP$Year) %>%
        hc_tooltip(crosshairs=TRUE, borderWidth=3, sort=TRUE, shared=TRUE, table=TRUE,
                   headerFormat = paste("<b>Year: {point.key}</b>"),
                   pointFormat = paste("</br><b>GDP: {point.y} ₹ Crores</b>")) %>%
        hc_legend(enabled = FALSE) %>%
        hc_credits(
          enabled = TRUE,
          text = "Source: MOSPI and DESA, Haryana",
          href = "https://esaharyana.gov.in/"
        ) %>%
        hc_add_theme(hc_theme_smpl())
    })
    
    # User Input for GDP Tree 
    dta2 <- reactive({
      req(input$yearGDP)
      df2 <- districtGDP %>% filter(districtGDP$Year==input$yearGDP)
    })
    
    # District GDP Tree Map Table
    output$gdpTreeMapTable <- renderDT({
      datatable(dta2(), rownames = FALSE,
                extensions = 'Responsive',
                style = "bootstrap5",
                options = list(
                  columnDefs = list(list(className = 'dt-center', targets = c(1))),
                  autoWidht = TRUE,
                  pageLength = 5,
                  lengthMenu = c(5, 10)
                ),
                class = 'cell-border stripe')
    })
    
    # GDP Tree Map
    output$gdpTreeMap <- renderHighchart({
      hc <- dta2() %>%
        hchart(
          type = "treemap", 
          hcaes(x = District, value = GDP, color = GDP)
        ) %>%
        hc_colorAxis(stops = color_stops(colors = viridis::inferno(10))) %>%
        hc_title(text = paste("Haryana District GDP Tree-Map (", input$yearGDP, ")"),
                 align = "center") %>% 
        hc_subtitle(text = "(at 2011-12 Prices)", 
                    align = "center") %>%
        hc_tooltip(crosshairs=TRUE, borderWidth=3, sort=TRUE, shared=TRUE, table=TRUE,
                   pointHeader = "",
                   pointFormatter = JS("function(){
                   var percent = parseFloat(this.value / this.series.tree.val * 100).toFixed(2);
                   var str1 = '</br><b>' + 'GDP : ' + this.value + ' ₹ Lakhs</b></br>'
                   var str2 = '<b>' + 'Percent : ' + percent + ' %' + '</b>';
                   return (str1 + str2)
                   }")
        ) %>%
        
        hc_credits(
          enabled = TRUE,
          text = "Source: DESA, Haryana",
          href = "https://esaharyana.gov.in/"
        ) %>%
        
        hc_add_theme(hc_theme_smpl())
      
    })
    
    # User Input for District 
    dta1 <- reactive({
      req(input$district)
      df1 <- districtGDP %>% filter(districtGDP$District==input$district)
    })
    
    # District GDP Table
    output$districtGDPTable <- renderDT({
      datatable(dta1(), rownames = FALSE,
                extensions = 'Responsive',
                style = "bootstrap5",
                options = list(
                  columnDefs = list(list(className = 'dt-center', targets = c(1))),
                  autoWidht = TRUE,
                  pageLength = 5,
                  lengthMenu = c(5, 10)
                ),
                class = 'cell-border stripe')
    })
    
    # District GDP Bar Plot with Trend
    output$districtGDP <- renderHighchart({
      hc <- highchart() %>%
        hc_add_series(data = dta1(), type = 'column', 
                      hcaes(x = Year, y = GDP, color = GDP)) %>%
        hc_title(text = paste(input$district, "GDP (in ₹ Lakhs)",sep=" "),
                 align = "center") %>% 
        hc_subtitle(text = "(at 2011-12 Prices)", 
                    align = "center") %>%
        hc_yAxis(title = list(text = "District GDP")) %>%
        hc_xAxis(title = list(text = "Year"), categories = dta1()$Year) %>%
        hc_tooltip(crosshairs=TRUE, borderWidth=3, sort=TRUE, shared=TRUE, table=TRUE,
                   headerFormat = paste("<b>Year: {point.key}</b>"),
                   pointFormat = paste("</br><b>GDP: {point.y} ₹ Lakhs</b>")) %>%
        hc_legend(enabled = FALSE) %>%
        hc_tooltip(borderWidth=3, shared=TRUE,
                   pointFormatter = JS("function(){
                   var str2 = '</br><b>GDP : ' + this.y + ' ₹ Lakhs</b>';
                   return (str2)
                   }")) %>%
        
        hc_credits(
          enabled = TRUE,
          text = "Source: DESA, Haryana",
          href = "https://esaharyana.gov.in/"
        ) %>%
        hc_add_theme(hc_theme_smpl())
    })
    
    # Filter GSVA Data
    gsvaData <- reactive({
      sectoralAll %>% dplyr::select("Year", input$gsvaVar)
    })
    
    # GSVA Variable Table
    output$gsvaTable <- renderDT({
      datatable(gsvaData(), 
                rownames = F,
                style = "bootstrap5",
                caption = "",
                options = list(
                  columnDefs = list(list(className = 'dt-center', targets = c(1))),
                  autoWidht = TRUE,
                  pageLength = 5,
                  lengthMenu = c(5, 10)
                ),
                class = 'cell-border stripe')
    })
    
    dv <- reactive({
      dv <- gsvaData() %>% setNames(c("Year", "Indicator"))
    })
    
    # GSVA Data Plot
    output$gsvaPlot <- renderHighchart({
      hc <- dv() %>% 
        hchart(type = 'column', hcaes(x = Year, y = Indicator,
                                      color = Indicator)) %>%
        hc_title(text = paste(input$gsvaVar),
                 align = "center") %>%
        hc_credits(
          enabled = TRUE,
          text = "Source: DESA, Haryana",
          href = "https://esaharyana.gov.in/"
        ) %>%
        hc_add_theme(hc_theme_smpl())
    })
    
    
    # User Input Year for State Sectoral GDP
    sectoralGDP_react <- reactive({
      req(input$sectorYear)
      sect <- sectoralGDP %>% filter(sectoralGDP$Year==input$sectorYear)
    })
    
    # Sectoral GDP Table
    output$sectorGDPTable <- renderDT({
      datatable(sectoralGDP_react(), rownames = FALSE,
                extensions = 'Responsive',
                style = "bootstrap5",
                options = list(
                  columnDefs = list(list(className = 'dt-center', targets = c(1))),
                  autoWidht = TRUE,
                  pageLength = 5,
                  lengthMenu = c(5, 10)
                ),
                class = 'cell-border stripe')
    })
    
    # Sectoral GDP Plot
    output$sectorGDP <- renderHighchart({
      
      # Set highcharter options
      options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))
      cols <- brewer.pal(12, "Set3")
      
      hc <- sectoralGDP_react() %>%
        hchart(
          "pie", hcaes(x = Sector, y = GVA),
          name = "Sectoral Distribution"
        ) %>%
        hc_title(text = paste("Sectoral Distribution (", input$sectorYear,")"), 
                 align = "center") %>%
        hc_subtitle(text = "Source: DESA, Haryana") %>%
        hc_colors(cols) %>%
        hc_tooltip(crosshairs=TRUE, borderWidth=3, sort=TRUE, shared=TRUE, table=TRUE,
                   pointFormat=paste('</br><b>{point.percentage:.1f}%</b>
                                     (GVA: {point.y} Lakhs)'))
      
    })
    
    # Sector All Data Table
    output$sectorAll <- renderDT({
      datatable(sectoralAll, rownames = FALSE,
                extensions = 'Responsive',
                style = "bootstrap5",
                options = list(
                  columnDefs = list(list(className = 'dt-center', targets = c(1))),
                  autoWidht = TRUE,
                  pageLength = 5,
                  lengthMenu = c(5, 10)
                ),
                class = 'cell-border stripe')
    })
    
    # Sectoral GDP Trend
    output$sectorGDPtrend <- renderHighchart({
      cols <- brewer.pal(12, "Set3")
      hchart(sectoralGDP, "streamgraph",
             hcaes(Year, GVA, group = Sector),
             stacking = "percent",
             borderWidth = 0,
             groupPadding = 0,
             pointPadding  = 0) %>%
        hc_colors(cols) %>%
        hc_size(height = 400) %>%
        hc_yAxis(visible = FALSE)
    })
    
  })
}