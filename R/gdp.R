gdpUI <- function(id) {
  ns <- NS(id)
  
  tagList(
      tabBox(
        id = ns("gdp"),
        title = "",
        type = "tabs",
        width = 12,
        closable = FALSE,
        solidHeader = TRUE,
        headerBorder = TRUE,
        collapsible = TRUE,
        maximizable = TRUE,
        elevation = 4,
        sidebar = boxSidebar(
          id = ns("gdpSidebar"),
          selectInput(
            inputId = ns("yearGDP"),
            label = "Year",
            c("2017-18", "2016-17", "2015-16", "2014-15", "2013-14", "2012-13", "2011-12")
          ),
          selectInput(
            inputId = ns("district"),
            label = "District",
            c("Panchkula","Ambala","Yamunanagar","Kurushetra","Kaithal",
              "Karnal","Sirsa","Jind","Fatehabad","Hisar","Panipat",
              "Sonipat","Rohtak","Bhiwani","Jhajjar","Gurugram",
              "Faridabad","Rewari","Mahendragarh","Mewat","Palwal")
          ),
          selectInput(inputId = ns("sectorYear"), 
                      label = "Year", 
                      c("2020-21","2019-20","2018-19","2017-18", "2016-17", "2015-16", "2014-15", "2013-14", "2012-13", "2011-12","2010-11","2009-10","2008-09","2007-08","2006-07","2005-06","2004-05","2003-04","2002-03","2001-02","2000-01")
          )
        ),
        tabPanel("GDP", plotlyOutput(ns("gdpBars"))),
        tabPanel("", plotlyOutput(ns("gdpTreeMap"))),
        tabPanel("District GDP", plotlyOutput(ns("districtGDP"))),
        tabPanel("Sectoral GDP", highchartOutput(ns("sectorGDP")))
        
      )
      
    )
}


gdpServer <- function(id, stateGDP, districtGDP, sectoralGDP) {
  moduleServer(id, function(input, output, session) {
    
    # State GDP Bar Plot with Trend
    output$gdpBars <- renderPlotly({
      
      plot_ly(data = stateGDP, x = ~Year, y = ~GSDP, type = 'bar',
              marker=list(color=~GSDP, showscale=FALSE),
              hovertemplate = '₹ %{y:.0f} Lakhs<extra></extra>') %>%
        add_lines(y = ~GSDP, showlegend=FALSE, color = 'black') %>%
        
        config(displaylogo = FALSE,
               modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "pan2d", 
                                          "lasso2d", "hoverClosestCartesian",
                                          "hoverCompareCartesian")
        ) %>% 
        layout(title = list(y=0.95, text="Haryana GDP (in ₹ Lakhs)",
                            font=list(size=18, family="Lato")),
               showlegend=F,
               annotations = list(
                 list(x = -0.1 , y = -0.28, text = "Source: DESA, Haryana",
                      font=list(color="grey", family="Courier New, monospace", size=12),
                      showarrow = F, xref='paper', yref='paper')),
               
               xaxis = list(side="right", showgrid=FALSE, title = "Year"),
               yaxis = list(title = "GDP (2011-12 Prices)", 
                            tickprefix = '₹'),
               paper_bgcolor='#fff0d8',
               plot_bgcolor='#fff0d8'
        )
    })
    
    # User Input for GDP Tree 
    dta2 <- reactive({
      req(input$yearGDP)
      df2 <- districtGDP %>% filter(districtGDP$Year==input$yearGDP)
    })
    
    
    # GDP Tree Map
    output$gdpTreeMap <- renderPlotly({
      plot_ly(
        type = 'treemap',
        labels= dta2()$District,
        parents = c(""), 
        values = dta2()$GDP,
        hovertemplate = paste("District :", dta2()$District,
                              "<br> GDP : ₹ ", dta2()$GDP, " Lakhs",
                              "<extra></extra>"),
        texttemplate = paste(dta2()$District,
                             "<br>", format(round(dta2()$Percent, 2), nsmall = 2),
                             "%"),
        marker=list(colorscale='Hot')
        #marker=list(colors=c("lightgreen", "aqua", "yellow", "purple", "#FFF", "lightgray", "pink"))
      ) %>%
        config(displaylogo = FALSE) %>% 
        layout(title = list(y=0.98, text=paste("Haryana GDP Distribution (in ", input$yearGDP, ")"),
                            font=list(size=18, family="Lato")),
               annotations = list(
                 list(x = 0.0 , y = -0.1, text = "Source: DESA, Haryana",
                      font=list(color="grey", family="Courier New, monospace", size=12),
                      showarrow = F, xref='paper', yref='paper')),
               paper_bgcolor='#fff0d8',
               plot_bgcolor='#fff0d8'
        )    
    })
    
    # User Input for District 
    dta1 <- reactive({
      req(input$district)
      df1 <- districtGDP %>% filter(districtGDP$District==input$district)
    })
    # District GDP Bar Plot with Trend
    output$districtGDP <- renderPlotly({
      
      plot_ly(data = dta1(), x = ~Year, y = ~GDP, type = 'bar', color = ~Year,
              marker=list(color=~GDP, showscale=FALSE),
              hovertemplate = "GDP: %{y:.0f} Lakhs <br>(%{x})</br><extra></extra>") %>%
        add_lines(y = ~GDP, showlegend=FALSE, color = 'gray') %>%
        config(displaylogo = FALSE,
               modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "pan2d", 
                                          "lasso2d", "hoverClosestCartesian",
                                          "hoverCompareCartesian")
        ) %>% 
        layout(title = list(y=0.95, text=paste(input$district, "GDP (in ₹ Lakhs)",sep=" "),
                            font=list(size=18, family="Lato")),
               annotations = list(
                 list(x = 0.0 , y = -0.12, text = "Source: DESA, Haryana",
                      font=list(color="grey", family="Courier New, monospace", size=12),
                      showarrow = F, xref='paper', yref='paper')),
               
               xaxis = list(title = "Year"),
               yaxis = list(title = "GDP (2011-12 Prices)", 
                            tickprefix = '₹'),
               paper_bgcolor='#fff0d8',
               plot_bgcolor='#fff0d8',
               legend = list(title=list(text="Year", font=list(size=16, family="Lato")))
        )    
    })
    
    # User Input Year for State Sectoral GDP
    sectoralGDP_react <- reactive({
      req(input$sectorYear)
      sect <- sectoralGDP %>% filter(sectoralGDP$Year==input$sectorYear)
    })
    
    # Sectoral GDP Plot
    output$sectorGDP <- renderHighchart({
      
      # Set highcharter options
      options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))
      
      
      hc <- sectoralGDP_react() %>%
        hchart(
          "pie", hcaes(x = Sector, y = GVA),
          name = "Sectoral Distribution"
        ) %>%
        hc_title(text = "Sectoral Distribution") %>%
        hc_subtitle(text = "Source: DESA, Haryana") %>%
        hc_tooltip(crosshairs=TRUE, borderWidth=3, sort=TRUE, shared=TRUE, table=TRUE,
                   pointFormat=paste('<br><b>{point.percentage:.1f}%</b>
                                     (GVA: {point.y} Lakhs)'))
      #plot_ly(sectoralGDP_react(), labels = ~Sector, values = ~GVA, type = 'pie')
      
      
      
    })
  })
}