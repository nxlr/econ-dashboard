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
      tabPanel("GDP",
               tabsetPanel(
                 tabPanel("Plot",
                          highchartOutput(ns("gdpBars"))
                 ),
                 tabPanel("Data", HTML("</br>"),
                          DTOutput(ns("gdpTable"))
                 )
               )
      ),
      tabPanel("GDP Tree Map",
               sidebarLayout(
                 sidebarPanel(
                   width = 2,
                   selectInput(
                     inputId = ns("yearGDP"),
                     label = "Select Year",
                     c("2017-18", "2016-17", "2015-16", "2014-15", "2013-14", "2012-13", "2011-12")
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
      tabPanel("District GDP",
               sidebarLayout(
                 sidebarPanel(
                   width = 2,
                   selectInput(
                     inputId = ns("district"),
                     label = "Select District",
                     c("Panchkula","Ambala","Yamunanagar","Kurushetra","Kaithal",
                       "Karnal","Sirsa","Jind","Fatehabad","Hisar","Panipat",
                       "Sonipat","Rohtak","Bhiwani","Jhajjar","Gurugram",
                       "Faridabad","Rewari","Mahendragarh","Mewat","Palwal")
                   )
                 ),
                 mainPanel(
                   width = 10,
                   tabsetPanel(
                     tabPanel("Plot",
                              highchartOutput(ns("districtGDP"))
                     ),
                     tabPanel("Data", HTML("</br>"),
                              DTOutput(ns("districtGDPTable"))
                     )
                   )
                 )
               )
      ),
      tabPanel("Sectoral GDP Distribution",
               sidebarLayout(
                 sidebarPanel(
                   width = 2,
                   selectInput(inputId = ns("sectorYear"), 
                               label = "Select Year", 
                               c("2020-21","2019-20","2018-19","2017-18", "2016-17", "2015-16", "2014-15", "2013-14", "2012-13", "2011-12","2010-11","2009-10","2008-09","2007-08","2006-07","2005-06","2004-05","2003-04","2002-03","2001-02","2000-01")
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
      tabPanel("Sectoral GDP Trend",
               tabsetPanel(
                 tabPanel("Plot",
                          highchartOutput(ns("sectorGDPtrend"))
                 )
               )
      )
    )
    
  )
}


gdpServer <- function(id, stateGDP, districtGDP, sectoralGDP) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$yearGDP, {
      updateBoxSidebar("gdpSidebar")
    })
    
    observeEvent(input$district, {
      updateBoxSidebar("gdpSidebar")
    })
    
    observeEvent(input$sectorYear, {
      updateBoxSidebar("gdpSidebar")
    })
    
    # GDP Table
    output$gdpTable <- renderDT({
      
      #'arg' should be one of “default”, “bootstrap”, “bootstrap4”, “bootstrap5”, 
      # “bulma”, “dataTables”, “foundation”, “jqueryui”, “semanticui”
      datatable(stateGDP, rownames = FALSE,
                extensions = 'Responsive',
                style = "bootstrap5",
                caption = "State GDP in Rs. Lakhs",
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
      hc <- stateGDP %>% 
        hchart(type = 'column', hcaes(x = Year, y = GSDP, color = GSDP)) %>%
        hc_title(text = "Haryana GDP (in ₹ Lakhs)",
                 align = "center") %>% 
        hc_subtitle(text = "(at 2011-12 Prices)", 
                    align = "center") %>%
        hc_yAxis(text = paste("GDP")) %>%
        hc_tooltip(crosshairs=TRUE, borderWidth=3, sort=TRUE, shared=TRUE, table=TRUE,
                   headerFormat = paste("<b>Year: {point.key}</b>"),
                   pointFormat = paste("</br><b>GDP: {point.y} ₹ Lakhs</b>")) %>%
        hc_credits(
          enabled = TRUE,
          text = "Source: DESA, Haryana",
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
                   headerFormat = paste("<b>District: {point.key}</b>"),
                   pointFormat = paste("</br><b>GDP: {point.value} ₹ Lakhs</b>
                                       </br><b>Percentage: {point.percentage:.1f} %</b>")
        ) %>%
        
        hc_credits(
          enabled = TRUE,
          text = "Source: DESA, Haryana",
          href = "https://esaharyana.gov.in/"
        ) %>%
        
        hc_add_theme(hc_theme_smpl())
      
    })
    
    # output$gdpTreeMap <- renderPlotly({
    #   plot_ly(
    #     type = 'treemap',
    #     labels= dta2()$District,
    #     parents = c(""), 
    #     values = dta2()$GDP,
    #     hovertemplate = paste("District :", dta2()$District,
    #                           "<br> GDP : ₹ ", dta2()$GDP, " Lakhs",
    #                           "<extra></extra>"),
    #     texttemplate = paste(dta2()$District,
    #                          "<br>", format(round(dta2()$Percent, 2), nsmall = 2),
    #                          "%"),
    #     marker=list(colorscale='Hot')
    #     #marker=list(colors=c("lightgreen", "aqua", "yellow", "purple", "#FFF", "lightgray", "pink"))
    #   ) %>%
    #     config(displaylogo = FALSE) %>% 
    #     layout(title = list(y=0.98, text=paste("Haryana GDP Distribution (in ", input$yearGDP, ")"),
    #                         font=list(size=18, family="Lato")),
    #            annotations = list(
    #              list(x = 0.0 , y = -0.1, text = "Source: DESA, Haryana",
    #                   font=list(color="grey", family="Courier New, monospace", size=12),
    #                   showarrow = F, xref='paper', yref='paper')),
    #            paper_bgcolor='#fff0d8',
    #            plot_bgcolor='#fff0d8'
    #     )    
    # })
    
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
      hc <- dta1() %>% 
        hchart(type = 'column', hcaes(x = Year, y = GDP, color = GDP)) %>%
        hc_title(text = paste(input$district, "GDP (in ₹ Lakhs)",sep=" "),
                 align = "center") %>% 
        hc_subtitle(text = "(at 2011-12 Prices)", 
                    align = "center") %>%
        hc_yAxis(text = paste("District GDP")) %>%
        hc_tooltip(crosshairs=TRUE, borderWidth=3, sort=TRUE, shared=TRUE, table=TRUE,
                   headerFormat = paste("<b>Year: {point.key}</b>"),
                   pointFormat = paste("</br><b>GDP: {point.y} ₹ Lakhs</b>")) %>%
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