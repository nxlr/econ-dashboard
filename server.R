# Define server logic
shinyServer(function(input, output, session) {
  
  session$allowReconnect("force")
  
    #shapeFile <- read_sf(dsn = "./www/Layers_DISTRICTS-polygon.shx")
    districtGDP <- read_excel("./datasets/gdp/districts_gdp.xlsx")
    stateGDP <- read_excel("./datasets/gdp/state_gdp_2011_12_prices.xlsx")
    indusData <- read_excel("./datasets/industry/industry_haryana.xlsx")
    sectoralGDP <- read_excel("./datasets/gdp/sectoral_gdp.xlsx")
    allLabor_data_tmp <- read_excel("./datasets/labor/labor_overall_haryana.xlsx")
    allLabor_data <- melt(allLabor_data_tmp, id=c("Year","Age","Sector","Gender"))
    edLabor_data_tmp <- read_excel("./datasets/labor/labor_education_haryana.xlsx")
    edLabor_data <- melt(edLabor_data_tmp, id=c("Year", "Age", "Sector", "Gender"))
    sectoralLabor_data_tmp <- read_excel("./datasets/labor/labor_sectoral_haryana.xlsx")
    sectoralLabor_data <- melt(sectoralLabor_data_tmp, id=c("Year", "Age", "Sector", "Gender"))
    #indus1 <- as.data.frame(read_excel("./datasets/industry/indus1.xlsx"))
    
    # output$gdpMap <- renderPlotly(
    #   gdpMapPlot(shapeFile)  
    # )
    
    dta1 <- reactive({
      req(input$district)
      df1 <- districtGDP %>% filter(districtGDP$District==input$district)
    })
    
    dta2 <- reactive({
      req(input$yearGDP)
      df2 <- districtGDP %>% filter(districtGDP$Year==input$yearGDP)
    })
    
    sectoralGDP_react <- reactive({
      req(input$sectorYear)
      sect <- sectoralGDP %>% filter(sectoralGDP$Year==input$sectorYear)
    })
    
    indusReact <- reactive({
      req(input$indusYear)
      df3 <- indusData %>% filter(indusData$Year==input$indusYear)
    })
    
    allLaborReact <- reactive({
      req(input$yearLabor)
      req(input$ageLabor)
      
      allLabor_data <- allLabor_data %>% filter(allLabor_data$Year == input$yearLabor) 
      df4 <- allLabor_data %>% filter(allLabor_data$Age == input$ageLabor)                                
                                            
    })
    
    edLaborReact <- reactive({
      req(input$yearLabor)
      req(input$ageLabor)
      
      edLabor_data <- edLabor_data %>% filter(edLabor_data$Year == input$yearLabor) 
      df5 <- edLabor_data %>% filter(edLabor_data$Age == input$ageLabor)                                
      
    })
    
    sectoralLaborReact <- reactive({
      req(input$yearLabor)
      req(input$ageLabor)
      
      sectoralLabor_data <- sectoralLabor_data %>% filter(sectoralLabor_data$Year == input$yearLabor) 
      df6 <- sectoralLabor_data %>% filter(sectoralLabor_data$Age == input$ageLabor)                                
      
    })
    
    output$barTabs <- renderUI({
        plotlyOutput("gdpBars")
    })
    
    output$treeTabs <- renderUI({
        plotlyOutput("gdpTree")
    })
    
    
    output$allLaborPlot <- renderPlotly({
      rural <- allLaborReact() %>% filter(allLaborReact()$Sector == "Rural") %>%
        group_by(Gender) %>% 
        arrange(Gender) %>%
        plot_ly(
          x = ~variable, 
          y = ~value,
          color= ~Gender,
          colors = 'Pastel1',
          type = 'bar',
          legendgroup =~Gender,
          hovertemplate = "%{y:.2f}%",
          showlegend = T) %>% 
        layout(xaxis = list(title = "Rural"))
      
      urban <- allLaborReact() %>% filter(allLaborReact()$Sector == "Urban") %>%
        group_by(Gender) %>% 
        arrange(Gender) %>%
        plot_ly(
          x = ~variable, 
          y = ~value, 
          color= ~Gender,
          colors = 'Pastel1',
          type = 'bar',
          legendgroup =~Gender,
          hovertemplate = "%{y:.2f}%",
          showlegend = F) %>% 
        layout(xaxis = list(title = "Urban"))
      
      total <- allLaborReact() %>% filter(allLaborReact()$Sector == "Rural+Urban") %>%
        group_by(Gender) %>% 
        arrange(Gender) %>%
        plot_ly(
          x = ~variable, 
          y = ~value, 
          color= ~Gender,
          colors = 'Pastel1',
          type = 'bar',
          legendgroup =~Gender,
          hovertemplate = "%{y:.2f}%",
          showlegend = F) %>% 
        layout(xaxis = list(title = "Rural+Urban"))
      
      subplot(rural, urban, total ,titleX = T, shareY = T) %>% 
        layout(barmode = 'group', showlegend = T,
               yaxis = list(title = "Rates"))
    })
    
    output$edLaborPlot <- renderText({
      paste("Coming Soon...")
      
    })
    
    output$sectoralLaborPlot <- renderPlotly({
      rural <- sectoralLaborReact() %>% filter(sectoralLaborReact()$Sector == "Rural") %>%
        group_by(variable) %>% 
        arrange(variable) %>%
        plot_ly(
          x = ~Gender, 
          y = ~value,
          color= ~variable,
          colors = 'Pastel2',
          type = 'bar',
          legendgroup =~variable,
          hovertemplate = "%{y:.2f}%",
          showlegend = T) %>% 
        layout(xaxis = list(title = "Rural"))
      
      urban <- sectoralLaborReact() %>% filter(sectoralLaborReact()$Sector == "Urban") %>%
        group_by(variable) %>% 
        arrange(variable) %>%
        plot_ly(
          x = ~Gender, 
          y = ~value, 
          color= ~variable,
          colors = 'Pastel2',
          type = 'bar',
          legendgroup =~variable,
          hovertemplate = "%{y:.2f}%",
          showlegend = F) %>% 
        layout(xaxis = list(title = "Urban"))
      
      total <- sectoralLaborReact() %>% filter(sectoralLaborReact()$Sector == "Rural+Urban") %>%
        group_by(variable) %>% 
        arrange(variable) %>%
        plot_ly(
          x = ~Gender, 
          y = ~value, 
          color= ~variable,
          colors = 'Pastel2',
          type = 'bar',
          legendgroup =~variable,
          hovertemplate = "%{y:.2f}%",
          showlegend = F) %>% 
        layout(xaxis = list(title = "Rural+Urban"))
      
      subplot(rural, urban, total ,titleX = T, shareY = T) %>% 
        layout(barmode = 'stack', showlegend = T,
               yaxis = list(title = "Sectoral Distribution of Labor"))
    })
    
    output$gdpBars <- renderPlotly({
      
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
    
    output$gdpTree <- renderPlotly({
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
    
    output$sectGDP <- renderHighchart({
      
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
    output$gsdpBars <- renderPlotly({
      
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
    
    output$scatterFact <- renderPlotly({
      m <- list(
        l = 70,
        r = 50,
        b = 80,
        t = 30,
        pad = 1
      )
      plot_ly(data=indusReact(), x=~`Number of Registered Working Factories`,
              y=~`No. of Workers Employed`,
              mode = 'markers',
              size = ~`Number of Registered Working Factories`,
              sizes = c(100,1000),
              color = ~District,
              colors = "Set2",
              marker = list(opacity = 0.6, sizemode = 'area')
      )%>%
        
        config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "pan2d", 
                                          "lasso2d", "hoverClosestCartesian",
                                          "hoverCompareCartesian"),
               displaylogo = FALSE) %>% 
        layout(title = list(y=0.98, 
                            text=paste("Factories and Workers in Districts (",
                                       input$indusYear, 
                                       ")"),
                            font=list(size=16, family="Lato")),
               showlegend=T,
               margin = m,
               annotations = list(
                 list(x = 1.2 , y = -0.12, text = "Source: DESA, Haryana",
                      font=list(color="grey", family="Courier New, monospace", size=12),
                      showarrow = F, xref='paper', yref='paper')),
               
               xaxis = list(side="right", showgrid=T, 
                            title = "Number of Registered Working Factories (log)",
                            type = "log",
                            dtick = 1),
               yaxis = list(title = "Number of Workers Employed (log)",  showgrid=T,
                            type = "log",
                            dtick = 1),
               paper_bgcolor='#fff0d8',
               plot_bgcolor='#fff0d8'
        ) 
    })
    
    
})


# Function to Plot Haryana Map
gdpMapPlot <- function(dat) {
  return(
    ggplotly(
          ggplot(dat) +
          geom_sf(aes(fill=DISTRICT)) +
          
          
          labs(title = "Haryana District-wise GDP") +

          theme(rect = element_blank(),
                axis.ticks = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_blank()
          )
        
    )
    %>% layout(showlegend = F,
               annotations = list(
                 list(x = 0.0 , y = 1.05, text = "Source: Statistical Abstract of Haryana, DESA.",
                      font=list(
                        color="grey", family="Courier New, monospace", size=12
                      ),
                      showarrow = F, xref='paper', yref='paper')
                 )
        )
  )
}

