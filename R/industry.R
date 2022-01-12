industryUI <- function(id) {
  ns <- NS(id)
  tagList(
    tabBox(
      id = ns("industry"),
      title = "",
      width = 12,
      type = "tabs",
      closable = FALSE,
      solidHeader = TRUE,
      headerBorder = TRUE,
      collapsible = TRUE,
      maximizable = TRUE,
      elevation = 4,
      sidebar = boxSidebar(
        id = ns("industrySidebar"),
        selectInput(
          inputId = ns("indusYear"),
          label = "Year",
          c("2019-20","2018-19","2017-18", "2016-17", "2015-16", "2014-15", "2013-14", "2012-13", "2011-12", "2010-11"))
      ),
      tabPanel("Factories and Workers Scatter-Plot", plotlyOutput(ns("industryScatter")))
    )
  )
}


industryServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # State - Industry and Number of Workers
    indusReact <- reactive({
      req(input$indusYear)
      df3 <- indusData %>% filter(indusData$Year==input$indusYear)
    })
    
    output$industryScatter <- renderPlotly({
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
}