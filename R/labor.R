laborUI <- function(id) {
  ns <- NS(id)
  tagList(
    tabBox(
      id = ns("allLabor"),
      type = "tabs",
      width = 12,
      elevation = 4,
      sidebar = boxSidebar(
        id = ns("allLaborSidebar"),
        selectInput(inputId = ns("yearLabor"), 
                    label = " PLFS Year", 
                    c("2019-20","2018-19","2017-18")),
        selectInput(inputId = ns("ageLabor"), 
                    label = " Age Group", 
                    c("15-29","15 and above","All ages"))
      ),
      tabPanel("Overall Distribution", plotlyOutput(ns("allLaborPlot"))),
      tabPanel("Education-wise Distribution", textOutput(ns("edLaborPlot"))),
      tabPanel("Sectoral Distribution", plotlyOutput(ns("sectoralLaborPlot")))
    )
  )
}


laborServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$yearLabor, {
      updateBoxSidebar("allLaborSidebar")
    })
    
    observeEvent(input$ageLabor, {
      updateBoxSidebar("allLaborSidebar")
    })
    
    # State - Overall Labor
    allLaborReact <- reactive({
      req(input$yearLabor)
      req(input$ageLabor)
      
      allLabor_data <- allLabor_data %>% filter(allLabor_data$Year == input$yearLabor) 
      df4 <- allLabor_data %>% filter(allLabor_data$Age == input$ageLabor)                                
      
    })
    
    # State - Labor by Education
    edLaborReact <- reactive({
      req(input$yearLabor)
      req(input$ageLabor)
      
      edLabor_data <- edLabor_data %>% filter(edLabor_data$Year == input$yearLabor) 
      df5 <- edLabor_data %>% filter(edLabor_data$Age == input$ageLabor)                                
      
    })
    
    # State - Labor by Sector
    sectoralLaborReact <- reactive({
      req(input$yearLabor)
      req(input$ageLabor)
      
      sectoralLabor_data <- sectoralLabor_data %>% filter(sectoralLabor_data$Year == input$yearLabor) 
      df6 <- sectoralLabor_data %>% filter(sectoralLabor_data$Age == input$ageLabor)                                
      
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
    
  })
}