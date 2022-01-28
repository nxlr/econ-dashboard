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
      tabPanel("Overall Distribution", plotlyOutput(ns("allLaborPlot")),
               HTML("<br/>"), DTOutput(ns("allLaborTable"))), 
      tabPanel("Education-wise Distribution", DTOutput(ns("edLaborTable"))), 
      tabPanel("Sectoral Distribution", plotlyOutput(ns("sectoralLaborPlot")),
               HTML("<br/>"), DTOutput(ns("sectoralLaborTable")))
    )
  )
}


laborServer <- function(id, allLabor_data, edLabor_data, sectoralLabor_data) {
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
    
    output$allLaborTable <- renderDT({
      datatable(allLaborReact(), rownames = FALSE,
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
    
    # State - Labor by Education
    edLaborReact <- reactive({
      req(input$yearLabor)
      req(input$ageLabor)
      
      edLabor_data <- edLabor_data %>% filter(edLabor_data$Year == input$yearLabor) 
      df5 <- edLabor_data %>% filter(edLabor_data$Age == input$ageLabor)                                
      
    })
    
    output$edLaborTable <- renderDT({
      datatable(edLaborReact(), rownames = FALSE,
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
    
    # State - Labor by Sector
    sectoralLaborReact <- reactive({
      req(input$yearLabor)
      req(input$ageLabor)
      
      sectoralLabor_data <- sectoralLabor_data %>% filter(sectoralLabor_data$Year == input$yearLabor) 
      df6 <- sectoralLabor_data %>% filter(sectoralLabor_data$Age == input$ageLabor)                                
      
    })
    
    output$sectoralLaborTable <- renderDT({
      datatable(sectoralLaborReact(), rownames = FALSE,
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
    
    output$allLaborPlot <- renderPlotly({
      rural <- allLaborReact() %>% filter(allLaborReact()$Sector == "Rural") %>%
        group_by(Gender) %>% 
        arrange(Gender) %>%
        plot_ly(
          x = ~Variable, 
          y = ~Value,
          color= ~Gender,
          colors = viridis(3),
          type = 'bar',
          legendgroup =~Gender,
          hovertemplate = "%{y:.2f}%",
          showlegend = T) %>% 
        layout(xaxis = list(title = "Rural"))
      
      urban <- allLaborReact() %>% filter(allLaborReact()$Sector == "Urban") %>%
        group_by(Gender) %>% 
        arrange(Gender) %>%
        plot_ly(
          x = ~Variable, 
          y = ~Value, 
          color= ~Gender,
          colors = viridis(3),
          type = 'bar',
          legendgroup =~Gender,
          hovertemplate = "%{y:.2f}%",
          showlegend = F) %>% 
        layout(xaxis = list(title = "Urban"))
      
      total <- allLaborReact() %>% filter(allLaborReact()$Sector == "Rural+Urban") %>%
        group_by(Gender) %>% 
        arrange(Gender) %>%
        plot_ly(
          x = ~Variable, 
          y = ~Value, 
          color= ~Gender,
          colors = viridis(3),
          type = 'bar',
          legendgroup =~Gender,
          hovertemplate = "%{y:.2f}%",
          showlegend = F) %>% 
        layout(xaxis = list(title = "Rural+Urban"))
      
      subplot(rural, urban, total ,titleX = T, shareY = T) %>% 
        layout(barmode = 'group', showlegend = T,
               yaxis = list(title = "Rates"))
    })
    
    output$sectoralLaborPlot <- renderPlotly({
      rural <- sectoralLaborReact() %>% filter(sectoralLaborReact()$Sector == "Rural") %>%
        group_by(Variable) %>% 
        arrange(Variable) %>%
        plot_ly(
          x = ~Gender, 
          y = ~Value,
          color= ~Variable,
          colors = 'Accent',
          type = 'bar',
          legendgroup =~Variable,
          hovertemplate = "%{y:.2f}%",
          showlegend = T) %>% 
        layout(xaxis = list(title = "Rural"))
      
      urban <- sectoralLaborReact() %>% filter(sectoralLaborReact()$Sector == "Urban") %>%
        group_by(Variable) %>% 
        arrange(Variable) %>%
        plot_ly(
          x = ~Gender, 
          y = ~Value, 
          color= ~Variable,
          colors = 'Accent',
          type = 'bar',
          legendgroup =~Variable,
          hovertemplate = "%{y:.2f}%",
          showlegend = F) %>% 
        layout(xaxis = list(title = "Urban"))
      
      total <- sectoralLaborReact() %>% filter(sectoralLaborReact()$Sector == "Rural+Urban") %>%
        group_by(Variable) %>% 
        arrange(Variable) %>%
        plot_ly(
          x = ~Gender, 
          y = ~Value, 
          color= ~Variable,
          colors = 'Accent',
          type = 'bar',
          legendgroup =~Variable,
          hovertemplate = "%{y:.2f}%",
          showlegend = F) %>% 
        layout(xaxis = list(title = "Rural+Urban"))
      
      subplot(rural, urban, total ,titleX = T, shareY = T) %>% 
        layout(barmode = 'stack', showlegend = T,
               yaxis = list(title = "Sectoral Distribution of Labor"))
    })
    
    output$edLaborPlot <- renderHighchart({
      
    })
    
  })
}