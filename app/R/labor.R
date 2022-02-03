laborUI <- function(id) {
  ns <- NS(id)
  tagList(
    tabBox(
      id = ns("allLabor"),
      type = "tabs",
      width = 12,
      elevation = 4,
      sidebar = NULL,
      tabPanel(paste(state, "(Labor Stats)", sep = " "),
               sidebarLayout(
                 sidebarPanel(
                   width = 2,
                   selectInput(inputId = ns("yearLabor"), 
                               label = " PLFS Year",
                               unique(allLabor_data$Year)
                   ),
                   selectInput(inputId = ns("ageLabor"), 
                               label = " Age Group",
                               unique(allLabor_data$Age)
                   )
                 ),
                 mainPanel(
                   width = 10,
                   tabsetPanel(
                     tabPanel("Plot",
                              plotlyOutput(ns("allLaborPlot"))
                              ),
                     tabPanel("Data",
                              DTOutput(ns("allLaborTable")) 
                              )
                   )
                 )
               )
      ), 
      tabPanel("Sectoral Distribution",
               sidebarLayout(
                 sidebarPanel(
                   width = 2,
                   selectInput(inputId = ns("yearSectoralLabor"), 
                               label = " PLFS Year",
                               unique(sectoralLabor_data$Year)
                   ),
                   selectInput(inputId = ns("ageSectoralLabor"), 
                               label = " Age Group",
                               unique(sectoralLabor_data$Age)
                   )
                 ),
                 mainPanel(
                   width = 10,
                   tabsetPanel(
                     tabPanel("Plot",
                              plotlyOutput(ns("sectoralLaborPlot"))
                     ),
                     tabPanel("Data",
                              DTOutput(ns("sectoralLaborTable")) 
                     )
                   )
                 )
               )
      ),
      
      tabPanel("Education-wise Distribution",
               sidebarLayout(
                 sidebarPanel(
                   width = 2,
                   selectInput(inputId = ns("yearEdLabor"), 
                               label = " PLFS Year",
                               unique(edLabor_data$Year)
                   ),
                   selectInput(inputId = ns("ageEdLabor"), 
                               label = " Age Group",
                               unique(edLabor_data$Age)
                   )
                 ),
                 mainPanel(
                   width = 10,
                   tabsetPanel(
                     # tabPanel("Plot",
                     #          plotlyOutput(ns("allLaborPlot"))
                     # ),
                     tabPanel("Data",
                              DTOutput(ns("edLaborTable")) 
                     )
                   )
                 )
               )
      )
    )
  )
}


laborServer <- function(id, allLabor_data, edLabor_data, sectoralLabor_data) {
  moduleServer(id, function(input, output, session) {
    
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
      req(input$yearEdLabor)
      req(input$ageEdLabor)
      
      edLabor_data <- edLabor_data %>% filter(edLabor_data$Year == input$yearEdLabor) 
      df5 <- edLabor_data %>% filter(edLabor_data$Age == input$ageEdLabor)                                
      
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
      req(input$yearSectoralLabor)
      req(input$ageSectoralLabor)
      
      sectoralLabor_data <- sectoralLabor_data %>% filter(sectoralLabor_data$Year == input$yearSectoralLabor) 
      df6 <- sectoralLabor_data %>% filter(sectoralLabor_data$Age == input$ageSectoralLabor)                                
      
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