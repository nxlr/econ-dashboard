agricultureUI <- function(id) {
  ns <- NS(id)
  tagList(
    tabBox(
      id = ns("agriculture"),
      title = "",
      type = "tabs",
      width = 12,
      status = "olive",
      closable = FALSE,
      solidHeader = TRUE,
      headerBorder = TRUE,
      collapsible = TRUE,
      maximizable = TRUE,
      elevation = 4,
      sidebar = NULL,
      tabPanel(paste(state, "Irrigation", sep = " "),
               sidebarLayout(
                 sidebarPanel(
                   width = 2,
                   varSelectInput(
                     inputId = ns("irrigationVar"),
                     label = "Irrigation Data",
                     irrigation_data %>% select(-one_of("Year", "District"))
                   )
                 ),
                 mainPanel(
                   width = 10,
                   tabsetPanel(
                     tabPanel("Plot",
                              highchartOutput(ns("irrigationPlot"))
                     ),
                     tabPanel("Data", HTML("</br>"),
                              DTOutput(ns("irrigationTable"))
                     )
                   )
                 )
               )
      )
      
    )
    
  )
}


agricultureServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Filter irrigation Data
    irrigationData <- reactive({
      irrigation_data %>% dplyr::select("Year", "District", input$irrigationVar)
    })
     
    # Irrigation Variable Table
    output$irrigationTable <- renderDT({
      datatable(irrigationData(), 
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
    
    df <- reactive({
      df <- irrigationData() %>% setNames(c("Year", "District", "Irrigated Area"))
    })
    
    # Irrigation Data Plot
    output$irrigationPlot <- renderHighchart({
      cols <- brewer.pal(12, "Set3")
      
      hc <- df() %>%
      hchart('streamgraph', hcaes(x = Year, y = `Irrigated Area`, 
                                  group = District),
             stacking = "percent",
             borderWidth = 0,
             groupPadding = 0,
             pointPadding  = 0
             ) %>%
        hc_title(text = paste(input$irrigationVar),
                 align = "center") %>%
        hc_colors(cols)
    })
    
  })
}