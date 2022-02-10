financeUI <- function(id) {
  ns <- NS(id)
  tagList(
    tabBox(
      id = ns("finance"),
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
      tabPanel("Public Finance",
               sidebarLayout(
                 sidebarPanel(
                   width = 2,
                   varSelectInput(
                     inputId = ns("financeVar"),
                     label = "Finance Data",
                     financeData %>% select(-one_of("Year"))
                   )
                 ),
                 mainPanel(
                   width = 10,
                   tabsetPanel(
                     tabPanel("Plot",
                              highchartOutput(ns("financePlot"))
                     ),
                     tabPanel("Data", HTML("</br>"),
                              DTOutput(ns("financeTable"))
                     )
                   )
                 )
               )
      )
    )
  )
}


financeServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Filter irrigation Data
    finData <- reactive({
      financeData %>% dplyr::select("Year", input$financeVar)
    })
    
    # Irrigation Variable Table
    output$financeTable <- renderDT({
      datatable(finData(), 
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
      df <- finData() %>% setNames(c("Year", "Value"))
    })
    
    # Irrigation Data Plot
    output$financePlot <- renderHighchart({
      
      if (input$financeVar == "Total Tax to GDP Ratio" || 
          input$financeVar == "State's Own -Tax Revenue to GDP Ratio") {
        rp = ""
      } else {
        rp = "(₹ Crores)"
      }
      
      hc <- df() %>% 
        hchart(type = 'column', name = 'Value', 
               hcaes(x = Year, y = Value, color = Value)) %>%
        hc_title(text = paste(input$financeVar, rp, sep = " "),
                 align = "center") %>%
        hc_credits(
          enabled = TRUE,
          text = "Source: DESA, Haryana",
          href = "https://esaharyana.gov.in/"
        ) %>%
        hc_add_theme(hc_theme_smpl())
      
      hc %>%
        hc_add_series(data = df(), name = 'Value', type = 'spline', 
                      hcaes(x = Year, y = Value, color = Value))
      
    })
    
    
  })
}