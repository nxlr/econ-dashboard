educationUI <- function(id) {
  ns <- NS(id)
  tagList(
    tabBox(
      id = ns("education"),
      title = "",
      type = "tabs",
      width = 12,
      closable = FALSE,
      solidHeader = TRUE,
      headerBorder = TRUE,
      collapsible = TRUE,
      maximizable = TRUE,
      elevation = 4,
      sidebar = NULL,
      tabPanel("Literacy Rate",
               
        tabsetPanel(
          tabPanel("Plot",
                   highchartOutput(ns("literacyPlot"))
          ),
          tabPanel("Data",
                   HTML("</br>"), DTOutput(ns("literacyTable"))
          )
        )
      )
    )
  )
}


educationServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Literacy Rate Table
    output$literacyTable <- renderDT({
      datatable(literacyData, 
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
    
    # Literacy Rate Plot
    output$literacyPlot <- renderHighchart({
      cols <- brewer.pal(12, "Set3")
      hc <- literacyData %>%
        hchart('column', hcaes(x = Gender, y = `Literacy Rate`, 
                               group = Year),
        ) %>%
        hc_title(text = paste("Literacy Rate"),
                 align = "center") %>%
        hc_subtitle(text = paste("Source: PLFS"),
                 align = "center") %>%
        hc_plotOptions(series = list(stacking = "normal")) %>%
        hc_xAxis(categories = literacyData$Sector) %>%
        hc_colors(cols)
    })
    
  })
}