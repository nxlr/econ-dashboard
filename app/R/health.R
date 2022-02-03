healthUI <- function(id) {
  ns <- NS(id)
  tagList(
    tabBox(
      id = ns("health"),
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
      tabPanel("Public Health", 
               sidebarLayout(
                 sidebarPanel(
                   width = 2,
                   varSelectInput(
                     inputId = ns("healthVar"),
                     label = "Health Data",
                     healthData %>% select(-one_of("Year", "District"))
                   )
                 ),
                 mainPanel(
                   width = 10,
                   tabsetPanel(
                     tabPanel("Plot",
                              highchartOutput(ns("healthPlot"))
                     ),
                     tabPanel("Data", HTML("</br>"), 
                              DTOutput(ns("healthTable"))
                       
                     )
                   )
                 )
               )
      )
    )
  )
}


healthServer <- function(id) {
  moduleServer(id, function(input, output, session) {
  
    # Filter irrigation Data
    healData <- reactive({
      healthData %>% dplyr::select("Year", "District", input$healthVar)
    })
    
    # Irrigation Variable Table
    output$healthTable <- renderDT({
      datatable(healData(), 
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
    
    dh <- reactive({
      dh <- healData() %>% setNames(c("Year", "District", "Indicator"))
    })
    
    # Irrigation Data Plot
    output$healthPlot <- renderHighchart({
      hc <- dh() %>% 
        hchart(type = 'column', hcaes(x = District, y = Indicator,
                                      group = Year, color = Indicator)) %>%
        hc_title(text = paste(input$healthVar),
                 align = "center") %>%
        hc_credits(
          enabled = TRUE,
          text = "Source: DESA, Haryana",
          href = "https://esaharyana.gov.in/"
        ) %>%
        hc_add_theme(hc_theme_smpl())
    })
    
    
  })
}