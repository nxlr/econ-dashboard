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
      sidebar = NULL,
      tabPanel(paste(state, "(Factories and Workers)", sep = " "),
               sidebarLayout(
                 sidebarPanel(
                   width = 2,
                   selectInput(
                     inputId = ns("indusYear"),
                     label = "Year",
                     unique(indusData$Year)
                  )
                 ),
                 mainPanel(
                   width = 10,
                   tabsetPanel(
                     tabPanel("Plot",
                       highchartOutput(ns("industryBubble"))
                     ),
                     tabPanel("Data",
                       DTOutput(ns("industryTable"))
                     )
                   )
                 )
               )
      )
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
    
    output$industryTable <- renderDT({
      datatable(indusReact(), rownames = FALSE,
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
    
    output$industryBubble <- renderHighchart({
      cols <- brewer.pal(12, "Set1")
      hc <- indusReact() %>% 
        hchart(
          'bubble', hcaes(x = `Number of Registered Working Factories`, 
                           y = `No. of Workers Employed`, 
                           size = `Number of Registered Working Factories`,
                           group = District),
          maxSize = "20%"
        ) %>%
        hc_title(text = paste("Factories and Workers in Districts (",
                              input$indusYear, 
                              ")"),
                 align = "center") %>%
        hc_tooltip(crosshairs=TRUE, borderWidth=3, sort=TRUE, shared=TRUE, table=TRUE,
                   headerFormat = paste("<b>District: {series.name}</b> </br>
                                        <b>Number of Registered Working Factories: {point.key}</b>"),
                   pointFormat = paste("</br><b>Number of Workers Employed: {point.y}</b>")) %>%
        hc_credits(
          enabled = TRUE,
          text = "Source: DESA, Haryana",
          href = "https://esaharyana.gov.in/"
        ) %>%
        hc_legend(enabled = TRUE, layout= 'horizontal') %>%
        hc_yAxis(type="logarithmic") %>%
        hc_xAxis(type= "logarithmic", max=4000) %>%
        hc_colors(cols) %>%
        hc_add_theme(hc_theme_smpl())
    })
    
  })
}