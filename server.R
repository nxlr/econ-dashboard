# Define server logic
shinyServer(function(input, output, session) {

  session$allowReconnect("force")
  
  gdpServer(state, stateGDP, districtGDP, sectoralGDP)
  agricultureServer(state)
  laborServer(state)
  industryServer(state)
  financeServer(state)
  healthServer(state)
  educationServer(state)
})

# output$gdpMap <- renderPlotly(
#   gdpMapPlot(shapeFile)  
# )

# Function to Plot Haryana Map
# gdpMapPlot <- function(dat) {
#   return(
#     ggplotly(
#           ggplot(dat) +
#           geom_sf(aes(fill=DISTRICT)) +
#           
#           
#           labs(title = "Haryana District-wise GDP") +
# 
#           theme(rect = element_blank(),
#                 axis.ticks = element_blank(),
#                 axis.text.x = element_blank(),
#                 axis.text.y = element_blank()
#           )
#         
#     )
#     %>% layout(showlegend = F,
#                annotations = list(
#                  list(x = 0.0 , y = 1.05, text = "Source: Statistical Abstract of Haryana, DESA.",
#                       font=list(
#                         color="grey", family="Courier New, monospace", size=12
#                       ),
#                       showarrow = F, xref='paper', yref='paper')
#                  )
#         )
#   )
# }

