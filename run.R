packages <- c("shiny", "dplyr", "plotly", "ggthemes", "thematic", "showtext",
              "sf", "readxl", "tidyr", "ggstream", "viridis", "hrbrthemes")

install.packages(setdiff(packages, rownames(installed.packages())))  


library(shiny)

port <- Sys.getenv('PORT')

shiny::runApp(
  appDir = getwd(),
  host = '0.0.0.0',
  port = as.numeric(port)
)