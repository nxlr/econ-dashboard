packages <- c("shiny", "dplyr", "plotly", "ggthemes", "thematic", "showtext",
              "readxl", "tidyr", "ggstream", "viridis", "hrbrthemes", "reshape2", 
              "highcharter", "shinydashboard", "shinydashboardPlus", "shinyWidgets",
              "waiter")

install_if_missing <- function(p) {
  if (!p %in% rownames(installed.packages())) {
    install.packages(p)
  }
}

invisible(sapply(packages, install_if_missing))