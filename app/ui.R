shinyUI(dashboardPage(
  title = HTML("GJUST Economy Dashboard"),
  fullscreen = TRUE,
  dark = NULL,
  header = dashboardHeader(
    title = dashboardBrand(
      title = "Economy Dashboard",
      color = "olive",
      href = "",
      image = "https://upload.wikimedia.org/wikipedia/commons/1/14/Indian_Rupee_symbol_in_circle_PD_version.svg",
    ),
    
    skin = "light",
    status = "white",
    border = TRUE,
    sidebarIcon = icon("bars"),
    controlbarIcon = icon("th"),
    fixed = TRUE
  ),
  
  sidebar = dashboardSidebar(
    skin = "light",
    status = "olive",
    elevation = 5,
    fixed = TRUE,
    collapsed = TRUE,
    sidebarMenu(
      menuItem(
        "Gross Domestic Product",
        tabName = "gdp",
        icon = icon("chart-line")
      ),
      menuItem(
        "Agriculture",
        tabName = "agriculture",
        icon = icon("seedling")
      ),
      menuItem(
        "Industry",
        tabName = "industry",
        icon = icon("industry")
      ),
      menuItem(
        "Labor Statistics",
        tabName = "labor",
        icon = icon("users-cog")
      ),
      menuItem(
        "Public Finance",
        tabName = "finance",
        icon = icon("rupee-sign")
      ),
      menuItem(
        "Health",
        tabName = "health",
        icon = icon("heartbeat")
      ),
      menuItem(
        "Education",
        tabName = "education",
        icon = icon("user-graduate")
      )
      
    )
  ),
  
  footer = dashboardFooter(
    left = p(HTML("Department of Economics,<br/>
                     Guru Jambheshwar University of Science and Technology,<br/>
                     Hisar, Haryana."))
  ),
  body = dashboardBody(
    
    # Auto adjust dimensions of select input box.
    tags$head(
      tags$style(HTML("
      .selectize-input {
        height: auto;
      }"))
    ),
    
    # Use Waiter if Loading Output Plots take time
    useWaiter(), 
    autoWaiter(html = tagList(spin_circle(), h5("Loading..."))),
    
    tabItems(
      tabItem(
        tabName = "gdp",
        gdpUI(state)
      ),
      tabItem(
        tabName = "agriculture",
        agricultureUI(state)
      ),
      tabItem(
        tabName = "industry",
        industryUI(state)
      ),
      tabItem(
        tabName = "labor",
        laborUI(state)
      ),
      tabItem(
        tabName = "finance",
        financeUI(state)
      ),
      tabItem(
        tabName = "health",
        healthUI(state)
      ),
      tabItem(
        tabName = "education",
        educationUI(state)
      )
    )
  )
)
)