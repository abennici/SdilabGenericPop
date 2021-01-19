ui <- fluidPage(
  
setBackgroundColor(color = "white"),
useShinydashboard(),
useShinyjs(),
tags$head(tags$link(rel="stylesheet", type="text/css", href="popup.css")),
  fluidRow(
    column(
      width = 4,
      tags$h4(FlagNameUI(id="name")),
      mainPanel(
        tabsetPanel(id="main",type = "tabs",
            tabPanel(id="hidden","")
        )
      )
    )
  ) 
)
