ui <- fluidPage(
  
setBackgroundColor(color = "white"),
useShinydashboard(),# to use ShinyDashboard widgets
useShinyjs(),#to call javascript code
tags$head(tags$link(rel="stylesheet", type="text/css", href="popup.css")),
  fluidRow(
    column(
      width = 4,
      tags$h4(FlagNameUI(id="name")),
      mainPanel(
        withSpinner(uiOutput("tabs"))
      )
    )
  ) 
)
