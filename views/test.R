test_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title=uiOutput(ns("title_panel")),value="test",
    a(id = ns("toggleAdvanced"), "Show/hide parameters", href = "#"),
    fluidRow(
    box(id=ns("test"),
    uiOutput(ns("test1")),
    uiOutput(ns("test2"))
  )
  ))
}

test_server <-  function(input, output, session,data,dsd,query) {
    ns <- session$ns 
    
    output$title_panel <- renderText({
      if (!is.null(query$test.title)){query$test.title}else{"Test"}
    })
    
  
  observe({
    shinyjs::hide("test")
    shinyjs::onclick("toggleAdvanced",
                     shinyjs::toggle("test",anim=T)
    )
  })
  
output$test1<-renderUI({
  textInput(ns("test1"), "Text 1")
})
output$test2<-renderUI({  
  selectInput(ns("test2"), label = "Select x-axis Variable:", choices = c("A","B","C"), selected = "A")
})
  
}

# library(shiny)
# 
# ui <- fluidPage(
#   
#   mainPanel(
#     tabsetPanel(id="main",type = "tabs",
#   test_UI("test")))
# )
# 
# server <- function(input, output, session) {
#   callModule(test, "test")
# }
# 
# shinyApp(ui, server)