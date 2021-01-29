###Template Module
# Function for module UI
mymodule_ui <- function(id) {
  ns <- NS(id)
  
 fluidRow("This is a minimal module structure for SdilabGenericPop")

}

# Function for module server
mymodule_server <- function(input, output, session,data,dsd,query) {
  ns<-session$ns  
  print(data)
  print(dsd)
  out <-reactiveValues()
  observe({
    out$title <- if (!is.null(query$mymodule.title)){query$mymodule.title}
  })
  
}
####