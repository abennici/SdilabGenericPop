###Data Module
# Function for module UI
ird_image_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(title=uiOutput(ns("title_panel")),value="ird",
           fluidRow(
           uiOutput(ns("img"))
           )
  )  
}

# Function for module server
ird_image_server <- function(input, output, session,data,dsd,query) {
  ns<-session$ns  
  
  output$img <-renderUI({
     df<-as.data.frame(data)
     ird_image.caption <-if (!is.null(query$ird_image.caption)){query$ird_image.caption}else{NULL}
     ird_image.imageCol <-if (!is.null(query$ird_image.imageCol)){query$ird_image.imageCol}else{NULL}
     
     if(!is.null(img)||img==""){
         img<-df[ird_image.imageCol][1]
         img_clean<-str_replace(img,"base64:","")
         img_url<-sprintf("<center><img src=\"data:image/png;base64,\n%s\" alt=\"image\" /></center>",  img_clean)
          HTML(img_url)
      }else{
      div("No image to display !")
      }
  })
  
}
####