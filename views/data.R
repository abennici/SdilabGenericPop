###Data Module
# Function for module UI
data_ui <- function(id) {
  ns <- NS(id)

tabPanel(title=uiOutput(ns("title_panel")),value="data",
  fluidRow(
    column(1,offset=10, uiOutput(ns("info")))
  ),
  fluidRow(
  div(DTOutput(ns('table'))%>%withSpinner(type = 2),  style = "font-size:80%")
  )
)  
}

# Function for module server
data_server <- function(input, output, session,data,dsd,query) {
  ns<-session$ns  
  
output$title_panel <- renderText({
  if (!is.null(query$data.title)){query$data.title}else{"Data"}
})
  
output$info <-renderUI({
  circleButton(ns("info"),icon = icon("info-circle"),size='xs')
})
  
observeEvent(input$info, {
  showModal(modalDialog(
    if (!is.null(query$data.info)){query$data.info}else{NULL}
  ))
})
  
output$table <- DT::renderDT(server = FALSE, {

  df<-as.data.frame(data)
  data.caption <-if (!is.null(query$data.caption)){query$data.caption}else{NULL}
  pid<-if (!is.null(query$pid)){query$pid}else{NULL}
  data.format<-if (!is.null(query$data.format)){query$data.format}else{'wide'}  
  
  if(length(setdiff('geometry',names(df)))==0){
    df<-subset(df,select=-c(geometry))
  }
  
  for (i in 1:nrow(df)){
    for(j in 1:ncol(df)){
      if(!is.na(df[i,j])&&str_detect(df[i,j],"base64:")){
      df[i,j]<-paste0("<img src=\"data:image/png;base64,",unlist(strsplit(df[i,j], "base64:"))[2],"\" width=\"50%\"></img>")  
      }
    }
  }

  colnames(df)<-paste0(dsd$MemberName," [",dsd$MemberCode,"] ",dsd$MeasureUnitSymbol)
  
  if(data.format=='long'){
  df<-data.frame(t(df))
  
  }
  
  DT::datatable(
          df,
          caption = data.caption,
          extensions = c("Buttons"),
          escape = FALSE,
          options = list(
            dom = 'Bfrtip',
            pageLength=5,
            buttons = list(
              list(extend = 'copy'),
              list(extend = 'csv', filename =  paste0(Sys.Date(),pid), title = NULL, header = TRUE),
              list(extend = 'excel', filename =  paste0(Sys.Date(),pid), title = NULL, header = TRUE),
              list(extend = "pdf", filename = paste0(Sys.Date(),pid), title = NULL, header = TRUE),
              list(extend = 'print')
            ),
            exportOptions = list(
              modifiers = list(page = "all",selected=TRUE)
            )
          )
  )
})

}
####