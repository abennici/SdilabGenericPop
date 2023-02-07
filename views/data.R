#' @title Data Module
#' @description 
#' This module create a tab inside 'Sdilab Generic Pop' app including a dynamic and downloadable data table .
#' @param data subset datatable produced by 'DataConfig module'.  
#' @param dsd metadata list produced by 'QueryInfo module' and including columns information.
#' @param query$data.title mandatory url parameter, give a title to the tabs. 
#' @param query$data.info optional url parameter, allow to add a user guideline or description of tab inside info bubble.
#' @param query$data.format optional url parameter, select the table format :variable in columns if 'wide' or in raws if 'long'. If NULL 'wide'format is select by default.   
#' @param query$data.caption optional parameter, allow to add a caption of table. 
#' @author Alexandre Bennici, \email{bennicialexandre@@gmail.com}
#' 
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
  
  out <-reactiveValues()
  
  observe({
    out$info <- if (!is.null(query$data.info)){query$data.info}else{NULL}
    out$caption <- if (!is.null(query$data.caption)){query$data.caption}else{NULL}
    out$format<-if (!is.null(query$data.format)){query$data.format}else{'wide'}  
  })
  
  output$title_panel <- renderText({
    if (!is.null(query$data.title)){query$data.title}else{"Data"}
  })
    
  output$info <-renderUI({
    if(!is.null(out$info)){
      circleButton(ns("info"),icon = icon("info-circle"),size='xs')
    }else{NULL}
  })

  observeEvent(input$info, {
    showModal(modalDialog(
      if (!is.null(out$info)){out$info}else{NULL}
    ))
  })
  
  output$table <- DT::renderDT(server = FALSE, {
  
    df<-as.data.frame(data)
    pid<-if (!is.null(query$pid)){query$pid}else{NULL}

    
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
  
    sub_dsd<-subset(dsd,MemberCode!="geometry")
        
    colnames(df)<-paste0(sub_dsd$MemberName," [",sub_dsd$MemberCode,"] ",sub_dsd$MeasureUnitSymbol)
    
    if(out$format=='long'){
    df<-data.frame(t(df))
    }
    
    DT::datatable(
            df,
            caption = out$caption,
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