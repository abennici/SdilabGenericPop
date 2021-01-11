###Module
# Function for module UI
DataTableUI <- function(id) {
  ns <- NS(id)
  # Options for Spinner
  options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=1)
  #tabPanel("DataTable", DTOutput(ns('table')))
  tabPanel("Data", div(DTOutput(ns('table'))%>%withSpinner(type = 2),  style = "font-size:80%"))
  
}

# Function for module server logic
DataTable <- function(input, output, session,data,dsd,pid) {
  observe({
    ###Reformat
    tab<-as.data.frame(data())
    if(length(setdiff('geometry',names(tab)))==0){
    tab<-subset(tab,select=-c(geometry))
    }
    name<-data.frame(MemberCode=names(tab))
    name<-name%>%left_join(dsd(),by="MemberCode")
    label<-paste0(name$MemberName," [",name$MemberCode,"] ",name$MeasureUnitSymbol)
    names(tab)<-label
    

      output$table <- DT::renderDT(server = FALSE, {
        DT::datatable(
          tab,
          extensions = c("Buttons"),
          options = list(
            dom = 'Bfrtip',
            pageLength=5,
            buttons = list(
              list(extend = 'copy'),
                       list(extend = 'csv', filename =  paste0(Sys.Date(),pid()), title = NULL, header = TRUE),
                       list(extend = 'excel', filename =  paste0(Sys.Date(),pid()), title = NULL, header = TRUE),
                       list(extend = "pdf", filename = paste0(Sys.Date(),pid()), title = NULL, header = TRUE),
                       list(extend = 'print')
                   ),exportOptions = list(
                     modifiers = list(page = "all",selected=TRUE)
                   )
              )
            )
          })

})
}
####