server <- function(input, output, session) {

#hide the default tab
  hideTab(inputId = "main", target = "hidden")
#query parameters
query<-reactiveValues(query=NULL)

  observe({
    query$query <- parseQueryString(session$clientData$url_search)
})
  
#data loading
  data<-callModule(module = QueryInfo, id = "dataAggr")
  data2<-callModule(module = QueryData, id = "dataRaw",reactive(data$data),reactive(data$query$wfs_server),reactive(data$query$wfs_version),reactive(data$query$layer),reactive(data$query$feature_geom),reactive(data$query$par),reactive(data$query$strategy),query=query$query)

#Flag name header
  callModule(module = FlagName,id="name",data=data2$data,query=query$query)
  
#Extraction of panel
##loop on panel
observe({
  panel_items <- if (!is.null(query$query$panel)){
    unlist(strsplit(as.character(query$query$panel),"[[:punct:][:space:]]+")) 
  }

 for(panel_item in panel_items){
   sourced <- try(source(sprintf("views/%s.R", panel_item)))
   if(class(sourced)!="try-error"){
     panel_server_fun <- try(eval(parse(text = paste0(panel_item, "_server"))))
     if(class(panel_server_fun) != "try-error") callModule(module = panel_server_fun,id=panel_item, data=data2$data,dsd=data$dsd, query=query$query)
     panel_ui_fun <- try(eval(parse(text = paste0(panel_item, "_ui"))))
     if(class(panel_ui_fun)!="try-error") appendTab(inputId = "main", panel_ui_fun(id=panel_item))
   }
 }
  
  updateTabsetPanel(session,"main",selected = panel_items[1])
  })

}