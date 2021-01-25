server <- function(input, output, session) {

  #query parameters
  query<-reactiveValues(query=NULL, panel_items = NULL,extra_module = NULL)
  
  observe({
    query$query <- parseQueryString(session$clientData$url_search)
    query$panel_items <- if (!is.null(query$query$panel)){
      unlist(strsplit(as.character(query$query$panel),"[[:punct:][:space:]]+")) 
    }
    query$extra_module <- if (!is.null(query$query$extra_module)){
      unlist(strsplit(query$query$extra_module,";",fixed = T)) 
    }else{NULL}
  })
  
  #data loading
  data<-callModule(module = QueryInfo, id = "data")
 # observe({
  #if(data$query$strategy=="ogc_viewparams"&&grep("aggregation_method|aggregation_methods",data$query$par)==1){
  data2<-callModule(module = QueryData, id = "data",reactive(data$data),reactive(data$dsd),reactive(data$query$wfs_server),reactive(data$query$wfs_version),reactive(data$query$layer),reactive(data$query$feature_geom),reactive(data$query$par),reactive(data$query$strategy),query=query$query)
  #}
  #})
  #Flag name header
  callModule(module = FlagName,id="name",data=data2$data,query=query$query)

  #tabsetPanel
  output$tabs <- renderUI({
    if(!is.null(query$panel_items)){
      tabPanels <- lapply(query$panel_items, function(panel_item){
        out_tab <- NULL
        sourced <- try(source(sprintf("views/%s.R", panel_item)))
        if(class(sourced)=="try-error"&&!is.null(query$query$extra_module)){
        sourced <- try(source(query$extra_module[str_detect(query$extra_module,sprintf("/%s.R", panel_item))]))  
        }
        if(class(sourced)!="try-error"){
          panel_server_fun <- try(eval(parse(text = paste0(panel_item, "_server"))))
          if(class(panel_server_fun) != "try-error") callModule(module = panel_server_fun,id=panel_item, data=data2$data,dsd=data2$dsd, query=query$query)
          panel_ui_fun <- try(eval(parse(text = paste0(panel_item, "_ui"))))
          if(class(panel_ui_fun)!="try-error") {
            out_tab <- tabPanel(
              title = query$query[[paste0(panel_item,".title")]], 
              panel_ui_fun(id=panel_item)
            )
          }
        }
        out_tab
      })
      tabPanels <- tabPanels[!is.null(tabPanels)]
      outTabset <- do.call(tabsetPanel, c(id="main",type = "tabs", tabPanels))
      outTabset
    }
  })

}