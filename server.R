server <- function(input, output, session) {

  #query parameters
  query<-reactiveValues(query=NULL, panel_items = NULL)
  
  observe({
    query$query <- parseQueryString(session$clientData$url_search)
    query$panel_items <- if (!is.null(query$query$panel)){
      unlist(strsplit(as.character(query$query$panel),"[[:punct:][:space:]]+")) 
    }
  })
  
  #data loading
  data<-callModule(module = QueryInfo, id = "dataAggr")
  data2<-callModule(module = QueryData, id = "dataRaw",reactive(data$data),reactive(data$query$wfs_server),reactive(data$query$wfs_version),reactive(data$query$layer),reactive(data$query$feature_geom),reactive(data$query$par),reactive(data$query$strategy),query=query$query)
  #Flag name header
  callModule(module = FlagName,id="name",data=data2$data,query=query$query)

  #tabsetPanel
  output$tabs <- renderUI({
    if(!is.null(query$panel_items)){
      tabPanels <- lapply(query$panel_items, function(panel_item){
        out_tab <- NULL
        sourced <- try(source(sprintf("views/%s.R", panel_item)))
        if(class(sourced)!="try-error"){
          panel_server_fun <- try(eval(parse(text = paste0(panel_item, "_server"))))
          if(class(panel_server_fun) != "try-error") callModule(module = panel_server_fun,id=panel_item, data=data2$data,dsd=data$dsd, query=query$query)
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