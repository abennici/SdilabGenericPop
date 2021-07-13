server <- function(input, output, session) {

  query<-reactiveValues(query=NULL, panel_items = NULL)
  
  observe({
  #query all parameters of url
    query$query <- parseQueryString(session$clientData$url_search)
  #format a vector with panel items decared in 'panel' parameter
    query$panel_items <- if (!is.null(query$query$panel)){
      unlist(strsplit(as.character(query$query$panel),"[[:space:]]")) 
    }
  })
  
  #Configurate data
  data<-callModule(module = DataConfig, id = "data",query=query$query)
  #Header content
  callModule(module = FlagName,id="name",data=data$data,query=query$query)

  #Add tabs items 
  output$tabs <- renderUI({
    if(!is.null(query$panel_items)){
      tabPanels <- lapply(query$panel_items, function(panel_item){
        out_tab <- NULL
        sourced <- NULL
        print(panel_item)
        print(query$query[[paste0(panel_item,".script")]])
        if(!is.null(query$query[[paste0(panel_item,".script")]])){
          sourced <- try(source(query$query[[paste0(panel_item,".script")]]))  
        }else{
          sourced <- try(source(sprintf("views/%s.R", panel_item)))
        }
        if(class(sourced)!="try-error"){
          panel_server_fun <- try(eval(parse(text = paste0(panel_item, "_server"))))
          if(class(panel_server_fun) != "try-error") callModule(module = panel_server_fun,id=panel_item, data=data$data,dsd=data$dsd, query=query$query)
          panel_ui_fun <- try(eval(parse(text = paste0(panel_item, "_ui"))))
          if(class(panel_ui_fun)!="try-error") {
            out_tab <- tabPanel(
              title = query$query[[paste0(panel_item,".title")]], 
              panel_ui_fun(id=panel_item)
            )
          }
        }else{
          out_tab <- tabPanel(
            title = query$query[[paste0(panel_item,".title")]], 
            tags$div("Ups, can't source the R remote script")
          )
        }
        out_tab
      })
      tabPanels <- tabPanels[!is.null(tabPanels)]
      outTabset <- do.call(tabsetPanel, c(id="main",type = "tabs", tabPanels))
      outTabset
    }
  })

}