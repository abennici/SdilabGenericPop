server <- function(input, output, session) {
  
  data<-callModule(module = QueryInfo, id = "id_1")
  data2<-callModule(module = QueryData, id = "id_5",reactive(data$data),reactive(data$query$wfs_server),reactive(data$query$wfs_version),reactive(data$query$layer),reactive(data$query$feature_geom),reactive(data$query$par),reactive(data$query$strategy))
  params<-callModule(module = Params,id="selectParam")
  
  callModule(module = FlagName,id="name",reactive(data2$data),reactive(params$withFlag),reactive(params$flagCol))
  
   observe({
    if(is.null(params$panel)||c("line")%in%params$panel){
      callModule(module = Line,id="time",reactive(data2$data),reactive(data$dsd),reactive(params$line.x),reactive(params$line.z))
    }else{removeTab(inputId = "main", target = "Line")}
  })
  
  observe({
    if(is.null(params$panel)||c("pie")%in%params$panel){
      callModule(module = Pie,id="pie",reactive(data2$data),reactive(data$dsd),reactive(params$pie.x),reactive(params$pie.z))
    }else{removeTab(inputId = "main", target = "Pie")}
  })

  observe({
    if(is.null(params$panel)||c("box")%in%params$panel){
      callModule(module = Box,id="box",reactive(data2$data),reactive(data$dsd),reactive(params$box.x),reactive(params$box.z))
    }else{removeTab(inputId = "main", target = "Box")}
  })
  
  observe({
    if(is.null(params$panel)||c("data")%in%params$panel){
      callModule(module = DataTable,id="table",reactive(data2$data),reactive(data$dsd),reactive(data$query$pid))
    }else{removeTab(inputId = "main", target = "Data")}
  })
}