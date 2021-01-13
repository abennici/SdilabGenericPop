server <- function(input, output, session) {
  
  data<-callModule(module = QueryInfo, id = "id_1")
  params<-callModule(module = Params,id="selectParam")
  data2<-callModule(module = QueryData, id = "id_5",reactive(data$data),reactive(data$query$wfs_server),reactive(data$query$wfs_version),reactive(data$query$layer),reactive(data$query$feature_geom),reactive(data$query$par),reactive(data$query$strategy),reactive(params$geoCol))
  
  
  callModule(module = FlagName,id="name",reactive(data2$data),reactive(params$geoCol))
  
   observe({
    if(is.null(params$panel)||c("line")%in%params$panel){
      callModule(module = Line,id="time",reactive(data2$data),reactive(data$dsd),reactive(params$line.x),reactive(params$line.y),reactive(params$line.z),reactive(params$line.title),reactive(params$line.caption))
    }else{removeTab(inputId = "main", target = "Line")}
  })
  
  observe({
    if(is.null(params$panel)||c("pie")%in%params$panel){
      callModule(module = Pie,id="pie",reactive(data2$data),reactive(data$dsd),reactive(params$pie.x),reactive(params$pie.y),reactive(params$pie.z),reactive(params$pie.title),reactive(params$pie.caption))
    }else{removeTab(inputId = "main", target = "Pie")}
  })

  observe({
    if(is.null(params$panel)||c("box")%in%params$panel){
      callModule(module = Box,id="box",reactive(data2$data),reactive(data$dsd),reactive(params$box.x),reactive(params$box.y),reactive(params$box.z),reactive(params$box.title),reactive(params$box.caption))
    }else{removeTab(inputId = "main", target = "Box")}
  })
  
  observe({
    if(is.null(params$panel)||c("data")%in%params$panel){
      callModule(module = DataTable,id="table",reactive(data2$data),reactive(data$dsd),reactive(data$query$pid),reactive(params$data.title),reactive(params$data.caption))
    }else{removeTab(inputId = "main", target = "Data")}
  })
}