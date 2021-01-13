###Module
# Function for module server logic
QueryData <- function(input, output, session,data_sp,wfs_server,wfs_version,layer,feature_geom,param,strategy,geoCol) {
  data<-reactiveValues(data=NULL)
  print("START QUERYDATA")
  observe({
    geoCol<-geoCol()
    #Flag Reference by WMS
    #data_sp<-data_sp()$flag[1]
    data_sp<-subset(as.data.frame(data_sp()),select=geoCol)[1,]
    print("geoCol")
    print(data_sp)
    #QueryParameters
    wfs_server<-wfs_server()
    wfs_version<-wfs_version()
    layer<-layer()
    feature_geom<-feature_geom()
    strategy<-strategy()
    par<-str_replace(param(), "aggregation_method:sum", "aggregation_method:none")
    #Remove existing flag query
    pattern<-unlist(str_extract_all(par, paste0(geoCol,":.+;")))
    if(length(pattern)!=0){
      par<-str_replace(par, pattern, "")}
    #Update flag query with map clicking position
    par<-paste(geoCol,":",data_sp,";",par,sep="")
    #Connect to OGC WFS to get DATA
    WFS <- WFSClient$new(
      url = wfs_server,
      serviceVersion = wfs_version,
      logger = "INFO"
    )
    #Get feature type for dataset
    ft <- WFS$capabilities$findFeatureTypeByName(layer)
    
    #Get columns names for propertyName argument
    desc <- ft$getDescription(TRUE) 
    
    ColumnName<-desc[,"name"]  
    
    propertyName<-paste(ColumnName, collapse = ',')
    
    if(is.null(par)){
      data_nsp <- ft$getFeatures(propertyName=propertyName)
    }
    
    if(!is.null(par)){
      data_nsp <- switch(strategy,
                     "ogc_filters"=ft$getFeatures(outputFormat ="json",propertyName=propertyName,cql_filter = gsub(" ", "%20", gsub("''", "%27%27", URLencode(par)))),
                     "ogc_viewparams"=ft$getFeatures(outputFormat ="json",propertyName=propertyName,viewparams = URLencode(par))
      )
    }
    
   # data$data<-subset(as.data.frame(data_nsp),flag==unique(as.data.frame(data_sp)$flag))
   # data$data<-subset(data$data,select=ColumnName)
    data$data<-subset(data_nsp,select=ColumnName)
    print(data$data)
  })
  return(data)
}
####