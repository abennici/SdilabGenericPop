###Module
# Function for module server logic
QueryData <- function(input, output, session,data_sp,wfs_server,wfs_version,layer,feature_geom,param,strategy,query) {
  data<-reactiveValues(data=NULL)
  observe({
    geoCol <- if (!is.null(query$geoCol)){query$geoCol}else{NULL}

    data_sp<-subset(as.data.frame(data_sp()),select=geoCol)[1,]
    print("geoCol")
    print(data_sp)
    #QueryParameters
    wfs_server<-wfs_server()
    wfs_version<-wfs_version()
    layer<-layer()
    feature_geom<-as.logical(feature_geom())
    print("feature_geom")
    print(feature_geom)
    print(class(feature_geom))
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
    
    if(!feature_geom){
      ColumnName<-desc[desc$type!="geometry","name"]
    }else{
      ColumnName<-desc[,"name"]  
    }
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
    print(data_nsp)
    print("ColumName")
    print(ColumnName)
    data$data<-subset(data_nsp,select=ColumnName)
    print(data$data)
  })
  return(data)
}
####