###Module
# Function for module server logic
DataConfig <- function(input, output, session,query) {
  dataConf<-reactiveValues(data=NULL,dsd=NULL)
  data<-callModule(module = QueryInfo, id = "data")
  observe({
    
    dataConf$dsd<-data$dsd

    #QueryParameters
	geoCol <- if (!is.null(query$geoCol)){query$geoCol}else{NULL}
    wfs_server<-query$wfs_server
    wfs_version<-query$wfs_version
    layer<-query$layer
	feature_geom<-as.logical(query$feature_geom)
    strategy<-query$strategy
	par<-query$par

if(strategy=="ogc_viewparams"&&grep("aggregation_method|aggregation_methods",par)==1){
#query par modification	
	data_id<-subset(as.data.frame(data$data),select=geoCol)[1,]
    par<-str_replace(query$par, "aggregation_method:sum", "aggregation_method:none")
    #Remove existing flag query
    pattern<-unlist(str_extract_all(par, paste0(geoCol,":.+;")))
    if(length(pattern)!=0){
      par<-str_replace(par, pattern, "")}
    #Update flag query with map clicking position
    par<-paste(geoCol,":",data_id,";",par,sep="")
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
      data <- ft$getFeatures(propertyName=propertyName)
    }
    
    if(!is.null(par)){
      data <- switch(strategy,
                     "ogc_filters"=ft$getFeatures(outputFormat ="json",propertyName=propertyName,cql_filter = gsub(" ", "%20", gsub("''", "%27%27", URLencode(par)))),
                     "ogc_viewparams"=ft$getFeatures(outputFormat ="json",propertyName=propertyName,viewparams = URLencode(par))
      )
    }
	dataConf$data<-subset(data,select=ColumnName)
	}else{
	dataConf<-data$data
	}
    

  })
  return(dataConf)
}
####