###FAO Aquaculture image display and environmental enrichment Module
##Query Example : /?pid=aquaculture_farm_detection&layer=aquaculture_farm_detection&wfs_server=https://geoserver-sdi-lab.d4science.org/geoserver/sdilab_fisheriesatlas/ows&wfs_version=1.0.0&wms_server=https://geoserver-sdi-lab.d4science.org/geoserver/sdilab_fisheriesatlas/wms&wms_version=1.1.0&feature_geom=false&strategy=ogc_filters&geom=the_geom&x=217&y=181&width=256&height=256&bbox=-8140237.7642581295,-5165920.119625352,-8101102.005776119,-5126784.361143342&srs=EPSG:3857&geoCol=null&panel=fao_aqua_env&fao_aqua_env.title=Aquaculture-Environmental%20enrichment&fao_aqua_env.script=https://raw.githubusercontent.com/abennici/SdilabGenericPop/master/remotes/fao_aqua_env.R
# Function for module UI
fao_aqua_env_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(title=uiOutput(ns("title_panel")),value="fao_aqua_env",
           tabsetPanel(
           tabPanel("Summary",
             fluidRow(
               uiOutput(ns("img"))
              ),
              fluidRow(
                htmlOutput(ns("data_time"))
              ),
              fluidRow(
               htmlOutput(ns("chlor_a_value"))
              ),
             fluidRow(
               htmlOutput(ns("sst_value"))
             )
           ),
           tabPanel("Table",
              fluidRow(
                div(DTOutput(ns('table'))%>%withSpinner(type = 2),  style = "font-size:50%")
              )),
           tabPanel("Stat",
             fluidRow(
               div(DTOutput(ns('stat'))%>%withSpinner(type = 2),  style = "font-size:70%")
             )
           ),
           tabPanel("Plot",
             fluidRow(
               plotlyOutput(ns('graph'))
             )
           )
          )
          
  )  
}

# Function for module server
fao_aqua_env_server <- function(input, output, session,data,dsd,query) {
  ns<-session$ns  
  
  out <-reactiveValues(
    data=as.data.frame(data)
  )
  
  output$img <-renderUI({
     df<-out$data
     img<-df[1,]$s2_crop
     print(img)
     if(!is.null(img)||img==""){
       img_clean<-str_replace(img,"base64:","")
       img_url<-sprintf("<center><img src=\"data:image/png;base64,\n%s\" alt=\"image\" /></center>",  img_clean)
       HTML(img_url)
     }else{
       div("No image to display !")
     }
   })
  
  output$data_time <-renderText({
    time<-unique(as.character(out$data$tile_date))
    out$data_time<-time
    posix_time<-as.POSIXct(out$data_time, format="%Y-%m-%d")
    out$posix_time<-posix_time
    weekBefore<-seq(out$posix_time, by = "-1 day", length.out = 8)
    weekAfter<-seq(out$posix_time, by = "1 day", length.out = 8)
    out$Period<-lubridate::floor_date(sort(unique(c(weekBefore,weekAfter))),'day')
    out$PeriodLabel<-c("Day -7","Day -6","Day -5","Day -4","Day -3","Day -2","Day -1","Select Day","Day +1","Day +2","Day +3","Day +4","Day +5","Day +6","Day +7")
    paste0("<b>Tile date :</b> ",time)
  })
   
  output$chlor_a_value <- renderText({
   CHLORO_WMS <- ows4R::WMSClient$new(url ="https://rsg.pml.ac.uk/thredds/wms/CCI_ALL-v5.0-1km-DAILY?", serviceVersion = "1.3.0", logger = "DEBUG")
   CHLORO_Layer<-CHLORO_WMS$capabilities$findLayerByName("chlor_a")
   out$CHLORO_Layer<-CHLORO_Layer
   timeSerie<-CHLORO_Layer$getTimeDimension()$values
   tmp<-data.frame(chlor_a=timeSerie)
   tmp$round<-lubridate::floor_date(as.POSIXct(tmp$chlor_a,format="%Y-%m-%dT%H:%M:%OSZ"), "day")
   timeMatch<-subset(tmp,round==lubridate::floor_date(out$posix_time, "day"))$chlor_a
   out$CHLORO_week<-subset(tmp,round%in%out$Period)$chlor_a
   
   #ExactMatch
   if(length(timeMatch)>0){
      CHLORO<-CHLORO_Layer$getFeatureInfo(srs = query$srs, x = query$x, y = query$y, width = query$width, height = query$height, feature_count = 1000000, bbox = query$bbox, time = timeMatch,info_format = "text/xml")
      paste0("<b>Concentration of chlorophyll a :</b> ",CHLORO$value," [",timeMatch,"]")
    }else{
      paste0("<b>Concentration of chlorophyll a :</b> No Data Available")
    }
   })
  
  output$sst_value <- renderText({
    SST_WMS <- ows4R::WMSClient$new(url ="https://pae-paha.pacioos.hawaii.edu/thredds/wms/dhw_5km?", serviceVersion = "1.3.0", logger = "DEBUG")
    SST_Layer<-SST_WMS$capabilities$findLayerByName("CRW_SST")
    out$SST_Layer<-SST_Layer
    timeSerie<-SST_Layer$getTimeDimension()$values
    tmp<-data.frame(sst=timeSerie)
    tmp$round<-lubridate::floor_date(as.POSIXct(tmp$sst,format="%Y-%m-%dT%H:%M:%OSZ"), "day")
    timeMatch<-subset(tmp,round==lubridate::floor_date(out$posix_time, "day"))$sst
    out$SST_week<-subset(tmp,round%in%out$Period)$sst
    #ExactMatch
    if(length(timeMatch)>0){
      SST<-SST_Layer$getFeatureInfo(srs = query$srs, x = query$x, y = query$y, width = query$width, height = query$height, feature_count = 1000000, bbox = query$bbox, time = timeMatch,info_format = "text/xml")
      paste0("<b>Sea surface temperature : </b>",SST$value," [",timeMatch,"]")
    }else{
      paste0("<b>Sea surface temperature :</b> No Data Available")
    }
  })
  
  output$table <- DT::renderDT(server = FALSE, {

    CHLORO_period<-do.call("rbind",lapply(out$CHLORO_week,function(time){out$CHLORO_Layer$getFeatureInfo(srs = query$srs, x = query$x, y = query$y, width = query$width, height = query$height, feature_count = 1000000, bbox = query$bbox, time =time,info_format = "text/xml")}))
    CHLORO_period$time<-lubridate::floor_date(as.POSIXct(CHLORO_period$time,format="%Y-%m-%dT%H:%M:%OSZ"), "day")
    CHLORO_period<-subset(as.data.frame(CHLORO_period),select=c(time,value))
    names(CHLORO_period)[names(CHLORO_period) == 'value'] <- 'chlor_a'
    CHLORO_period<-as.data.frame(CHLORO_period)
    
    SST_period<-do.call("rbind",lapply(out$SST_week,function(time){out$SST_Layer$getFeatureInfo(srs = query$srs, x = query$x, y = query$y, width = query$width, height = query$height, feature_count = 1000000, bbox = query$bbox, time =time,info_format = "text/xml")}))
    SST_period$time<-lubridate::floor_date(as.POSIXct(SST_period$time,format="%Y-%m-%dT%H:%M:%OSZ"), "day")
    SST_period<-subset(as.data.frame(SST_period),select=c(time,value))
    names(SST_period)[names(SST_period) == 'value'] <- 'sst'
    
    data_period<-merge(CHLORO_period,SST_period)
    data_period$chronology<-out$PeriodLabel
    out$data_period<-data_period
   datatable(out$data_period)%>%
     formatStyle("chronology",target = 'row',fontWeight = "bold",backgroundColor = styleEqual(c("Select Day"), c("orange")))
     
   })
  
  output$stat <- DT::renderDT(server = FALSE, {
    datatable(
      reshape::melt(out$data_period, id=c("time","chronology"))%>%
      group_by(variable)%>%
      mutate(value = na_if(value,"none"))%>%
      summarise(min=round(min(as.numeric(na.omit(value))),2),
                max=round(max(as.numeric(na.omit(value))),2),
                mean=round(mean(as.numeric(na.omit(value))),2),
                median=round(median(as.numeric(na.omit(value))),2),
                sd=round(sd(as.numeric(na.omit(value))),2)),
    options = list(dom = 't'), rownames = FALSE)
  })   
  
  output$graph <- renderPlotly({
    data<-out$data_period%>%
      mutate(chlor_a = na_if(chlor_a,"none"))%>%
      mutate(sst = na_if(sst,"none"))%>%
      mutate(time = as.POSIXct(time,format="%Y-%m-%dT%H:%M:%OSZ"))
      plot_ly(data)%>%
      add_trace(x = ~time, y = ~chlor_a, type = 'scatter',mode = 'lines+markers',name="Chlor_a", text = paste("Concentration of chlorophyll a"," : ",data$chlor_a))%>%
      add_trace(x = ~time, y = ~sst, type = 'scatter',mode = 'lines+markers',name="SST",yaxis="y2", text = paste("Sea surface temperature"," : ",data$sst))%>%
      layout(legend = list(orientation = "h", xanchor = "center",x = 0.5),
             xaxis = list(type = "date",range=c(min(data$time), max(data$time)),title=NULL),
             yaxis = list(title="Concentration of chlorophyll a"),
             yaxis2 = list(overlaying = "y",side = "right",title="Sea surface temerature",showticklabels = TRUE,automargin = TRUE))
  })
  
}
####