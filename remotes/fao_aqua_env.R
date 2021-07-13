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
                textOutput(ns("data_time"))
              ),
              fluidRow(
               textOutput(ns("env_value"))
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
    paste0("Tile date : ",time)
  })
   
  output$env_value <- renderText({
   posix_time<-as.POSIXct(out$data_time, format="%Y-%m-%d")
   out$env_time<-posix_time
   WMS <- ows4R::WMSClient$new(url ="https://rsg.pml.ac.uk/thredds/wms/CCI_ALL-v5.0-1km-DAILY?", serviceVersion = "1.3.0", logger = "DEBUG")
   Layer<-WMS$capabilities$findLayerByName("chlor_a")
   timeSerie<-Layer$getTimeDimension(time_format="posix")
   #ExactMatch
   #if(posix_time %in% timeSerie){
    Env<-Layer$getFeatureInfo(srs = query$srs, x = query$x, y = query$y, width = query$width, height = query$height, feature_count = 1000000, bbox = query$bbox, time = as.character(posix_time),info_format = "text/xml")
    paste0("Concentration of chlorophyll a : ",Env$value,"[",posix_time,"]")
    #}else{
    # "No corresponding value for this date"
    #}
   })
  
   #period
  output$table <- DT::renderDT(server = FALSE, {
  posix_time<-out$env_time
   weekBefore<-seq(posix_time, by = "-1 day", length.out = 8)
   weekAfter<-seq(posix_time, by = "1 day", length.out = 8)
   Period<-as.character(sort(unique(c(weekBefore,weekAfter))))
   WMS <- ows4R::WMSClient$new(url ="https://rsg.pml.ac.uk/thredds/wms/CCI_ALL-v5.0-1km-DAILY?", serviceVersion = "1.3.0", logger = "DEBUG")
   Layer<-WMS$capabilities$findLayerByName("chlor_a")
   out$data_period<-do.call("rbind",lapply(Period,function(time){Layer$getFeatureInfo(srs = query$srs, x = query$x, y = query$y, width = query$width, height = query$height, feature_count = 1000000, bbox = query$bbox, time =time,info_format = "text/xml")}))
   out$data_period
   })
  
  output$stat <- DT::renderDT(server = FALSE, {
    datatable(
    as.data.frame(out$data_period)%>%
      mutate(value = na_if(value,"none"))%>%
      summarise(min=round(min(as.numeric(na.omit(value))),2),
                max=round(max(as.numeric(na.omit(value))),2),
                mean=round(mean(as.numeric(na.omit(value))),2),
                median=round(median(as.numeric(na.omit(value))),2),
                sd=round(sd(as.numeric(na.omit(value))),2)),
    options = list(dom = 't'), rownames = FALSE)
  })   
  
  output$graph <- renderPlotly({
    data<-as.data.frame(out$data_period)%>%
      mutate(value = na_if(value,"none"))%>%
      mutate(time = as.POSIXct(time,format="%Y-%m-%dT%H:%M:%OSZ"))
      plot_ly(data,x = ~time, y = ~value, type = 'scatter',mode = 'lines+markers', text = paste("Chlorophill concentration:",data$value))%>%
      layout(xaxis = list(type = "date",range=c(min(data$time), max(data$time))),
             yaxis = list(rangemode="tozero")
      )
  })
  
}
####