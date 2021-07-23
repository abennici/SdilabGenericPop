###FAO Aquaculture image display and environmental enrichment Module
##Query Example : /?pid=aquaculture_farm_detection&layer=aquaculture_farm_detection&wfs_server=https://geoserver-sdi-lab.d4science.org/geoserver/sdilab_fisheriesatlas/ows&wfs_version=1.0.0&wms_server=https://geoserver-sdi-lab.d4science.org/geoserver/sdilab_fisheriesatlas/wms&wms_version=1.1.0&feature_geom=false&strategy=ogc_filters&geom=the_geom&x=217&y=181&width=256&height=256&bbox=-8140237.7642581295,-5165920.119625352,-8101102.005776119,-5126784.361143342&srs=EPSG:3857&geoCol=null&panel=fao_aqua_env&fao_aqua_env.title=Aquaculture-Environmental%20enrichment&fao_aqua_env.script=https://raw.githubusercontent.com/abennici/SdilabGenericPop/master/remotes/fao_aqua_env.R
# Function for module UI
jsCode <- "shinyjs.SwitchMapView = function(){window.parent.OFV.SwitchMapView();}"

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
                htmlOutput(ns("env_values"))
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
           ),
           tabPanel("Wind Rose",
              fluidRow(
                plotOutput(ns('windrose'))
              )
            
           ),
           tabPanel("Map_interaction",
                    fluidRow(
    #                   tags$script("
    #   Shiny.addCustomMessageHandler('background-color', function(color) {
    #     document.body.style.backgroundColor = color;
    #     document.body.innerText = color;
    #   });
    # "),
                      extendShinyjs(text = jsCode,functions = c()),
                      actionButton(ns("mapview"), "Switch 2D/3D view on map"),
                    )
           )
           # tabPanel("Selection",
           #          fluidRow(
           #            uiOutput(ns("selector"))
           #          ))
          )
          
  )  
}

# Function for module server
fao_aqua_env_server <- function(input, output, session,data,dsd,query) {
  ns<-session$ns
  
  observeEvent(input$mapview,{
    #session$sendCustomMessage("background-color", nextColor())
    #window.parent.OFV.SwitchMapView()
    js$SwitchMapView()
    cat("click!")
  })
  
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
  
  env<-data.frame(
    id=c("chlor_a",
         "sst",
       #  "sss",
         "wave_height",
         "wind_dir"),
    wms=c("https://rsg.pml.ac.uk/thredds/wms/CCI_ALL-v5.0-1km-DAILY?",
          "https://pae-paha.pacioos.hawaii.edu/thredds/wms/dhw_5km?",
          "https://pae-paha.pacioos.hawaii.edu/thredds/wms/ww3_global/WaveWatch_III_Global_Wave_Model_best.ncd?",
          "https://pae-paha.pacioos.hawaii.edu/thredds/wms/ww3_global/WaveWatch_III_Global_Wave_Model_best.ncd?"),
    layer=c("chlor_a",
            "CRW_SST",
         #   "sss",
            "Thgt",
            "wdir"),
    label=c("Concentration of chlorophyll a",
          "Sea surface temperature",
          "Sea surface wave height",
          "Sea surface wind direction"),
    source=c("https://oceancolor.gsfc.nasa.gov/atbd/chlor_a",
             "https://coralreefwatch.noaa.gov/product/5km/index_5km_sst.php",
             "https://polar.ncep.noaa.gov/waves/products.shtml?",
             "https://polar.ncep.noaa.gov/waves/products.shtml?"
             ),
    unit=c("mg/m3",
           "°C",
           "m",
           "degree"
           )
  )
  
  # output$selector <- renderUI({
  #   checkboxGroupInput(ns("env_select"), "Choose variables:",
  #                    choiceNames =unique(env$label),
  #                    choiceValues =unique(env$id),
  #                    selected = unique(env$id))
  # })

   output$env_values <- renderText({
     txt<-""
     outt<-list()
     #if(!is.null(input$env_select))env<-subset(env,id %in% input$env_select)
     for(i in 1:nrow(env)){
       WMS<-ows4R::WMSClient$new(url = env[i,2], serviceVersion = "1.3.0", logger = "DEBUG")
       outt[[env[i,1]]]$wms<-WMS
       Layer<-WMS$capabilities$findLayerByName(env[i,3])
       outt[[env[i,1]]]$layer<-Layer
       timeSerie<-Layer$getTimeDimension()$values
       outt[[env[i,1]]]$timeSerie<-timeSerie
       tmp<-data.frame(timeSerie)
       names(tmp)<-env[i,1]
       tmp$round<-lubridate::floor_date(as.POSIXct(tmp[,1],format="%Y-%m-%dT%H:%M:%OSZ"), "day")
       timeMatch<-subset(tmp,round==lubridate::floor_date(out$posix_time, "day"))[1,1]
       Week<-tmp%>%
         filter(round %in% out$Period)%>%
          group_by(round)%>%
          summarise(timeSerie=first(!! sym(env[i,1])))%>%
         select(timeSerie)
       outt[[env[i,1]]]$week<-as.data.frame(Week)$timeSerie
       
       if(length(timeMatch)>0){
         Feature<-Layer$getFeatureInfo(srs = query$srs, x = query$x, y = query$y, width = query$width, height = query$height, feature_count = 1000000, bbox = query$bbox, time = timeMatch,info_format = "text/xml")
         txt<-paste0(txt,"<a href=",env[i,5],">",env[i,4]," : </a> ",Feature$value," ",env[i,6]," [",timeMatch,"]<br>")
       }else{
         txt<-paste0(txt,"<a href=",env[i,5],">",env[i,4]," : </a>No Data Available")
       }
     }
     out$env<-outt
     txt
   })
  
  
  output$table <- DT::renderDT(server = FALSE, {
    #if(!is.null(input$env_selection))env<-subset(env,id %in% input$env_select)
    data_period<-NULL
    for(i in env$id){
      week<-out$env[[i]]$week
      table<-do.call("rbind",lapply(week,function(time){out$env[[i]]$layer$getFeatureInfo(srs = query$srs, x = query$x, y = query$y, width = query$width, height = query$height, feature_count = 1000000, bbox = query$bbox, time =time,info_format = "text/xml")}))
      table$time<-as.character(lubridate::floor_date(as.POSIXct(table$time,format="%Y-%m-%dT%H:%M:%OSZ"), "day"))
      table<-subset(as.data.frame(table),select=c(time,value))
      names(table)[names(table) == 'value'] <- i
      table<-as.data.frame(table)
      print(table)
      data_period<-if(is.null(data_period)) table else merge(data_period,table)
    }
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
    vline <- function(x = 0, color = "grey") {
      list(
        type = "line", 
        y0 = 0, 
        y1 = 1, 
        yref = "paper",
        x0 = x, 
        x1 = x, 
        line = list(color = color)
      )
    }
    data<-out$data_period%>%
      mutate(chlor_a = as.numeric(na_if(chlor_a,"none")))%>%
      mutate(sst = as.numeric(na_if(sst,"none")))%>%
      mutate(wave_height = as.numeric(na_if(wave_height,"none")))%>%
      mutate(wind_dir = as.numeric(na_if(wind_dir,"none")))%>%
      mutate(time = as.POSIXct(time,format="%Y-%m-%d"))
    
      plot_ly(data)%>%
      add_trace(x = ~time, y = ~chlor_a,line = list(color = 'green'),marker = list(color = 'green'), type = 'scatter',mode = 'lines+markers',name="Chlor_a", text = paste("Concentration of chlorophyll a"," : ",data$chlor_a))%>%
      add_trace(x = ~time, y = ~sst,line = list(color='red'),marker = list(color = 'red'), type = 'scatter',mode = 'lines+markers',name="SST",yaxis="y2", text = paste("Sea surface temperature"," : ",data$sst))%>%
      add_trace(x = ~time, y = ~wave_height,marker = list(color = 'blue'), type = 'scatter',mode = 'markers',name="wave_height",yaxis="y3", text = paste("Sea surface wave height"," : ",data$sst))%>%
      add_trace(x = ~time, y = ~wind_dir,line = list(color='orange'),marker = list(color = 'orange'), type = 'scatter',mode = 'lines+markers',name="wind_dir",yaxis="y4", text = paste("Sea surface wind direction"," : ",data$sst))%>%
      layout(shapes = list (vline(out$posix_time)),
             xaxis = list(domain = c(0.13, 0.83),type = "date",range=c(min(data$time), max(data$time)),title="",titlefont = list(size = 7), tickfont = list(size = 7)),
             yaxis = list(position=0,title=list(text="Concentration of chlorophyll a (mg/m3)",standoff=1),hoverformat = '.3f',showticklabels = T,automargin = F,titlefont = list(size = 7,color = "green"), tickfont = list(size = 7,color = "green")),
             yaxis2 = list(position=0.13,overlaying = "y",side = "left",title=list(text="Sea surface temperature (°C)",standoff=1),hoverformat = '.3f',showticklabels = T,automargin = F,titlefont = list(size = 7,color = "red"), tickfont = list(size = 7,color = "red")),
             yaxis3 = list(position =0.83,overlaying = "y",side = "right",title=list(text="Sea surface wave height (m)",standoff=1),hoverformat = '.3f',showticklabels = T,automargin = F,titlefont = list(size = 7,color = "blue"), tickfont = list(size = 7,color = "blue")),
             yaxis4 = list(position =0.95,overlaying = "y",side = "right",title=list(text="Sea surface wind direction (degree)",standoff=3),hoverformat = '.2f',showticklabels = T,automargin = F,titlefont = list(size = 7,color = "orange"), tickfont = list(size = 7,color = "orange")),
             legend = list(orientation = "h", x = 0, y= -0.2, anchor="center",font = list(size = 7)))
  })
 
  output$windrose <- renderPlot({
    directions<-data.frame(
      cardinal=c("N","NNE","NE","ENE","E","ESE","SE","SSE","S","SSW","SW","WSW","W","WNW","NW","NNW"),
      degree_min=c(348.75,11.25,33.75,56.25,78.75,101.25,123.75,146.25,168.75,191.25,213.75,236.25,258.75,281.25,303.75,326.25),
      degree_max=c(11.25,33.75,56.25,78.75,101.25,123.75,146.25,168.75,191.25,213.75,236.25,258.75,281.25,303.75,326.25,348.75)
    )
    
    wind_dir <- out$data_period %>%
      select(time,wind_dir)%>%
      filter(wind_dir!="none")%>%
      mutate(wd_cardinal = cut(
        as.numeric(wind_dir), 
        breaks = c(0, directions$degree_max, 360), 
        labels = c(directions$cardinal, 'N')
      ))%>%
      count(wd_cardinal)%>%
      complete(wd_cardinal = c("N","NNE","NE","ENE","E","ESE","SE","SSE","S","SSW","SW","WSW","W","WNW","NW","NNW"),fill=list(n=0))
    
    wind_dir$wd_cardinal<-factor(wind_dir$wd_cardinal,c("N","NNE","NE","ENE","E","ESE","SE","SSE","S","SSW","SW","WSW","W","WNW","NW","NNW"))
    
    ggplot(wind_dir, aes(x=wd_cardinal))+
      geom_col(aes(y=max(n)),width = 1, fill = "black") +
      labs(x=NULL,y=NULL)+
      geom_col(aes(y=n,fill=n),width = 1)+
      scale_fill_gradient2(low="green", high="red", mid="yellow",midpoint=median(wind_dir$n))+
      geom_hline(yintercept = seq(0, max(wind_dir$n), by = 1),
                 color = "white", size = 0.5) +
      geom_vline(xintercept = seq(.5, 16.5, by = 1),
                 color = "white", size = 0.5)+
      coord_polar(start=)+
      theme(axis.text.x = element_text(size=5),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.ticks.x = element_blank(),
            panel.background = element_rect(fill="NA"),
            legend.position = "none",
            plot.margin = unit(c(1,1,1,1), "cm"),
            panel.grid = element_blank()
      )
  }) 
   
}

####