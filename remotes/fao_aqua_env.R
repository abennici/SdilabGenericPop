###FAO Aquaculture image display and environmental enrichment Module
##Query Example : /?pid=aquaculture_farm_detection&layer=aquaculture_farm_detection&wfs_server=https://geoserver-sdi-lab.d4science.org/geoserver/sdilab_fisheriesatlas/ows&wfs_version=1.0.0&wms_server=https://geoserver-sdi-lab.d4science.org/geoserver/sdilab_fisheriesatlas/wms&wms_version=1.1.0&feature_geom=false&strategy=ogc_filters&geom=the_geom&x=217&y=181&width=256&height=256&bbox=-8140237.7642581295,-5165920.119625352,-8101102.005776119,-5126784.361143342&srs=EPSG:3857&geoCol=null&panel=fao_aqua_env&fao_aqua_env.title=Aquaculture-Environmental%20enrichment&fao_aqua_env.script=https://raw.githubusercontent.com/abennici/SdilabGenericPop/master/remotes/fao_aqua_env.R
#/?pid=aquaculture_farm_detection&layer=aquaculture_farm_detection&wfs_server=https://geoserver-sdi-lab.d4science.org/geoserver/sdilab_fisheriesatlas/ows&wfs_version=1.0.0&wms_server=https://geoserver-sdi-lab.d4science.org/geoserver/sdilab_fisheriesatlas/wms&wms_version=1.1.0&feature_geom=false&strategy=ogc_filters&geom=the_geom&x=217&y=181&width=256&height=256&bbox=-8140237.7642581295,-5165920.119625352,-8101102.005776119,-5126784.361143342&srs=EPSG:3857&geoCol=null&panel=fao_aqua_env&fao_aqua_env.title=Aquaculture-Environmental%20enrichment
#Function for module UI

##Environemental table

# env<-data.frame(
#   id=c("chlor_a",
#        "sst",
#        "wave_height",
#        "wind_dir"),
#   wms=c("https://rsg.pml.ac.uk/thredds/wms/CCI_ALL-v5.0-1km-DAILY?",
#         "https://pae-paha.pacioos.hawaii.edu/thredds/wms/dhw_5km?",
#         "https://pae-paha.pacioos.hawaii.edu/thredds/wms/ww3_global/WaveWatch_III_Global_Wave_Model_best.ncd?",
#         "https://pae-paha.pacioos.hawaii.edu/thredds/wms/ww3_global/WaveWatch_III_Global_Wave_Model_best.ncd?"),
#   layer=c("chlor_a",
#           "CRW_SST",
#           "Thgt",
#           "wdir"),
#   label=c("Concentration of chlorophyll a",
#           "Sea surface temperature",
#           "Sea surface wave height",
#           "Sea surface wind direction"),
#   source=c("https://oceancolor.gsfc.nasa.gov/atbd/chlor_a",
#            "https://coralreefwatch.noaa.gov/product/5km/index_5km_sst.php",
#            "https://polar.ncep.noaa.gov/waves/products.shtml?",
#            "https://polar.ncep.noaa.gov/waves/products.shtml?"
#   ),
#   unit=c("mg/m3",
#          "°C",
#          "m",
#          "degree"
#   ),
#   color=c("green",
#           "red",
#           "blue",
#           "orange"),stringsAsFactors = F
# )

env<-data.frame(
  id=c("chlor_a"),
  wms=c("https://rsg.pml.ac.uk/thredds/wms/CCI_ALL-v5.0-1km-DAILY?"),
  layer=c("chlor_a"),
  label=c("Concentration of chlorophyll a"),
  source=c("https://oceancolor.gsfc.nasa.gov/atbd/chlor_a"),
  unit=c("mg/m3"),
  color=c("green"),stringsAsFactors = F
)

# Cardinal directions
directions<-data.frame(
  cardinal=c("N","NNE","NE","ENE","E","ESE","SE","SSE","S","SSW","SW","WSW","W","WNW","NW","NNW"),
  degree_min=c(348.75,11.25,33.75,56.25,78.75,101.25,123.75,146.25,168.75,191.25,213.75,236.25,258.75,281.25,303.75,326.25),
  degree_max=c(11.25,33.75,56.25,78.75,101.25,123.75,146.25,168.75,191.25,213.75,236.25,258.75,281.25,303.75,326.25,348.75)
)

fao_aqua_env_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(title=uiOutput(ns("title_panel")),value="fao_aqua_env",
           
    tabsetPanel(
      # tabPanel("Test1",
      #          fluidRow(
      #            actionButton(ns("mapview1"), "Switch 2D/3D view on map")
      #          )),
      #  tabPanel("Test2",
      #           fluidRow(
      #             tags$script("parent.postMessage('OFV.switchMapView()','*');")
      #           )),
#       tabPanel("Test3",
#                fluidRow(
#                  actionButton(ns("mapview3"), "Switch 2D/3D view on map"),
#                  tags$script("
# Shiny.addCustomMessageHandler('alert', function(arg) {
#   alert(arg.val);
#   alert(arg.size);
# });")
#                )),
#       tabPanel("Test4",
#                fluidRow(
#                  actionButton(ns("mapview4"), "Switch 2D/3D view on map"),
#                  tags$script("
# Shiny.addCustomMessageHandler('switch', function(arg) {
# parent.postMessage(arg.text,arg.origin);
# });")
#                )),
      tabPanel("Summary",
        fluidRow(
          uiOutput(ns("img"))
        ),
        fluidRow(
          htmlOutput(ns("data_time"))
        ),
        fluidRow(
          div(htmlOutput(ns("env_values"))%>%withSpinner(type = 2))
        )
      ),
      tabPanel("Table",
        fluidRow(
          div(DTOutput(ns('table'))%>%withSpinner(type = 2),  style = "font-size:50%")
        )
      ),
      tabPanel("Stat",
        fluidRow(
          div(DTOutput(ns('stat'))%>%withSpinner(type = 2),  style = "font-size:70%")
        )
      ),
      tabPanel("Global Plot",
        fluidRow(
          div(plotlyOutput(ns('graph'))%>%withSpinner(type = 2))
        )
      ),
      tabPanel("Individual Plot",
               fluidRow(
                 uiOutput(ns('selector'))
               ),
               fluidRow(
                 div(plotlyOutput(ns('plot'))%>%withSpinner(type = 2))
               )
      ),
      tabPanel("Wind Rose",
        fluidRow(
          div(plotOutput(ns('windrose'))%>%withSpinner(type = 2))
        )
      ),
      tabPanel("Proximity Tools",
               fluidRow(
                 sliderInput(ns("dist"), "Searching at how many distance (km) around point :",min=0.1,max=30,step=0.1,value=0.1, post=" km"),
                 uiOutput(ns("draw_buffer"))
                 
               ),
               fluidRow(column(4,
                 selectInput(ns("interactWith"),
                             "Type of item:",
                             choices = list("Open Street Map" = c("Town"="town",
                                                                  "Industrial"="industrial",
                                                                  "Aerodrome"="aerodrome",
                                                                  "Harbour"="harbour",
                                                                  "Marina"="marina",
                                                                  "Ferry terminal"="ferry_terminal",
                                                                  "Ferry route"="ferry_route",
                                                                  "Riverbank"="riverbank",
                                                                  "Wetland"="wetland",
                                                                  "Natural"="natural",
                                                                  "Nature reserve"="nature_reserve",
                                                                  "Protected Area"="protected_area"),
                                            "Data" = c("others Farm"="farm")),
                             selected = "town",multiple=F,selectize=F)
               ),
               column(4,
                  selectInput(ns("type_geometry"),
                            "Type of features:",
                            choices = c("points"="osm_points",
                                        "lines"="osm_lines",
                                        "multilines"="osm_multilines",
                                        "polygons"="osm_polygons",
                                        "multipolygons"="osm_multipolygons"),
                            selected = "points",multiple=F,selectize=F)
                )
               ),
               fluidRow(
                actionButton(ns("send_request"),"Send request"),
                materialSwitch(ns("project_result"),"Project result in viewer : "),
                ),
               fluidRow(
                 uiOutput(ns("result")),
                 uiOutput(ns("message"))
               )
               # fluidRow(
               #   htmlOutput(ns("nb_farm"))
               # ),
               # fluidRow(
               #   htmlOutput(ns("nearest_farm"))
               # )
    ))
  )  
}

# Function for module server
fao_aqua_env_server <- function(input, output, session,data,dsd,query) {
  ns<-session$ns
  
  out <-reactiveValues(
    data=NULL
  )
  
  observe({
    out$sf<-data
    
    WFS <- ows4R::WFSClient$new(url = query$wfs_server,serviceVersion = query$wfs_version,logger = "DEBUG")
    ft <- WFS$capabilities$findFeatureTypeByName(query$layer)
    data <- ft$getFeatures(propertyName="area,geom")
    
    other_data <- data%>%
      select(gml_id)%>%
      filter(gml_id!=out$sf$id)
    
    st_crs(other_data)<-4326
    
    out$other_data<-other_data
  })
  
  bbox<-reactiveVal(NULL)
  
  observeEvent(input$dist,{
  if(input$dist>0){
    print(sprintf("Searching area : %s km",input$dist))
    newbbox<-st_transform( st_sfc(st_buffer(out$sf$geometry[[1]], dist = input$dist*1000, endCapStyle="ROUND"), crs = 3857),4326)
    bbox(newbbox)  
  }
  })

   output$draw_buffer<-renderUI({
     if(!is.null(bbox())){
       style<-"new Style({stroke: new Stroke({color: \"rgba(51, 224,255, 1.0)\",width: 1,}),fill: new Fill({color: \"rgba(51, 224,255, 0.2)\",}),}),"
       tags$script(paste0("parent.postMessage('OFV.drawFeatureFromWKT(\"",st_as_text(bbox()),"\",",style,")','*');"))  
     }else{
       return(NULL)
     }
   })
   
   osm<-data.frame(
     id=c("town",
          "industrial",
          "aerodrome",
          "harbour",
          "marina",
          "ferry_terminal",
          "ferry_route",
          "riverbank",
          "wetland",
          "nature_reserve",
          "protected_area",
          "natural"),
     key=c("place",
           "landuse",
           "aeroway",
           "harbour",
           "leisure",
           "amenity",
           "route",
           "waterway",
           "wetland",
           "leisure",
           "boundary",
           "natural"),
     value=c("town",
             "industrial",
             "aerodrome",
             "yes",
             "marina",
             "ferry_terminal",
             "ferry",
             "riverbank",
             "yes",
             "nature_reserve",
             "protected_area",
             "yes"),stringsAsFactors = F)

osm_response<-reactiveVal(NULL)
osm_info<-reactiveVal(NULL)
    observeEvent(input$send_request,{
      print(input$interactWith)
       if(input$interactWith=='farm'){
       }else{
         target<-subset(osm,id==input$interactWith)
         show_modal_spinner(spin = "flower", color = "#112446",
                            text = "Request sending...please wait", session = shiny::getDefaultReactiveDomain()) 
        
        #Request send to OSM API
         q <- opq (bbox()) %>%
           add_osm_feature(key = target$key, value = target$value) %>%
           osmdata_sf()
         print(q)
        
        #Buffer intersection
         q<-q[[input$type_geometry]]
         if(is.null(q)){
          info<-paste0("Quantity of elements [",input$type_geometry,"] corresponding to '",input$interactWith,"' : ","0")
          osm_info(info)
          osm_response(NULL)
         }else if(nrow(q)==0){
           info<-paste0("Quantity of elements [",input$type_geometry,"] corresponding to '",input$interactWith,"' : ","0")
           osm_info(info)
           osm_response(NULL)
         }else{
           in_buffer<-st_intersection(q,bbox())
           #in_buffer<-q
           info<-paste0("Quantity of elements [",input$type_geometry,"] corresponding to '",input$interactWith,"' : ",nrow(in_buffer))
           osm_info(info)
           osm_response(in_buffer)
         }
         
         remove_modal_spinner()
       }
     })
   
   
   output$result<-renderUI({
   if(!is.null(osm_info())){
     fluidRow(
     HTML(osm_info())
     )
   } 
   })
   
   layer_message<-reactiveVal(NULL)
   observeEvent(input$send_request,{
     req(osm_response())
     if(!is.null(osm_response())){
     if(nrow(osm_response())>0){x<-gsub("'","\'",as(geojson::as.geojson(osm_response()),"character"))
     
      #style
     switch(input$interactWith,
      "town"={
        if(input$type_geometry=="osm_points"){
          style<-"new Style({image: new Circle({radius: 25,fill: new Fill({color: \"rgba(255, 51,57, 0.3)\",}),stroke: null,}),}),"
        }else{
          style<-"new Style({stroke: new Stroke({color: \"rgba(255, 51,57, 1.0)\",width: 1,}),fill: new Fill({color: \"rgba(255, 51,57, 0.3)\",}),}),"
        }
      },
      "industrial"={
        if(input$type_geometry=="osm_points"){
          style<-"new Style({image: new Circle({radius: 15,fill: new Fill({color: \"rgba(241, 196, 15, 0.3)\",}),stroke: null,}),}),"
        }else{
          style<-"new Style({stroke: new Stroke({color: \"rgba(241, 196, 15, 1.0)\",width: 1,}),fill: new Fill({color: \"rgba(241, 196, 15, 0.3)\",}),}),"
        }
      },
      "aerodrome"={
        if(input$type_geometry=="osm_points"){
          style<-"new Style({image: new Icon({src:\"https://upload.wikimedia.org/wikipedia/commons/1/17/Plane_icon_nose_up.svg\", scale:0.1,}),}),"
        }else{
          style<-"new Style({stroke: new Stroke({color: \"rgba(112, 123, 124, 1.0)\",width: 1,}),fill: new Fill({color: \"rgba(112, 123, 124, 0.3)\",}),}),"
        }
      },
      "harbour"={
        if(input$type_geometry=="osm_points"){
          style<-"new Style({image: new Icon({src:\"https://upload.wikimedia.org/wikipedia/commons/6/62/Anchor_pictogram.svg\", scale:0.1,}),}),"
        }else{
          style<-"new Style({stroke: new Stroke({color: \"rgba(255, 51,57, 1.0)\",width: 1,}),fill: new Fill({color: \"rgba(255, 51,57, 0.3)\",}),}),"
        }
      },
      "marina"={
        if(input$type_geometry=="osm_points"){
          style<-"new Style({image: new Icon({src:\"https://nl.wikipedia.org/wiki/Bestand:Sailboat_symbol.svg\", scale:0.1,}),}),"
        }else{
          style<-"new Style({stroke: new Stroke({color: \"rgba(36, 113, 163, 1.0)\",width: 1,}),fill: new Fill({color: \"rgba(36, 113, 163, 0.3)\",}),}),"
        }
      },
      "ferry_terminal"={
        if(input$type_geometry=="osm_points"){
          style<-"new Style({image: new Icon({src:\"https://upload.wikimedia.org/wikipedia/commons/1/1e/Ferry_symbol.svg\", scale:0.1,}),}),"
        }else{
          style<-"new Style({stroke: new Stroke({color: \"rgba(36, 113, 163, 1.0)\",width: 1,}),fill: new Fill({color: \"rgba(36, 113, 163, 0.3)\",}),}),"
        }
      },
      "ferry_route"={
        if(input$type_geometry=="osm_points"){
          style<-"new Style({image: new Circle({radius: 15,fill: new Fill({color: \"rgba(36, 113, 163, 0.3)\",}),stroke: null,}),}),"
        }else{
          style<-"new Style({stroke: new Stroke({color: \"rgba(36, 113, 163, 1.0)\",width: 1,}),fill: new Fill({color: \"rgba(36, 113, 163, 0.3)\",}),}),"
        }
      },    
      "riverbank"={
        if(input$type_geometry=="osm_points"){
          style<-"new Style({image: new Circle({radius: 15,fill: new Fill({color: \"rgba(133, 193, 233, 0.3)\",}),stroke: null,}),}),"
        }else{
          style<-"new Style({stroke: new Stroke({color: \"rgba(133, 193, 233, 1.0)\",width: 1,}),fill: new Fill({color: \"rgba(133, 193, 233, 0.3)\",}),}),"
        }
      },      
      "wetland"={
        if(input$type_geometry=="osm_points"){
          style<-"new Style({image: new Circle({radius: 15,fill: new Fill({color: \"rgba(118, 215, 196, 0.3)\",}),stroke: null,}),}),"
        }else{
          style<-"new Style({stroke: new Stroke({color: \"rgba(118, 215, 196, 1.0)\",width: 1,}),fill: new Fill({color: \"rgba(118, 215, 196, 0.3)\",}),}),"
        }
      },        
      "nature_reserve"={
        if(input$type_geometry=="osm_points"){
          style<-"new Style({image: new Circle({radius: 15,fill: new Fill({color: \"rgba(80, 216, 205, 0.3)\",}),stroke: null,}),}),"
        }else{
          style<-"new Style({stroke: new Stroke({color: \"rgba(80, 216, 205, 1.0)\",width: 1,}),fill: new Fill({color: \"rgba(80, 216, 205, 0.3)\",}),}),"
        }
      }, 
      "protected_area"={
        if(input$type_geometry=="osm_points"){
          style<-"new Style({image: new Circle({radius: 15,fill: new Fill({color: \"rgba(165, 105, 189, 0.3)\",}),stroke: null,}),}),"
        }else{
          style<-"new Style({stroke: new Stroke({color: \"rgba(165, 105, 189, 1.0)\",width: 1,}),fill: new Fill({color: \"rgba(165, 105, 189, 0.3)\",}),}),"
        }
      }, 
      "natural"={
        if(input$type_geometry=="osm_points"){
          style<-"new Style({image: new Circle({radius: 15,fill: new Fill({color: \"rgba(82, 190, 128, 0.3)\",}),stroke: null,}),}),"
        }else{
          style<-"new Style({stroke: new Stroke({color: \"rgba(82, 190, 128, 1.0)\",width: 1,}),fill: new Fill({color: \"rgba(82, 190, 128, 0.3)\",}),}),"
        }
      })

     request<-paste0("parent.postMessage('OFV.setGeoJSONLayer(0, \"",input$interactWith[1],"\", \"",input$interactWith[1],"\", \"",input$interactWith[1],"\", \"",input$interactWith[1],"\",",x[1],",",style[1],")','*');")
     }else{
     request<-NULL}
     layer_message(request)
     }else{
       request<-NULL}
     layer_message(request)
   })
   output$message<-renderUI({
     req(osm_response())
     req(layer_message())
     if(input$project_result){
     if(!is.null(layer_message())){
       print(layer_message())
     tags$script(layer_message()) 
     }else{NULL}
     }else{NULL}
   })

   # output$nb_ferry<-renderText({
   #   if(input$dist>0){
   #   bbox<-reactive({st_transform( st_sfc(st_buffer(out$sf$geometry[[1]], dist = input$dist*1000, endCapStyle="ROUND"), crs = 3857),4326)})
   # q <- opq (bbox()) %>%
   #   add_osm_feature(key = "amenity", value = "ferry_terminal") %>%
   #   osmdata_sf()
   #   paste0("Number of ferry terminal around <b>",input$dist,"</b> km : ",nrow(q$osm_points)) 
   #   }
   # })
   # 
   # output$near_ferry<-renderText({
   #   if(input$dist>0){
   #     bbox<-reactive({st_transform( st_sfc(st_buffer(out$sf$geometry[[1]], dist = input$dist*1000, endCapStyle="ROUND"), crs = 3857),4326)})
   #     ferry <- opq (bbox()) %>%
   #       add_osm_feature(key = "amenity", value = "ferry_terminal") %>%
   #       osmdata_sf()
   #     ferry<-ferry$osm_points
   #     target<-st_transform( st_sfc(out$sf$geometry[[1]], crs = 3857),4326)
   #     if(nrow(ferry)>0){
   #       for(i in 1:nrow(ferry)){
   #         ferry[i,"dist"]<-as.numeric(st_distance(target,ferry[i,]))
   #       }
   #       nearest<-ferry[order(ferry$dist),][1,]
   #       
   #       paste0("The nearest ferry terminal is distant to : <b>",round(nearest$dist/1000,2),"</b> km") 
   #       }
   #   }
   # })
   # 
   # output$near_town<-renderText({
   #   if(input$dist>0){
   #     bbox<-reactive({st_transform( st_sfc(st_buffer(out$sf$geometry[[1]], dist = input$dist*1000, endCapStyle="ROUND"), crs = 3857),4326)})
   #     q <- opq (bbox()) %>%
   #       add_osm_feature(key = "place", value = "town") %>%
   #       osmdata_sf()
   #     if(!is.null(q$osm_points)){
   #     paste0("Near town around <b>",input$dist,"</b> km : ",as.data.frame(q$osm_points)[1,"name"])
   #     }else{
   #      paste0("No town around <b>",input$dist,"</b> km")  
   #     }
   #   }
   # })
   # 
   # output$nb_farm<-renderText({
   # if(input$dist>0){
   #   bbox<-reactive({st_transform( st_sfc(st_buffer(out$sf$geometry[[1]], dist = input$dist*1000, endCapStyle="ROUND"), crs = 3857),4326)})
   #   
   #   in_buffer<-st_intersection(out$other_data,st_sf(bbox()))
   #   out$in_buffer<-in_buffer
   #   if(!is.null(in_buffer)){
   #     paste0("Number of farm around <b>",input$dist,"</b> km : ",nrow(in_buffer))
   #   }else{
   #     paste0("No farm around <b>",input$dist,"</b> km")  
   #   }
   # }   
   # })
   # 
   # output$nearest_farm<-renderText({
   #   if(!is.null(out$in_buffer)){
   #     target<-st_transform( st_sfc(out$sf$geometry[[1]], crs = 3857),4326)
   #     in_buffer<-out$in_buffer
   #     if(nrow(in_buffer)>0){
   #       for(i in 1:nrow(in_buffer)){
   #         in_buffer[i,"dist"]<-as.numeric(st_distance(target,in_buffer[i,]))
   #       }
   #       nearest<-in_buffer[order(in_buffer$dist),][1,]
   #       
   #       print(target)
   #       print(nearest)
   #       
   #       paste0("The nearest farm is distant to : <b>",round(nearest$dist/1000,2),"</b> km") 
   #     }
   #   }
   # })
   
  observe({
    out$data<-as.data.frame(data)
  })
  
  img_url<-reactive({
    df<-out$data
    if(is.null(df)){
      return(NULL)
    }else{
      img<-df[1,]$s2_crop
      img_clean<-str_replace(img,"base64:","")
      img_url<-sprintf("<center><img src=\"data:image/png;base64,\n%s\" alt=\"image\" /></center>",  img_clean)
      return(img_url)
    }
  })
  
  output$img <-renderUI({
    if(!is.null(img_url())){
      HTML(img_url())
    }else{
      div("Image loading ...")
    }
  })
  
  observe({
    time<-unique(as.character(out$data$tile_date))
    out$data_time<-time
    posix_time<-as.POSIXct(out$data_time, format="%Y-%m-%d")
    out$posix_time<-posix_time
    weekBefore<-seq(out$posix_time, by = "-1 day", length.out = 8)
    weekAfter<-seq(out$posix_time, by = "1 day", length.out = 8)
    out$Period<-lubridate::floor_date(sort(unique(c(weekBefore,weekAfter))),'day')
    out$PeriodLabel<-c("Day -7","Day -6","Day -5","Day -4","Day -3","Day -2","Day -1","Select Day","Day +1","Day +2","Day +3","Day +4","Day +5","Day +6","Day +7")
  })
  
  output$data_time <-renderText({
    if(is.null(out$data_time)){
      paste0("<b>Tile date :</b> ","loading...") 
    }else{
      paste0("<b>Tile date :</b> ",out$data_time)
    }
  })
  
  observe({
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
        txt<-paste0(txt,"<a href=",env[i,5]," target=_blank>",env[i,4]," : </a> ",Feature$value," ",env[i,6]," [",timeMatch,"]<br>")
      }else{
        txt<-paste0(txt,"<a href=",env[i,5]," target=_blank>",env[i,4]," : </a>No Data Available")
      }
    }
    out$go<-TRUE
    out$env<-outt
    out$txt<-txt
  })
  
  output$env_values <- renderText({
    out$txt
  })
  
  data_period<-reactive({
    if(out$go){
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
    }
  })
  
  output$table <- DT::renderDT(server = FALSE, {
    if(!is.null(data_period())){
      datatable(data_period())%>%
        formatStyle("chronology",target = 'row',fontWeight = "bold",backgroundColor = styleEqual(c("Select Day"), c("orange")))
    }else{NULL}
  })
  
  output$stat <- DT::renderDT(server = FALSE, {
    if(!is.null(data_period())){
      datatable(
        reshape::melt(data_period(), id=c("time","chronology"))%>%
          group_by(variable)%>%
          mutate(value = na_if(value,"none"))%>%
          summarise(min=round(min(as.numeric(na.omit(value))),2),
                    max=round(max(as.numeric(na.omit(value))),2),
                    mean=round(mean(as.numeric(na.omit(value))),2),
                    median=round(median(as.numeric(na.omit(value))),2),
                    sd=round(sd(as.numeric(na.omit(value))),2)),
        options = list(dom = 't'), rownames = FALSE)
    }else{NULL}
  })
  
  
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
  
  output$graph <- renderPlotly({
    if(!is.null(data_period())){
 
      data<-data_period()%>%
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
    }else{NULL}
  })
  
  output$windrose <- renderPlot({
    if(!is.null(data_period())){

      wind_dir <- data_period() %>%
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
    }else{NULL}
  }) 
  
   output$selector <- renderUI({
     selectInput(ns("env_select"), "Choose variable:",
                      choices = unique(env$id),
                      multiple = F,
                      selected = unique(env$id)[1])
   })
   
   output$plot <- renderPlotly({
     if(!is.null(data_period())){
     data<-data_period()%>%
       gather(key="env_variable", value="value",-time,-chronology)%>%
       mutate(time = as.POSIXct(time,format="%Y-%m-%d"))%>%
       filter(env_variable == input$env_select)%>%
       mutate(value = as.numeric(na_if(value,"none")))
     
     text<-reactive({subset(env,id==input$env_select)$label})
     unit<-reactive({subset(env,id==input$env_select)$unit})
     color<-reactive({subset(env,id==input$env_select)$color})
     
     plot_ly(data)%>%
      add_trace(x = ~time, y = ~value,line = list(color = color()),marker = list(color = color()), type = 'scatter',mode = 'lines+markers',name=input$env_select, text =paste0(text()," : ",data$value))%>%
      layout(shapes = list (vline(out$posix_time)),
               xaxis = list(type = "date",range=c(min(data$time), max(data$time)),title="",titlefont = list(size = 9), tickfont = list(size = 9)),
               yaxis = list(title=list(text=paste0(text()," (",unit(),")")),hoverformat = '.3f',showticklabels = T,automargin = F,titlefont = list(size = 9,color = color()), tickfont = list(size = 7,color = color())),
               showlegend = FALSE)
   }else{NULL}
})
 
}

####