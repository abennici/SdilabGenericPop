###Communication with parent viewer
##Query Example : /?pid=aquaculture_farm_detection&layer=aquaculture_farm_detection&wfs_server=https://geoserver-sdi-lab.d4science.org/geoserver/sdilab_fisheriesatlas/ows&wfs_version=1.0.0&wms_server=https://geoserver-sdi-lab.d4science.org/geoserver/sdilab_fisheriesatlas/wms&wms_version=1.1.0&feature_geom=false&strategy=ogc_filters&geom=the_geom&x=217&y=181&width=256&height=256&bbox=-8140237.7642581295,-5165920.119625352,-8101102.005776119,-5126784.361143342&srs=EPSG:3857&geoCol=null&panel=fao_aqua_env&fao_aqua_env.title=Aquaculture-Environmental%20enrichment&fao_aqua_env.script=https://raw.githubusercontent.com/abennici/SdilabGenericPop/master/remotes/fao_aqua_env.R
# Function for module UI
jsCode <- "shinyjs.SwitchMapView = function(){parent.postMessage('OFV.switchMapView()','*');}"

map_interaction_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(title=uiOutput(ns("title_panel")),value="map_interaction",
    tabsetPanel(
      tabPanel("Test1",
        fluidRow(
          actionButton(ns("mapview1"), "Switch 2D/3D view on map")
        )),
      tabPanel("Test2",
               fluidRow(
                 tags$script("parent.postMessage('OFV.switchMapView()','*');")
               )),
      tabPanel("Test3",
        fluidRow(
          actionButton(ns("mapview3"), "Switch 2D/3D view on map"),
         tags$script("
Shiny.addCustomMessageHandler('alert', function(arg) {
  alert(arg.val);
  alert(arg.size);
});")
      ))
    )
  )
}

# Function for module server
map_interaction_server <- function(input, output, session,data,dsd,query) {
  ns<-session$ns
  
  onclick(input$mapview1,"parent.postMessage('OFV.switchMapView()','*');")

  observeEvent(input$mapview3{
    session$sendCustomMessage("alert", list(
      val = 2, 
      size = 11
    ))
  })
  #   #session$sendCustomMessage("background-color", nextColor())
  #   #window.parent.OFV.SwitchMapView()
  #   js$SwitchMapView()
  #   cat("click!")
  # })

   
}

####