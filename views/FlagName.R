###Module
# Function for module UI
FlagNameUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns('flag_header'))
  }

# Function for module server logic
FlagName <- function(input, output, session,data,query) {
  observe({
    geoCol <- if (!is.null(query$geoCol)){query$geoCol}else{NULL}

    if(toupper(geoCol)%in%c("FLAG","FLAGS","FLAGSTATE","FLAGSTATES","COUNTRY","COUNTRIES")){
    
    c_flag<-as.data.frame(data)
    c_flag<-unique(subset(c_flag,select=geoCol))
    names(c_flag)<-"flag"
    l_flag<-readr::read_csv("https://raw.githubusercontent.com/openfigis/RefData/gh-pages/country/CL_FI_COUNTRY_ITEM.csv", guess_max = 0)
    l_flag<-l_flag[,c("ISO3_Code","Name_En")]
    print(c_flag)
    print(head(l_flag))
    flag_name<-merge(c_flag,l_flag,by.x="flag",by.y="ISO3_Code",all.x=T,all.y=F)
   output$flag_header <- renderUI({
     url<-paste0("https://raw.githubusercontent.com/eblondel/OpenFairViewer/master/src/img/flags/",tolower(as.vector(flag_name$flag)[1]),".gif")
     tags$div(
       tags$img(src="https://www.blue-cloud.org/sites/all/themes/arcadia/logo.png",height=18,align = "right"),
       tags$img(src=url,height=15,align = "left",style="margin-right:5px"),
       tags$h4(paste0(flag_name$Name_En[1]," [",flag_name$flag[1],"]"))
       
       
     )
   })
    }else{
      output$flag_header <- renderUI({
        tags$div(
          tags$img(src="https://www.blue-cloud.org/sites/all/themes/arcadia/logo.png",height=18,align = "right"))
      })   
    }
    })                        

  }