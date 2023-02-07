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

    if(!is.null(geoCol)&&toupper(geoCol)%in%c("FLAG","FLAGS","FLAGSTATE","FLAGSTATES","COUNTRY","COUNTRIES")){
    
    c_flag<-as.data.frame(data)
    c_flag<-unique(subset(c_flag,select=geoCol))
    names(c_flag)<-"flag"
    l_flag<-readr::read_csv("https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/cl_flagstate_iso3.csv", guess_max = 0)
    l_flag<-l_flag[,c("code","label")]
    print(c_flag)
    print(head(l_flag))
    flag_name<-merge(c_flag,l_flag,by.x="flag",by.y="code",all.x=T,all.y=F)
   output$flag_header <- renderUI({
     url<-paste0("https://raw.githubusercontent.com/fdiwg/flags/main/",tolower(as.vector(flag_name$flag)[1]),".gif")
     tags$div(
       tags$img(src="https://hackathon.blue-cloud.org/wp-content/uploads/2021/11/Blue-cloud_extended_color.png",height=18,align = "right"),
       tags$img(src=url,height=15,align = "left",style="margin-right:5px"),
       tags$h4(paste0(flag_name$label[1]," [",flag_name$flag[1],"]"))
       
       
     )
   })
    }else{
      output$flag_header <- renderUI({
        tags$div(
          tags$img(src="https://hackathon.blue-cloud.org/wp-content/uploads/2021/11/Blue-cloud_extended_color.png",height=18,align = "right"))
      })   
    }
    
    })                        

  }