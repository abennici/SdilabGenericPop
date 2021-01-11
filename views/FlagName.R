###Module
# Function for module UI
FlagNameUI <- function(id) {
  ns <- NS(id)
  #tags$h4(
  #uiOutput(ns('img')),
  #textOutput(ns('value')),
  #tags$img(src="https://www.blue-cloud.org/sites/all/themes/arcadia/logo.png",height=18,align = "right"))
  uiOutput(ns('flag_header'))
  }

# Function for module server logic
FlagName <- function(input, output, session,data,withFlag,flagCol) {
  observe({
    withFlag<-withFlag()
    if(withFlag){
    flagCol<-flagCol()
    print("flagCol")
    print(flagCol)
    c_flag<-as.data.frame(data())
    c_flag<-unique(subset(c_flag,select=flagCol))
    names(c_flag)<-"flag"
    l_flag<-readr::read_csv("https://raw.githubusercontent.com/openfigis/RefData/gh-pages/country/CL_FI_COUNTRY_ITEM.csv", guess_max = 0)
    l_flag<-l_flag[,c("ISO3_Code","Name_En")]
    print(c_flag)
    print(head(l_flag))
    flag_name<-merge(c_flag,l_flag,by.x="flag",by.y="ISO3_Code",all.x=T,all.y=F) 
   # cat( paste0("http://www.fao.org/fileadmin/assets/countries/flags/",tolower(as.vector(flag_name$flag)[1]),".gif"))
   #output$value <- renderText({ paste0(flag_name$Name_En," [",flag_name$flag,"]") })
   output$flag_header <- renderUI({
     #url<-paste0("http://www.fao.org/fileadmin/assets/countries/flags/",tolower(as.vector(flag_name$flag)[1]),".gif")
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