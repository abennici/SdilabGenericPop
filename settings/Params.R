# Function for module server logic
Params <- function(input, output, session) {
  params <- reactiveValues(
    panel=NULL,
    withFlag=NULL,
    flagCol=NULL,
    line.x=NULL,
    line.z=NULL,
    pie.x=NULL,
    pie.z=NULL,
    box.x=NULL,
    box.y=NULL
      )
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    ####Extraction of url parameters
    
    panel <- if (!is.null(query$panel)){
      unlist(strsplit(as.character(query$panel),"[[:punct:][:space:]]+")) 
    }else{
      NULL
    }
    params$panel<-panel
    
    withFlag <- if (!is.null(query$withFlag)){
      as.logical(query$withFlag)
    }else{
      FALSE
    }
    params$withFlag<-withFlag

    flagCol <- if (!is.null(query$flagCol)){
      query$flagCol
    }else{
      NULL
    }
    params$flagCol<-flagCol
    
    line.x <- if (!is.null(query$line.x)){
      query$line.x
    }else{
      NULL
    }
     params$line.x<-line.x
     
    line.z <- if (!is.null(query$line.z)){
      query$line.z
    }else{
      NULL
    }
     params$line.z<-line.z  
     
     pie.x <- if (!is.null(query$pie.x)){
       query$pie.x
     }else{
       NULL
     }
     params$pie.x<-pie.x
     
     pie.z <- if (!is.null(query$pie.z)){
       query$pie.z
     }else{
       NULL
     }
     params$pie.z<-pie.z
     
     box.x <- if (!is.null(query$box.x)){
       query$box.x
     }else{
       NULL
     }
     params$box.x<-box.x
     
     box.z <- if (!is.null(query$box.z)){
       query$box.z
     }else{
       NULL
     }
     params$box.z<-box.z  
    })
    return(params)
}