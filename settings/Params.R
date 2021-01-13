# Function for module server logic
Params <- function(input, output, session) {
  params <- reactiveValues(
    panel=NULL,
    geoCol=NULL,
   # withFlag=NULL,
    flagCol=NULL,
    line.title=NULL,
    line.x=NULL,
    line.y=NULL,
    line.z=NULL,
    line.caption=NULL,
    pie.title=NULL,
    pie.x=NULL,
    pie.y=NULL,
    pie.z=NULL,
    pie.caption=NULL,
    box.title=NULL,
    box.x=NULL,
    box.y=NULL,
    box.z=NULL,
    box.caption=NULL,
    data.title=NULL,
    data.caption=NULL
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
    
    geoCol <- if (!is.null(query$geoCol)){
      query$geoCol
    }else{
      NULL
    }
    params$geoCol<-geoCol
    
    # withFlag <- if (!is.null(query$withFlag)){
    #   as.logical(query$withFlag)
    # }else{
    #   FALSE
    # }
    # params$withFlag<-withFlag

    line.title <- if (!is.null(query$line.title)){
      query$line.title
    }else{
      "Line"
    }
    params$line.title<-line.title
    
    line.x <- if (!is.null(query$line.x)){
      query$line.x
    }else{
      NULL
    }
     params$line.x<-line.x
     
    line.y <- if (!is.null(query$line.y)){
      query$line.y
    }else{
      NULL
    }
    
    params$line.y<-line.y
     
    line.z <- if (!is.null(query$line.z)){
      query$line.z
    }else{
      NULL
    }
    params$line.z<-line.z  
    
    line.caption <- if (!is.null(query$line.caption)){
      query$line.caption
    }else{
      NULL
    }
    params$line.caption<-line.caption
    
    pie.title <- if (!is.null(query$pie.title)){
      query$pie.title
    }else{
      "Pie"
    }
    params$pie.title<-pie.title
    
    pie.x <- if (!is.null(query$pie.x)){
       query$pie.x
     }else{
       NULL
     }
     params$pie.x<-pie.x
     
     pie.y <- if (!is.null(query$pie.y)){
       query$pie.y
     }else{
       NULL
     }
     params$pie.y<-pie.y
     
     pie.z <- if (!is.null(query$pie.z)){
       query$pie.z
     }else{
       NULL
     }
     params$pie.z<-pie.z
     
     pie.caption <- if (!is.null(query$pie.caption)){
       query$pie.caption
     }else{
       NULL
     }
     params$pie.caption<-pie.caption
     
     box.title <- if (!is.null(query$box.title)){
       query$box.title
     }else{
       "Box"
     }
     params$box.title<-box.title
     
     box.x <- if (!is.null(query$box.x)){
       query$box.x
     }else{
       NULL
     }
     params$box.x<-box.x
    
     box.y <- if (!is.null(query$box.y)){
       query$box.y
     }else{
       NULL
     }
     params$box.y<-box.y 
      
     box.z <- if (!is.null(query$box.z)){
       query$box.z
     }else{
       NULL
     }
     params$box.z<-box.z  
     
     box.caption <- if (!is.null(query$box.caption)){
       query$box.caption
     }else{
       NULL
     }
     params$box.caption<-box.caption
     
     data.title <- if (!is.null(query$data.title)){
       query$data.title
     }else{
       "Data"
     }
     params$data.title<-data.title
     
     data.caption <- if (!is.null(query$data.caption)){
       query$data.caption
     }else{
       NULL
     }
     params$data.caption<-data.caption  
    })
    return(params)
}