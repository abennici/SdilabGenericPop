###Module
# Function for module UI
LineUI <- function(id) {
  ns <- NS(id)
  # Options for Spinner
  options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=1)
  tabPanel("Line",
       fluidRow(
    #without box
  column(6,selectInput(inputId = ns('x'), label = "Select x-axis Variable:",choices = NULL)),
  column(6,selectInput(inputId = ns('y'), label = "Select y-axis Variable:",choices = NULL))
      ),
  uiOutput(ns("slider")),
      fluidRow(
  column(6,radioButtons(ns("SplitByColumn"),"Split by another column:",choices = c("no", "yes"),selected = "no",inline = TRUE)),
  column(6,uiOutput(ns("colorColumn")))
      ),
  uiOutput(ns("number")),
  div(plotOutput(ns('plot'),height="250px")%>%withSpinner(type = 2),  style = "font-size:80%"))
    #with box
  # box(title = "Parameters",
  #     width = NULL,
  #     collapsible = T,
  #     class = "collapsed-box",
  #     collapsed = T,
  #   box(
  #     selectInput(inputId = ns('x'), label = "Select x-axis Variable:",choices = NULL),
  #     uiOutput(ns("slider")),
  #     radioButtons(ns("SplitByColumn"),"Split by another column:",choices = c("no", "yes"),selected = "no",inline = TRUE)
  #     ),
  #   box(   
  #     selectInput(inputId = ns('y'), label = "Select y-axis Variable:",choices = NULL),
  #     uiOutput(ns("colorColumn")),
  #     uiOutput(ns("number")))
  #   ),
  # div(plotOutput(ns('plot'),height="250px")%>%withSpinner(type = 2),  style = "font-size:80%")
  # ))
}

# Function for module server logic
Line <- function(input, output, session,data,dsd,line.x,line.z) {
  ns <- session$ns 
   observe({
    dsd <- dsd()
    line.x<-line.x()
    attribute<-setdiff(as.character(dsd[dsd$MemberType=='attribute',]$MemberCode),c("geometry","aggregation_method"))
    updateSelectInput(session, 'x', choices = attribute, selected = line.x)
  }) 
  
  xVarName <- reactive({
    input$x
  }) 
  
  observe({
    
    output$slider<-renderUI({
      df<-as.data.frame(data())
      tmp<-subset(df,select=input$x)
      sliderInput(inputId = ns('s'), label = "Choose Period:", min=min(tmp),max=max(tmp),value = c(min(tmp),max(tmp)),step=1,sep="")
    })    
    
  })
  
  observe({
    dsd <- dsd()
    variable<-as.character(dsd[dsd$MemberType=='variable',]$MemberCode)
    updateSelectInput(session, 'y', choices = variable)
  }) 

  yVarName <- reactive({
    input$y
  })
  

      
  observe({
    observeEvent(input$SplitByColumn, {
      if(input$SplitByColumn == "yes"){ 
      dsd <- dsd()
      df<-as.data.frame(data())
      line.z<-line.z()
      attribute<-setdiff(as.character(dsd[dsd$MemberType=='attribute',]$MemberCode),c("geometry","aggregation_method"))
      output$colorColumn<-renderUI({
        
      selectInput(inputId = ns('z'), label = "Select Color Variable:", choices = attribute, selected = line.z)
      })
      
      zVarName <- reactive({
        input$z
      })
      print(zVarName())
      output$number<-renderUI({
        selectInput(inputId = ns('n'), label = "Numbers of element:", choices = seq(1,nrow(unique(subset(df,select=input$z))),by=1), selected = 10)
      })
      
      nVarName <- reactive({
        input$n
      })
      
      }
      
      output$plot <- renderPlot({
        df<-as.data.frame(data())
        
        df_rank<-df%>%
        group_by(!! sym(input$z))%>%
        summarise(sum = sum(!! sym(input$y)))%>%
        mutate(rank = rank(-sum))%>%
        filter(rank <= as.numeric(input$n))
        names(df_rank)<-c("gr_name","sum","rank")
        print("NB values :")
        print(input$n)
        print(df_rank)
        
        df <- df %>%
          filter(!! sym(input$x) %in% seq(min(input$s),max(input$s),1)) %>%
          filter(!! sym(input$z) %in% df_rank$gr_name)%>%
          group_by(!!! syms(c(input$x,input$z)))%>%
          summarise(var_sum = sum(!! sym(input$y)))%>%
          ungroup()
        
      names(df)<-c("attr_name","attr_col","var_sum")
      
      
        print(df)
        ggplot(df, aes(x = attr_name, y = var_sum, color= attr_col))+
          geom_line()+
          labs(x=input$x,y=input$y,color=input$z)+
          theme_bw()+
          theme(
            axis.text.x = element_text(angle=90,vjust=0.5)
          ) 
      })
    })
  }) 

  observe({
    observeEvent(input$SplitByColumn, {
      if(input$SplitByColumn == "no"){  
        output$colorColumn<-renderUI({})
        output$number<-renderUI({})
        
     output$plot <- renderPlot({
    df<-as.data.frame(data())
    
    df <- df %>%
      filter(!! sym(input$x) %in% seq(min(input$s),max(input$s),1)) %>%
      group_by(!! sym(input$x)) %>% 
      summarise(var_sum = sum(!! sym(input$y)))%>%
      ungroup()
    names(df)<-c("attr_name","var_sum")
print(df)
       ggplot(df, aes(x = attr_name, y = var_sum))+
       geom_line()+
       labs(x=input$x,y=input$y)+
       theme_bw()+
       theme(
         axis.text.x = element_text(angle=90,vjust = 0.5)
       ) 
      })
      }
    }) 

  })
}
####