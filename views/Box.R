###Module
# Function for module UI
BoxUI <- function(id) {
  ns <- NS(id)
  # Options for Spinner
  options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=1)
  tabPanel(title=uiOutput(ns("title_panel")),
           fluidRow(
             column(6,selectInput(inputId = ns('x'), label = "Select x-axis Variable:",choices = NULL)),               
             column(6,selectInput(inputId = ns('y'), label = "Select y-axis Variable:",choices = NULL))
           ),
           
           fluidRow(
             column(6,uiOutput(ns("time"))),
             column(6,uiOutput(ns("slider")))
           ),
           uiOutput(ns("split")),
           div(plotlyOutput(ns('plot'),height="250px")%>%withSpinner(type = 2),  style = "font-size:80%"))
  
}

# Function for module server logic
Box <- function(input, output, session,data,dsd,box.x,box.y,box.z,box.title,box.caption) {
  ns <- session$ns 
  
  output$title_panel = renderText({
    box.title()
  })
  
  observe({
    dsd <- dsd()
    box.x<-box.x()
    attribute<-setdiff(as.character(dsd[dsd$MemberType=='attribute',]$MemberCode),c("geometry","aggregation_method"))
    updateSelectInput(session, 'x', choices = attribute, selected = box.x)
  }) 
  
   observe({
    dsd <- dsd()
    box.y<-box.y()
    variable<-as.character(dsd[dsd$MemberType=='variable',]$MemberCode)
    updateSelectInput(session, 'y', choices = variable, selected=box.y)
  }) 
  

  observe({
        dsd <- dsd()
        box.z <- box.z()
        df<-as.data.frame(data())
        attribute<-setdiff(as.character(dsd[dsd$MemberType=='attribute',]$MemberCode),c("geometry","aggregation_method"))
        
        output$time<-renderUI({
          selectInput(inputId = ns('z'), label = "Select Time Variable:", choices = attribute, selected = box.z)
        })
  })
  
        
        observe({
          df<-as.data.frame(data())  
        output$split<-renderUI({
          choice<-unique(subset(df,select=input$x))
          selectInput(inputId = ns('n'), label = "Selection elements to show:", choices = choice,selected=choice[1,1],multiple = TRUE)
        })
        })
        
        observe({
          
          output$slider<-renderUI({
            df<-as.data.frame(data())
            tmp<-subset(df,select=input$z)
            sliderInput(inputId = ns('s'), label = "Choose Period:", min=min(tmp),max=max(tmp),value = c(min(tmp),max(tmp)),step=1,sep="")
          })    
          
        })

      
      output$plot <- renderPlotly({
        df<-as.data.frame(data())
        
        df <- df %>%
          select(!!! syms(c(input$x,input$y,input$z))) %>%
          filter(!! sym(input$z) %in% seq(min(input$s),max(input$s),1)) %>%
          filter(!! sym(input$x) %in% input$n)%>%
          ungroup()
        names(df)<-c("attr_name","var_sum","time")
        print(df)
        
        fig <- plot_ly(df,
          x = ~attr_name, 
          y = ~var_sum,
          height = 300,
          type = 'box'
        )
        fig <- fig %>%  layout(
          title = box.caption(),
          xaxis = list(
            titlefont = list(size = 10), 
            tickfont = list(size = 10),
            title = input$x,
            zeroline = F
          ),
          yaxis = list(
            titlefont = list(size = 10), 
            tickfont = list(size = 10),
            title = input$y,
            zeroline = F
          ))
        })
    

  
  
}
####