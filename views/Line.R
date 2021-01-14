###Module
# Function for module UI
LineUI <- function(id) {
  ns <- NS(id)
  # Options for Spinner
  options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=1)
  tabPanel(title=uiOutput(ns("title_panel")),
           fluidRow(
             box(width=12, collapsible=T,collapsed=F,
                 uiOutput(ns("x")),
                 uiOutput(ns("y")),
                 uiOutput(ns("slider")),
                 uiOutput(ns("SplitByColumn")),
                 uiOutput(ns("colorColumn")),
                 uiOutput(ns("number"))
                 )),
           fluidRow(
            div(plotlyOutput(ns('plot'),height="250px")%>%withSpinner(type = 2),  style = "font-size:80%")
           ))
}

# Function for module server logic
Line <- function(input, output, session,data,dsd,line.x,line.y,line.z,line.title,line.caption) {
  ns <- session$ns 
  
  output$title_panel = renderText({
    line.title()
  })
  
  output$x<-renderUI({
    dsd <- dsd()
    line.x<-line.x()
    attribute<-setdiff(as.character(dsd[dsd$MemberType=='attribute',]$MemberCode),c("geometry","aggregation_method"))
    selectInput(inputId = ns('x'), label = "Select x-axis Variable:", choices = attribute, selected = line.x)
  }) 
  
  xVarName <- reactive({
    input$x
  }) 
  
  output$y<-renderUI({
    dsd <- dsd()
    line.y<-line.y()
    variable<-as.character(dsd[dsd$MemberType=='variable',]$MemberCode)
    selectInput(inputId = ns('y'), label = "Select y-axis Variable:", choices =variable, selected=line.y)
  }) 
  
  yVarName <- reactive({
    input$y
  })
  
  output$slider<-renderUI({
    df<-as.data.frame(data())
    tmp<-subset(df,select=input$x)
    sliderInput(inputId = ns('s'), label = "Choose Period:", min=min(tmp),max=max(tmp),value = c(min(tmp),max(tmp)),step=1,sep="")
  })    
  
 output$SplitByColumn<-renderUI({
  radioButtons(ns("SplitByColumn"),"Split by another column:",choices = c("no", "yes"),selected = "no",inline = TRUE)
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
          nb_unique<-nrow(unique(subset(df,select=input$z)))
          nb_select<-ifelse(nb_unique>=10,10,nb_unique)                
          selectInput(inputId = ns('n'), label = "Numbers of element:", choices = seq(1,nb_unique,by=1), selected = nb_select)
        })
        
        nVarName <- reactive({
          input$n
        })
        
      }
      
      output$plot <- renderPlotly({
        df<-as.data.frame(data())

        df_rank<-df%>%
          group_by(!! sym(input$z))%>%
          summarise(sum = sum(!! sym(input$y)))%>%
          mutate(rank = rank(-sum))%>%
          filter(rank <= as.numeric(input$n))
        names(df_rank)<-c("gr_name","sum","rank")
        
        df <- df %>%
          filter(!! sym(input$x) %in% seq(min(input$s),max(input$s),1)) %>%
          filter(!! sym(input$z) %in% df_rank$gr_name)%>%
          group_by(!!! syms(c(input$x,input$z)))%>%
          summarise(var_sum = sum(!! sym(input$y)))%>%
          ungroup()
        
        names(df)<-c("attr_name","attr_col","var_sum")
        
        
        print(df)
        
        fig <- df %>% 
          plot_ly(
            x = ~attr_name, 
            y = ~var_sum,
            split=~attr_col,
            height = 300,
            type = 'scatter', 
            mode = 'lines',
            line = list(simplyfy = F),
            text = ~paste(input$x,": ", attr_name, "<br>",input$z,": ", attr_col, "<br>",input$y,": ", var_sum), 
            hoverinfo = 'text'
          )
        
        fig <- fig %>% layout(
          title = line.caption(),
          legend = list(orientation = "v",
                        font = list(size = 10),
                        bgcolor ='rgba(0,0,0,0)',
                        title = input$z),
          xaxis = list(
            titlefont = list(size = 10), 
            tickfont = list(size = 10),
            range = range(unique(df$attr_name)),
            title = input$x,
            zeroline = F
          ),
          yaxis = list(
            titlefont = list(size = 10), 
            tickfont = list(size = 10),
            range = c(0,max(df$var_sum)+25*max(df$var_sum)/100),
            title = input$y,
            zeroline = F
          ))
       
         fig
           
      })
    })
  }) 
  
  observe({
    observeEvent(input$SplitByColumn, {
      if(input$SplitByColumn == "no"){  
        output$colorColumn<-renderUI({})
        output$number<-renderUI({})
        
        output$plot <- renderPlotly({
          
          df<-as.data.frame(data())
          
          df <- df %>%
            filter(!! sym(input$x) %in% seq(min(input$s),max(input$s),1)) %>%
            group_by(!! sym(input$x)) %>% 
            summarise(var_sum = sum(!! sym(input$y)))%>%
            stats::setNames(c("attr_name","var_sum")) %>%
            ungroup()
        
          print(df)
          
          fig <- df %>% 
            plot_ly(
              x = ~attr_name, 
              y = ~var_sum,
              height = 300,
              type = 'scatter', 
              mode = 'lines',
              fill = 'tozeroy',
              line = list(simplyfy = F),
              text = ~paste(input$x,": ", attr_name, "<br>",input$y,": ", var_sum), 
              hoverinfo = 'text'
            )
          
          fig <- fig %>% layout(title = line.caption(),
                                yaxis = list(title = input$y, range = c(0,max(df$var_sum)+25*max(df$var_sum)/100), zeroline = F),
                                xaxis = list( title = input$x, zeroline = F)
          ) 
          fig
        })
      }
    }) 
    
  })
}
####