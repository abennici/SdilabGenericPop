###Line Module
# Function for module UI
line_ui <- function(id) {
  ns <- NS(id)

tabPanel(title=uiOutput(ns("title_panel")),value="line",
  fluidRow(
    column(1,offset=10,uiOutput(ns("info")))
#    a(id = ns("toggleAdvanced"), "Show/hide parameters", href = "#"),
  ),
#  fluidRow(
#    box(id=ns("box"),width=12,"TEXT")
#  ),
  fluidRow(
    box(id=ns("box2"),width=12,collapsible = T,collapsed = F,
      uiOutput(ns("x")),
      uiOutput(ns("y")),
      uiOutput(ns("slider")),
      uiOutput(ns("SplitByColumn")),
      uiOutput(ns("colorColumn")),
      uiOutput(ns("number"))
    )
  ),
  fluidRow(
    div(plotlyOutput(ns('plot'),height="250px")%>%withSpinner(type = 2),  style = "font-size:80%")
  )
)

}

# Function for module server
line_server <- function(input, output, session,data,dsd,query) {
  ns <- session$ns 
  
output$title_panel <- renderText({
  if (!is.null(query$line.title)){query$line.title}else{"Line"}
})
  
output$info <-renderUI({
  circleButton(ns("info"),icon = icon("info-circle"),size='xs')
})
  
# observe({
#   shinyjs::hide(ns("box"))
#   shinyjs::onclick(ns("toggleAdvanced"),shinyjs::toggle(ns("box"))
#   )
# })
  
observeEvent(input$info, {
  showModal(modalDialog(
    if (!is.null(query$line.info)){query$line.info}else{NULL}
  ))
})
    
output$x<-renderUI({
  line.x<-if (!is.null(query$line.x)){query$line.x} else {NULL}
  attribute<-setdiff(as.character(dsd[dsd$MemberType=='attribute',]$MemberCode),c("geometry","aggregation_method"))
    selectInput(inputId = ns('x'), label = "Select x-axis Variable:", choices = attribute, selected = line.x)
}) 
  
output$y<-renderUI({
  line.y<-if (!is.null(query$line.y)){query$line.y}else{NULL}
  variable<-as.character(dsd[dsd$MemberType=='variable',]$MemberCode)
    selectInput(inputId = ns('y'), label = "Select y-axis Variable:", choices =variable, selected=line.y)
}) 
  
output$slider<-renderUI({
  df<-as.data.frame(data)
  tmp<-subset(df,select=input$x)
    sliderInput(inputId = ns('s'), label = "Choose Period:", min=min(tmp),max=max(tmp),value = c(min(tmp),max(tmp)),step=1,sep="")
})    
  
output$SplitByColumn<-renderUI({
  radioButtons(ns("SplitByColumn"),"Split by another column:",choices = c("no", "yes"),selected = "no",inline = TRUE)
})
 
observeEvent(input$SplitByColumn, {
  line.caption <-if (!is.null(query$line.caption)){query$line.caption}else{NULL}
  
  if(input$SplitByColumn == "no"){
    df<-as.data.frame(data)
    
    output$colorColumn<-renderUI({})
    
    output$number<-renderUI({})
    
    output$plot <- renderPlotly({
      
      df <- df %>%
        filter(!! sym(input$x) %in% seq(min(input$s),max(input$s),1)) %>%
        group_by(!! sym(input$x)) %>% 
        summarise(var_sum = sum(!! sym(input$y)))%>%
        stats::setNames(c("attr_name","var_sum")) %>%
        ungroup()
      
      fig <- df %>% plot_ly(
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
      
      fig <- fig %>% layout(
        title = line.caption,
        yaxis = list(title = input$y, range = c(0,max(df$var_sum)+25*max(df$var_sum)/100), zeroline = F),
        xaxis = list( title = input$x, zeroline = F)
      ) 
      fig
    })
  }
  
  if(input$SplitByColumn == "yes"){ 
    df<-as.data.frame(data)
  
    output$colorColumn<-renderUI({
      line.z<-if (!is.null(query$line.z)){query$line.z}else{NULL}
      attribute<-setdiff(as.character(dsd[dsd$MemberType=='attribute',]$MemberCode),c("geometry","aggregation_method"))
        selectInput(inputId = ns('z'), label = "Select Color Variable:", choices = attribute, selected = line.z)
    })

    output$number<-renderUI({
      nb_unique<-nrow(unique(subset(df,select=input$z)))
      nb_select<-ifelse(nb_unique>=10,10,nb_unique)                
        selectInput(inputId = ns('n'), label = "Numbers of element:", choices = seq(1,nb_unique,by=1), selected = nb_select)
    })
      
    output$plot <- renderPlotly({

      df_rank<-df%>%
        group_by(!! sym(input$z))%>%
        summarise(sum = sum(!! sym(input$y)))%>%
        mutate(rank = rank(-sum))%>%
        stats::setNames(c("gr_name","sum","rank"))%>%
        filter(rank <= as.numeric(input$n))

      df <- df %>%
        filter(!! sym(input$x) %in% seq(min(input$s),max(input$s),1)) %>%
        filter(!! sym(input$z) %in% df_rank$gr_name)%>%
        group_by(!!! syms(c(input$x,input$z)))%>%
        summarise(var_sum = sum(!! sym(input$y)))%>%
        stats::setNames(c("attr_name","attr_col","var_sum"))%>%
        ungroup()
        
      fig <- df %>% plot_ly(
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
              title = line.caption,
              legend = list(
                          orientation = "v",
                          font = list(size = 10),
                          bgcolor ='rgba(0,0,0,0)',
                          title = input$z
              ),
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
              )
            )
       
         fig
           
    })
  }

  
}) 
    
}
####