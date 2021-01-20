###Box Module
# Function for module UI
box_ui <- function(id) {
  ns <- NS(id)

tabPanel(title=uiOutput(ns("title_panel")),value="box",
  fluidRow(
    column(1,offset=10, uiOutput(ns("info")))
  ),
  fluidRow(
    div(plotlyOutput(ns('plot'),height="300px")%>%withSpinner(type = 2),  style = "font-size:80%")
  ),
  br(),
  fluidRow(
    box(width=12,collapsible = T,collapsed = F,
      uiOutput(ns("x")),
      uiOutput(ns("y")),
      uiOutput(ns("time")),
      uiOutput(ns("slider")),
      uiOutput(ns("split")),
    )
  )
)

}

# Function for module server
box_server <- function(input, output, session,data,dsd,query) {
  ns <- session$ns 

output$title_panel <- renderText({
  if (!is.null(query$box.title)){query$box.title}else{"Box"}
})
  
output$info <-renderUI({
    circleButton(ns("info"),icon = icon("info-circle"),size='xs')
})
  
observeEvent(input$info, {
  showModal(modalDialog(
    if (!is.null(query$box.info)){query$box.info}else{NULL}
    ))
})
  
output$x<-renderUI({
  box.x<-if (!is.null(query$box.x)){query$box.x} else {NULL}
  attribute<-setdiff(as.character(dsd[dsd$MemberType=='attribute',]$MemberCode),c("geometry","aggregation_method"))
    selectInput(inputId = ns('x'), label = "Select x-axis Variable:",choices = attribute, selected = box.x)
}) 
  
output$y<-renderUI({
  box.y<-if (!is.null(query$box.y)){query$box.y}else{NULL}
  variable<-as.character(dsd[dsd$MemberType=='variable',]$MemberCode)
    selectInput(inputId = ns('y'), label = "Select y-axis Variable:",choices = variable, selected=box.y)
}) 
  
output$time<-renderUI({
  box.z <- if (!is.null(query$box.z)){query$box.z}else{NULL}
  attribute<-setdiff(as.character(dsd[dsd$MemberType=='attribute',]$MemberCode),c("geometry","aggregation_method"))
    selectInput(inputId = ns('z'), label = "Select Time Variable:", choices = attribute, selected = box.z)
})
  
output$slider<-renderUI({
  df<-as.data.frame(data)
  tmp<-subset(df,select=input$z)
    sliderInput(inputId = ns('s'), label = "Choose Period:", min=min(tmp),max=max(tmp),value = c(min(tmp),max(tmp)),step=1,sep="")
})  

output$split<-renderUI({
  df<-as.data.frame(data)  
  choice<-unique(subset(df,select=input$x))
  selectInput(inputId = ns('n'), label = "Selection elements to show:", choices = choice,selected=choice[1,1],multiple = TRUE)
})

output$plot <- renderPlotly({
  df<-as.data.frame(data)
  box.caption <-if (!is.null(query$box.caption)){query$box.caption}else{NULL}      
        
  df <- df %>%
        select(!!! syms(c(input$x,input$y,input$z))) %>%
        filter(!! sym(input$z) %in% seq(min(input$s),max(input$s),1)) %>%
        filter(!! sym(input$x) %in% input$n) %>%
        stats::setNames(c("attr_name","var_sum","time")) %>%
        ungroup()
  
  fig <- plot_ly(df,
                  x = ~attr_name, 
                  y = ~var_sum,
                  height = 250,
                  type = 'box'
        )
  fig <- fig %>%  layout(
          title = box.caption,
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
fig
})

}
####