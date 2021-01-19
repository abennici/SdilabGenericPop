###Pie Module
# Function for module UI
pie_ui <- function(id) {
  ns <- NS(id)

tabPanel(title=uiOutput(ns("title_panel")),value="pie", 
  fluidRow(
    column(1,offset=10, uiOutput(ns("info")))
  ),    
  fluidRow(
    box(width=12, collapsible=T,collapsed=F,
      uiOutput(ns("x")),
      uiOutput(ns("y")),
      uiOutput(ns("time")),               
      uiOutput(ns("slider"))
    )
  ),
  fluidRow(
    div(plotlyOutput(ns('pie'))%>%withSpinner(type = 2),  style = "font-size:80%")
  )
)
  
}

# Function for module server
pie_server <- function(input, output, session,data,dsd,query) {
  ns <-session$ns

output$title_panel <- renderText({
  if (!is.null(query$pie.title)){query$pie.title}else{"Pie"}
})

output$info <-renderUI({
  circleButton(ns("info"),icon = icon("info-circle"),size='xs')
})

observeEvent(input$info, {
  showModal(modalDialog(
    if (!is.null(query$pie.info)){query$pie.info}else{NULL}
  ))
})
 
output$x<-renderUI({
  pie.x <- if (!is.null(query$pie.x)){query$pie.x} else {NULL}
  attribute<-setdiff(as.character(dsd[dsd$MemberType=='attribute',]$MemberCode),c("geometry","aggregation_method"))
    selectInput(inputId = ns('x'), label = "Select Attribute:", choices = attribute, selected = pie.x)
}) 

output$y<-renderUI({
  pie.y<-if (!is.null(query$pie.y)){query$pie.y}else{NULL}
  variable<-as.character(dsd[dsd$MemberType=='variable',]$MemberCode)
    selectInput(inputId = ns('y'), label = "Select Variable:",choices = variable,selected=pie.y)
}) 


output$time<-renderUI({
  pie.z<-if (!is.null(query$pie.z)){query$pie.z}else{NULL}
  attribute<-setdiff(as.character(dsd[dsd$MemberType=='attribute',]$MemberCode),c("geometry","aggregation_method"))
    selectInput(inputId = ns('z'), label = "Select Time Variable:", choices = attribute, selected = pie.z)
})

output$slider<-renderUI({
  df<-as.data.frame(data)
  tmp<-subset(df,select=input$z)
    sliderInput(inputId = ns('s'), label = "Choose Period:", min=min(tmp),max=max(tmp),value = c(min(tmp),max(tmp)),step=1,sep="")
})    
  
output$pie <- renderPlotly({
  df<-as.data.frame(data)
  pie.caption <-if (!is.null(query$pie.caption)){query$pie.caption}else{NULL}
  
  df <- df %>%
      filter(!! sym(input$z) %in% seq(min(input$s),max(input$s),1)) %>%
      group_by(!! sym(c(input$x))) %>% 
      summarise(sum = sum(!! sym(input$y))) %>%
      stats::setNames(c("attr_name","var_sum"))

  fig <- plot_ly(df, labels = ~as.factor(attr_name), values = ~var_sum,textinfo = 'none')
  fig <- fig %>% add_pie(hole = 0.6)
  fig <- fig %>% layout(title = pie.caption,
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        showlegend = FALSE)    
    
  fig
})
    
}