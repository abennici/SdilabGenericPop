###Module
# Function for module UI
PieUI <- function(id) {
  ns <- NS(id)
  # Options for Spinner
  options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=1)
  tabPanel("Pie", 
    fluidRow(
     column(6,selectInput(inputId = ns('x'), label = "Select Attribute:",choices = NULL)),               
     column(6,selectInput(inputId = ns('y'), label = "Select Variable:",choices = NULL))
    ),
    fluidRow(
      column(6,uiOutput(ns("time"))),               
      column(6,uiOutput(ns("slider")))
    ),
    div(plotlyOutput(ns('pie'))%>%withSpinner(type = 2),  style = "font-size:80%"))
  
}

# Function for module server logic
Pie <- function(input, output, session,data,dsd,pie.x,pie.y,pie.z,pie.title) {
ns <-session$ns
 
observe({
  dsd <- dsd()
  pie.x<-pie.x()
  attribute<-setdiff(as.character(dsd[dsd$MemberType=='attribute',]$MemberCode),c("geometry","aggregation_method"))
  updateSelectInput(session, 'x', choices = attribute, selected = pie.x)
}) 

xVarName <- reactive({
  input$x
}) 

observe({
  dsd <- dsd()
  pie.y<-pie.y()
  variable<-as.character(dsd[dsd$MemberType=='variable',]$MemberCode)
  updateSelectInput(session, 'y', choices = variable,selected=pie.y)
}) 

yVarName <- reactive({
  input$y
})

observe({
output$time<-renderUI({
  dsd<-dsd()
  pie.z<-pie.z()
  attribute<-setdiff(as.character(dsd[dsd$MemberType=='attribute',]$MemberCode),c("geometry","aggregation_method"))
  selectInput(inputId = ns('z'), label = "Select Time Variable:", choices = attribute, selected = pie.z)
})
})

zVarName <- reactive({
  input$z
})

observe({
  
  output$slider<-renderUI({
    df<-as.data.frame(data())
    tmp<-subset(df,select=input$z)
    sliderInput(inputId = ns('s'), label = "Choose Period:", min=min(tmp),max=max(tmp),value = c(min(tmp),max(tmp)),step=1,sep="")
  })    
  
})

observe({
output$pie <- renderPlotly({
  df<-as.data.frame(data())

      df <- df %>%
      filter(!! sym(input$z) %in% seq(min(input$s),max(input$s),1)) %>%
      group_by(!! sym(c(input$x))) %>% 
      summarise(sum = sum(!! sym(input$y)))
    
    names(df)<-c("attr_name","var_sum")
    
    fig <- plot_ly(df, labels = ~as.factor(attr_name), values = ~var_sum,textinfo = 'none')
    fig <- fig %>% add_pie(hole = 0.6)
    fig <- fig %>% layout(title = pie.title(),
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          showlegend = FALSE)    
    
    fig
    })
})    
}