#' @title Donut Module
#' @description 
#' This module create a tab inside 'Sdilab Generic Pop' app including a dynamic donuts chart.
#' It include a parameter box for dynamically select the attribute to display, the value and the time period
#' @param data subset datatable produced by 'QueryData module'.  
#' @param dsd metadata list produced by 'QueryInfo module' and including columns information.
#' @param query$donut.title mandatory url parameter, give a title to the tabs. 
#' @param query$donut.info optional url parameter, allow to add a user guideline or description of tab inside info bubble.
#' @param query$donut.x optional url parameter, select the default column to xxx. Must be an exact column name of data. If NULL the first column name of data is choosed.   
#' @param query$donut.y optional url parameter, select the default column to xxx. Must be an exact column name of data. If NULL the first column name of data is choosed.
#' @param query$donut.z optional url parameter, select the default column to xxx. Must be an exact column name of data. If NULL the first column name of data is choosed.
#' @param query$donut.caption optional parameter, allow to add a caption of plot. 
#' @author Alexandre Bennici, \email{bennicialexandre@@gmail.com}

# Function for module UI
donut_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(title=uiOutput(ns("title_panel")),value="donut", 
           fluidRow(
             column(1,offset=10, uiOutput(ns("info")))
           ), 
           fluidRow(
             div(plotlyOutput(ns('donut'))%>%withSpinner(type = 2),  style = "font-size:80%")
           ),
           fluidRow(
             box(width=12, collapsible=T,collapsed=F,
                 uiOutput(ns("x")),
                 uiOutput(ns("y")),
                 uiOutput(ns("time")),               
                 uiOutput(ns("slider"))
             )
           )
  )
  
}

# Function for module server
donut_server <- function(input, output, session,data,dsd,query) {
  ns <-session$ns
  
  output$title_panel <- renderText({
    if (!is.null(query$donut.title)){query$donut.title}else{"Donut"}
  })
  
  output$info <-renderUI({
    circleButton(ns("info"),icon = icon("info-circle"),size='xs')
  })
  
  observeEvent(input$info, {
    showModal(modalDialog(
      if (!is.null(query$donut.info)){query$donut.info}else{NULL}
    ))
  })
  
  output$x<-renderUI({
    donut.x <- if (!is.null(query$donut.x)){query$donut.x} else {NULL}
    attribute<-setdiff(as.character(dsd[dsd$MemberType=='attribute',]$MemberCode),c("geometry","aggregation_method"))
    selectInput(inputId = ns('x'), label = "Select Attribute:", choices = attribute, selected = donut.x)
  }) 
  
  output$y<-renderUI({
    donut.y<-if (!is.null(query$donut.y)){query$donut.y}else{NULL}
    variable<-as.character(dsd[dsd$MemberType=='variable',]$MemberCode)
    selectInput(inputId = ns('y'), label = "Select Variable:",choices = variable,selected=donut.y)
  }) 
  
  
  output$time<-renderUI({
    donut.z<-if (!is.null(query$donut.z)){query$donut.z}else{NULL}
    attribute<-setdiff(as.character(dsd[dsd$MemberType=='attribute',]$MemberCode),c("geometry","aggregation_method"))
    selectInput(inputId = ns('z'), label = "Select Time Variable:", choices = attribute, selected = donut.z)
  })
  
  output$slider<-renderUI({
    df<-as.data.frame(data)
    tmp<-subset(df,select=input$z)
    sliderInput(inputId = ns('s'), label = "Choose Period:", min=min(tmp),max=max(tmp),value = c(min(tmp),max(tmp)),step=1,sep="")
  })    
  
  output$donut <- renderPlotly({
    df<-as.data.frame(data)
    donut.caption <-if (!is.null(query$donut.caption)){query$donut.caption}else{NULL}
    
    df <- df %>%
      filter(!! sym(input$z) %in% seq(min(input$s),max(input$s),1)) %>%
      group_by(!! sym(c(input$x))) %>% 
      summarise(sum = sum(!! sym(input$y))) %>%
      stats::setNames(c("attr_name","var_sum"))
    
    fig <- plot_ly(df, labels = ~as.factor(attr_name), values = ~var_sum,textinfo = 'none')
    fig <- fig %>% add_pie(hole = 0.6)
    fig <- fig %>% layout(title = donut.caption,
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          showlegend = FALSE)    
    
    fig
  })
  
}