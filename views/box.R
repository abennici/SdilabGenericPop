#' @title Box Module
#' @description 
#' This module create a tab inside 'Sdilab Generic Pop' app including a dynamic box plot chart.
#' It include a parameter box for dynamically select the attribute to display, the value and the time period
#' @param data subset datatable produced by 'DataConfig module'.  
#' @param dsd metadata list produced by 'QueryInfo module' and including columns information.
#' @param query$box.title mandatory url parameter, give a title to the tabs. 
#' @param query$box.info optional url parameter, allow to add a user guideline or description of tab inside info bubble.
#' @param query$box.x optional url parameter, select the default column to attribute. Must be an exact column name of data. If NULL the first column name of data is choosed.   
#' @param query$box.y optional url parameter, select the default column to value. Must be an exact column name of data. If NULL the first column name of data is choosed.
#' @param query$box.z optional url parameter, select the default column to display as time period. Must be an exact column name of data. If NULL the first column name of data is choosed.
#' @param query$box.caption optional parameter, allow to add a caption of plot. 
#' @author Alexandre Bennici, \email{bennicialexandre@@gmail.com}
#' 
# Function for module UI
box_ui <- function(id) {
  ns <- NS(id)

  tabPanel(title=uiOutput(ns("title_panel")),value="box",
    fluidRow(
      column(1,offset=10, uiOutput(ns("info")))
    ),
    fluidRow(
      #actionLink required to set button-like action
      actionLink(ns("toggleButton"), "Show parameters", style = "padding-left:15px;") 
    ),
    fluidRow(
      #combined UI for selector (managed server-side)  
      uiOutput(ns("selector"))
    ),
    fluidRow(
      div(plotlyOutput(ns('plot'))%>%withSpinner(type = 2),  style = "font-size:80%")
    )
  )

}

# Function for module server
box_server <- function(input, output, session,data,dsd,query) {
  ns <- session$ns 
  
  #we create a reactiveValues controller to store all variables that are input to plot
  out <-reactiveValues(
    data_to_display = as.data.frame(data),
    param_x = NULL,
    param_y = NULL,
    param_s = NULL,
    param_z = NULL,
    param_n = NULL
  )
  
  #fill reactive if param values (required after for plotting)
  observe({
    out$info <- if (!is.null(query$pie.info)){query$pie.info}else{NULL}
    out$caption <- if (!is.null(query$pie.caption)){query$pie.caption}else{NULL}
    out$param_x <-if (!is.null(query$pie.x)){query$pie.x} else {NULL}
    out$param_y <-if (!is.null(query$pie.y)){query$pie.y}else{NULL}
    out$param_z <- if (!is.null(query$pie.z)){query$pie.z}else{NULL}
    
  })

  output$title_panel <- renderText({
    if (!is.null(query$box.title)){query$box.title}else{"Box"}
  })
    
  output$info <-renderUI({
    if(!is.null(out$info)){
      circleButton(ns("info"),icon = icon("info-circle"),size='xs')
    }else{NULL}
  })
  
  observeEvent(input$info, {
    showModal(modalDialog(
      if (!is.null(out$info)){out$info}else{NULL}
    ))
  })
  
  ###CHECKED
  #UI for selector
  output$selector <- renderUI({
    
    attribute<-setdiff(as.character(dsd[dsd$MemberType=='attribute',]$MemberCode),c("geometry","aggregation_method"))
    variable<-as.character(dsd[dsd$MemberType=='variable',]$MemberCode)
    period <- list(
      min = min(out$data_to_display[,out$param_z]), 
      max = max(out$data_to_display[,out$param_z])
    )
    out$param_s <- c(period$min, period$max)
    
    nb_unique<-nrow(unique(subset(out$data_to_display,select=out$param_z)))
    nb_select<-ifelse(nb_unique>=10,10,nb_unique)
    choice<-unique(subset(out$data_to_display,select=out$param_x))
    
    tags$div(
      id = ns("selector_form"), width=12,style = "display:none",
      selectInput(inputId = ns('x'), label = "Select x-axis Variable:", choices = attribute, selected = out$param_x),
      selectInput(inputId = ns('y'), label = "Select y-axis Variable:", choices =variable, selected= out$param_y),
      selectInput(inputId = ns('z'), label = "Select Time Variable:", choices = attribute, selected = out$param_z),
      sliderInput(inputId = ns('s'), label = "Choose Period:", min=period$min, max=period$max,value = c(period$min, period$max),step=1,sep=""),
      selectInput(inputId = ns('n'), label = "Selection elements to show:", choices = choice,selected=choice[1,1],multiple = TRUE)
      
    )
    
  })
    
  #UI for toggle actionLink
  observeEvent(input$toggleButton,{
    if (input$toggleButton %% 2 == 1) {
      print("Action: Show parameters")
      txt <- "Hide parameters"
      shinyjs::addClass(selector = paste0("#",ns("selector_form")), class = "show")
    } else {
      print("Action: Hide parameters")
      txt <- "Show parameters"
      toggle(ns("selector_form"))
      shinyjs::removeClass(selector = paste0("#",ns("selector_form")), class = "show")
    }
    updateActionButton(session, "toggleButton", label = txt)
  })
  
  #fill reactive values with input values from selector (from UI, required after for ploting)
  observeEvent(input$x,{ out$param_x <- input$x })
  observeEvent(input$y,{ out$param_y <- input$y })
  observeEvent(input$z,{ out$param_z <- input$z })
  observeEvent(input$s,{ out$param_s <- input$s })
  observeEvent(input$n,{ out$param_n <- input$n })
  
  output$plot <- renderPlotly({
    df<-out$data_to_display     
          
    df <- df %>%
          select(!!! syms(c(out$param_x,out$param_y,out$param_z))) %>%
          filter(!! sym(out$param_z) %in% seq(min(out$param_s),max(out$param_s),1)) %>%
          filter(!! sym(out$param_x) %in% out$param_n) %>%
          stats::setNames(c("attr_name","var_sum","time")) %>%
          ungroup()
    
    fig <- plot_ly(df,
                    x = ~attr_name, 
                    y = ~var_sum,
                    height = 250,
                    type = 'box'
          )
    fig <- fig %>%  layout(
            title = out$caption,
            xaxis = list(
              titlefont = list(size = 10), 
              tickfont = list(size = 10),
              title = out$param_x,
              zeroline = F
            ),
            yaxis = list(
              titlefont = list(size = 10), 
              tickfont = list(size = 10),
              title = out$param_y,
              zeroline = F
            ))
  fig
  })

}
####