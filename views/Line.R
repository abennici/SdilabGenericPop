#' @title Line Module
#' @description 
#' This module create a tab inside 'Sdilab Generic Pop' app including a dynamic line plot for time serie views.
#' It include a parameter box for dynamically select the x and y axis, the time period, 
#' a cumulate or slit by element views and the number of most present elements to show.
#' @param data subset datatable produced by 'QueryData module'.  
#' @param dsd metadata list produced by 'QueryInfo module' and including columns information.
#' @param query$line.title mandatory url parameter, give a title to the tabs. 
#' @param query$line.info optional url parameter, allow to add a user guideline or description of tab inside info bubble.
#' @param query$line.x optional url parameter, select the default column to the x axis of the plot. Must be an exact column name of data. If NULL the first column name of data is choosed.   
#' @param query$line.y optional url parameter, select the default column to the y axis of the plot. Must be an exact column name of data. If NULL the first column name of data is choosed.
#' @param query$line.z optional url parameter, select the default column to the split viewing if 'yes' is choose for 'Split by another column:' argument. Must be an exact column name of data. If NULL the first column name of data is choosed.
#' @param query$line.caption optional parameter, allow to add a caption of plot. 
#' @author Alexandre Bennici, \email{bennicialexandre@@gmail.com}
#' 
###Line Module
# Function for module UI
line_ui <- function(id) {
  ns <- NS(id)

  tabPanel(title=uiOutput(ns("title_panel")),value="line",
    fluidRow(
      #actionLink required to set button-like action
      actionLink(ns("toggleButton"), "Show parameters", style = "padding-left:15px;") 
    ),
    fluidRow(
      #combined UI for selector (managed server-side)  
      uiOutput(ns("selector"))
    ),
    fluidRow(
      div(plotlyOutput(ns('plot'),height="250px")%>%withSpinner(type = 2),  style = "font-size:80%")
    )
  )

}

# Function for module server
line_server <- function(input, output, session,data,dsd,query) {
  
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
  
  output$title_panel <- renderText({
    if (!is.null(query$line.title)){query$line.title}else{"Line"}
  })
  
  #fill reactive if param values (required after for plotting)
  observe({
    out$caption <- if (!is.null(query$line.caption)){query$line.caption}else{NULL}
    out$param_x<-if (!is.null(query$line.x)){query$line.x} else {NULL}
    out$param_y<-if (!is.null(query$line.y)){query$line.y}else{NULL}
  })

  #UI for selector
  output$selector <- renderUI({
    
    attribute<-setdiff(as.character(dsd[dsd$MemberType=='attribute',]$MemberCode),c("geometry","aggregation_method"))
    variable<-as.character(dsd[dsd$MemberType=='variable',]$MemberCode)
    period <- list(
      min = min(out$data_to_display[,out$param_x]), 
      max = max(out$data_to_display[,out$param_x])
    )
    out$param_s <- c(period$min, period$max)
    
    tags$div(
      id = "selector_form", width=12,
      selectInput(inputId = ns('x'), label = "Select x-axis Variable:", choices = attribute, selected = out$param_x),
      selectInput(inputId = ns('y'), label = "Select y-axis Variable:", choices =variable, selected= out$param_y),
      sliderInput(inputId = ns('s'), label = "Choose Period:", min=period$min, max=period$max,value = c(period$min, period$max),step=1,sep=""),
      radioButtons(ns("SplitByColumn"),"Split by another column:",choices = c("no", "yes"),selected = "no",inline = TRUE),
      tags$div(inputId = ns('z')),
      tags$div(inputId = ns('n'))
    )
  
  })
  
  #UI for toggle actionLink
  observeEvent(input$toggleButton,{
    if (input$toggleButton %% 2 == 1) {
      print("Action: Show parameters")
      txt <- "Hide parameters"
      shinyjs::addClass(selector = paste0("#selector_form"), class = "show")
    } else {
      print("Action: Hide parameters")
      txt <- "Show parameters"
      toggle("selector_form")
      shinyjs::removeClass(selector = paste0("#selector_form"), class = "show")
    }
    updateActionButton(session, "toggleButton", label = txt)
  })

  #fill reactive values with input values from selector (from UI, required after for ploting)
  observeEvent(input$x,{ out$param_x <- input$x })
  observeEvent(input$y,{ out$param_y <- input$y })
  observeEvent(input$s,{ out$param_s <- input$s })
  observeEvent(input$z,{ out$param_z <- input$z })
  observeEvent(input$n,{ out$param_n <- input$n })

  #observe for ploting
  #plots are based on reactive values
  observe({
    
    default_rendering <- is.null(input$SplitByColumn)
    if(!is.null(input$SplitByColumn)) default_rendering <- ifelse(input$SplitByColumn == "no", TRUE, FALSE)
    
    if(default_rendering){
      #default rendering
      df<-as.data.frame(data)
      
      output$colorColumn<-renderUI({})
      
      output$number<-renderUI({})
      
      output$plot <- renderPlotly({
        
        df <- df %>%
          filter(!! sym(out$param_x) %in% seq(min(out$param_s),max(out$param_s),1)) %>%
          group_by(!! sym(out$param_x)) %>% 
          summarise(var_sum = sum(!! sym(out$param_y)))%>%
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
          text = ~paste(out$param_x,": ", attr_name, "<br>",out$param_y,": ", var_sum), 
          hoverinfo = 'text'
        )
        
        fig <- fig %>% layout(
          title = out$caption,
          yaxis = list(title = out$param_y, range = c(0,max(df$var_sum)+25*max(df$var_sum)/100), zeroline = F),
          xaxis = list( title = out$param_x, zeroline = F)
        ) 
        fig
      })
    }else{ 
      #render with split by column
      df<-as.data.frame(data)
      
      output$colorColumn<-renderUI({
        line.z<-if (!is.null(query$line.z)){query$line.z}else{NULL}
        attribute<-setdiff(as.character(dsd[dsd$MemberType=='attribute',]$MemberCode),c("geometry","aggregation_method"))
        selectInput(inputId = ns('z'), label = "Select Color Variable:", choices = attribute, selected = line.z)
      })
      
      output$number<-renderUI({
        nb_unique<-nrow(unique(subset(df,select=out$param_z)))
        nb_select<-ifelse(nb_unique>=10,10,nb_unique)                
        selectInput(inputId = ns('n'), label = "Numbers of element:", choices = seq(1,nb_unique,by=1), selected = nb_select)
      })
      
      output$plot <- renderPlotly({
        
        df_rank<-df%>%
          group_by(!! sym(out$param_z))%>%
          summarise(sum = sum(!! sym(out$param_y)))%>%
          mutate(rank = rank(-sum))%>%
          stats::setNames(c("gr_name","sum","rank"))%>%
          filter(rank <= as.numeric(out$param_n))
        
        df <- df %>%
          filter(!! sym(out$param_x) %in% seq(min(out$param_s),max(out$param_s),1)) %>%
          filter(!! sym(out$param_z) %in% df_rank$gr_name)%>%
          group_by(!!! syms(c(out$param_x,out$param_z)))%>%
          summarise(var_sum = sum(!! sym(out$param_y)))%>%
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
          text = ~paste(out$param_x,": ", attr_name, "<br>",out$param_z,": ", attr_col, "<br>",out$param_y,": ", var_sum), 
          hoverinfo = 'text'
        )
        
        fig <- fig %>% layout(
          title = out$caption,
          legend = list(
            orientation = "v",
            font = list(size = 10),
            bgcolor ='rgba(0,0,0,0)',
            title = out$param_z
          ),
          xaxis = list(
            titlefont = list(size = 10), 
            tickfont = list(size = 10),
            range = range(unique(df$attr_name)),
            title = out$param_x,
            zeroline = F
          ),
          yaxis = list(
            titlefont = list(size = 10), 
            tickfont = list(size = 10),
            range = c(0,max(df$var_sum)+25*max(df$var_sum)/100),
            title = out$param_y,
            zeroline = F
          )
        )
        
        fig
        
      })
    }
  }) 
      
}
####