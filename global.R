#---------------------------------------------------------------------------------------------------------
#packages

library("shiny")
library("shinyjs")
library("shinydashboard")
library("shinyWidgets")
library("shinycssloaders")
library("ows4R")
library("sp")
library('readr')
library("dplyr")
library("plotly")
library("DT")
library("jsonlite")
library("stringr")

#Commons

# Options for Spinner
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=1)

#load module functions
source("https://raw.githubusercontent.com/eblondel/OpenFairViewer/master/src/resources/shinyModule/QueryInfo.R")
source("settings/QueryData.R")
source("views/FlagName.R")
source("ui.R")
source("server.R")

