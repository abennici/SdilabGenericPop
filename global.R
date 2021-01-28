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
library("base64")

#Commons

# Options for Spinner
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=1)

#load module functions
source("https://raw.githubusercontent.com/eblondel/OpenFairViewer/master/dist/resources/shinyModule/QueryInfo.R")
source("settings/DataConfig.R")
source("settings/FlagName.R")
source("ui.R")
source("server.R")

