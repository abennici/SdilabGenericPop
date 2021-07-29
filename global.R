#---------------------------------------------------------------------------------------------------------
#packages

library("shiny")
library("shinyjs")
library("shinydashboard")
library("shinyWidgets")
library("shinycssloaders")
library("ows4R")
library("sp")
library("sf")
library('readr')
library("dplyr")
library("plotly")
library("DT")
library("jsonlite")
library("stringr")
library("base64")
library("lubridate")
library("reshape")
library('tidyr')
#Commons

# Options for Spinner
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=1,stringsAsFactors = FALSE)

#load module functions
#source("https://raw.githubusercontent.com/eblondel/OpenFairViewer/master/src/resources/shinyModule/QueryInfo.R")
source("D:/FAO-BLUECLOUD_04052020_11082020/02-R/04-Github/OpenFairViewer/src/resources/shinyModule/QueryInfo.R")
source("settings/DataConfig.R")
source("settings/FlagName.R")
source("ui.R")
source("server.R")

