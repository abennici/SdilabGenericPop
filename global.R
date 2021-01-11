#---------------------------------------------------------------------------------------------------------
#packages
library("ows4R")
library("sp")
library('readr')
library("shiny")
library("dplyr")
library("plotly")
library("DT")
library("shinyWidgets")
library("shinycssloaders")
library("jsonlite")
library("stringr")

#load module functions
source("https://raw.githubusercontent.com/eblondel/OpenFairViewer/master/src/resources/shinyModule/QueryInfo.R")
source("settings/QueryData.R")
source("settings/Params.R")
source("views/FlagName.R")
source("views/Line.R")
source("views/Pie.R")
source("views/Box.R")
source("views/DataTable.R")
source("ui.R")
source("server.R")

