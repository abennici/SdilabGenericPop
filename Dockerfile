FROM rocker/r-ver:3.6.3

MAINTAINER Alexandre Bennici "bennicialexandre@gmail.com"


# system libraries of general use
  # mainly for installing sf (which requires units/rgeos/rgdal)


RUN apt-get update && apt-get install -y \
    sudo \
    libudunits2-dev \
    libproj-dev \
    libgeos-dev \
    libgdal-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2 \
    libxml2-dev \
    git 
   
  RUN apt-get update && apt-get upgrade -y

# install dependencies of the Fao Capture popup app
RUN R -e "install.packages(c('devtools'), repos='https://cran.r-project.org/')"
RUN R -e "devtools::install_version('XML', version='3.99-0.3', repos = 'http://cran.r-project.org')"
RUN R -e "install.packages(c('R6','readr','shinycssloaders','stringr','ggplot2', 'httr','plotly','dplyr','sp','sf','rgdal','geometa','shiny','DT','shinyWidgets','jsonlite','remotes'), repos='http://cran.r-project.org')"

RUN R -e "remotes::install_github('eblondel/ows4R')"
#RUN R -e "remotes::install_github('daattali/shinycssloaders')"

RUN git -C /root/ clone https://github.com/abennici/SdilabGenericPop.git && echo "OK!"
RUN mkdir -p /srv/shiny/
RUN ln -s /root/SdilabGenericPop /srv/shiny/SdilabGenericPop
 
EXPOSE 3838

RUN apt-get install -y curl
CMD ["R", "-e shiny::runApp('/srv/shiny/SdilabGenericPop',port=3838,host='0.0.0.0')"]
#CMD ["R", "-e shiny::runApp('/srv/shiny/SdilabGenericPop')"]