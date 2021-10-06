FROM rocker/r-ver:3.6.3

MAINTAINER Alexandre Bennici "bennicialexandre@gmail.com"

# system libraries of general use

RUN apt-get update && apt-get install -y \
    sudo \
    libudunits2-dev \
    libproj-dev \
    libgeos-dev \
    libgdal-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    git 
   
RUN apt-get update && apt-get upgrade -y

# install dependencies of the app

RUN R -e "install.packages(c('devtools'), repos='https://cran.r-project.org/')"
RUN R -e "devtools::install_version('XML', version='3.99-0.3', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('shiny', version='1.5.0', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('shinyjs', version='1.1', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('shinydashboard', version='0.7.1', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('shinyWidgets', version='0.5.3', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('lubridate', version='1.7.9', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('dplyr', version='1.0.0', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('plotly', version='4.9.2.1', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('readr', version='1.3.1', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('jsonlite', version='1.7.1', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('stringr', version='1.4.0', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('DT', version='0.17', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('reshape', version='0.8.8', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('base64', version='2.0', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('sp', version='1.4-2', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('sf', version='0.9-4', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('shinycssloaders', version='1.0.0', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('shinybusy', version='0.2.1', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('tidyr', version='1.1.0', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('geojson', version='0.3.4', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('osmdata', version='0.1.6', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_github('eblondel/ows4R')"

RUN git -C /root/ clone https://github.com/abennici/SdilabGenericPop.git && echo "OK!"
RUN mkdir -p /srv/shiny/
RUN ln -s /root/SdilabGenericPop /srv/shiny/SdilabGenericPop
 
EXPOSE 3838

RUN apt-get install -y curl
CMD ["R", "-e shiny::runApp('/srv/shiny/SdilabGenericPop',port=3838,host='0.0.0.0')"]
#CMD ["R", "-e shiny::runApp('/srv/shiny/SdilabGenericPop')"]