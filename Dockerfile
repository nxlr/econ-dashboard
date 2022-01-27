# This can change with the whatever version of R the app was built
# sets the base image and OS on which the entire computer will be built
FROM rocker/shiny:4.1.2

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

## required libraries
RUN apt-get install build-essential -y \
    libxml2 \
    libglpk40

## Pre-install packages creating issues
RUN R -e "install.packages(c('igraph', 'ggdag'), dependencies = T)"

## add any R packages here 
RUN install2.r tidyverse\
    shiny \ 
    dplyr \ 
    plotly \
    ggthemes \
    thematic \
    showtext \
    readxl \
    tidyr \
    ggstream \
    viridis \
    hrbrthemes \
    reshape2 \
    highcharter \
    bs4Dash \
    shinyWidgets \
    waiter \
    shinyalert \
    DT \
    RColorBrewer

## clean up
RUN rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# The /app and /srv/shiny-server/ are two different arguments that relate to your local file and the 
# file path in the container; not to be read as one string. 
# Copy files from the app folder /app into the docker image file location /srv/shiny-server/ 
COPY /app /srv/shiny-server/

# Exposing port to listen on. This was needed when running on an indivdual VM as opposed to an app service plan
# so when the docker starts port 3838 is exposed. 
EXPOSE 3838

#USER shiny
