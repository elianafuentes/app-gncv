FROM rocker/shiny:4.4.2

RUN apt-get update -qq && apt-get install -y --no-install-recommends \
  libudunits2-dev libgdal-dev libgeos-dev libproj-dev libcurl4-openssl-dev \
  && rm -rf /var/lib/apt/lists/*

RUN R -q -e 'install.packages(c("shiny","shinyWidgets","bslib","sf","readr","dplyr","stringr","leaflet","plotly","lubridate","forcats","scales","ggplot2","markdown"), repos="https://cloud.r-project.org")'

COPY . /srv/shiny-server/app
RUN chown -R shiny:shiny /srv/shiny-server

EXPOSE 3838
