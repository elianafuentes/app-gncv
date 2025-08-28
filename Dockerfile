FROM --platform=linux/amd64 rocker/shiny:4.4.2

# libs del sistema para sf/leaflet
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
    libudunits2-dev libgdal-dev libgeos-dev libproj-dev libcurl4-openssl-dev \
    && rm -rf /var/lib/apt/lists/*

# paquetes R (usa littler::install2.r que ya viene en las imágenes rocker)
RUN install2.r --error \
    shiny shinyWidgets bslib readr dplyr stringr leaflet plotly \
    lubridate forcats scales ggplot2 markdown sf

# copia tu app al lugar que sirve shiny-server
COPY . /srv/shiny-server/app
RUN chown -R shiny:shiny /srv/shiny-server

# (opcional) servir en la raíz "/" si agregaste este archivo
# si NO tienes shiny-server.conf, borra la siguiente línea
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

EXPOSE 3838
