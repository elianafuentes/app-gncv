FROM --platform=linux/amd64 rocker/geospatial:4.4.2

# Paquetes R (rápidos con install2.r que trae rocker)
RUN install2.r --error \
  shiny shinyWidgets bslib readr dplyr stringr leaflet plotly \
  lubridate forcats scales ggplot2 markdown

# Copia tu app
COPY . /srv/shiny-server/app
RUN chown -R shiny:shiny /srv/shiny-server

# Configura Shiny Server para servir tu app en "/"
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf


EXPOSE 3838