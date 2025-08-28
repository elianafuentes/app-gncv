# syntax=docker/dockerfile:1
FROM --platform=linux/amd64 rocker/shiny:latest

# Configurar variables de entorno
ENV DEBIAN_FRONTEND=noninteractive
ENV SHINY_LOG_STDERR=1

# Instalar dependencias del sistema
RUN apt-get update && apt-get install -y --no-install-recommends \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libjpeg-dev \
    g++ \
    make \
    && rm -rf /var/lib/apt/lists/*

# Instalar paquetes de R
RUN install2.r --error --skipinstalled \
    sf \
    leaflet \
    plotly \
    shiny \
    shinyWidgets \
    bslib \
    readr \
    dplyr \
    stringr \
    lubridate \
    forcats \
    scales \
    ggplot2 \
    htmltools \
    markdown \
    DT \
    htmlwidgets

# Configurar directorio de trabajo
WORKDIR /srv/shiny-server

# Copiar archivos de la aplicación
COPY . /srv/shiny-server/

# Cambiar permisos
RUN chown -R shiny:shiny /srv/shiny-server && \
    chmod -R 755 /srv/shiny-server

# Crear directorio de logs si no existe
RUN mkdir -p /var/log/shiny-server && \
    chown shiny:shiny /var/log/shiny-server

# Exponer puerto
EXPOSE 3838

# Comando por defecto
CMD ["/usr/bin/shiny-server"]