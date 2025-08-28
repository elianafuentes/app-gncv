# GNCV Colombia — Shiny + Leaflet + Docker

App Shiny para explorar precios de Gas Natural Comprimido Vehicular (GNCV) en Colombia por departamento: **mapa choropleth**, **series temporales**, **Top-N** y **filtros**.

![demo](docs/screenshot.png)

## Quickstart

### Docker (recomendado)
```bash
docker build -t gncv-shiny .
docker run --rm -p 3838:3838 gncv-shiny
# abre http://localhost:3838