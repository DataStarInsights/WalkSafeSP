
###########################################################################

# dt_all <- openxlsx::read.xlsx(xlsxFile = "CelularesSubtraidos_2023.xlsx",
#                           sheet = "CELULAR_2023",
#                           detectDates = TRUE)
# 
# dt <- data.table::as.data.table(dt_all)
# 
# dt <- dt[, .(
#          NOME_MUNICIPIO,
#          ANO_BO,
#          DATA_OCORRENCIA_BO,
#          HORA_OCORRENCIA,
#          DESCR_TIPOLOCAL,
#          DESCR_SUBTIPOLOCAL,
#          LOGRADOURO,
#          NUMERO_LOGRADOURO,
#          LATITUDE,
#          LONGITUDE
#          )]
# 
# saveRDS(dt, file = "dt.RDS")

###########################################################################

library(magrittr)
library(plotly)
library(data.table)
library(leaflet)
library(leaflet.extras)
library(DT)


# Mobile phone robbery occurences 

dt <- readRDS("dt.RDS")

dt$DATA_OCORRENCIA_BO <- as.Date(dt$DATA_OCORRENCIA_BO)

dt$NOME_MUNICIPIO[grep(x = tolower(dt$NOME_MUNICIPIO),
                       pattern = "s.paulo")] <- "SÃO PAULO"

dt <- dt[NOME_MUNICIPIO == "SÃO PAULO", ]

dt$LATITUDE <- as.numeric(dt$LATITUDE)
dt$LONGITUDE <- as.numeric(dt$LONGITUDE)

Amelia::missmap(dt[, .(LATITUDE, LONGITUDE)])

dt <- dt[dt$LATITUDE < 0 & dt$LONGITUDE < 0, ]

plot(y = dt$LATITUDE, x = dt$LONGITUDE)


# District polygons

dt_poly <- geobr::read_neighborhood()
dt_poly <- dt_poly[dt_poly$name_muni == "São Paulo", ]


# Occurences per district

occurrences <- c()
for (i in 1:nrow(dt_poly)) {
  poly_coor <- data.table::data.table(dt_poly$geom[i][[1]][[1]][[1]])
  
  n_i <- 0
  for (j in 1:nrow(dt)) {
    n_i <- n_i + sp::point.in.polygon(point.x = dt$LONGITUDE[j],
                                      point.y = dt$LATITUDE[j],
                                      pol.x = poly_coor$V1,
                                      pol.y = poly_coor$V2)
  }
  
  occurrences <- c(occurrences, n_i)
  
  print(paste0(100*i/nrow(dt_poly), " %"))
}

dt_poly$occurrences <- occurrences

# Convert the CRS to WGS84

dt_poly <- sf::st_transform(dt_poly, 4326)  # 4326 is the EPSG code for WGS84

saveRDS(dt_poly, "dt_poly.RDS")


dt_poly <- readRDS("dt_poly.RDS")


# Create a leaflet map

bins <- seq(from = min(dt_poly$occurrences),
            to = max(dt_poly$occurrences) + 101,
            by = 100)
pal <- colorBin("YlOrRd", domain = dt_poly$occurrences, bins = bins)
labels <- sprintf(
  "<strong>%s</strong><br/>%g assaltos</sup>",
  dt_poly$name_district, dt_poly$occurrences
) %>% lapply(htmltools::HTML)


leaflet(dt_poly) %>%
  addTiles(
    options = providerTileOptions(opacity = 0.8)
  ) %>%
  setView(lat = -23.550520, lng = -46.6333085, zoom = 11) %>%
  addPolygons(
    fillColor = ~pal(occurrences),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.6,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = labels,
    labelOptions = labelOptions(
      style = list(
        "font-weight" = "normal",
        padding = "3px 8px"
      ),
      textsize = "15px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    "bottomright",
    pal = colorNumeric("YlOrRd", domain = dt_poly$occurrences),
    values = dt_poly$occurrences,
    title = "Occurrences",
    opacity = 1
  )











