
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
library(sf)


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
dt_districts <- geobr::read_neighborhood()
dt_districts <- dt_districts[dt_districts$name_muni == "São Paulo", ]

# Subdistrict polygons
dt_subdistrict <- sf::st_read("git_repo/WalkSafeSP/shapefiles/neighborhoods.shp")
st_crs(dt_subdistrict) <- st_crs(31983)
dt_subdistrict <- st_transform(dt_subdistrict, crs = 4326)
dt_subdistrict <- dt_subdistrict[dt_subdistrict$od_municip == "36", ]

dt_poly <- dt_subdistrict

# Occurences per subdistrict
occurrences <- c()
for (i in 1:nrow(dt_poly)) {
  poly_coor <- data.table::data.table(dt_poly$geom[i][[1]][[1]])
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

saveRDS(dt_poly, "dt_poly.RDS")
dt_poly <- readRDS("dt_poly.RDS")


# Occurences over poylgon area
dt_poly$od_area <- as.numeric(dt_poly$od_area)
dt_poly$occurrences_per_area <- dt_poly$occurrences/dt_poly$od_area

# New variables
dt_poly$rob_norm <- dt_poly$occurrences_per_area/max(dt_poly$occurrences_per_area)
dt_poly$rob_norm[dt_poly$rob_norm == 0] <- min(dt_poly$rob_norm[dt_poly$rob_norm > 0])
dt_poly$rob_log <- -log(dt_poly$rob_norm)
dt_poly$rob_log <- -dt_poly$rob_log




# Add layers

## Subway lines
dt_subway_lines <- sf::st_read("git_repo/WalkSafeSP/shapefiles/subway_lines.shp")
st_crs(dt_subway_lines) <- st_crs(31983)
dt_subway_lines <- st_transform(dt_subway_lines, crs = 4326)
dt_subway_lines$color <- c("#002CEF", "#00EF17", "#EF0000", "#EF00E0", "#C2C2C2", "#FEFA00")

## Subway stations
dt_subway_stations <- sf::st_read("git_repo/WalkSafeSP/shapefiles/subway_stations.shp")
st_crs(dt_subway_stations) <- st_crs(31983)
dt_subway_stations <- st_transform(dt_subway_stations, crs = 4326)
station_colors <- data.frame(
  color = c("#002CEF", "#00EF17", "#EF0000", "#EF00E0", "#C2C2C2", "#FEFA00"),
  color_name = c("AZUL", "VERDE", "VERMELHA", "LILAS", "PRATA", "AMARELA")
)
dt_subway_stations <- merge(dt_subway_stations, station_colors, 
                            by.x = "emt_linha",
                            by.y = "color_name")

## Slums
dt_slums <- sf::st_read("git_repo/WalkSafeSP/shapefiles/slums.shp")
st_crs(dt_slums) <- st_crs(31983)
dt_slums <- st_transform(dt_slums, crs = 4326)



# Create a leaflet map

x_name = "rob_log"
x_val <- dt_poly[[x_name]]
bins <- seq(from = min(x_val),
            to = max(x_val),
            length.out = 9)
palette_name = "BuPu"
pal <- colorBin(palette = palette_name,
                domain = x_val, 
                bins = bins)
labels <- sprintf(
  paste0("<strong>%s</strong><br/>%g</sup>"),
  dt_poly$od_nome, x_val
) %>% lapply(htmltools::HTML)


leaflet(dt_poly) %>%
  addTiles(
    options = providerTileOptions(opacity = 0.8)
  ) %>%
  setView(
    lat = -23.550520, 
    lng = -46.6333085, 
    zoom = 11
  ) %>%
  addPolygons(
    fillColor = ~pal(get(x_name)),
    weight = 0.5,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.8,
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
  addPolylines(
    data = dt_subway_lines,
    color = ~color,
    weight = 4
  ) %>%
  addCircleMarkers(
    data = dt_subway_stations,
    fillColor = ~color,
    radius = 3,
    fillOpacity = 0.8,
    weight = 1
  ) %>%
  addPolygons(
    data = dt_slums,
    fillColor = "#FF9D00",
    weight = 0.5,
    opacity = 1,
    color = "#FF9D00",
    fillOpacity = 0.7,
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
    pal = colorNumeric(palette_name, domain = x_val),
    values = x_val,
    title = x_name,
    opacity = 1,
    layerId = "legend"
  ) %>%
  addControl(
    html = "
    <div id='legend' 
      style='background-color: #FF9D00; 
      padding: 5px;
      border-radius: 5px;
      color: white;
    '>Slums</div>",
    position = "bottomright"
  )











