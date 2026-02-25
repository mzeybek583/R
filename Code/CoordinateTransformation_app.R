# app.R
# install.packages(c("shiny","sf","leaflet"))

library(shiny)
library(sf)
library(leaflet)

# -----------------------------
# 1) CRS kataloğu (istediğiniz kadar ekleyin)
# -----------------------------
crs_catalog <- data.frame(
  name = c(
    "WGS84 (EPSG:4326) - Enlem/Boylam",
    "TUREF / TM 33-3 (EPSG:5255) - X/Y",
    "TUREF / TM 30-3 (EPSG:5252) - X/Y",
    "TUREF / TM 31-3 (EPSG:5253) - X/Y",
    "TUREF / TM 32-3 (EPSG:5254) - X/Y",
    "TUREF / TM 34-3 (EPSG:5256) - X/Y",
    "TUREF / TM 35-3 (EPSG:5257) - X/Y",
    "TUREF / TM 36-3 (EPSG:5258) - X/Y"
  ),
  epsg = c(4326, 5255, 5252, 5253, 5254, 5256, 5257, 5258),
  type = c("geographic", "projected", "projected", "projected", "projected", "projected", "projected", "projected"),
  stringsAsFactors = FALSE
)

choices_named <- setNames(crs_catalog$epsg, crs_catalog$name)

# -----------------------------
# 2) Yardımcı fonksiyonlar
# -----------------------------
is_geographic_epsg <- function(epsg) {
  crs_catalog$type[match(epsg, crs_catalog$epsg)] == "geographic"
}

format_out <- function(vals, target_epsg) {
  if (is_geographic_epsg(target_epsg)) {
    sprintf("Hedef CRS: EPSG:%s (coğrafi)\nBoylam (lon): %.8f\nEnlem (lat):  %.8f",
            target_epsg, vals[1], vals[2])
  } else {
    sprintf("Hedef CRS: EPSG:%s (projeksiyon)\nX (Easting):  %.3f m\nY (Northing): %.3f m",
            target_epsg, vals[1], vals[2])
  }
}

# -----------------------------
# 3) UI
# -----------------------------
ui <- fluidPage(
  titlePanel("Koordinat Dönüştürücü (TUREF TM 3° dilimler ↔ WGS84 ve diğer CRS)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1) Girdi CRS seç"),
      selectInput("src_epsg", "Girdi Koordinat Sistemi:", choices = choices_named, selected = 5255),
      
      uiOutput("input_fields"),
      
      hr(),
      h4("2) Çıktı CRS seç"),
      selectInput("dst_epsg", "Çıktı Koordinat Sistemi:", choices = choices_named, selected = 4326),
      
      actionButton("calc", "Hesapla", class = "btn-primary"),
      hr(),
      verbatimTextOutput("out"),
      tags$small("Not: Harita her zaman WGS84 (EPSG:4326) üzerinde gösterilir.")
    ),
    
    mainPanel(
      leafletOutput("map", height = 620)
    )
  )
)

# -----------------------------
# 4) Server
# -----------------------------
server <- function(input, output, session) {
  
  # Girdi alanlarını CRS tipine göre değiştir
  output$input_fields <- renderUI({
    src_is_geo <- is_geographic_epsg(input$src_epsg)
    
    if (src_is_geo) {
      tagList(
        numericInput("lon", "Boylam (lon) [°]:", value = NA, min = -180, max = 180),
        numericInput("lat", "Enlem (lat)  [°]:", value = NA, min = -90,  max = 90)
      )
    } else {
      tagList(
        numericInput("x", "X (Easting / Doğu) [m]:", value = NA),
        numericInput("y", "Y (Northing / Kuzey) [m]:", value = NA)
      )
    }
  })
  
  # Başlangıç haritası
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      setView(lng = 35, lat = 39, zoom = 6)
  })
  
  # Dönüşüm
  res <- eventReactive(input$calc, {
    src_epsg <- as.integer(input$src_epsg)
    dst_epsg <- as.integer(input$dst_epsg)
    src_is_geo <- is_geographic_epsg(src_epsg)
    
    # 1) Girdiyi doğrula
    if (src_is_geo) {
      validate(
        need(!is.na(input$lon), "Boylam (lon) girin."),
        need(!is.na(input$lat), "Enlem (lat) girin.")
      )
      
      pt_src <- st_as_sf(
        data.frame(lon = input$lon, lat = input$lat),
        coords = c("lon", "lat"),
        crs = src_epsg
      )
    } else {
      validate(
        need(!is.na(input$x), "X (Easting) girin."),
        need(!is.na(input$y), "Y (Northing) girin.")
      )
      
      pt_src <- st_as_sf(
        data.frame(x = input$x, y = input$y),
        coords = c("x", "y"),
        crs = src_epsg
      )
    }
    
    # 2) Hedefe dönüştür
    pt_dst <- st_transform(pt_src, dst_epsg)
    dst_xy <- st_coordinates(pt_dst)[1, ]
    
    # 3) Harita için 4326'ya dönüştür
    pt_map <- st_transform(pt_src, 4326)
    map_xy <- st_coordinates(pt_map)[1, ]
    
    list(
      src_epsg = src_epsg,
      dst_epsg = dst_epsg,
      dst_vals = c(dst_xy["X"], dst_xy["Y"]),
      map_lon = as.numeric(map_xy["X"]),
      map_lat = as.numeric(map_xy["Y"])
    )
  })
  
  # Çıktıyı yaz
  output$out <- renderPrint({
    r <- res()
    if (is.null(r)) return(invisible())
    cat(format_out(r$dst_vals, r$dst_epsg), "\n")
  })
  
  # Haritada göster
  observeEvent(res(), {
    r <- res()
    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(
        lng = r$map_lon, lat = r$map_lat,
        popup = sprintf(
          "Harita (EPSG:4326)\nlon: %.8f\nlat: %.8f\n\nSeçilen çıktı EPSG:%s ekranda yazdırıldı.",
          r$map_lon, r$map_lat, r$dst_epsg
        )
      ) %>%
      setView(lng = r$map_lon, lat = r$map_lat, zoom = 16)
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)
