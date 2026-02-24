# =========================================================
# TG03: CubicSpline yüzey + FACTOR slider + Türkiye sınırı + "Yeni nokta"
# Haritada: nokta + (h, N, H) etiketi + font büyütme + progress bar
# =========================================================
# install.packages(c("terra","shiny","sf","rnaturalearth","rnaturalearthdata"))

library(terra)
library(shiny)
library(sf)
library(rnaturalearth)

# ---- 1) VERİ OKU ----
xyz_path <- "tg03.xyz"  # lon lat N
d <- read.table(xyz_path, header = FALSE, sep = "", stringsAsFactors = FALSE)
stopifnot(ncol(d) >= 3)
names(d)[1:3] <- c("lon", "lat", "N")

# Orijinal grid raster
r0 <- rast(d[, c("lon", "lat", "N")], type = "xyz", crs = "EPSG:4326")

# Harita için hızlı önizleme
r_plot <- aggregate(r0, fact = 6, fun = mean, na.rm = TRUE)

# Türkiye sınırı (sf)
library(rnaturalearthhires)
turkey_sf <- ne_countries(country = "Turkey", scale = "large", returnclass = "sf")
turkey_sf <- st_transform(turkey_sf, 4326)

rx <- res(r0)[1]
ry <- res(r0)[2]

# Terra sürümlerine göre cubic method güvenli seçimi
resample_cubic_safe <- function(r_src, r_tgt) {
  methods_to_try <- c("cubicspline", "cubic", "bilinear")
  last_err <- NULL
  for (m in methods_to_try) {
    out <- tryCatch(
      resample(r_src, r_tgt, method = m),
      error = function(e) { last_err <<- e; NULL }
    )
    if (!is.null(out)) return(list(r = out, method = m))
  }
  stop(last_err)
}

# ---- 2) UI ----
ui <- fluidPage(
  titlePanel("TG03 Jeoit Ondülasyonu (N) + Ortometrik Yükseklik (H)"),
  sidebarLayout(
    sidebarPanel(
      tags$div(
        style = "font-size: 16px;",
        helpText("Boylam/enlem girin. N cubic enterpolasyonlu yüzeyden alınır. h girerseniz H = h - N hesaplanır.")
      ),
      numericInput("lon", "Boylam (lon, °):", value = 32.0, min = 24, max = 46, step = 0.0001),
      numericInput("lat", "Enlem (lat, °):",  value = 39.0, min = 35, max = 43, step = 0.0001),
      numericInput("h",   "Elipsoidal yükseklik h (m):", value = NA, step = 0.001),
      sliderInput("factor", "Yüzey çözünürlüğü (FACTOR):", min = 1, max = 6, value = 3, step = 1),
      actionButton("calc", "Hesapla (N ve H)"),
      tags$hr(),
      tags$div(style = "font-size: 16px;", verbatimTextOutput("out"))
    ),
    mainPanel(
      plotOutput("map", height = 560)
    )
  )
)

# ---- 3) SERVER ----
server <- function(input, output, session) {
  
  # Cache: factor değişince yeniden üretilecek
  rN_cache <- reactiveVal(NULL)
  method_used <- reactiveVal(NULL)
  factor_used <- reactiveVal(NULL)
  
  # Son nokta ve metin bilgileri
  lastPoint <- reactiveVal(NULL)
  lastLabel <- reactiveVal(NULL)
  
  output$map <- renderPlot({
    # fontları büyüt
    op <- par(cex.main = 1.15, cex.lab = 1.1, cex.axis = 1.05, mar = c(4, 4, 3, 1))
    on.exit(par(op), add = TRUE)
    
    plot(r_plot, main = "TG03 N Yüzeyi (Hızlı Önizleme)")
    # Türkiye sınırı ekle
    plot(vect(turkey_sf), add = TRUE, lwd = 1)
    
    # Son nokta ve etiket
    p <- lastPoint()
    lab <- lastLabel()
    
    if (!is.null(p)) {
      points(p, pch = 19, cex = 1.3, col = "red")
      # Nokta adı: "Yeni nokta"
      text(p, labels = "Yeni nokta", pos = 4, cex = 1.1, col = "red")
      # Değerleri yaz
      if (!is.null(lab)) {
        text(p, labels = lab, pos = 3, cex = 1.05, col = "black", offset = 1.4)
      }
    }
  })
  
  observeEvent(input$calc, {
    
    withProgress(message = "Hesaplanıyor...", value = 0, {
      
      incProgress(0.10, detail = "Kapsam kontrolü")
      
      ex <- ext(r0)
      inside <- (input$lon >= ex$xmin && input$lon <= ex$xmax &&
                   input$lat >= ex$ymin && input$lat <= ex$ymax)
      
      if (!inside) {
        output$out <- renderText("Girilen koordinat model kapsamı dışında.")
        lastPoint(NULL)
        lastLabel(NULL)
        return()
      }
      
      # Noktayı kaydet
      p <- vect(data.frame(lon = input$lon, lat = input$lat),
                geom = c("lon", "lat"), crs = "EPSG:4326")
      lastPoint(p)
      
      # Cache kontrol: factor değiştiyse yeniden üret
      need_rebuild <- is.null(rN_cache()) || is.null(factor_used()) || (factor_used() != input$factor)
      
      if (need_rebuild) {
        incProgress(0.20, detail = sprintf("Cubic yüzey hazırlanıyor (factor=%d)", input$factor))
        
        r_target <- rast(
          ext(r0),
          resolution = c(rx / input$factor, ry / input$factor),
          crs = crs(r0)
        )
        
        incProgress(0.30, detail = "Enterpolasyon uygulanıyor")
        
        res <- resample_cubic_safe(r0, r_target)
        rN_cache(res$r)
        method_used(res$method)
        factor_used(input$factor)
        
        incProgress(0.10, detail = paste0("Yöntem: ", res$method))
      } else {
        incProgress(0.10, detail = "Hazır yüzey kullanılıyor")
      }
      
      incProgress(0.15, detail = "N sorgulanıyor")
      
      rN <- rN_cache()
      N_val <- extract(rN, p)[1, 2]
      
      if (is.na(N_val)) {
        output$out <- renderText("Bu noktada N hesaplanamadı (NA).")
        lastLabel(NULL)
        return()
      }
      
      # H hesapla
      h_in <- input$h
      has_h <- !(is.na(h_in) || !is.finite(h_in))
      
      if (has_h) {
        H_val <- h_in - N_val
        
        output$out <- renderText(sprintf(
          "N = %.4f m\nh = %.4f m\nH = %.4f m\n(Yüzey: %s | factor=%d)",
          N_val, h_in, H_val, method_used(), factor_used()
        ))
        
        # Harita etiketi
        lastLabel(sprintf("h=%.3f m\nN=%.3f m\nH=%.3f m", h_in, N_val, H_val))
        
      } else {
        output$out <- renderText(sprintf(
          "N = %.4f m\n(h girerseniz H = h - N hesaplanır)\n(Yüzey: %s | factor=%d)",
          N_val, method_used(), factor_used()
        ))
        
        lastLabel(sprintf("N=%.3f m", N_val))
      }
      
      incProgress(0.15, detail = "Tamam")
    })
  })
}

shinyApp(ui, server)
