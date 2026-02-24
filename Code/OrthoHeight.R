# =========================================================
# TG03: CubicSpline yüzey + FACTOR slider + Türkiye sınırı + "Yeni nokta"
# Haritada: nokta + (h, N, H) etiketi + font büyütme + progress bar
# + İstatistiksel bilgi: yerel std. sapma, std. hata, min-max (komşuluk)
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

# Türkiye sınırı (sf) - hires sende çalışıyorsa kalsın
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

# ---- Yerel istatistik (komşuluk) fonksiyonu ----
# ring = 1 -> 8 komşu + merkez (en hafif)
# ring = 2/3 -> daha geniş komşuluk (daha yumuşak istatistik)
local_stats_from_r0 <- function(r, xy, ring = 2) {
  cell0 <- cellFromXY(r, xy)
  if (is.na(cell0)) return(list(n = 0, sd = NA, se = NA, min = NA, max = NA, mean = NA))
  
  cells <- cell0
  frontier <- cell0
  
  # komşu halkaları genişlet
  for (i in seq_len(ring)) {
    nb <- adjacent(r, frontier, directions = 8, pairs = FALSE)
    nb <- unique(nb[!is.na(nb)])
    cells <- unique(c(cells, nb))
    frontier <- nb
    if (length(frontier) == 0) break
  }
  
  vals <- values(r, cells = cells, mat = FALSE)
  vals <- vals[is.finite(vals)]
  
  if (length(vals) < 2) {
    return(list(n = length(vals), sd = NA, se = NA,
                min = ifelse(length(vals)==0, NA, min(vals)),
                max = ifelse(length(vals)==0, NA, max(vals)),
                mean = ifelse(length(vals)==0, NA, mean(vals))))
  }
  
  s <- sd(vals)
  n <- length(vals)
  list(
    n = n,
    sd = s,
    se = s / sqrt(n),
    min = min(vals),
    max = max(vals),
    mean = mean(vals)
  )
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
      numericInput("lon", "Boylam (lon, °):", value = 32.717813, min = 24, max = 46, step = 0.0001),
      numericInput("lat", "Enlem (lat, °):",  value = 37.281586, min = 35, max = 43, step = 0.0001),
      numericInput("h",   "Elipsoidal yükseklik h (m):", value = 1100, step = 0.001),
      sliderInput("factor", "Yüzey çözünürlüğü (FACTOR):", min = 1, max = 6, value = 3, step = 1),
      sliderInput("ring", "İstatistik komşuluğu (ring):", min = 1, max = 5, value = 2, step = 1),
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
    op <- par(cex.main = 1.15, cex.lab = 1.1, cex.axis = 1.05, mar = c(4, 4, 3, 1))
    on.exit(par(op), add = TRUE)
    
    plot(r_plot, main = "TG03 N Yüzeyi (Hızlı Önizleme)")
    plot(vect(turkey_sf), add = TRUE, lwd = 1)
    
    p <- lastPoint()
    lab <- lastLabel()
    
    if (!is.null(p)) {
      points(p, pch = 19, cex = 1.3, col = "red")
      text(p, labels = "Yeni nokta", pos = 4, cex = 1.1, col = "red")
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
        lastPoint(NULL); lastLabel(NULL)
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
      
      # ---- Yerel istatistik (r0 komşuluğundan) ----
      incProgress(0.10, detail = "Yerel istatistik hesaplanıyor")
      xy <- as.matrix(data.frame(input$lon, input$lat))
      stats <- local_stats_from_r0(r0, xy, ring = input$ring)
      
      # Basit belirsizlik yorumu:
      # - sd: komşuluk içi varyasyon (yüzeyin yerel dalgalanması)
      # - se: sd/sqrt(n) (örnek standart hata; sadece fikir verir)
      sigmaN <- stats$sd
      sigmaH <- sigmaN  # h belirsizliği girilmediği varsayımıyla
      
      # H hesapla
      h_in <- input$h
      has_h <- !(is.na(h_in) || !is.finite(h_in))
      
      if (has_h) {
        H_val <- h_in - N_val
        
        output$out <- renderText(sprintf(
          paste0(
            "N = %.4f m\nh = %.4f m\nH = %.4f m\n\n",
            "Yerel istatistik (ring=%d, n=%d)\n",
            "mean(N0) = %.4f m | sd = %.4f m | se = %.4f m\n",
            "min = %.4f m | max = %.4f m\n\n",
            "Yaklaşık belirsizlik: σN≈%.4f m, σH≈%.4f m\n",
            "(Yüzey: %s | factor=%d)"
          ),
          N_val, h_in, H_val,
          input$ring, stats$n,
          stats$mean, stats$sd, stats$se,
          stats$min, stats$max,
          sigmaN, sigmaH,
          method_used(), factor_used()
        ))
        
        lastLabel(sprintf(
          "h=%.3f m\nN=%.3f m\nH=%.3f m\nsd(N0)=%.3f m",
          h_in, N_val, H_val, sigmaN
        ))
        
      } else {
        output$out <- renderText(sprintf(
          paste0(
            "N = %.4f m\n\n",
            "Yerel istatistik (ring=%d, n=%d)\n",
            "mean(N0) = %.4f m | sd = %.4f m | se = %.4f m\n",
            "min = %.4f m | max = %.4f m\n\n",
            "Yaklaşık belirsizlik: σN≈%.4f m\n",
            "(h girerseniz H = h - N hesaplanır)\n",
            "(Yüzey: %s | factor=%d)"
          ),
          N_val,
          input$ring, stats$n,
          stats$mean, stats$sd, stats$se,
          stats$min, stats$max,
          sigmaN,
          method_used(), factor_used()
        ))
        
        lastLabel(sprintf("N=%.3f m\nsd(N0)=%.3f m", N_val, sigmaN))
      }
      
      incProgress(0.10, detail = "Tamam")
    })
  })
}

shinyApp(ui, server)
