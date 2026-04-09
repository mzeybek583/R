# =========================================================
# TUSAGA PDF'DEN OTOMATIK ISTASYON OKUMA
# EPOK TASIMA + ECEF + GEODETIK + EPSG:5255 (TUREF / TM33)
# =========================================================

# =========================================================
# 0) GEREKLI PAKETLER
# =========================================================
required_packages <- c("pdftools", "sf", "dplyr")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(pdftools)
library(sf)
library(dplyr)

sf::sf_use_s2(FALSE)

# =========================================================
# 1) KULLANICI PARAMETRELERI
# =========================================================

# PDF adresi
pdf_url <- "https://www.tusaga-aktif.gov.tr/Content/Files/IstasyonKonumBilgileri.pdf"

# Referans epok
reference_epoch <- 2005.0

# Saat dilimi
time_zone <- "UTC"

# Girdi koordinat sistemi aciklamasi
input_coord_system_label <- "ITRF96 / epoch 2005.0 / ECEF"

# Cikti koordinat sistemleri
output_geodetic_epsg  <- 4326
output_projected_epsg <- 5255   # TUREF / TM33

# Istasyon secme tercihleri
prefer_active <- TRUE
prefer_latest_version <- TRUE

# ---------------------------------------------------------
# ISTASYON KODLARI
# Ctrl+A > Run kullaniyorsaniz bunu doldurun
# Ornek:
# station_codes_input <- "KNY1"
# station_codes_input <- "KNY1,ANK2,AKSR"
# Bos birakirsaniz readline() ile sormayi dener
# ---------------------------------------------------------
station_codes_input <- "KNY1"

# Oturumlar
sessions <- data.frame(
  session_id = c("2024_session", "2025_session"),
  date = c("2024-11-28", "2025-11-21"),
  start_datetime = c("2024-11-28 09:00:00", "2025-11-21 10:15:00"),
  end_datetime   = c("2024-11-28 11:30:00", "2025-11-21 12:45:00"),
  stringsAsFactors = FALSE
)

# Ornek:
# sessions <- data.frame(
#   session_id = c("2024_session", "2025_session"),
#   date = c("2024-11-28", "2025-11-21"),
#   start_datetime = c(NA, NA),
#   end_datetime   = c(NA, NA),
#   stringsAsFactors = FALSE
# )

# CSV cikti
write_csv_results <- TRUE
output_dir <- getwd()

# Yuvarlama
coord_digits <- 4
angle_digits <- 8
time_digits  <- 9

# =========================================================
# 2) ELIPSOID SABITLERI
# =========================================================
a_ell <- 6378137.0
f_ell <- 1 / 298.257222101
e2_ell <- 2 * f_ell - f_ell^2

# =========================================================
# 3) YARDIMCI FONKSIYONLAR
# =========================================================

tr_num <- function(x) {
  x <- gsub(",", ".", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

dms_to_decimal <- function(deg, min, sec, sign = 1) {
  sign * (deg + min / 60 + sec / 3600)
}

normalize_pdf_line <- function(x) {
  x <- trimws(x)
  x <- gsub("[\u00A0\t]+", " ", x, perl = TRUE)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  x
}

is_missing_string <- function(x) {
  is.null(x) || is.na(x) || trimws(x) == ""
}

is_station_line <- function(x) {
  grepl("^\\d+\\s+\\d+,\\d+\\s+[A-Z0-9]+\\s+(aktif|pasif)\\b", x, perl = TRUE)
}

decimal_year_from_date <- function(date_str) {
  d <- as.Date(date_str)
  if (is.na(d)) stop("Gecersiz tarih: ", date_str)
  
  y <- as.integer(format(d, "%Y"))
  y0 <- as.Date(sprintf("%d-01-01", y))
  y1 <- as.Date(sprintf("%d-01-01", y + 1))
  
  y + as.numeric(d - y0) / as.numeric(y1 - y0)
}

decimal_year_from_datetime <- function(datetime_str, tz = "UTC") {
  dt <- as.POSIXct(datetime_str, tz = tz)
  if (is.na(dt)) stop("Gecersiz datetime: ", datetime_str)
  
  y <- as.integer(format(dt, "%Y"))
  y0 <- as.POSIXct(sprintf("%d-01-01 00:00:00", y), tz = tz)
  y1 <- as.POSIXct(sprintf("%d-01-01 00:00:00", y + 1), tz = tz)
  
  y + as.numeric(difftime(dt, y0, units = "secs")) /
    as.numeric(difftime(y1, y0, units = "secs"))
}

get_session_epoch <- function(date_str, start_datetime = NA, end_datetime = NA, tz = "UTC") {
  has_start <- !is_missing_string(start_datetime)
  has_end   <- !is_missing_string(end_datetime)
  
  if (has_start && has_end) {
    t1 <- as.POSIXct(start_datetime, tz = tz)
    t2 <- as.POSIXct(end_datetime, tz = tz)
    
    if (is.na(t1) || is.na(t2)) stop("Baslangic veya bitis zamani gecersiz.")
    if (t2 < t1) stop("Bitis zamani baslangic zamanindan once olamaz.")
    
    t_mid <- t1 + as.numeric(difftime(t2, t1, units = "secs")) / 2
    
    return(list(
      epoch_decimal = decimal_year_from_datetime(format(t_mid, "%Y-%m-%d %H:%M:%S"), tz = tz),
      epoch_type = "midpoint_from_start_end",
      used_datetime = format(t_mid, "%Y-%m-%d %H:%M:%S")
    ))
  }
  
  if (has_start && !has_end) {
    t1 <- as.POSIXct(start_datetime, tz = tz)
    if (is.na(t1)) stop("Baslangic zamani gecersiz.")
    
    return(list(
      epoch_decimal = decimal_year_from_datetime(start_datetime, tz = tz),
      epoch_type = "start_datetime_only",
      used_datetime = format(t1, "%Y-%m-%d %H:%M:%S")
    ))
  }
  
  if (!has_start && has_end) {
    t2 <- as.POSIXct(end_datetime, tz = tz)
    if (is.na(t2)) stop("Bitis zamani gecersiz.")
    
    return(list(
      epoch_decimal = decimal_year_from_datetime(end_datetime, tz = tz),
      epoch_type = "end_datetime_only",
      used_datetime = format(t2, "%Y-%m-%d %H:%M:%S")
    ))
  }
  
  return(list(
    epoch_decimal = decimal_year_from_date(date_str),
    epoch_type = "date_only",
    used_datetime = NA_character_
  ))
}

ecef_to_geodetic <- function(X, Y, Z, a = a_ell, e2 = e2_ell, tol = 1e-12, max_iter = 100) {
  lon <- atan2(Y, X)
  p <- sqrt(X^2 + Y^2)
  lat <- atan2(Z, p * (1 - e2))
  
  for (i in seq_len(max_iter)) {
    sin_lat <- sin(lat)
    N <- a / sqrt(1 - e2 * sin_lat^2)
    h <- p / cos(lat) - N
    lat_new <- atan2(Z, p * (1 - e2 * N / (N + h)))
    
    if (abs(lat_new - lat) < tol) {
      lat <- lat_new
      break
    }
    lat <- lat_new
  }
  
  sin_lat <- sin(lat)
  N <- a / sqrt(1 - e2 * sin_lat^2)
  h <- p / cos(lat) - N
  
  list(
    lat_deg = lat * 180 / pi,
    lon_deg = lon * 180 / pi,
    h = h
  )
}

project_geodetic_to_epsg <- function(lat_deg, lon_deg, output_epsg = 5255) {
  pt <- st_as_sf(
    data.frame(lon = lon_deg, lat = lat_deg),
    coords = c("lon", "lat"),
    crs = 4326
  )
  
  pt_out <- st_transform(pt, output_epsg)
  xy <- st_coordinates(pt_out)
  
  list(
    Easting = xy[1, 1],
    Northing = xy[1, 2]
  )
}

deg_to_dms_string <- function(deg_value, type = c("lat", "lon")) {
  type <- match.arg(type)
  
  sign_char <- ifelse(
    deg_value < 0,
    ifelse(type == "lat", "S", "W"),
    ifelse(type == "lat", "N", "E")
  )
  
  deg_abs <- abs(deg_value)
  d <- floor(deg_abs)
  m_float <- (deg_abs - d) * 60
  m <- floor(m_float)
  s <- (m_float - m) * 60
  
  sprintf("%d° %d' %.5f\" %s", d, m, s, sign_char)
}

# =========================================================
# 4) PDF ISLEMLERI
# =========================================================

download_tusaga_pdf <- function(pdf_url, pdf_file) {
  download.file(pdf_url, pdf_file, mode = "wb", quiet = TRUE)
  if (!file.exists(pdf_file)) stop("PDF indirilemedi.")
  pdf_file
}

extract_pdf_lines <- function(pdf_file) {
  txt <- pdf_text(pdf_file)
  lines <- unlist(strsplit(txt, "\n", fixed = TRUE))
  lines <- trimws(lines)
  lines[nzchar(lines)]
}

parse_one_station_line <- function(line) {
  line <- normalize_pdf_line(line)
  
  if (!is_station_line(line)) return(NULL)
  
  parts <- unlist(strsplit(line, " ", fixed = TRUE))
  
  if (length(parts) < 17) {
    warning("Eksik token sayisi nedeniyle satir atlandi: ", line)
    return(NULL)
  }
  
  physical_station_no <- suppressWarnings(as.integer(parts[1]))
  version_text        <- parts[2]
  station_name        <- parts[3]
  status              <- parts[4]
  
  lat_d <- suppressWarnings(as.integer(parts[5]))
  lat_m <- suppressWarnings(as.integer(parts[6]))
  lat_s <- suppressWarnings(as.numeric(parts[7]))
  
  lon_d <- suppressWarnings(as.integer(parts[8]))
  lon_m <- suppressWarnings(as.integer(parts[9]))
  lon_s <- suppressWarnings(as.numeric(parts[10]))
  
  h_m <- tr_num(parts[11])
  X_m <- tr_num(parts[12])
  Y_m <- tr_num(parts[13])
  Z_m <- tr_num(parts[14])
  
  Vx_m_per_year <- tr_num(parts[15])
  Vy_m_per_year <- tr_num(parts[16])
  Vz_m_per_year <- tr_num(parts[17])
  
  note <- if (length(parts) >= 18) {
    paste(parts[18:length(parts)], collapse = " ")
  } else {
    ""
  }
  
  mandatory_vals <- c(
    physical_station_no, lat_d, lat_m, lat_s,
    lon_d, lon_m, lon_s, h_m, X_m, Y_m, Z_m,
    Vx_m_per_year, Vy_m_per_year, Vz_m_per_year
  )
  
  if (any(is.na(mandatory_vals))) {
    warning("Sayisal donusum hatasi nedeniyle satir atlandi: ", line)
    return(NULL)
  }
  
  data.frame(
    physical_station_no = physical_station_no,
    version_text = version_text,
    station_name = station_name,
    status = status,
    lat_deg_d = lat_d,
    lat_deg_m = lat_m,
    lat_deg_s = lat_s,
    lon_deg_d = lon_d,
    lon_deg_m = lon_m,
    lon_deg_s = lon_s,
    h_m = h_m,
    X_m = X_m,
    Y_m = Y_m,
    Z_m = Z_m,
    Vx_m_per_year = Vx_m_per_year,
    Vy_m_per_year = Vy_m_per_year,
    Vz_m_per_year = Vz_m_per_year,
    note = note,
    stringsAsFactors = FALSE
  )
}

parse_station_lines <- function(lines) {
  lines <- vapply(lines, normalize_pdf_line, character(1))
  lines <- lines[nzchar(lines)]
  lines <- lines[is_station_line(lines)]
  
  parsed_list <- lapply(lines, parse_one_station_line)
  parsed_list <- Filter(Negate(is.null), parsed_list)
  
  if (length(parsed_list) == 0) {
    stop("PDF'den istasyon satirlari parse edilemedi.")
  }
  
  df <- do.call(rbind, parsed_list)
  
  df$version_num <- suppressWarnings(as.numeric(gsub(",", ".", df$version_text)))
  df$latitude_deg  <- dms_to_decimal(df$lat_deg_d, df$lat_deg_m, df$lat_deg_s, sign = 1)
  df$longitude_deg <- dms_to_decimal(df$lon_deg_d, df$lon_deg_m, df$lon_deg_s, sign = 1)
  
  rownames(df) <- NULL
  df
}

choose_station_records <- function(stations_df, station_codes,
                                   prefer_active = TRUE,
                                   prefer_latest_version = TRUE) {
  station_codes <- toupper(trimws(station_codes))
  station_codes <- unique(station_codes)
  
  out <- list()
  
  for (code in station_codes) {
    sub <- stations_df[stations_df$station_name == code, , drop = FALSE]
    
    if (nrow(sub) == 0) {
      warning(sprintf("'%s' PDF icinde bulunamadi.", code))
      next
    }
    
    if (prefer_active && any(sub$status == "aktif")) {
      sub <- sub[sub$status == "aktif", , drop = FALSE]
    }
    
    if (prefer_latest_version && "version_num" %in% names(sub)) {
      sub <- sub[order(-sub$version_num), , drop = FALSE]
    }
    
    out[[code]] <- sub[1, , drop = FALSE]
  }
  
  if (length(out) == 0) {
    return(data.frame())
  }
  
  result <- do.call(rbind, out)
  rownames(result) <- NULL
  result
}

get_station_codes <- function(station_codes_input = "") {
  if (!is_missing_string(station_codes_input)) {
    codes <- unlist(strsplit(toupper(station_codes_input), ",", fixed = TRUE))
    codes <- trimws(codes)
    codes <- codes[nzchar(codes)]
    return(codes)
  }
  
  cat("\nKullanmak istediginiz istasyon kodlarini virgulle giriniz.\n")
  cat("Ornek: KNY1,AKSR,AKHR\n\n")
  user_input <- readline(prompt = "Istasyon kodlari: ")
  
  if (is_missing_string(user_input)) {
    stop("Istasyon kodu girilmedi. Ctrl+A > Run kullaniyorsaniz station_codes_input degiskenini kodun basinda doldurun.")
  }
  
  codes <- unlist(strsplit(toupper(user_input), ",", fixed = TRUE))
  codes <- trimws(codes)
  codes <- codes[nzchar(codes)]
  
  if (length(codes) == 0) {
    stop("Gecerli bir istasyon kodu elde edilemedi.")
  }
  
  codes
}

# =========================================================
# 5) EPOK TASIMA VE DONUSUM
# =========================================================

propagate_station_for_sessions <- function(station_row, sessions_df,
                                           reference_epoch = 2005.0,
                                           output_projected_epsg = 5255,
                                           tz = "UTC") {
  out_list <- lapply(seq_len(nrow(sessions_df)), function(i) {
    s <- sessions_df[i, ]
    
    ep <- get_session_epoch(
      date_str = s$date,
      start_datetime = s$start_datetime,
      end_datetime = s$end_datetime,
      tz = tz
    )
    
    dt <- ep$epoch_decimal - reference_epoch
    
    Xt <- station_row$X_m + station_row$Vx_m_per_year * dt
    Yt <- station_row$Y_m + station_row$Vy_m_per_year * dt
    Zt <- station_row$Z_m + station_row$Vz_m_per_year * dt
    
    geo <- ecef_to_geodetic(Xt, Yt, Zt)
    prj <- project_geodetic_to_epsg(
      lat_deg = geo$lat_deg,
      lon_deg = geo$lon_deg,
      output_epsg = output_projected_epsg
    )
    
    data.frame(
      station_name = station_row$station_name,
      physical_station_no = station_row$physical_station_no,
      version_text = station_row$version_text,
      status = station_row$status,
      source_note = station_row$note,
      
      session_id = s$session_id,
      input_date = s$date,
      start_datetime = s$start_datetime,
      end_datetime = s$end_datetime,
      epoch_type = ep$epoch_type,
      used_datetime = ep$used_datetime,
      decimal_year = ep$epoch_decimal,
      delta_t_year = dt,
      
      X_m = Xt,
      Y_m = Yt,
      Z_m = Zt,
      
      latitude_deg = geo$lat_deg,
      longitude_deg = geo$lon_deg,
      ellipsoid_height_m = geo$h,
      
      latitude_dms = deg_to_dms_string(geo$lat_deg, "lat"),
      longitude_dms = deg_to_dms_string(geo$lon_deg, "lon"),
      
      tm33_easting_m = prj$Easting,
      tm33_northing_m = prj$Northing,
      
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, out_list)
}

make_leica_table <- function(results_df) {
  results_df %>%
    transmute(
      PointID  = paste0(station_name, "_", session_id),
      Station  = station_name,
      Session  = session_id,
      Easting  = round(tm33_easting_m, 4),
      Northing = round(tm33_northing_m, 4),
      Height   = round(ellipsoid_height_m, 4)
    )
}

# =========================================================
# 6) ANA AKIS
# =========================================================

pdf_file <- file.path(tempdir(), "IstasyonKonumBilgileri.pdf")
download_tusaga_pdf(pdf_url, pdf_file)

pdf_lines <- extract_pdf_lines(pdf_file)
stations_all <- parse_station_lines(pdf_lines)

cat("\n=========================================================\n")
cat("PDF'den parse edilen toplam istasyon satiri sayisi:", nrow(stations_all), "\n")
cat("=========================================================\n")

selected_codes <- get_station_codes(station_codes_input)

selected_stations <- choose_station_records(
  stations_df = stations_all,
  station_codes = selected_codes,
  prefer_active = prefer_active,
  prefer_latest_version = prefer_latest_version
)

if (nrow(selected_stations) == 0) {
  stop("Girilen istasyonlar PDF icinde bulunamadi.")
}

cat("\n=========================================================\n")
cat("SECILEN ISTASYON KAYITLARI\n")
cat("=========================================================\n")
print(
  selected_stations[, c(
    "station_name", "status", "version_text",
    "X_m", "Y_m", "Z_m",
    "Vx_m_per_year", "Vy_m_per_year", "Vz_m_per_year",
    "note"
  )],
  row.names = FALSE
)

result_list <- lapply(seq_len(nrow(selected_stations)), function(i) {
  propagate_station_for_sessions(
    station_row = selected_stations[i, ],
    sessions_df = sessions,
    reference_epoch = reference_epoch,
    output_projected_epsg = output_projected_epsg,
    tz = time_zone
  )
})

results <- do.call(rbind, result_list)

results_print <- results
results_print$decimal_year       <- round(results_print$decimal_year, time_digits)
results_print$delta_t_year       <- round(results_print$delta_t_year, time_digits)
results_print$X_m                <- round(results_print$X_m, coord_digits)
results_print$Y_m                <- round(results_print$Y_m, coord_digits)
results_print$Z_m                <- round(results_print$Z_m, coord_digits)
results_print$latitude_deg       <- round(results_print$latitude_deg, angle_digits)
results_print$longitude_deg      <- round(results_print$longitude_deg, angle_digits)
results_print$ellipsoid_height_m <- round(results_print$ellipsoid_height_m, coord_digits)
results_print$tm33_easting_m     <- round(results_print$tm33_easting_m, coord_digits)
results_print$tm33_northing_m    <- round(results_print$tm33_northing_m, coord_digits)

cat("\n=========================================================\n")
cat("EPOK TASINMIS SONUCLAR\n")
cat("Girdi koordinat sistemi :", input_coord_system_label, "\n")
cat("Geodetik cikti EPSG     :", output_geodetic_epsg, "\n")
cat("Projeksiyon cikti EPSG  :", output_projected_epsg, "\n")
cat("=========================================================\n")
print(results_print, row.names = FALSE)

leica_table <- make_leica_table(results)

cat("\n=========================================================\n")
cat("LEICA INFINITY ICIN KOPYALANABILIR TABLO (TM33)\n")
cat("=========================================================\n")
print(leica_table, row.names = FALSE)

if (write_csv_results) {
  full_csv  <- file.path(output_dir, "tusaga_epoch_results_full.csv")
  leica_csv <- file.path(output_dir, "tusaga_epoch_results_leica_tm33.csv")
  
  write.table(results, full_csv, row.names = FALSE,sep = ";")
  write.table(leica_table, leica_csv, row.names = FALSE, sep = ";")
  
  cat("\nCSV dosyalari kaydedildi:\n")
  cat(full_csv, "\n")
  cat(leica_csv, "\n")
}
