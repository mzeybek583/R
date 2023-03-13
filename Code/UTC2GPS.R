
# Convert UTC2GPS
# https://novatel.com/support/knowledge-and-learning/unit-conversions
## PPP CSRS output data convert to GPS Week


library(lubridate)

year <- "2019"
time.file <- "E:/path/to/file/full_output/100_0003.clk"
times <- read.csv(time.file, header = F, sep = "", skip=10)

day.raw <- times[,3:5]
day <- data.frame(day=c(paste(day.raw$V3, day.raw$V4, day.raw$V5, sep= "-")))
hour.raw <- times[,6:8]
hours <- data.frame(day=c(paste(hour.raw$V6, hour.raw$V7, hour.raw$V8, sep= ":")))

UTC2GPS <- function(year, day, hours){
  if (is.Date(day)) {
    print("day is a date! continue")
  } else {
    day <- date(day)  
    print("day is not a date! we converted it")
  }
  if (is.numeric(year)) {
    print("Year is numeric! continue")
  } else {
    year <- as.numeric(year) 
    print("Year is not a numeric! we converted it")
  }
  years <- length(seq(from=1980, to=year)) # 6 jan 1980
  days <- 365*years 
  leaps <- sum(leap_year(seq(from=1980, to=year)))
  
  survey <- data.frame(date=c("1980-01-06"),tx_start=day)
  
  survey$date_diff <- as.Date(as.character(survey$tx_start), format="%Y-%m-%d")-
    as.Date(as.character(survey$date), format="%Y-%m-%d")
  as.numeric(survey$date_diff)*86400/ 604800
  
  dec <- as.numeric(survey$date_diff)*86400/ 604800 - floor(as.numeric(survey$date_diff)*86400/ 604800)
  num.sec <- dec*7*86400
  hours <- hms(hours)
  x.lt <- hours
  x.hhdd <- x.lt@hour + x.lt@minute/60 + x.lt@.Data/3600
  GPST <- num.sec + x.hhdd*3600
  return(GPST)
}

out <- data.frame(GPST=seq(1:nrow(day)))
for (i in 1:nrow(day)) {
  out[i,1] <-   UTC2GPS(year, day = day[i,1], hours[i,1])
}

write.table(out, "GPST.csv", sep=";", row.names = FALSE)


