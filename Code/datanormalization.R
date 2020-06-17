## Normalization


set.seed(4208)

#Generate random numbers
df <- rnorm(10, mean = 40000, sd=2000)
df
hist(df) # plot data


normalize <- function(df, mn, mx, int=TRUE, plot=TRUE){
  range <- max(df) - min(df)
  norm <- (df - min(df))/ (range)
  norm <- mn + as.integer((mx-mn)*norm)
  if (int==TRUE) {
    norm <- as.integer(norm)
  }
  if (plot==TRUE) {
    par(mfrow=c(1,2))    # set the plotting area into a 1*2 array
    hist(df,main = "Non-Normalized Data", xlab = "Data")
    hist(norm, main = "Normalized Data", xlab = "Data")
  }
    return(norm)
  par(mfrow=c(1,1))    # set the plotting area into a 1*2 array
  
}

mx <- 200 # maximum value of normalization
mn <- 10
normalize(df, mn, mx, TRUE, TRUE)
