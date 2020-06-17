## Normalization


set.seed(4208)

#Generate random numbers
df <- rnorm(10, mean = 40000, sd=2000)
df
hist(df) # plot data

mx <- 255 # maximum value of normalization

normalize <- function(df, mx, int=TRUE, plot=TRUE){
  norm <- (df - min(df)) * mx / (max(df) - min(df))
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
normalize(df, mx, TRUE, TRUE)
