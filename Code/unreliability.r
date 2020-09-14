
## Unrelibility

se <- function(x) sqrt(var(x)/length(x))

ci95 <- function(x) {
  t.value <- qt(0.975,length(x)-1) 
  standard.error <- se(x) 
  ci <- t.value*standard.error
  cat("95 Confidence Interval = ", mean(x) -ci, "to ", mean(x) +ci,"\n") }

x <- rnorm(150,25,3) 
ci95(x)


xv <- rnorm(30)
sem <- numeric(30)
sem[1] <- NA 
for(i in 2:30){
  sem[i] <- se(xv[1:i])
plot(1:30,sem,ylim=c(0,0.8), 
     ylab="standard error of mean",xlab="sample size n",pch=16)
lines(2:30,1/sqrt(2:30))
}
