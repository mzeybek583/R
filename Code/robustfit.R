  
  # Robust Regression
  ## Coder: Assist. Prof. Dr. Mustafa Zeybek
  
  set.seed(4208) # Rastgele sayi uretim
  
  veri <- within(data.frame(x=1:10), y <- rnorm(x, mean=x))
  model1 <- lm(y ~ x, data=veri)   # fitted model for original data LSE
  
  ## Add noise
  veri$y[2] <- 15
  model1.update <- update(model1)
  
  coef(summary(model1))
  coef(summary(model1.update))
  
  library(tikzDevice)
  tikz('/home/mzeybek/Desktop/plot1.tex',width=5,height=5)
  plot(y ~ x, data=veri, xlab="X", ylab = "Y", cex.lab=1.3, cex=0.7, cex.axis=1.3)
  abline(model1, lty="dashed")    # use a dashed line
  abline(model1.update)
  points(2,veri$y[2], pch=8, col="red")
  legend("topright", inset=0.03, bty="n",
         legend = c("Model fitted on inliers", "Model fitted with noise"),
         lty = c("dashed", "solid"),cex = 1.25  )
  dev.off()
  
  library("MASS")   #Robust package
  
  # Robust Model
  
  model.rob <- rlm(y ~ x, data=veri)
  
  tikz('/home/mzeybek/Desktop/plot2.tex',width=5,height=5) # change WD
  #setwd("/path/to/your/working/folder")
  
  plot(y ~ x, data=veri, xlab="X", ylab = "Y", cex.lab=1.3, cex=0.7, cex.axis=1.3)
  abline(model1, lty="dashed")    # use a dashed line
  abline(model.rob, col="green")
  points(2,veri$y[2], pch=8, col="red")
  legend("topright", inset=0.03, bty="n",
         legend = c("Model fitted on inliers", "Model with Robust fitting ","Outlier"), 
         col=c("black", "green","red"),
         lty = c("dashed", "solid",NA),
         pch=c(NA,NA,8),
         cex = 1.25  )
  dev.off()

#Results
# https://github.com/mzeybek583/R/blob/master/Code/Data/plot1.tex
# https://github.com/mzeybek583/R/blob/master/Code/Data/plot2.tex
# https://github.com/mzeybek583/R/blob/master/Code/images/robustfit.pdf
