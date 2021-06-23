library(pROC)
# Create a few ROC curves:
data(aSAH)
roc.s100b <- roc(aSAH$outcome, aSAH$s100b)
roc.wfns <- roc(aSAH$outcome, aSAH$wfns)
roc.ndka <- roc(aSAH$outcome, aSAH$ndka)

# Simple example:
plot(roc.s100b,print.auc=FALSE)
plot(roc.wfns, add=TRUE, col="red",print.auc=FALSE)
plot(roc.ndka, add=TRUE, col="blue",print.auc=FALSE)

legend("bottomright", legend=c(paste("S100b:",round(auc(roc.s100b),3)), 
                               paste("Wfns:",round(auc(roc.wfns),3)), 
                               paste("ndka:",round(auc(roc.ndka),3))),
     col=c(par("fg"), "red", "blue"), lwd=2)


