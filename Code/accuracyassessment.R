
## Reference : https://www.wikiwand.com/en/Confusion_matrix
## Accuracy Assessment
## Coder: Assist. Prof. Dr. Mustafa Zeybek

# Load Libraries
library("caret")

# Ground Truth Data
ground =  as.factor(c(1,1,2,1,2,3,3,2,1,3,3,3,2,2,2,2,1,3))

# Model data
model = as.factor(c(1,1,2,1,1,1,3,3,2,1,3,1,2,2,2,2,3,2))


# Create the confusion table
confusionMatrix(model, ground, positive = NULL, dnn = c("Model Prediction", "Ground Truth Data"))

## Results
#Confusion Matrix and Statistics

#                Ground Truth Data
#Model Prediction 1 2 3
#               1 3 1 3
#               2 1 5 1
#               3 1 1 2
#
#Overall Statistics
#                                          
#               Accuracy : 0.5556          
#                 95% CI : (0.3076, 0.7847)
#    No Information Rate : 0.3889          
#    P-Value [Acc > NIR] : 0.1144          
#                                          
#                  Kappa : 0.3333          
#                                          
# Mcnemar's Test P-Value : 0.8013          
#
#Statistics by Class:
#
#                     Class: 1 Class: 2 Class: 3
#Sensitivity            0.6000   0.7143   0.3333
#Specificity            0.6923   0.8182   0.8333
#Pos Pred Value         0.4286   0.7143   0.5000
#Neg Pred Value         0.8182   0.8182   0.7143
#Prevalence             0.2778   0.3889   0.3333
#Detection Rate         0.1667   0.2778   0.1111
#Detection Prevalence   0.3889   0.3889   0.2222
#Balanced Accuracy      0.6462   0.7662   0.5833
