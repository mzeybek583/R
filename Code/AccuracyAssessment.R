
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
