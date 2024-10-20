# Load necessary library
if (!require("caret")) install.packages("caret", dependencies = TRUE)
library(caret)

# Ground Truth Data
ground_truth <- as.factor(c(1, 1, 2, 1, 2, 3, 3, 2, 1, 3, 3, 3, 2, 2, 2, 2, 1, 3))

# Model Prediction Data
model_prediction <- as.factor(c(1, 1, 2, 1, 1, 1, 3, 3, 2, 1, 3, 1, 2, 2, 2, 2, 3, 2))

# Create the confusion matrix
confusion_result <- confusionMatrix(model_prediction, ground_truth, dnn = c("Model Prediction", "Ground Truth"))

# Print the confusion matrix and statistics
print(confusion_result)
