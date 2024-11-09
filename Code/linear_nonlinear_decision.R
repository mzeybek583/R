# Sample data generation
set.seed(123)
x <- 1:100
y_linear <- 3 + 2 * x + rnorm(100, sd = 20)          # Linear relationship with noise
y_nonlinear <- 3 + 2 * x + 0.05 * x^2 + rnorm(100, sd = 20)  # Nonlinear relationship with noise

# Fit linear and nonlinear models
linear_model <- lm(y_linear ~ x)
nonlinear_model <- lm(y_linear ~ x + I(x^2))

linear_model_nonlin <- lm(y_nonlinear ~ x)
nonlinear_model_nonlin <- lm(y_nonlinear ~ x + I(x^2))

# Model comparison and decision function
compare_models <- function(model1, model2, data_x, data_y, data_name = "Data") {
  aic1 <- AIC(model1)
  aic2 <- AIC(model2)
  r2_1 <- summary(model1)$r.squared
  r2_2 <- summary(model2)$r.squared
  
  # Display AIC and R^2 values
  print(paste("Comparing models for", data_name))
  print(paste("AIC for linear model:", round(aic1, 2)))
  print(paste("AIC for nonlinear model:", round(aic2, 2)))
  print(paste("R-squared for linear model:", round(r2_1, 3)))
  print(paste("R-squared for nonlinear model:", round(r2_2, 3)))
  
  # Determine the best model
  if (aic1 < aic2) {
    print("Decision: The linear model is a better fit based on AIC.")
    best_model <- model1
    model_type <- "Linear"
    formula_text <- paste("y =", round(coef(model1)[1], 2), "+", round(coef(model1)[2], 2), "* x")
  } else {
    print("Decision: The nonlinear model is a better fit based on AIC.")
    best_model <- model2
    model_type <- "Nonlinear"
    formula_text <- paste("y =", round(coef(model2)[1], 2), "+", round(coef(model2)[2], 2), "* x +", round(coef(model2)[3], 4), "* x^2")
  }
  
  # Print the mathematical model and R-squared of the best model
  print(paste("Selected Model Type:", model_type))
  print(paste("Mathematical Model:", formula_text))
  print(paste("R-squared of the selected model:", round(ifelse(model_type == "Linear", r2_1, r2_2), 3)))
  
  # Plot to visualize fit
  plot(data_x, data_y, main = paste(data_name, "Model Comparison"), col = "blue", pch = 19)
  abline(model1, col = "red", lwd = 2)
  lines(data_x, predict(model2, data.frame(x = data_x)), col = "green", lwd = 2)
  legend("bottomright", legend = c("Data", "Linear Fit", "Nonlinear Fit"), col = c("blue", "red", "green"), lty = 1)
}

# Compare for linear data
compare_models(linear_model, nonlinear_model, x, y_linear, "Linear Data")

# Compare for nonlinear data
compare_models(linear_model_nonlin, nonlinear_model_nonlin, x, y_nonlinear, "Nonlinear Data")
