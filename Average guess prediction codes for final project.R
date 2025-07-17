data = read.csv("C:\\Users\\admin\\Desktop\\Gargi study\\TYBsc project\\final project\\For final project dataset.csv", header = TRUE)

View(data)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)

# Choosing predictors
predictors <- data[, c("ScrabbleScore", "CV_Ratio", "UniqueLetters", 
                       "Positional_Score", "Bigram", "Trigram")]

# Scale predictors
scaled_predictors <- as.data.frame(scale(predictors))

# Add the target and Word columns to the scaled data
scaled_data <- cbind(scaled_predictors, Average.guess = data$Average.guess, Word = data$Wordle.Answer)

# Split data into train and test sets
set.seed(123)
train_index <- createDataPartition(scaled_data$Average.guess, p = 0.8, list = FALSE)
train_data <- scaled_data[train_index, ]
test_data <- scaled_data[-train_index, ]


##RANDOM FOREST

library(randomForest)

rf_model <- randomForest(Average.guess ~ ScrabbleScore + CV_Ratio + UniqueLetters + 
                           Positional_Score + Bigram + Trigram  , data = train_data, ntree = 1500, importance = TRUE)

rf_pred <- predict(rf_model, newdata = test_data)

# RMSE
rf_rmse <- sqrt(mean((rf_pred - test_data$Average.guess)^2))

# R-squared
rf_r2 <= 1 - sum((rf_pred - test_data$Average.guess)^2) / 
  sum((test_data$Average.guess - mean(test_data$Average.guess))^2)

cat("Random Forest RMSE:", rf_rmse, "\nRandom Forest R²:", rf_r2)

importance(rf_model)

varImpPlot(rf_model)


##linear regression

shapiro_results = shapiro.test(data$Average.guess)
shapiro_results
hist(data$Average.guess)

NR=(data$Average.guess-mean(data$Average.guess))/ sd(data$Average.guess)
hist(NR, main = "Checking Normality", xlab = "Average number of attempts", col ="light blue")

# Fit the linear regression model on the training data
lm_model = lm(Average.guess ~ ScrabbleScore + CV_Ratio + UniqueLetters + 
                 Positional_Score + Bigram + Trigram, 
               data = train_data)

vif(lm_model)

sub_data = data[11:17]

par(mfrow = c(2,2))
plot(sub_data$ScrabbleScore, sub_data$Average.guess, xlab = "Scrabble Score", ylab = "Average no of attempts", main = "Scrabble Score vs Avg Guess", pch =16)
abline(lm(Average.guess ~ ScrabbleScore, data = data), col = "red")

plot(sub_data$CV_Ratio, sub_data$Average.guess, xlab = "CV Ratio", ylab = "Average no of attempts", main = "CV_Ratio vs Avg Guess", pch =16)
abline(lm(Average.guess ~ CV_Ratio, data = data), col = "red")

plot(sub_data$UniqueLetters, sub_data$Average.guess, xlab = "Number of unique letters", ylab = "Average no of attempts", main = "Unique letters vs Avg Guess", pch =16)
abline(lm(Average.guess ~ UniqueLetters, data = data), col = "red")

plot(sub_data$Positional_Score, sub_data$Average.guess, xlab = "Positional Score", ylab = "Average no of attempts", main = "Positional Score vs Avg Guess", pch = 16)
abline(lm(Average.guess ~ Positional_Score, data = data), col = "red") 


## guesses on the test set
predictions = predict(lm_model, newdata = test_data)

# Calculate R-squared on the test set
actuals = test_data$Average.guess
SS_tot = sum((actuals - mean(actuals))^2)
SS_res = sum((actuals - predictions)^2)
R_squared = 1 - (SS_res / SS_tot)
print(paste("R-squared: ", R_squared))

# Alternatively, use postResample to calculate RMSE and R-squared
library(caret)
postResample(predictions, actuals)

# Plot the residuals to check assumptions
residuals = actuals - predictions
plot(residuals, main = "Residuals vs Predicted", ylab = "Residuals", xlab = "Predicted values")
abline(h = 0, col = "red")

##Since the data is non normal we transform

library(MASS)

# Fit initial model to check Box-Cox transformation
boxcox_model = boxcox(lm(Average.guess ~ ScrabbleScore + CV_Ratio + UniqueLetters + 
                            Positional_Score + Bigram + Trigram, 
                          data = data))

# Get the optimal lambda for Box-Cox transformation
lambda_optimal = boxcox_model$x[which.max(boxcox_model$y)]
print(paste("Optimal lambda:", lambda_optimal))


# Apply Box-Cox transformation to the target variable
train_data$transformed_AvgGuess = (train_data$Average.guess^lambda_optimal - 1) / lambda_optimal
test_data$transformed_AvgGuess = (test_data$Average.guess^lambda_optimal - 1) / lambda_optimal

train_data$transformed_AvgGuess = (train_data$Average.guess^lambda_optimal - 1) / lambda_optimal

hist(train_data$transformed_AvgGuess, main = "Transformed Average Guess Distribution", xlab = "Transformed Average Guess", col ="pink")


# Fit linear regression model using the transformed target variable
lm_model = lm(transformed_AvgGuess ~ ScrabbleScore + CV_Ratio + UniqueLetters + 
                 Positional_Score + Bigram + Trigram, 
               data = train_data)

# Display the model summary
summary(lm_model)

# Make predictions on the test set
predictions = predict(lm_model, newdata = test_data)

# Actual values from the test set
actuals = test_data$transformed_AvgGuess

# Calculate R-squared
SS_tot = sum((actuals - mean(actuals))^2)
SS_res = sum((actuals - predictions)^2)
R_squared = 1 - (SS_res / SS_tot)
print(paste("R-squared:", R_squared))

# Calculate RMSE 
library(caret)
postResample(predictions, actuals)

# Inverse Box-Cox transformation
inverse_predictions = (predictions * lambda_optimal + 1)^(1 / lambda_optimal)

# If lambda = 0, use logarithmic inverse transformation
if (lambda_optimal == 0) {
  inverse_predictions <- exp(predictions)
}

# View the inverse predictions
head(inverse_predictions)

# Combine test_data with the predictions for easy lookup
test_data$Predicted_Average_Guess = inverse_predictions

# Add the Word column from original data to scaled_data
scaled_data$Word = data$Wordle.Answer

# Redo the train-test split (assumes train_index is already defined)
train_data = scaled_data[train_index, ]
test_data = scaled_data[-train_index, ]

# ---------------- LINEAR REGRESSION (WITH BOX-COX) ----------------

# Apply Box-Cox transformation to training and testing target
train_data$transformed_AvgGuess = (train_data$Average.guess^lambda_optimal - 1) / lambda_optimal
test_data$transformed_AvgGuess = (test_data$Average.guess^lambda_optimal - 1) / lambda_optimal

# Fit linear regression model using transformed target
lm_model = lm(transformed_AvgGuess ~ ScrabbleScore + CV_Ratio + UniqueLetters + 
                 Positional_Score + Bigram + Trigram, 
               data = train_data)

# Predict and inverse Box-Cox transform
lm_pred = predict(lm_model, newdata = test_data)
lm_inverse_pred = if (lambda_optimal == 0) {
  exp(lm_pred)
} else {
  (lm_pred * lambda_optimal + 1)^(1 / lambda_optimal)
}

# ---------------- RANDOM FOREST ----------------

# Predict on test data
rf_pred = predict(rf_model, newdata = test_data)

# ---------------- COMBINE RESULTS ----------------

# Combine into one table
results_table = test_data[, c("Word", "Average.guess")]
results_table$RF_Prediction = rf_pred
results_table$LM_Prediction = lm_inverse_pred

results_table
# View top results
head(results_table)
results_table$RF_Prediction <- round(results_table$RF_Prediction); results_table$LM_Prediction <- round(results_table$LM_Prediction)
results_table$LM_Prediction <- round(results_table$LM_Prediction); results_table$RF_Prediction <- round(results_table$RF_Prediction)
results_table$Average.guess <- round(results_table$Average.guess); results_table$Average.guess <- round(results_table$Average.guess)


# ---------------- LOOKUP FOR A SPECIFIC WORD ----------------

word_to_check <- ""  # Change as needed
results_table[results_table$Word == word_to_check, ]

library(caret)

# Define 5-fold cross-validation
train_control <- trainControl(method = "cv", number = 5)

# Create a version of train_data with transformed target
train_data_trans <- train_data
train_data_trans$transformed_AvgGuess <- (train_data_trans$Average.guess^lambda_optimal - 1) / lambda_optimal

# Train the model with cross-validation
cv_model <- train(
  transformed_AvgGuess ~ ScrabbleScore + CV_Ratio + UniqueLetters + 
    Positional_Score + Bigram + Trigram,
  data = train_data_trans,
  method = "lm",
  trControl = train_control
)

# Print results
print(cv_model)

# This gives you RMSE, R-squared, and MAE from CV
cv_model$results

##Cross validation for random forest

# Load required library

library(caret)
library(randomForest)
install.packages("doParallel")
library(doParallel)

# Parallel setup
cl <- makePSOCKcluster(2)  # adjust based on your system
registerDoParallel(cl)

ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 2)


# Set up cross-validation method: 10-fold CV repeated 3 times (you can customize this)
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 3,
                     search = "grid")

# Train Random Forest model with cross-validation
set.seed(123)
rf_cv_model <- train(Average.guess ~ ScrabbleScore + CV_Ratio + UniqueLetters + 
                       Positional_Score + Bigram + Trigram,
                     data = scaled_data,
                     method = "rf",
                     trControl = ctrl,
                     tuneLength = 5,
                     ntree = 500,
                     importance = TRUE)

# View the best model and performance metrics
print(rf_cv_model)

# Plot cross-validation results
plot(rf_cv_model)

rf_final_model <- randomForest(Average.guess ~ ScrabbleScore + CV_Ratio + UniqueLetters + 
                                 Positional_Score + Bigram + Trigram,
                               data = scaled_data,
                               mtry = 2,
                               ntree = 500,
                               importance = TRUE)

cv_predictions <- cv_model$pred
rf_cv_predictions <- rf_final_model$pred

str(cv_model$pred)

# Get row indexes from the predictions
cv_predictions_ordered <- cv_predictions[order(cv_predictions$rowIndex), ]
rf_final_predictions_ordered <- rf_final_predictions[order(rf_final_predictions$rowIndex), ]

# Create result table
cv_results_table <- data.frame(
  Word = scaled_data$Word[cv_predictions_ordered$rowIndex],
  Actual = cv_predictions_ordered$obs,
  LM_CV_Pred = cv_predictions_ordered$pred,
  RF_CV_Pred = rf_final_predictions_ordered$pred
)

head(cv_results_table)
