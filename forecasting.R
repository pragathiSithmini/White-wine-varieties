# Import Required Libraries
library(readxl)
library(neuralnet)
library(caTools)
library(MLmetrics)
library(Metrics)
library(xlsx)

# Create Predicted and Actual Table
predicted_actual <- function (expected, mlp_test) {
  expectedTest <- cbind(expected, as.data.frame(mlp_test$net.result))
  colnames(expectedTest) <- c("Expected Output", "NeuralNetwork Output")
  return(expectedTest)
}

# Evaluation Function
rmse <- function (actual, predicted) 
{
  RMSE <- sqrt(mean((actual - predicted)^2))
  return(RMSE)
}
evaluation_function <- function(actual,predict) {
  rmse_mlp <- rmse(actual = actual, predicted = predict)
  mae_mlp <- Metrics::mae(actual = actual, predicted = predict)
  mape_mlp <- MAPE(y_pred = predict, y_true = actual)
  return(c(rmse_mlp, mae_mlp, mape_mlp))
}

# Import Processed Dataset
power_usage <- read_xlsx("./powerUsage-scaled.xlsx")

# # For 9th Hour and 10th Hour (Note: Run the both file individually to get the input as 9th hour and 11th hour)
# power_usage <- read_xlsx("./powerUsage-scaled-09.xlsx")
# power_usage <- read_xlsx("./powerUsage-scaled-10.xlsx")

# Split Data
set.seed(123)
splitRule <- sample(seq_len(nrow(powerOutage)), size = 430)
train_set <- power_usage[splitRule, ]
test_set <- power_usage[-splitRule, ]
dim(train_set)
dim(test_set)

# Define X values of test data
x_test_set <- test_set[-1]

# Define Y values of test data
y_test_set <- test_set[1]

# Neural Networks - Configuration 1
relation_1 <- as.formula("original~v2+v3+v4+v5+v6")
mlp_1 <- neuralnet(formula = relation_1, data = train,hidden = c(4,3), stepmax = 1e+10)
plot(mlp_1)

# Make prediction on X test data
y_pred_1 <- neuralnet::compute(mlp_1, x_test_set)
evaluationData_1 <- predicted_actual(y_test_set, y_pred_1)
mlpTestResult_1 <- evaluation_function(actual = evaluationData_1$`Expected Output`, predict = evaluationData_1$`NeuralNetwork Output`)

# Neural Networks - Configuration 2
relation_2 <- as.formula("original~v2+v3+v4+v5+v6")
mlp_2 <- neuralnet(formula = relation_2, data = train,hidden = c(4,3), stepmax = 1e+10, learningrate = 0.001)
plot(mlp_2)

# Make prediction on X test data
y_pred_2 <- neuralnet::compute(mlp_2, x_test_set)
evaluationData_2 <- predicted_actual(y_test_set, y_pred_2)
mlpTestResult_2 <- evaluation_function(actual = evaluationData_2$`Expected Output`, predict = evaluationData_2$`NeuralNetwork Output`)

# Neural Networks - Configuration 3
relation_3 <- as.formula("original~v2+v3+v4+v5+v6")
mlp_3 <- neuralnet(formula = relation_3, data = train,hidden = c(3,2), stepmax = 1e+10)
plot(mlp_3)

# Make prediction on X test data
y_pred_3 <- neuralnet::compute(mlp_3, x_test_set)
evaluationData_3 <- predicted_actual(y_test_set, y_pred_3)
mlpTestResult_3 <- evaluation_function(actual = evaluationData_3$`Expected Output`, predict = evaluationData_3$`NeuralNetwork Output`)

# Neural Networks - Configuration 4
relation_4 <- as.formula("original~v2+v3+v4+v5+v6")
mlp_4 <- neuralnet(formula = relation_4, data = train,hidden = c(5,2), stepmax = 1e+10, learningrate = 0.001)
plot(mlp_4)

# Make prediction on X test data
y_pred_4 <- neuralnet::compute(mlp_4, x_test_set)
evaluationData_4 <- predicted_actual(y_test_set, y_pred_4)
mlpTestResult_4 <- evaluation_function(actual = evaluationData_4$`Expected Output`, predict = evaluationData_4$`NeuralNetwork Output`)

# Neural Networks - Configuration 5
train_0 <- train_set[1:4]
relation_5 <- as.formula("original~v2+v3+v4")
mlp_5 <- neuralnet(formula = relation_5, data = train_0,hidden = c(4,3), stepmax = 1e+10, learningrate = 0.001)
plot(mlp_5)

x_test_0 <- test_set[2:4]

# Make prediction on X test data
# (Note: First try with configuration 1 and run this 98:100 and then 2, 3, 4, 5)
y_pred_5 <- neuralnet::compute(mlp5, x_test_0)
evaluationData_5 <- predicted_actual(y_test_set, y_pred_5)
mlpTestResult_5 <- evaluation_function(actual = evaluationData_5$`Expected Output`, predict = evaluationData_5$`NeuralNetwork Output`)

comp <- rbind(mlpTestResult_1, mlpTestResult_2, mlpTestResult_3, mlpTestResult_4, mlpTestResult_5)
colnames(comp) <- c("RMSE","MAE","MAPE")
rownames(comp) <- c("config-1","config-2","config-3","config-4","config-5")
comp

write.xlsx(comp,
           file = "./prowerUsage-comparison.xlsx",
           col.names = TRUE, append = TRUE, row.names = TRUE)
