library(shiny)
library(plotly)




#log mod
logistic_model <- glm(renewal_decision ~ total_logins + feature_a_time_spent + feature_b_time_spent + feature_c_time_spent + feature_d_time_spent + highest_consecutive_weekly_login_streak, 
                      data = df, family = binomial(link = "logit"))
print(paste("Log Model accuracy:", log_accuracy))
log_accuracy_percent = log_accuracy*100
tidy_logistic_model

#randomforest
best_randomForest_model
best_randomForest_accuracy_percent = best_randomForest_accuracy*100
importance_df

#torch
torch_model
cat("torch Accuracy:", as.numeric(torch_accuracy), "\n")
plot_heatmap(weights_fc1, "Layer 1")
torch_accuracy_percent = round(as.numeric(torch_accuracy)*100,1)

#df
df_test_log$torch_preds = torch_df$torch_predictions
df_test_log$randomforest_preds = random_forest_df$randomforestpreds
df_test_log <- df_test_log %>%
  rowwise() %>%
  mutate(all_preds = ifelse(any(c(predicted_renewal, torch_preds, randomforest_preds) == 0), 0, 1)) %>%
  ungroup()



##############################################

library(shiny)
library(ggplot2)
library(tidyr)
library(coefplot)
library(dplyr)

accuracy_df <- data.frame(
  Model = c("Logistic Regression", "Random Forest Regression", "Deep Learning (PyTorch)"),
  Accuracy = c(log_accuracy_percent, best_randomForest_accuracy_percent, torch_accuracy_percent))
  
# Create the heatmap plotting function
plot_heatmap <- function(weights, layer_name) {
  weights_array <- as.array(weights)
  weights_matrix <- matrix(weights_array, ncol = ncol(weights_array))
  df <- as.data.frame(weights_matrix)
  df$y <- 1:nrow(df)
  df <- tidyr::gather(df, x, value, -y)
  df$x <- as.numeric(gsub("V", "", df$x))
  
  ggplot(df, aes(x = x, y = y, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
    labs(title = paste("Weights Heatmap -", layer_name), x = "Neurons", y = "Features") +
    theme_minimal()
}
