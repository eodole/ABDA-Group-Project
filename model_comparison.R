library(brms)
library(bayesplot)
library(ggplot2)
# model comparison 

# model number is what it says it is , and the model color should be a unique color 
# associated with that model, for graphing
# you can use standard ggplot color strings
model.comp <- function(model, model_number, model_color){

  # post predictive check 
  pp_check(model, type = "dens_overlay")
  
  # R2
  print(bayes_R2(model))
  
  #rmse 
  model_error <- predictive_error(model, method = "posterior_epred")
  rmse_model <- sqrt(rowMeans(model_error^2))
  
  df <- data.frame(
    RMSE = rmse_model
  )
  
  p <- df %>% ggplot( aes(x = RMSE) ) +
    geom_histogram(bins =15, color = model_color, fill = "white") + 
    labs(title = paste0("RMSE Histogram for Model ", model_number)) +
    theme_minimal()
  
  print(p)
  
  
  # log likelihood of the data 
  llm_model <- colMeans(log_lik(model))
  
  df <- data.frame(
    log.like = llm_model
  )
  
  q <- df %>% ggplot( aes(x = log.like) ) +
    geom_histogram(bins =15, color = model_color, fill = "white") + 
    labs(title = paste0("Log-Likelihood Histogram for Model ", model_number), xlab = "Log Likelihood") +
    theme_minimal()
  
  print(q)
}
