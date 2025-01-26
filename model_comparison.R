library(brms)
library(bayesplot)
library(ggplot2)
# model comparison 

# model number is what it says it is , and the model color should be a unique color 
# associated with that model, for graphing
# you can use standard ggplot color strings
model.comp <- function(model, model_number, model_color, graph_num){

  # post predictive check 
  if(graph_num==4){
  pp_check(model, type = "dens_overlay")
  }
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
    labs(title = paste0("Model ", model_number)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  if(graph_num ==1){
    print(p)
  }
 
  
  
  # log likelihood of the data 
  llm_model <- colMeans(log_lik(model))
  
  df <- data.frame(
    log.like = llm_model
  )
  
  q <- df %>% ggplot( aes(x = log.like) ) +
    geom_histogram(bins =15, color = model_color, fill = "white") + 
    labs(title = paste0("Model ", model_number), x = "Log Likelihood") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  if(graph_num==2){
    print(q)
  }
  
  # posterior distribution of urbanindex 
  posts <- as_draws(model)
  ui_post <- c(posts$`1`$b_urbanindex,posts$`2`$b_urbanindex,posts$`3`$b_urbanindex,posts$`4`$b_urbanindex)

  df <- data.frame(
    ui = ui_post
  )
  
  
  r <- df %>% ggplot( aes( x = ui) ) +
    geom_histogram(bins =30, color = model_color, fill = "white") +
    labs(title = paste0("Model", model_number), x = "Parameter Value" ) +
    theme_minimal()+ 
    theme(plot.title = element_text(hjust = 0.5))
  
  if(graph_num==3){
    print(r)
  }
  

}

loom1 <-loo(model1.final)

loom2 <- loo(model2.final)

loom3 <- loo(model3.final)

loom4 <- loo(model4.final)


stargazer(loo_compare(loom1, loom2,loom3, loom4))

## Leave one out 