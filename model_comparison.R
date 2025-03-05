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
  return(pp_check(model, type = "bars", ndraws = NULL))
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
    geom_histogram(bins =15, color = model_color, fill = model_color, alpha = 0.5) + 
    labs(title = paste0("Model ", model_number)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  if(graph_num ==1){
    return(p)
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
    return(q)
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
    return(r)
  }
  

}


## RMSE

rmse1 <- model.comp(model1.final, 1, "darkgreen", 1)
rmse2 <- model.comp(model2.final, 2, "navy", 1)
rmse3 <- model.comp(model3.final, 3, "brown", 1)
rmse4 <-model.comp(model4.final, 4, "darkviolet", 1)


combined_rmse <- ggplot() +
  geom_histogram(data = rmse1$data, aes(x = RMSE, fill = "Model 1", color = "Model 1"), alpha = 0.5) +
  geom_histogram(data = rmse2$data, aes(x = RMSE, fill = "Model 2", color = "Model 2"), alpha = 0.5) +
  geom_histogram(data = rmse3$data, aes(x = RMSE, fill = "Model 3", color = "Model 3"), alpha = 0.5) +
  geom_histogram(data = rmse4$data, aes(x = RMSE, fill = "Model 4", color = "Model 4"), alpha = 0.5) +
  scale_fill_manual(name = "Models", values = c("Model 1" = "darkgreen", "Model 2" = "navy", "Model 3" = "brown", "Model 4" = "darkviolet")) +
  scale_color_manual(name = "Models", values = c("Model 1" = "darkgreen", "Model 2" = "navy", "Model 3" = "brown", "Model 4" = "darkviolet")) +
  theme_minimal() +  # Apply minimal theme
  ggtitle("RMSE Distribution by Model") +  # Add title
  theme(
    text = element_text(size = 20),  # Increase font size for all text elements
    plot.title = element_text(size = 25, face = "bold")  # Customize title font size and style
  )

# Print the combined plot with legend
print(combined_rmse)



## Log-Likelihood

ll1 <- model.comp(model1.final, 1, "darkgreen", 2)
ll2 <- model.comp(model2.final, 2, "navy", 2)
ll3 <- model.comp(model3.final, 3, "brown", 2)
ll4 <-model.comp(model4.final, 4, "darkviolet", 2)


combined_loglik <- ggplot() +
  geom_histogram(data = ll1$data, aes(x = log.like, fill = "white", color = "Model 1"), alpha = 0) +
  geom_histogram(data = ll2$data, aes(x = log.like, fill = "white", color = "Model 2"), alpha = 0) +
  geom_histogram(data = ll3$data, aes(x = log.like, fill = "white", color = "Model 3"), alpha = 0) +
  geom_histogram(data = ll4$data, aes(x = log.like, fill = "white", color = "Model 4"), alpha = 0) +
  scale_fill_manual(name = "Models", values = c("Model 1" = "darkgreen", "Model 2" = "navy", "Model 3" = "brown", "Model 4" = "darkviolet")) +
  scale_color_manual(name = "Models", values = c("Model 1" = "darkgreen", "Model 2" = "navy", "Model 3" = "brown", "Model 4" = "darkviolet")) +
  theme_minimal() +  # Apply minimal theme
  ggtitle("Log-Likelihood Scores Distribution by Model") +  # Add title
  theme(
    text = element_text(size = 20),  # Increase font size for all text elements
    plot.title = element_text(size = 25, face = "bold")  # Customize title font size and style
  )

# Print the combined plot with legend
print(combined_loglik)

# Calculate pairwise differences
differences <- data.frame(
  Comparison = rep(c("ll2 - ll1", "ll3 - ll1", "ll4 - ll1", "ll3 - ll2", "ll4 - ll2", "ll4 - ll3"), each = nrow(ll1$data)),
  Difference = c(
    ll2$data$log.like - ll1$data$log.like,
    ll3$data$log.like - ll1$data$log.like,
    ll4$data$log.like - ll1$data$log.like,
    ll3$data$log.like - ll2$data$log.like,
    ll4$data$log.like - ll2$data$log.like,
    ll4$data$log.like - ll3$data$log.like
  )
)

# Calculate the mean for each comparison
means <- differences %>%
  group_by(Comparison) %>%
  summarize(Mean = mean(Difference))

# Plot the histograms with facets, mean lines, and fixed scales
difference_histograms <- ggplot(differences, aes(x = Difference)) +
  geom_histogram(bins = 50, fill = "lightblue", color = "darkblue", alpha = 0.7) +
 geom_vline(data = means, aes(xintercept = Mean), color = "black", linetype = "solid", size = 0.5) +
  facet_wrap(~ Comparison) +
 # scale_x_continuous(limits = c(-1.5, 1.5)) +
 # scale_y_continuous(limits = c(0, 400)) +
  theme_minimal() +
  ggtitle("Histograms of Log-Likelihood Differences by Model Comparison") +
  theme(
    text = element_text(size = 16),
    plot.title = element_text(size = 20, face = "bold")
  )

# Print the histogram plots with mean lines and fixed scales
print(difference_histograms)

differences %>%
  group_by(Comparison) %>%
  summarize(LPD = sum(Difference))





## Posterior draws

pp1 <- model.comp(model1.final, 1, "darkgreen", 4)
pp2 <- model.comp(model2.final, 2, "navy", 4)
pp3 <- model.comp(model3.final, 3, "brown", 4)
pp4 <-model.comp(model4.final, 4, "darkviolet", 4)


combined_rmse <- ggplot() +
  geom_histogram(data = rmse1$data, aes(x = RMSE, fill = "Model 1", color = "Model 1"), alpha = 0.5) +
  geom_histogram(data = rmse2$data, aes(x = RMSE, fill = "Model 2", color = "Model 2"), alpha = 0.5) +
  geom_histogram(data = rmse3$data, aes(x = RMSE, fill = "Model 3", color = "Model 3"), alpha = 0.5) +
  geom_histogram(data = rmse4$data, aes(x = RMSE, fill = "Model 4", color = "Model 4"), alpha = 0.5) +
  scale_fill_manual(name = "Models", values = c("Model 1" = "darkgreen", "Model 2" = "navy", "Model 3" = "brown", "Model 4" = "darkviolet")) +
  scale_color_manual(name = "Models", values = c("Model 1" = "darkgreen", "Model 2" = "navy", "Model 3" = "brown", "Model 4" = "darkviolet")) +
  theme_minimal() +  # Apply minimal theme
  ggtitle("RMSE Distribution by Model") +  # Add title
  theme(
    text = element_text(size = 20),  # Increase font size for all text elements
    plot.title = element_text(size = 25, face = "bold")  # Customize title font size and style
  )

# Print the combined plot with legend
print(combined_rmse)




## Leave one out 
loom1 <-loo(model1.final, moment_match = TRUE)

loom2 <- loo(model2.final, moment_match = TRUE)

loom3 <- loo(model3.final, moment_match = TRUE)

loom4 <- loo(model4.final, moment_match = TRUE)


stargazer(loo_compare(loom1, loom2,loom3, loom4))




