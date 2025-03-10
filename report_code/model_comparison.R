library(brms)
library(bayesplot)
library(ggplot2)
library(xtable)
library(easystats)
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

rmse1 <- model.comp(model1.final, 1, "darkgreen", 1) +
  theme(
    text = element_text(size = 20),  # Increase font size for all text elements
    plot.title = element_text(size = 25)  # Customize title font size and style
  )
rmse2 <- model.comp(model2.final, 2, "navy", 1) +
  theme(
    text = element_text(size = 20),  # Increase font size for all text elements
    plot.title = element_text(size = 25)  # Customize title font size and style
  )
rmse3 <- model.comp(model3.final, 3, "brown", 1) +
  theme(
    text = element_text(size = 20),  # Increase font size for all text elements
    plot.title = element_text(size = 25)  # Customize title font size and style
  )
rmse4 <-model.comp(model4.final, 4, "darkviolet", 1) +
  theme(text = element_text(size = 20),plot.title = element_text(size = 25))


combined_rmse <- ggplot() +
  geom_histogram(data = rmse1$data, aes(x = RMSE, fill = "Model 1", color = "Model 1"), alpha = 0.5) +
  geom_histogram(data = rmse2$data, aes(x = RMSE, fill = "Model 2", color = "Model 2"), alpha = 0.5) +
  geom_histogram(data = rmse3$data, aes(x = RMSE, fill = "Model 3", color = "Model 3"), alpha = 0.5) +
  geom_histogram(data = rmse4$data, aes(x = RMSE, fill = "Model 4", color = "Model 4"), alpha = 0.5) +
  scale_fill_manual(name = "Models", values = c("Model 1" = "darkgreen", "Model 2" = "navy", "Model 3" = "brown", "Model 4" = "darkviolet")) +
  scale_color_manual(name = "Models", values = c("Model 1" = "darkgreen", "Model 2" = "navy", "Model 3" = "brown", "Model 4" = "darkviolet")) +
  theme_minimal() +  # Apply minimal theme
#  ggtitle("RMSE Distribution by Model") +  # Add title
  theme(
    text = element_text(size = 20),  # Increase font size for all text elements
    plot.title = element_text(size = 25)  # Customize title font size and style
  )

# Print the combined plot with legend
print(combined_rmse)



## Log-Likelihood

ll1 <- model.comp(model1.final, 1, "darkgreen", 2) +
  theme(text = element_text(size = 20),plot.title = element_text(size = 25))
ll2 <- model.comp(model2.final, 2, "navy", 2) +
  theme(text = element_text(size = 20),plot.title = element_text(size = 25))
ll3 <- model.comp(model3.final, 3, "brown", 2) +
  theme(text = element_text(size = 20),plot.title = element_text(size = 25))
ll4 <-model.comp(model4.final, 4, "darkviolet", 2) +
  theme(text = element_text(size = 20),plot.title = element_text(size = 25))


combined_loglik <- ggplot() +
  geom_histogram(data = ll1$data, aes(x = log.like, fill = "Model 1", color = "Model 1"), alpha = 0.2) +
  geom_histogram(data = ll2$data, aes(x = log.like, fill = "Model 2", color = "Model 2"), alpha = 0.2) +
  geom_histogram(data = ll3$data, aes(x = log.like, fill = "Model 3", color = "Model 3"), alpha = 0.2) +
  geom_histogram(data = ll4$data, aes(x = log.like, fill = "Model 4", color = "Model 4"), alpha = 0.2) +
  scale_fill_manual(name = "Models", values = c("Model 1" = "darkgreen", "Model 2" = "navy", "Model 3" = "brown", "Model 4" = "darkviolet")) +
  scale_color_manual(name = "Models", values = c("Model 1" = "darkgreen", "Model 2" = "navy", "Model 3" = "brown", "Model 4" = "darkviolet")) +
  theme_minimal() +  # Apply minimal theme
  ggtitle("Log-Likelihood Scores Distribution by Model") +  # Add title
  theme(
    text = element_text(size = 20),  # Increase font size for all text elements
    plot.title = element_text(size = 25)  # Customize title font size and style
  )

# Print the combined plot with legend
print(combined_loglik)

# Calculate pairwise differences
ll_differences <- data.frame(
  Comparison = rep(c("Model 1 - Model 2", "Model 1 - Model 3", "Model 1 - Model 4", "Model 2 - Model 3", "Model 2 - Model 4", "Model 3 - Model 4"), each = nrow(ll1$data)),
  LL_Difference = c(
    ll1$data$log.like - ll2$data$log.like,
    ll1$data$log.like - ll3$data$log.like,
    ll1$data$log.like - ll4$data$log.like,
    ll2$data$log.like - ll3$data$log.like,
    ll2$data$log.like - ll4$data$log.like,
    ll3$data$log.like - ll4$data$log.like
  )
)

# Calculate the mean for each comparison
ll_means <- ll_differences %>%
  group_by(Comparison) %>%
  summarize(Mean = mean(LL_Difference))

# Plot the histograms with facets, mean lines, and fixed scales
difference_histograms <- ggplot(ll_differences, aes(x = LL_Difference)) +
  geom_histogram(binwidth = 0.2, fill = "lightblue", color = "darkblue", alpha = 0.7) +
  geom_vline(data = ll_means, aes(xintercept = 0), color = "black", linetype = "dashed", size = 0.5) +
  geom_vline(data = ll_means, aes(xintercept = Mean), color = "red", linetype = "solid", size = 0.7) +
  facet_wrap(~ Comparison) +
 # scale_x_continuous(limits = c(-1.5, 1.5)) +
 # scale_y_continuous(limits = c(0, 400)) +
  theme_minimal() +
  ggtitle("Histograms of Log-Likelihood Differences by Model") +
  theme(
    text = element_text(size = 20),
    plot.title = element_text(size = 25)
  )

# Print the histogram plots with mean lines and fixed scales
print(difference_histograms)


se_sum <- function(x) {
  sd(x) * sqrt(length(x))
}

LPD <- ll_differences %>%
  group_by(Comparison) %>%
  summarize(LPD_Diff = sum(LL_Difference), SE_LPD_Diff = se_sum(LL_Difference))

print(LPD)

xtable(LPD)


## Posterior Predictive Checks
# color argument does not matter here

pp1 <- model.comp(model1.final, 1, "darkgreen", 4) + labs(title = "Model 1")
pp2 <- model.comp(model2.final, 2, "navy", 4) + labs(title = "Model 2")
pp3 <- model.comp(model3.final, 3, "brown", 4) + labs(title = "Model 3")
pp4 <-model.comp(model4.final, 4, "darkviolet", 4) + labs(title = "Model 4")


combined_pp <- (pp1 + pp2 + pp3 + pp4)


# Print the combined plot with legend
print(combined_pp)




## Posterior distribution of urbanindex

post_ui1 <- model.comp(model1.final, 1, "darkgreen", 3)
post_ui2 <- model.comp(model2.final, 2, "navy", 3)
post_ui3 <- model.comp(model3.final, 3, "brown", 3)
post_ui4 <-model.comp(model4.final, 4, "darkviolet", 3)


combined_post_ui <- ggplot() +
  geom_histogram(data = post_ui1$data, aes(x = ui, fill = "Model 1", color = "Model 1"), alpha = 0.3) +
  geom_histogram(data = post_ui2$data, aes(x = ui, fill = "Model 2", color = "Model 2"), alpha = 0.3) +
  geom_histogram(data = post_ui3$data, aes(x = ui, fill = "Model 3", color = "Model 3"), alpha = 0.3) +
  geom_histogram(data = post_ui4$data, aes(x = ui, fill = "Model 4", color = "Model 4"), alpha = 0.3) +
  scale_fill_manual(name = "Models", values = c("Model 1" = "darkgreen", "Model 2" = "navy", "Model 3" = "brown", "Model 4" = "darkviolet")) +
  scale_color_manual(name = "Models", values = c("Model 1" = "darkgreen", "Model 2" = "navy", "Model 3" = "brown", "Model 4" = "darkviolet")) +
  labs(x = "urbanindex") +
  theme_minimal() +  # Apply minimal theme
 # ggtitle("Urban Index Coefficient Posterior Distribution by Model") +  # Add title
  theme(
    text = element_text(size = 20),  # Increase font size for all text elements
    plot.title = element_text(size = 25)  # Customize title font size and style
  )

# Print the combined plot with legend
print(combined_post_ui)







## Leave one out
loo1 <-loo(model1.final)

loo2 <- loo(model2.final)

loo3 <- loo(model3.final)

loo4 <- loo(model4.final)

print(loo1)
print(loo2)
print(loo3)
print(loo4)
xtable(loo_compare(loo1, loo2,loo3, loo4))



## Leave one out, moment matching
loom1 <-loo(model1.final, moment_match = TRUE)

xtable(loo_compare(loom1, loo2,loo3, loo4))



### Bayes Factors

bayes_factor(model1.final, model2.final, log = TRUE)
bayes_factor(model1.final, model3.final, log = TRUE)
bayes_factor(model1.final, model4.final, log = TRUE)
bayes_factor(model2.final, model3.final, log = TRUE)
bayes_factor(model2.final, model4.final, log = TRUE)
bayes_factor(model3.final, model4.final, log = TRUE)



