rm(list = ls())
df <- read.csv('C:/Users/anarm/OneDrive/Documents/R Scripts/bayesian/data_with_region_indices.csv')[,-1]


library(ggplot2)
library(dplyr)
library(tibble)
library(brms)
library(tidybayes)
library(stargazer)

colnames(df)
form1 <- as.formula('Winning.party ~ 1 + urbanindex + (1|region_index)')

form2 <- as.formula('Winning.party ~ 1 + urbanindex + Total.Population + Percentage.Women + 
                    + Median.Household.Income + Median.Age + Percentage.Retirees..65.. + 
                    + Percentage.Bachelors.Degree.in.Population.over.25 + (1|region_index)')

fit1 <- brm(form1, df, family = 'bernoulli')
fit2 <- brm(form2, df, family =  'bernoulli')

bench_form1 <- as.formula('Winning.party ~ 1 + urbanindex')
bench_form2 <- as.formula('Winning.party ~ 1 + urbanindex + Total.Population + Percentage.Women + 
                            + Median.Household.Income + Median.Age + Percentage.Retirees..65.. + 
                            + Percentage.Bachelors.Degree.in.Population.over.25')


plot(fit1)
plot(fit1, variable = "Intercept")

summary(fit1)


#i am interested in the posterior densities and predictions for now

get_variables(fit1) #b indicates global coefficient, r region index the groupwise coefficients



posterior_samples <- fit1 %>%
  spread_draws(r_region_index[Region,]) 

#let us limit our analysis to one chain for now 
posterior_samples <- posterior_samples %>% 
  filter(.chain == 1)

hist(posterior_samples$r_region_index)


# Generate histograms for each region
par(mfrow = c(2, 2))  # Set up the plot area to fit 4 plots

for (i in 1:4) {
  region_data <- subset(posterior_samples, Region == i)
  hist(region_data$r_region_index,
       main = paste("Region", i, "Histogram"),
       xlab = "r_region_index",
       col = "lightblue",
       border = "black")
}


#generate summary statistics

summ_stats <- as.data.frame(posterior_samples %>% 
  summarise_draws)

stargazer(summ_stats, summary = FALSE)


##next step - posterior group wise predictions


plot(df$urbanindex, df$Winning.party) #global 




bench_fit2 <- brm(bench_form2, df, family =  'bernoulli')
get_variables(bench_fit2)

plot(bench_fit2, combo = c('trace', 'hist'))


plot(fit2, combo = c('trace', 'hist'))





post_pred_bench_fit2 <- brms::posterior_epred(bench_fit2)



plot(df$urbanindex, df$Winning.party)
lines(df$urbanindex, colMeans(post_pred_bench_fit2), type = 'p')



plot_region_with_posterior <- function(region, df, post_pred_matrix) {
  # Filter the dataframe for the specific region
  subset_df <- df[df$Region == region, ]
  
  # Calculate the posterior means for this region
  # Assuming post_pred_matrix has rows corresponding to posterior samples and columns matching df rows
  region_indices <- which(df$Region == region)
  region_post_pred <- post_pred_matrix[, region_indices]
  
  # Calculate column-wise means for this region's posterior predictions
  posterior_means <- colMeans(region_post_pred)
  
  # Combine the subset dataframe with posterior means for plotting
  subset_df$PosteriorMeans <- posterior_means
  
  # Create the plot with ggplot2
  ggplot(subset_df, aes(x = urbanindex, y = Winning.party)) +
    geom_point(color = "gray", size = 2) +  # Original data points
    geom_point(aes(y = PosteriorMeans), color = "blue", size = 1) +  # Posterior means
    labs(
      title = paste("Urban Index vs Winning Party in Region:", region),
      x = "Urban Index",
      y = "Winning Party (DEM = 1)"
    ) +
    theme_minimal()
}

# Generate the plots for each region
plots <- lapply(unique(df$Region), function(region) {
  plot_region_with_posterior(region, df, post_pred_bench_fit2)
})

# Display all plots using gridExtra to arrange them
do.call(grid.arrange, plots)


as.data.frame(prior_summary(bench_fit2))


conditional_effects(bench_fit2)



#####priors 


intercept_prior <- set_prior('normal(0, 5)', class = 'b')


fit3 <- brm(form2, family = 'bernoulli', prior = intercept_prior, data = df)


conditional_effects(fit3)

plot(fit3, combo = c('trace', 'hist'))








