rm(list = ls())
df <-read.csv(file = 'C:/Users/anarm/OneDrive/Documents/R Scripts/bayesian/numeric_df_without_new.csv')

##set up the regression formulas to work with 
# Specify the columns to exclude
exclude_cols <- c("stcd", "state_code", "district", "grouping", 'X')

# Create a formula dynamically excluding the specified columns
full_form <- as.formula(paste("Winning.party ~", paste(setdiff(colnames(df), c("Winning.party", exclude_cols)), collapse = " + ")))

sparse_form <- as.formula('Winning.party ~ urbanindex')

lm(sparse_form, df)
lm(Winning.party ~ 1, df)

library(rstan)

sdata <- list(
  y = df$Winning.party,
  x = df$urbanindex,
  N = nrow(df)
)

mod1 <- stan(file = 'simple_lin_reg.stan', data = sdata) #simple linear regression with normally dist. numeric response

#maybe we can select better priors 


traceplot(mod1, pars = c('alpha', 'beta', 'sigma'), inc_warmup = TRUE)
stan_dens(mod1, pars = c('alpha', 'beta', 'sigma'))

## bernoulli modelling - here we explicitly consider the binary response


mod2 <- stan(file = 'bernoulli_logit.stan', data = sdata) #simple linear regression with normally dist. numeric response

traceplot(mod2, pars = c('alpha', 'beta'), inc_warmup = TRUE)
stan_dens(mod2, pars = c('alpha', 'beta'))


print(mod2)



# Extract posterior samples
# Generate new x values for prediction
x_new <- seq(min(sdata$x), max(sdata$x), length.out = 100)

prediction_df <- data.frame(
  x_new = x_new,
  p_pred_mean = p_pred_mean,
  p_pred_lower = p_pred_lower,
  p_pred_upper = p_pred_upper
)

# Generate posterior predictive probabilities
n_samples <- length(alpha_samples)
p_pred <- matrix(0, nrow = n_samples, ncol = length(x_new))

for (i in 1:n_samples) {
  logit_p <- alpha_samples[i] + beta_samples[i] * x_new
  p_pred[i, ] <- 1 / (1 + exp(-logit_p))  # Inverse logit
}

# Calculate 95% credible intervals for the probabilities
p_pred_mean <- apply(p_pred, 2, mean)
p_pred_lower <- apply(p_pred, 2, quantile, probs = 0.025)
p_pred_upper <- apply(p_pred, 2, quantile, probs = 0.975)


library(ggplot2)
ggplot(prediction_df, aes(x = x_new)) +
  geom_ribbon(aes(ymin = p_pred_lower, ymax = p_pred_upper), fill = "blue", alpha = 0.2) +
  geom_line(aes(y = p_pred_mean), color = "blue") +
  geom_point(data = df, aes(x = urbanindex, y = Winning.party), color = "red", alpha = 0.6, position = position_jitter(height = 0.02)) +
  labs(title = "Posterior Predictions with 95% Credible Interval", x = "x", y = "Predicted Probability") +
  theme_minimal()

#no brms models here

