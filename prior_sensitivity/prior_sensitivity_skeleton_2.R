rm(list = ls())
df <- read.csv('C:/Users/anarm/OneDrive/Documents/R Scripts/bayesian/data_with_region_indices.csv')[,-1]

library(ggplot2)
library(dplyr)
library(tibble)
library(brms)
library(tidybayes)
library(stargazer)

set.seed(1234)


df_scaled <- df
df_scaled$Total.Population <- df_scaled$Total.Population/1000000
df_scaled$Median.Household.Income <- df_scaled$Median.Household.Income/100000



#1. 
###region level only -  single level varying slope model
form1 <- as.formula('Winning.party ~ 1 + urbanindex + Percentage.Retirees..65.. +  (0 + urbanindex|| Region)')
int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -2, 1)', class = 'b', coef = 'Percentage.Retirees..65..')

sd_urb_prior <- set_prior('cauchy(0,10)', class = 'sd')


vary_slope_fit <- brm(form1, family = 'bernoulli',  
                      prior = int_prior + urb_prior + pct_ret_prior + 
                        sd_urb_prior, chains = 2, iter = 2000,
                      data = df_scaled)


#2. 
###region level only -  single level varying slope model
form1 <- as.formula('Winning.party ~ 1 + urbanindex + Percentage.Retirees..65.. +  (0 + urbanindex|| Region)')
int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -6, 1)', class = 'b', coef = 'Percentage.Retirees..65..')

sd_urb_prior <- set_prior('cauchy(0,10)', class = 'sd')


vary_slope_fit <- brm(form1, family = 'bernoulli',  
                      prior = int_prior + urb_prior + pct_ret_prior + 
                        sd_urb_prior, chains = 2, iter = 2000,
                      data = df_scaled)


#3. 
###region level only -  single level varying slope model
form1 <- as.formula('Winning.party ~ 1 + urbanindex + Percentage.Retirees..65.. +  (0 + urbanindex|| Region)')
int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -2, 1)', class = 'b', coef = 'Percentage.Retirees..65..')

sd_urb_prior <- set_prior('gamma(5,2)', class = 'sd')


vary_slope_fit <- brm(form1, family = 'bernoulli',  
                      prior = int_prior + urb_prior + pct_ret_prior + 
                        sd_urb_prior, chains = 2, iter = 2000,
                      data = df_scaled)

#4. 
###region level only -  single level varying slope model
form1 <- as.formula('Winning.party ~ 1 + urbanindex + Percentage.Retirees..65.. +  (0 + urbanindex|| Region)')
int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -2, 1)', class = 'b', coef = 'Percentage.Retirees..65..')

sd_urb_prior <- set_prior('gamma(2, 0.5)', class = 'sd')


vary_slope_fit <- brm(form1, family = 'bernoulli',  
                      prior = int_prior + urb_prior + pct_ret_prior + 
                        sd_urb_prior, chains = 2, iter = 2000,
                      data = df_scaled)


#5. 
###region level only -  single level varying slope model
form1 <- as.formula('Winning.party ~ 1 + urbanindex + Percentage.Retirees..65.. +  (0 + urbanindex|| Region)')
int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -2, 1)', class = 'b', coef = 'Percentage.Retirees..65..')

sd_urb_prior <- set_prior('gamma(2, 2)', class = 'sd')


vary_slope_fit <- brm(form1, family = 'bernoulli',  
                      prior = int_prior + urb_prior + pct_ret_prior + 
                        sd_urb_prior, chains = 2, iter = 2000,
                      data = df_scaled)


#6. 
###region level only -  single level varying slope model
form1 <- as.formula('Winning.party ~ 1 + urbanindex + Percentage.Retirees..65.. +  (0 + urbanindex|| Region)')
int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -6, 1)', class = 'b', coef = 'Percentage.Retirees..65..')

sd_urb_prior <- set_prior('gamma(5, 2)', class = 'sd')


vary_slope_fit <- brm(form1, family = 'bernoulli',  
                      prior = int_prior + urb_prior + pct_ret_prior + 
                        sd_urb_prior, chains = 2, iter = 2000,
                      data = df_scaled)


#7. 
###region level only -  single level varying slope model
form1 <- as.formula('Winning.party ~ 1 + urbanindex + Percentage.Retirees..65.. +  (0 + urbanindex|| Region)')
int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -6, 1)', class = 'b', coef = 'Percentage.Retirees..65..')

sd_urb_prior <- set_prior('gamma(2, 0.5)', class = 'sd')


vary_slope_fit <- brm(form1, family = 'bernoulli',  
                      prior = int_prior + urb_prior + pct_ret_prior + 
                        sd_urb_prior, chains = 2, iter = 2000,
                      data = df_scaled)


#8. 
###region level only -  single level varying slope model
form1 <- as.formula('Winning.party ~ 1 + urbanindex + Percentage.Retirees..65.. +  (0 + urbanindex|| Region)')
int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -6, 1)', class = 'b', coef = 'Percentage.Retirees..65..')

sd_urb_prior <- set_prior('gamma(2, 2)', class = 'sd')


vary_slope_fit <- brm(form1, family = 'bernoulli',  
                      prior = int_prior + urb_prior + pct_ret_prior + 
                        sd_urb_prior, chains = 2, iter = 2000,
                      data = df_scaled)











