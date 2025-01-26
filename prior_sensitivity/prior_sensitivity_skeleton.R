rm(list = ls())
df <- read.csv('C:/Users/anarm/OneDrive/Documents/R Scripts/bayesian/data_with_region_indices.csv')[,-1]
df <- read.csv("./data/data_with_region_indices.csv")
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



#1. (model 3)
###state level only -  single level varying slope model
form1 <- as.formula('Winning.party ~ 1 + urbanindex + Percentage.Retirees..65.. +  (0 + urbanindex|| State)')
int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -2, 1)', class = 'b', coef = 'Percentage.Retirees..65..')

sd_urb_prior <- set_prior('cauchy(0,10)', class = 'sd')


vary_slope_fit <- brm(form1, family = 'bernoulli',  
                      prior = int_prior + urb_prior + pct_ret_prior + 
                        sd_urb_prior, chains = 4, iter = 2000,
                      data = df_scaled)

model.comp(vary_slope_fit, 3, "lightblue")

#2. 
###state level only -  single level varying slope model
form1 <- as.formula('Winning.party ~ 1 + urbanindex + Percentage.Retirees..65.. +  (0 + urbanindex|| State)')
int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -6, 1)', class = 'b', coef = 'Percentage.Retirees..65..')

sd_urb_prior <- set_prior('cauchy(0,10)', class = 'sd')


vary_slope_fit <- brm(form1, family = 'bernoulli',  
                      prior = int_prior + urb_prior + pct_ret_prior + 
                        sd_urb_prior, chains = 2, iter = 2000,
                      data = df_scaled)


#3. 
###state level only -  single level varying slope model
form1 <- as.formula('Winning.party ~ 1 + urbanindex + Percentage.Retirees..65.. +  (0 + urbanindex|| State)')
int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -2, 1)', class = 'b', coef = 'Percentage.Retirees..65..')

sd_urb_prior <- set_prior('gamma(5,2)', class = 'sd')


vary_slope_fit <- brm(form1, family = 'bernoulli',  
                      prior = int_prior + urb_prior + pct_ret_prior + 
                        sd_urb_prior, chains = 2, iter = 2000,
                      data = df_scaled)

#4. 
###state level only -  single level varying slope model
form1 <- as.formula('Winning.party ~ 1 + urbanindex + Percentage.Retirees..65.. +  (0 + urbanindex|| State)')
int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -2, 1)', class = 'b', coef = 'Percentage.Retirees..65..')

sd_urb_prior <- set_prior('gamma(2, 0.5)', class = 'sd')


vary_slope_fit <- brm(form1, family = 'bernoulli',  
                      prior = int_prior + urb_prior + pct_ret_prior + 
                        sd_urb_prior, chains = 2, iter = 2000,
                      data = df_scaled)


#5. 
###state level only -  single level varying slope model
form1 <- as.formula('Winning.party ~ 1 + urbanindex + Percentage.Retirees..65.. +  (0 + urbanindex|| State)')
int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -2, 1)', class = 'b', coef = 'Percentage.Retirees..65..')

sd_urb_prior <- set_prior('gamma(2, 2)', class = 'sd')


vary_slope_fit <- brm(form1, family = 'bernoulli',  
                      prior = int_prior + urb_prior + pct_ret_prior + 
                        sd_urb_prior, chains = 2, iter = 2000,
                      data = df_scaled)


#6. 
###state level only -  single level varying slope model
form1 <- as.formula('Winning.party ~ 1 + urbanindex + Percentage.Retirees..65.. +  (0 + urbanindex|| State)')
int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -6, 1)', class = 'b', coef = 'Percentage.Retirees..65..')

sd_urb_prior <- set_prior('gamma(5, 2)', class = 'sd')


vary_slope_fit <- brm(form1, family = 'bernoulli',  
                      prior = int_prior + urb_prior + pct_ret_prior + 
                        sd_urb_prior, chains = 2, iter = 2000,
                      data = df_scaled)


#7. 
###state level only -  single level varying slope model
form1 <- as.formula('Winning.party ~ 1 + urbanindex + Percentage.Retirees..65.. +  (0 + urbanindex|| State)')
int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -6, 1)', class = 'b', coef = 'Percentage.Retirees..65..')

sd_urb_prior <- set_prior('gamma(2, 0.5)', class = 'sd')


vary_slope_fit <- brm(form1, family = 'bernoulli',  
                      prior = int_prior + urb_prior + pct_ret_prior + 
                        sd_urb_prior, chains = 2, iter = 2000,
                      data = df_scaled)


#8. 
###state level only -  single level varying slope model
form1 <- as.formula('Winning.party ~ 1 + urbanindex + Percentage.Retirees..65.. +  (0 + urbanindex|| State)')
int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -6, 1)', class = 'b', coef = 'Percentage.Retirees..65..')

sd_urb_prior <- set_prior('gamma(2, 2)', class = 'sd')


vary_slope_fit <- brm(form1, family = 'bernoulli',  
                      prior = int_prior + urb_prior + pct_ret_prior + 
                        sd_urb_prior, chains = 2, iter = 2000,
                      data = df_scaled)


#9. ( model number 2)
###tiny model - two level varying slope model 

form3 <- as.formula('Winning.party ~ 1 + urbanindex + Percentage.Retirees..65.. +  (0 + urbanindex | State / Region)')

int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -2, 1)', class = 'b', coef = 'Percentage.Retirees..65..')

sd_urb_prior <- set_prior('cauchy(0,10)', class = 'sd')


two_level_vary_slope_fit <- brm(form3, family = 'bernoulli',  
                                prior = int_prior + urb_prior + pct_ret_prior + 
                                  sd_urb_prior, chains = 4, iter = 2000,
                                data = df_scaled)


model.comp(two_level_vary_slope_fit)

#10. 
###tiny model - two level varying slope model 

form3 <- as.formula('Winning.party ~ 1 + urbanindex + Percentage.Retirees..65.. +  (0 + urbanindex | State / Region)')

int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -6, 1)', class = 'b', coef = 'Percentage.Retirees..65..')

sd_urb_prior <- set_prior('cauchy(0,10)', class = 'sd')


two_level_vary_slope_fit <- brm(form3, family = 'bernoulli',  
                                prior = int_prior + urb_prior + pct_ret_prior + 
                                  sd_urb_prior, chains = 2, iter = 2000,
                                data = df_scaled)


#11. 
###tiny model - two level varying slope model 

form3 <- as.formula('Winning.party ~ 1 + urbanindex + Percentage.Retirees..65.. +  (0 + urbanindex | State / Region)')

int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -2, 1)', class = 'b', coef = 'Percentage.Retirees..65..')

sd_urb_prior <- set_prior('gamma(5,2)', class = 'sd')


two_level_vary_slope_fit <- brm(form3, family = 'bernoulli',  
                                prior = int_prior + urb_prior + pct_ret_prior + 
                                  sd_urb_prior, chains = 2, iter = 2000,
                                data = df_scaled)


#12. 
###tiny model - two level varying slope model 

form3 <- as.formula('Winning.party ~ 1 + urbanindex + Percentage.Retirees..65.. +  (0 + urbanindex | State / Region)')

int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -2, 1)', class = 'b', coef = 'Percentage.Retirees..65..')

sd_urb_prior <- set_prior('gamma(2,0.5)', class = 'sd')


two_level_vary_slope_fit <- brm(form3, family = 'bernoulli',  
                                prior = int_prior + urb_prior + pct_ret_prior + 
                                  sd_urb_prior, chains = 2, iter = 2000,
                                data = df_scaled)


#13. 
###tiny model - two level varying slope model 

form3 <- as.formula('Winning.party ~ 1 + urbanindex + Percentage.Retirees..65.. +  (0 + urbanindex | State / Region)')

int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -2, 1)', class = 'b', coef = 'Percentage.Retirees..65..')

sd_urb_prior <- set_prior('gamma(2,2)', class = 'sd')


two_level_vary_slope_fit <- brm(form3, family = 'bernoulli',  
                                prior = int_prior + urb_prior + pct_ret_prior + 
                                  sd_urb_prior, chains = 2, iter = 2000,
                                data = df_scaled)



#14. 
###tiny model - two level varying slope model 

form3 <- as.formula('Winning.party ~ 1 + urbanindex + Percentage.Retirees..65.. +  (0 + urbanindex | State / Region)')

int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -6, 1)', class = 'b', coef = 'Percentage.Retirees..65..')

sd_urb_prior <- set_prior('gamma(5,2)', class = 'sd')


two_level_vary_slope_fit <- brm(form3, family = 'bernoulli',  
                                prior = int_prior + urb_prior + pct_ret_prior + 
                                  sd_urb_prior, chains = 2, iter = 2000,
                                data = df_scaled)


#15. 
###tiny model - two level varying slope model 

form3 <- as.formula('Winning.party ~ 1 + urbanindex + Percentage.Retirees..65.. +  (0 + urbanindex | State / Region)')

int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -6, 1)', class = 'b', coef = 'Percentage.Retirees..65..')

sd_urb_prior <- set_prior('gamma(2,0.5)', class = 'sd')


two_level_vary_slope_fit <- brm(form3, family = 'bernoulli',  
                                prior = int_prior + urb_prior + pct_ret_prior + 
                                  sd_urb_prior, chains = 2, iter = 2000,
                                data = df_scaled)


#16. 
###tiny model - two level varying slope model 

form3 <- as.formula('Winning.party ~ 1 + urbanindex + Percentage.Retirees..65.. +  (0 + urbanindex | State / Region)')

int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -6, 1)', class = 'b', coef = 'Percentage.Retirees..65..')

sd_urb_prior <- set_prior('gamma(2,2)', class = 'sd')


two_level_vary_slope_fit <- brm(form3, family = 'bernoulli',  
                                prior = int_prior + urb_prior + pct_ret_prior + 
                                  sd_urb_prior, chains = 2, iter = 2000,
                                data = df_scaled)







