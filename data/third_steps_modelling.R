rm(list = ls())
df <- read.csv('C:/Users/anarm/OneDrive/Documents/R Scripts/bayesian/data_with_region_indices.csv')[,-1]

library(ggplot2)
library(dplyr)
library(tibble)
library(brms)
library(tidybayes)
library(stargazer)


##region wise model

form <- as.formula('Winning.party ~ 1 + Total.Population + Percentage.Women + 
                    + Median.Household.Income + Median.Age + Percentage.Retirees..65.. + 
                    + Percentage.Bachelors.Degree.in.Population.over.25 + 
                   (1 + urbanindex |Region)')


raw_fit <- brm(form, df)

plot(raw_fit, combo = c('trace', 'hist'))


##region wise model with category instead of urban index

cat_form <- as.formula('Winning.party ~ 1 + Total.Population + Percentage.Women + 
                    + Median.Household.Income + Median.Age + Percentage.Retirees..65.. + 
                    + Percentage.Bachelors.Degree.in.Population.over.25 + 
                   (1 + grouping || Region)')



##prior selection


hist(df$region_index)

hist(df$Median.Age, freq =  FALSE)
lines(density(df$Median.Age))

library(glmnet)
#lasso


summary(df$Median.Age)   #scale?
summary(df$Median.Household.Income)    #scale?
summary(df$Percentage.Bachelors.Degree.in.Population.over.25)
summary(df$Percentage.Women)




med_age_prior <-  set_prior('normal(-1, 1)', class = 'b', coef = 'Median.Age')   
med_income_prior <- set_prior('normal(1, 0.5)', class = 'b', coef = 'Median.Household.Income')
bachelor_prior <- set_prior('beta(2, 10)', class = 'b', coef = 'Percentage.Bachelors.Degree.in.Population.over.25')
women_prior <- set_prior('beta(0.86, 1)', class =  'b', coef = 'Percentage.Women')


##varying slope and intercept model

vary_intercept_slope_fit <- brm(cat_form, df, prior = c(med_age_prior, med_income_prior
                                ,bachelor_prior,  women_prior), family = 'bernoulli', chains = 2)


get_prior(vary_intercept_slope_fit)


plot(vary_intercept_slope_fit, combo = c('trace', 'hist'))









#varying intercept model

cat_form_intercept <- as.formula('Winning.party ~ 1 + Total.Population + Percentage.Women + 
                    + Median.Household.Income + Median.Age + Percentage.Retirees..65.. + 
                    + Percentage.Bachelors.Degree.in.Population.over.25 + grouping +
                   (1 || Region)')

index_form_intercept <- as.formula('Winning.party ~ 1 + Total.Population + Percentage.Women + 
                    + Median.Household.Income + Median.Age + Percentage.Retirees..65.. + 
                    + Percentage.Bachelors.Degree.in.Population.over.25 + urbanindex +
                   (1 || Region)')


int_region_prior <- set_prior('cauchy(0, 10)', class = 'sd')



vary_slope_fit <- brm(cat_form_intercept, family = 'bernoulli', 
                      prior = med_age_prior + med_income_prior + int_region_prior, chains = 1, iter = 1000,
                      data = df)

get_prior(vary_slope_fit)


plot(vary_slope_fit, combo = c('trace', 'hist'))



vary_slope_fit_index <- brm(index_form_intercept, family = 'bernoulli', 
                            prior = med_age_prior + med_income_prior + int_region_prior,
                            chains = 1, iter = 1000,
                            data = df)


get_prior(vary_slope_fit_index)


plot(vary_slope_fit, combo = c('trace', 'hist'))



##these results are quite poor, so let us try to 
## (1) rescale the variables        
## (2) simplify the model by dropping explanatory variables




summary(df$Total.Population) #scale? yes
summary(df$Median.Household.Income)    #scale? yes
summary(df$Median.Age)   #scale? no 
summary(df$Unemployment.Rate..16.and.over.) #no


df_scaled <- df
df_scaled$Total.Population <- df_scaled$Total.Population/1000000
df_scaled$Median.Household.Income <- df_scaled$Median.Household.Income/100000


sparse_cat_form_intercept <- as.formula('Winning.party ~ 1 + Total.Population + Median.Household.Income + Median.Age +Unemployment.Rate..16.and.over. + grouping + (1 || Region)')


sparse_index_form_intercept <- as.formula('Winning.party ~ 1 + Total.Population + Median.Household.Income + Median.Age   +Unemployment.Rate..16.and.over. + urbanindex + (1 || Region)')



total_pop_prior <- set_prior('normal(1, 0.5', class = 'b', coef = 'Total.Population')
med_income_prior <- set_prior('normal(1, 0.5)', class = 'b', coef = 'Median.Household.Income')
med_age_prior <-  set_prior('normal(-1, 1)', class = 'b', coef = 'Median.Age')  
unemp_prior <- set_prior('normal(1, 0.5)', class = 'b', coef = 'Unemployment.Rate..16.and.over.')


int_region_prior <- set_prior('cauchy(0, 10)', class = 'sd')




vary_slope_fit <- brm(sparse_cat_form_intercept, family = 'bernoulli', 
                      prior = total_pop_prior + med_income_prior + med_age_prior + unemp_prior + int_region_prior, chains = 2, iter = 2000,
                      data = df_scaled)



get_prior(vary_slope_fit)

plot(vary_slope_fit, combo = c('trace', 'hist'))


conditional_effects(vary_slope_fit, method = 'posterior_predict', re_formula = sparse_cat_form_intercept)

mcmc_plot(vary_slope_fit, type = "dens", pars = c('groupingDenseUrban', 'groupingMostlyRural', 
                                                  'groupingRuralMExurban', 'groupingUrbanMSuburban'))

mcmc_dens()




vary_slope_fit_index <- brm(sparse_index_form_intercept, family = 'bernoulli', 
                            prior = total_pop_prior + med_income_prior + med_age_prior + unemp_prior + int_region_prior, chains = 2, iter = 2000,
                            data = df_scaled)



mcmc_plot(vary_slope_fit_index, type = 'dens')




