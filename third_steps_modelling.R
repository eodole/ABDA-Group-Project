rm(list = ls())
df <- read.csv('C:/Users/anarm/OneDrive/Documents/R Scripts/bayesian/data_with_region_indices.csv')[,-1]

library(ggplot2)
library(dplyr)
library(tibble)
library(brms)
library(tidybayes)
library(stargazer)


##single level varying intercept model 

cat_form_intercept <- as.formula('Winning.party ~ 1 + Total.Population + Median.Household.Income + Median.Age + Percentage.Bachelors.Degree.in.Population.over.25 + grouping + (1 || Region)')

index_form_intercept <- as.formula('Winning.party ~ 1 + Total.Population + Median.Household.Income + Median.Age + Percentage.Bachelors.Degree.in.Population.over.25 + urbanindex + (1 || Region)')


df_scaled <- df
df_scaled$Total.Population <- df_scaled$Total.Population/1000000
df_scaled$Median.Household.Income <- df_scaled$Median.Household.Income/100000



total_pop_prior <- set_prior('normal(1, 0.5)', class = 'b', coef = 'Total.Population')
med_income_prior <- set_prior('normal(1, 0.5)', class = 'b', coef = 'Median.Household.Income')
med_age_prior <-  set_prior('normal(-1, 1)', class = 'b', coef = 'Median.Age')  
bachelor_prior <- set_prior('beta(2, 10)', class = 'b', coef = 'Percentage.Bachelors.Degree.in.Population.over.25')

g1_prior <- set_prior('normal(1, 0.5)', class = 'b', coef = 'groupingDenseUrban')
g2_prior <- set_prior('normal(1, 0.5)', class = 'b', coef = 'groupingMostlyRural')
g3_prior <- set_prior('normal(1, 0.5)', class = 'b', coef = 'groupingRuralMExurban')
g4_prior <- set_prior('normal(1, 0.5)', class = 'b', coef = 'groupingSuburbanMExurban')
g5_prior <- set_prior('normal(1, 0.5)', class = 'b', coef = 'groupingUrbanMSuburban')


int_region_prior <- set_prior('cauchy(0, 10)', class = 'sd')

vary_slope_fit <- brm(cat_form_intercept, family = 'bernoulli', 
                      prior = total_pop_prior + med_income_prior + med_age_prior + 
                        bachelor_prior + int_region_prior + g1_prior + g2_prior +
                        g3_prior + g4_prior + g5_prior, chains = 2, iter = 2000,
                      data = df_scaled)


get_prior(vary_slope_fit)

plot(vary_slope_fit, combo = c('trace', 'hist'))


##let us try to 
## (4) experiment with the priors


##single level varying slope and intercept model 

cat_form_slope_intercept <- as.formula('Winning.party ~ 1 + Total.Population + Median.Household.Income + Median.Age + Percentage.Bachelors.Degree.in.Population.over.25 + grouping + (1 + grouping|| Region)')

index_form_slope_intercept <- as.formula('Winning.party ~ 1 + Total.Population + Median.Household.Income + Median.Age + Percentage.Bachelors.Degree.in.Population.over.25 + urbanindex + (1 + urbanindex|| Region)')



##double level varying intercept model 
two_level_cat_form_slope <- as.formula('Winning.party ~ 1 + Total.Population + Median.Household.Income + Median.Age + Percentage.Bachelors.Degree.in.Population.over.25 + grouping + (1 || Region || State)')

two_level_index_form_slope <- as.formula('Winning.party ~ 1 + Total.Population + Median.Household.Income + Median.Age + Percentage.Bachelors.Degree.in.Population.over.25 + urbanindex + (1 || Region || State)')



