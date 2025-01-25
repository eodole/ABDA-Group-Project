

#load data
df <- read.csv("./data/data_with_region_indices.csv")


library(ggplot2)
library(dplyr)
library(tibble)
library(brms)
library(tidybayes)
library(stargazer)
library(modelsummary)



#change necessary data types to factor
df$stcd <- as.factor(df$stcd)
df$State <- as.factor(df$State)
df$grouping <- as.factor(df$grouping)  # this is urban category
df$Region <- as.factor(Region)

df$Total.Population <- df$Total.Population/1000000
df$Median.Household.Income <- df$Median.Household.Income/100000

#rename columns for convenience
df = df %>% 
  rename(
    urban.cat = grouping,
    pct.retirees = Percentage.Retirees..65.., 
    pct.unemployed = Unemployment.Rate..16.and.over.,
    pct.bach = Percentage.Bachelors.Degree.in.Population.over.25
  )



model1 <- as.formula("Winning.party ~ urbanindex + (0 + urbanindex || State) +  ")


model1.priors <- c(
  set_prior("normal(0,0.5)", class = "Intercept"), 
  set_prior("normal(0,1)", class = "b", coef = "urbanindex"),
  set_prior("gamma(2,5)", class = "sd", coef = "urbanindex", group = "State") # chisq(10)
)

model1.fit <- brm(model1, family = "bernoulli", prior = model1.priors, data = df)

summary(model1.fit)

plot(model1.fit)

## model 2 - tiny model 

model2 <- as.formula("Winning.party ~ urbanindex + (0 + urbanindex || State) + pct.retirees + (0 + pct.retirees || State)")


model2.priors <- c(
  set_prior("normal(0,0.5)", class = "Intercept"), 
  set_prior("normal(0,1)", class = "b", coef = "urbanindex"),
  set_prior("gamma(2,5)", class = "sd", coef = "urbanindex", group = "State"),
  set_prior("normal(-1,1)", class = "b", coef = "pct.retirees"),
  set_prior("gamma(2,5)", class = "sd", coef = "pct.retirees", group = "State")
  
)

model2.fit <- brm(model2, family = "bernoulli", prior = model2.priors, data = df)

summary(model2.fit)
bayes_R2(model2.fit) # approx .52

pp_check(model2.fit) # also looks like a horseshoe 
pp_check(model2.fit, type = "error_scatter_avg")

#look at rmse of the model 
errors_model2 <- predictive_error(model2.fit, method = "posterior_epred")
rmse_model2 <- sqrt(rowMeans(errors_model2^2))

hist(rmse_model2)

# log likelihoods 
ll_model2 <- log_lik(model2.fit)
llm_model2 <- colMeans(ll_model2)
hist(llm_model2)


## model 3 single level with more covariates big ass model 

set.seed(1234)
model3 <- as.formula( "Winning.party ~ urbanindex + (0 + urbanindex || State) + 
                      Percentage.Women + pct.bach + ( 0 + pct.bach || Region)  + 
                      Median.Household.Income + (0 + Median.Household.Income|| State) + pct.retirees")

model3.priors <- c(
  set_prior("normal(0,0.5)", class = "Intercept"), 
  set_prior("normal(0,1)", class = "b", coef = "urbanindex"),
  set_prior("gamma(2,5)", class = "sd", coef = "urbanindex", group = "State"),
  set_prior("normal(0,1)", class = "b", coef = "Percentage.Women" ), 
  set_prior("cauchy(0,1)", class = 'b', coef = "pct.bach"),
  set_prior("normal(0,1)", class = "sd", coef = "pct.bach", group = "Region"),
  set_prior("normal(0,1)", class = "b", coef = "Median.Household.Income"),
  set_prior("normal(0,1)", class = "sd", coef = "Median.Household.Income", group = "State"),
  set_prior("student_t(1,-1,1)", class = "b", coef = "pct.retirees")
)

model3.fit <- brm(model3, family = "bernoulli", prior = model3.priors, data = df)

plot(model3.fit)


modelsummary()
## model analysis 
summary(model3.fit)
# percentage of variation explained by the model 
bayes_R2(model3.fit) # roughly .59 # 

pp_check(model3.fit) # looks like a horseshoe

# look at rmse of the model 
errors_model3 <- predictive_error(model3.fit, method = "posterior_epred")
rmse_model3 <- sqrt(rowMeans(errors_model3^2))

hist(rmse_model3) #the average error of theta is about 1/3 which is a lot for a bernouli model 

# comparing log likelihoods of the data given the model 
ll_model3 <- log_lik(model3.fit)
llm_model3 <- colMeans(ll_model3)
hist(llm_model3)



## model 4 small model with nested hierarchy butt currently broken 

set.seed(1234)
model4.priors <- as.formula( "Winning.party ~ urbanindex + (0 + urbanindex || State / Region) + 
                      Percentage.Women + pct.bach + ( 0 + pct.bach || Region)  + 
                      Median.Household.Income + (0 + Median.Household.Income|| State) + pct.retirees")

model4.priors <- c(
  set_prior("normal(0,0.5)", class = "Intercept"), 
  set_prior("normal(0,1)", class = "b", coef = "urbanindex"),
  set_prior("normal( 0,1)", class = "sd", coef = "urbanindex", group = "State"),
  #set_prior("gamma(2,5)", class = "sd", coef = "", group = "State:Region"),
  set_prior("normal(0,1)", class = "b", coef = "Percentage.Women" ), 
  set_prior("cauchy(0,1)", class = 'b', coef = "pct.bach"),
  set_prior("normal(0,1)", class = "sd", coef = "pct.bach", group = "Region"),
  set_prior("normal(0,1)", class = "b", coef = "Median.Household.Income"),
  set_prior("normal(0,1)", class = "sd", coef = "Median.Household.Income", group = "State"),
  set_prior("student_t(1,-1,1)", class = "b", coef = "pct.retirees")
)

model4.fit <- brm(model3, family = "bernoulli", prior = model4.priors, data = df)

plot(model4.fit)
get_prior(model4.fit)
