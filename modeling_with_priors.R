

#load data
df <- read.csv("./data/data_with_region_indices.csv")


library(ggplot2)
library(dplyr)
library(tibble)
library(brms)
library(tidybayes)
library(stargazer)




#change necessary data types to factor
df$stcd <- as.factor(df$stcd)
df$State <- as.factor(df$State)
df$grouping <- as.factor(df$grouping)  # this is urban category
df$Region <- as.factor(Region)


#rename columns for convenience
df = df %>% 
  rename(
    urban.cat = grouping,
    pct.retirees = Percentage.Retirees..65.., 
    pct.unemployed = Unemployment.Rate..16.and.over.,
    pct.bach = Percentage.Bachelors.Degree.in.Population.over.25
  )

model1 <- as.formula("Winning.party ~ urbanindex + (0 + urbanindex || State) ")


model1.priors <- c(
  set_prior("normal(0,0.5)", class = "Intercept"), 
  set_prior("normal(0,1)", class = "b", coef = "urbanindex"),
  set_prior("gamma(2,5)", class = "sd", coef = "urbanindex", group = "State") # chisq(10)
)

model1.fit <- brm(model1, family = "bernoulli", prior = model1.priors, data = df)

summary(model1.fit)

plot(model1.fit)

## model 2 

model2 <- as.formula("Winning.party ~ urbanindex + (0 + urbanindex || State) + pct.retirees + (0 + pct.retirees || State)")


model2.priors <- c(
  set_prior("normal(0,0.5)", class = "Intercept"), 
  set_prior("normal(0,1)", class = "b", coef = "urbanindex"),
  set_prior("gamma(2,5)", class = "sd", coef = "urbanindex", group = "State"),
  set_prior("normal(0,1)", class = "b", coef = "pct.retirees"),
  set_prior("gamma(2,5)", class = "sd", coef = "pct.retirees", group = "State")
  
)

model2.fit <- brm(model2, family = "bernoulli", prior = model2.priors, data = df)

summary(model2.fit)


## model 3 single level with more covariates 

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
  set_prior("student_t(1,1,1)", class = "b", coef = "pct.retirees")
)

model3.fit <- brm(model3, family = "bernoulli", prior = model3.priors, data = df)

plot(model3.fit)
