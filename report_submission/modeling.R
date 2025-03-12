
library(ggplot2)
library(dplyr)
library(tibble)
library(brms)
library(tidybayes)
library(stargazer)


### Import Dataset 
df <- read.csv("./report_code/final_data_for_report.csv")

### Change catagorical variables to factors in R
df$stcd <- as.factor(df$stcd)
df$State <- as.factor(df$State)
df$urban.cat <- as.factor(df$urban.cat)  # this is urban category
df$Region <- as.factor(df$Region)


## Model 4  (Big Model) 
set.seed(1234) # set seed for consistency across runs, devices, and models
model4 <- as.formula( "Winning.party ~ urbanindex + (0 + urbanindex || State) + 
                      pct.women + pct.bach + ( 0 + pct.bach || Region)  + 
                      Median.Household.Income + (0 + Median.Household.Income|| State) + pct.retirees")

model4.priors <- c(
  set_prior("normal(0,10)", class = "Intercept"),  # here i updated from normal(0,0,5) to normal(0,10) as in the report 
  set_prior("normal(0,1)", class = "b", coef = "urbanindex"),
  set_prior("cauchy(0,10)", class = "sd", coef = "urbanindex", group = "State"),
  set_prior("student_t(1,-2,1)", class = "b", coef = "pct.retirees"), # changed to -2 instead of -1 as in report 
  set_prior("normal(0,1)", class = "b", coef = "pct.women" ), 
  set_prior("student_t(1,0,1)", class = 'b', coef = "pct.bach"),
  set_prior("normal(0,1)", class = "sd", coef = "pct.bach", group = "Region"),
  set_prior("normal(0,1)", class = "b", coef = "Median.Household.Income"),
  set_prior("normal(0,1)", class = "sd", coef = "Median.Household.Income", group = "State")
  
)

model4.final <- brm(model4, family = "bernoulli", prior = model4.priors, data = df, chains = 4, iter = 4000, save_pars = save_pars(all = TRUE))


## Model 3 ( Nested )
set.seed(1234)
model3 <- as.formula("Winning.party ~ urbanindex + (0 + urbanindex | Region / State)  +  pct.retirees")

model3.priors <-c(
  set_prior("normal(0,10)", class = "Intercept"), 
  set_prior("normal(0,1)", class = "b", coef = "urbanindex"),
  set_prior('cauchy(0,10)', class = 'sd'), 
  set_prior("student_t(1,-2,1)", class = "b", coef = "pct.retirees")
)

model3.final <- brm(model3, family = "bernoulli", prior = model3.priors, data = df, chains = 4, iter = 4000, save_pars = save_pars(all = TRUE))


# Model 1 (State Level)
set.seed(1234)
model1 <- as.formula("Winning.party ~ urbanindex + (0 + urbanindex | State ) + pct.retirees") 

model1.priors <- c(
  set_prior("normal(0,10)", class = "Intercept"), 
  set_prior("normal(0,1)", class = "b", coef = "urbanindex"),
  set_prior("cauchy(0,10)", class = "sd", coef = "urbanindex", group = "State"),
  set_prior("student_t(1,-2,1)", class = "b", coef = "pct.retirees")
)

model1.final <- brm(model1, family = "bernoulli", prior = model1.priors, data = df, chains = 4, iter = 4000, save_pars = save_pars(all = TRUE))



# Model 2 ( Region Level )
set.seed(1234)
model2 <- as.formula("Winning.party ~ urbanindex + (0 + urbanindex | Region ) + pct.retirees") 

model2.priors <- c(
  set_prior("normal(0,10)", class = "Intercept"), 
  set_prior("normal(0,1)", class = "b", coef = "urbanindex"),
  set_prior("cauchy(0,10)", class = "sd", coef = "urbanindex", group = "Region"),
  set_prior("student_t(1,-2,1)", class = "b", coef = "pct.retirees")
)

model2.final <- brm(model2, family = "bernoulli", prior = model2.priors, data = df, chains = 4, iter = 4000, save_pars = save_pars(all = TRUE))










