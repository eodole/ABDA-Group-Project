
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
df$grouping <- as.factor(df$grouping)  # this is urban category
df$Region <- as.factor(Region)


## Model 4  (Big Model) 
set.seed(1234) # set seed for consistency across runs, devices, and models
model1 <- as.formula( "Winning.party ~ urbanindex + (0 + urbanindex || State) + 
                      Percentage.Women + pct.bach + ( 0 + pct.bach || Region)  + 
                      Median.Household.Income + (0 + Median.Household.Income|| State) + pct.retirees")

model1.priors <- c(
  set_prior("normal(0,10)", class = "Intercept"),  # here i updated from normal(0,0,5) to normal(0,10) as in the report 
  set_prior("normal(0,1)", class = "b", coef = "urbanindex"),
  set_prior("gamma(2,5)", class = "sd", coef = "urbanindex", group = "State"),
  set_prior("student_t(1,-2,1)", class = "b", coef = "pct.retirees"), # changed to -2 instead of -1 as in report 
  set_prior("normal(0,1)", class = "b", coef = "Percentage.Women" ), 
  set_prior("normal(0,1)", class = 'b', coef = "pct.bach"), # changed from cauchy to std normal
  set_prior("normal(0,1)", class = "sd", coef = "pct.bach", group = "Region"),
  set_prior("normal(0,1)", class = "b", coef = "Median.Household.Income"),
  set_prior("normal(0,1)", class = "sd", coef = "Median.Household.Income", group = "State")
  
)

model1.final <- brm(model1, family = "bernoulli", prior = model1.priors, data = df, save_pars = save_pars(all = TRUE))


## Model 3 ( Nested )
set.seed(1234)
model2 <- as.formula("Winning.party ~ urbanindex + (0 + urbanindex | State / Region)  +  pct.retirees")

model2.priors <-c(
  set_prior("normal(0,10)", class = "Intercept"), 
  set_prior("normal(0,1)", class = "b", coef = "urbanindex"),
  set_prior("normal(0,1)", class = "sd", coef = "urbanindex", group = "State"),
  set_prior('cauchy(0,10)', class = 'sd'), # unsure about this to be honest? 
  set_prior("student_t(1,-2,1)", class = "b", coef = "pct.retirees")
)

model2.final <- brm(model2, family = "bernoulli", prior = model2.priors, data = df, save_pars = save_pars(all = TRUE))


# Model 1 (State Level)
set.seed(1234)
model3 <- as.formula("Winning.party ~ urbanindex + (0 + urbanindex | State ) + pct.retirees") 

model3.priors <- c(
  set_prior("normal(0,10)", class = "Intercept"), 
  set_prior("normal(0,1)", class = "b", coef = "urbanindex"),
  set_prior("cauchy(0,10)", class = "sd", coef = "urbanindex", group = "State"),
  set_prior("student_t(1,-2,1)", class = "b", coef = "pct.retirees")
)

model3.final <- brm(model3, family = "bernoulli", prior = model3.priors, data = df, save_pars = save_pars(all = TRUE))



# Model 1 ( State Level )
set.seed(1234)
model4 <- as.formula("Winning.party ~ urbanindex + (0 + urbanindex | Region ) + pct.retirees") 

model4.priors <- c(
  set_prior("normal(0,10)", class = "Intercept"), 
  set_prior("normal(0,1)", class = "b", coef = "urbanindex"),
  set_prior("cauchy(0,10)", class = "sd", coef = "urbanindex", group = "Region"),
  set_prior("student_t(1,-2,1)", class = "b", coef = "pct.retirees")
)

model4.final <- brm(model4, family = "bernoulli", prior = model4.priors, data = df, save_pars = save_pars(all = TRUE))










