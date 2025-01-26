<<<<<<< HEAD
rm(list = ls())
df <- read.csv('C:/Users/anarm/OneDrive/Documents/R Scripts/bayesian/data_with_region_indices.csv')[,-1]

library(ggplot2)
library(dplyr)
library(tibble)
library(brms)
library(tidybayes)
library(stargazer)
library(modelsummary)

set.seed(1234)

df <- df %>%
  rename(
    Winning.party = Winning.party,
    urbanindex = urbanindex,
    Pct.Women = Percentage.Women,
    Median.Income = Median.Household.Income,
    Pct.Retirees.65 = Percentage.Retirees..65.. ,
    Unempl.16plus = Unemployment.Rate..16.and.over. ,
    Median.Age = Median.Age,
    Pct.Bsc.25plus = Percentage.Bachelors.Degree.in.Population.over.25 ,
    Total.Pop = Total.Population ,
    urban.cat = grouping
  )

df_scaled <- df
df_scaled$Total.Pop <- df_scaled$Total.Pop/1000000
df_scaled$Median.Income <- df_scaled$Median.Income/100000


# Set a global theme to increase text sizes and remove rectangles around titles
theme_set(
  theme_classic(base_size = 20) +       # Adjust base size
    theme(
      plot.title = element_text(
        size = 18, face = "bold", hjust = 0.5,  # Center and style the title
        margin = margin(t = 10, b = 10),       # Add some spacing
        color = "black"                        # Ensure normal text color
      ),
      panel.background = element_rect(fill = NA, color = NA), # No panel background
      legend.background = element_rect(fill = NA, color = NA), # No legend background
      axis.title = element_text(size = 16, background = element_blank()), # No background for axis titles
      axis.text = element_text(size = 14),   # Axis tick text size
      legend.title = element_text(size = 16, background = element_blank()), # No background for legend titles
      legend.text = element_text(size = 14)  # Legend text size
    )
)




#1. 
###state level only -  single level varying slope model
form1 <- as.formula('Winning.party ~ 1 + urbanindex + Pct.Retirees.65 +  (0 + urbanindex|| State)')
int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -2, 1)', class = 'b', coef = 'Pct.Retirees.65')

sd_urb_prior <- set_prior('cauchy(0,10)', class = 'sd')

set.seed(1234)
vary_slope_fit <- brm(form1, family = 'bernoulli',  
                      prior = int_prior + urb_prior + pct_ret_prior + 
                        sd_urb_prior, chains = 4, iter = 2000,
                      data = df_scaled)



# Call the plot function
plot(vary_slope_fit, combo = c('trace', 'hist')) 



mcmc_plot(vary_slope_fit, type = "trace") + theme(
  strip.background = element_blank(),  # Remove background for variable names (strip labels)
  axis.text.x = element_text(size = 16),  # Set the variable name (x-axis) text size to 20
  axis.text.y = element_text(size = 16),  # Set the variable name (y-axis) text size to 20
  strip.text = element_text(size = 20),  # Set the size of the variable name in the title (facet labels)
)

mcmc_plot(vary_slope_fit, type = "hist") + theme(
  strip.background = element_blank(),  # Remove background for variable names (strip labels)
  axis.text.x = element_text(size = 16),  # Set the variable name (x-axis) text size to 20
  axis.text.y = element_text(size = 16),  # Set the variable name (y-axis) text size to 20
  strip.text = element_text(size = 20),  # Set the size of the variable name in the title (facet labels)
)

modelsummary(vary_slope_fit)

conditional_effects(vary_slope_fit) 


#2. 
###state level only -  single level varying slope model
form1 <- as.formula('Winning.party ~ 1 + urbanindex + Pct.Retirees.65 +  (0 + urbanindex|| State)')
int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -6, 1)', class = 'b', coef = 'Pct.Retirees.65')

sd_urb_prior <- set_prior('cauchy(0,10)', class = 'sd')

set.seed(1234)

vary_slope_fit <- brm(form1, family = 'bernoulli',  
                      prior = int_prior + urb_prior + pct_ret_prior + 
                        sd_urb_prior, chains = 4, iter = 2000,
                      data = df_scaled)


mcmc_plot(vary_slope_fit, type = "trace") + theme(
  strip.background = element_blank(),  # Remove background for variable names (strip labels)
  axis.text.x = element_text(size = 16),  # Set the variable name (x-axis) text size to 20
  axis.text.y = element_text(size = 16),  # Set the variable name (y-axis) text size to 20
  strip.text = element_text(size = 20),  # Set the size of the variable name in the title (facet labels)
)

mcmc_plot(vary_slope_fit, type = "hist") + theme(
  strip.background = element_blank(),  # Remove background for variable names (strip labels)
  axis.text.x = element_text(size = 16),  # Set the variable name (x-axis) text size to 20
  axis.text.y = element_text(size = 16),  # Set the variable name (y-axis) text size to 20
  strip.text = element_text(size = 20),  # Set the size of the variable name in the title (facet labels)
)



conditional_effects(vary_slope_fit) 







#3. 
###state level only -  single level varying slope model
form1 <- as.formula('Winning.party ~ 1 + urbanindex + Pct.Retirees.65 +  (0 + urbanindex|| State)')
int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -2, 1)', class = 'b', coef = 'Pct.Retirees.65')

sd_urb_prior <- set_prior('gamma(5,2)', class = 'sd')


set.seed(1234)

vary_slope_fit <- brm(form1, family = 'bernoulli',  
                      prior = int_prior + urb_prior + pct_ret_prior + 
                        sd_urb_prior, chains = 4, iter = 2000,
                      data = df_scaled)


mcmc_plot(vary_slope_fit, type = "trace") + theme(
  strip.background = element_blank(),  # Remove background for variable names (strip labels)
  axis.text.x = element_text(size = 16),  # Set the variable name (x-axis) text size to 20
  axis.text.y = element_text(size = 16),  # Set the variable name (y-axis) text size to 20
  strip.text = element_text(size = 20),  # Set the size of the variable name in the title (facet labels)
)

mcmc_plot(vary_slope_fit, type = "hist") + theme(
  strip.background = element_blank(),  # Remove background for variable names (strip labels)
  axis.text.x = element_text(size = 16),  # Set the variable name (x-axis) text size to 20
  axis.text.y = element_text(size = 16),  # Set the variable name (y-axis) text size to 20
  strip.text = element_text(size = 20),  # Set the size of the variable name in the title (facet labels)
)



conditional_effects(vary_slope_fit) 


#4. 
###state level only -  single level varying slope model
form1 <- as.formula('Winning.party ~ 1 + urbanindex + Pct.Retirees.65 +  (0 + urbanindex|| State)')
int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -2, 1)', class = 'b', coef = 'Pct.Retirees.65')

sd_urb_prior <- set_prior('gamma(2, 0.5)', class = 'sd')

set.seed(1234)

vary_slope_fit <- brm(form1, family = 'bernoulli',  
                      prior = int_prior + urb_prior + pct_ret_prior + 
                        sd_urb_prior, chains = 4, iter = 2000,
                      data = df_scaled)



mcmc_plot(vary_slope_fit, type = "trace") + theme(
  strip.background = element_blank(),  # Remove background for variable names (strip labels)
  axis.text.x = element_text(size = 16),  # Set the variable name (x-axis) text size to 20
  axis.text.y = element_text(size = 16),  # Set the variable name (y-axis) text size to 20
  strip.text = element_text(size = 20),  # Set the size of the variable name in the title (facet labels)
)

mcmc_plot(vary_slope_fit, type = "hist") + theme(
  strip.background = element_blank(),  # Remove background for variable names (strip labels)
  axis.text.x = element_text(size = 16),  # Set the variable name (x-axis) text size to 20
  axis.text.y = element_text(size = 16),  # Set the variable name (y-axis) text size to 20
  strip.text = element_text(size = 20),  # Set the size of the variable name in the title (facet labels)
)



conditional_effects(vary_slope_fit) 


#5. 
###state level only -  single level varying slope model
form1 <- as.formula('Winning.party ~ 1 + urbanindex + Pct.Retirees.65 +  (0 + urbanindex|| State)')
int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -2, 1)', class = 'b', coef = 'Pct.Retirees.65')

sd_urb_prior <- set_prior('gamma(2, 2)', class = 'sd')

set.seed(1234)

vary_slope_fit <- brm(form1, family = 'bernoulli',  
                      prior = int_prior + urb_prior + pct_ret_prior + 
                        sd_urb_prior, chains = 4, iter = 2000,
                      data = df_scaled)


mcmc_plot(vary_slope_fit, type = "trace") + theme(
  strip.background = element_blank(),  # Remove background for variable names (strip labels)
  axis.text.x = element_text(size = 16),  # Set the variable name (x-axis) text size to 20
  axis.text.y = element_text(size = 16),  # Set the variable name (y-axis) text size to 20
  strip.text = element_text(size = 20),  # Set the size of the variable name in the title (facet labels)
)

mcmc_plot(vary_slope_fit, type = "hist") + theme(
  strip.background = element_blank(),  # Remove background for variable names (strip labels)
  axis.text.x = element_text(size = 16),  # Set the variable name (x-axis) text size to 20
  axis.text.y = element_text(size = 16),  # Set the variable name (y-axis) text size to 20
  strip.text = element_text(size = 20),  # Set the size of the variable name in the title (facet labels)
)



conditional_effects(vary_slope_fit) 


#6. 
###state level only -  single level varying slope model
form1 <- as.formula('Winning.party ~ 1 + urbanindex + Pct.Retirees.65 +  (0 + urbanindex|| State)')
int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -6, 1)', class = 'b', coef = 'Pct.Retirees.65')

sd_urb_prior <- set_prior('gamma(5, 2)', class = 'sd')

set.seed(1234)

vary_slope_fit <- brm(form1, family = 'bernoulli',  
                      prior = int_prior + urb_prior + pct_ret_prior + 
                        sd_urb_prior, chains = 4, iter = 2000,
                      data = df_scaled)



mcmc_plot(vary_slope_fit, type = "trace") + theme(
  strip.background = element_blank(),  # Remove background for variable names (strip labels)
  axis.text.x = element_text(size = 16),  # Set the variable name (x-axis) text size to 20
  axis.text.y = element_text(size = 16),  # Set the variable name (y-axis) text size to 20
  strip.text = element_text(size = 20),  # Set the size of the variable name in the title (facet labels)
)

mcmc_plot(vary_slope_fit, type = "hist") + theme(
  strip.background = element_blank(),  # Remove background for variable names (strip labels)
  axis.text.x = element_text(size = 16),  # Set the variable name (x-axis) text size to 20
  axis.text.y = element_text(size = 16),  # Set the variable name (y-axis) text size to 20
  strip.text = element_text(size = 20),  # Set the size of the variable name in the title (facet labels)
)



conditional_effects(vary_slope_fit) 



#7. 
###state level only -  single level varying slope model
form1 <- as.formula('Winning.party ~ 1 + urbanindex + Pct.Retirees.65 +  (0 + urbanindex|| State)')
int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -6, 1)', class = 'b', coef = 'Pct.Retirees.65')

sd_urb_prior <- set_prior('gamma(2, 0.5)', class = 'sd')

set.seed(1234)

vary_slope_fit <- brm(form1, family = 'bernoulli',  
                      prior = int_prior + urb_prior + pct_ret_prior + 
                        sd_urb_prior, chains = 4, iter = 2000,
                      data = df_scaled)


mcmc_plot(vary_slope_fit, type = "trace") + theme(
  strip.background = element_blank(),  # Remove background for variable names (strip labels)
  axis.text.x = element_text(size = 16),  # Set the variable name (x-axis) text size to 20
  axis.text.y = element_text(size = 16),  # Set the variable name (y-axis) text size to 20
  strip.text = element_text(size = 20),  # Set the size of the variable name in the title (facet labels)
)

mcmc_plot(vary_slope_fit, type = "hist") + theme(
  strip.background = element_blank(),  # Remove background for variable names (strip labels)
  axis.text.x = element_text(size = 16),  # Set the variable name (x-axis) text size to 20
  axis.text.y = element_text(size = 16),  # Set the variable name (y-axis) text size to 20
  strip.text = element_text(size = 20),  # Set the size of the variable name in the title (facet labels)
)



conditional_effects(vary_slope_fit) 



#8. 
###state level only -  single level varying slope model
form1 <- as.formula('Winning.party ~ 1 + urbanindex + Pct.Retirees.65 +  (0 + urbanindex|| State)')
int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -6, 1)', class = 'b', coef = 'Pct.Retirees.65')

sd_urb_prior <- set_prior('gamma(2, 2)', class = 'sd')

set.seed(1234)

vary_slope_fit <- brm(form1, family = 'bernoulli',  
                      prior = int_prior + urb_prior + pct_ret_prior + 
                        sd_urb_prior, chains = 4, iter = 2000,
                      data = df_scaled)

mcmc_plot(vary_slope_fit, type = "trace") + theme(
  strip.background = element_blank(),  # Remove background for variable names (strip labels)
  axis.text.x = element_text(size = 16),  # Set the variable name (x-axis) text size to 20
  axis.text.y = element_text(size = 16),  # Set the variable name (y-axis) text size to 20
  strip.text = element_text(size = 20),  # Set the size of the variable name in the title (facet labels)
)

mcmc_plot(vary_slope_fit, type = "hist") + theme(
  strip.background = element_blank(),  # Remove background for variable names (strip labels)
  axis.text.x = element_text(size = 16),  # Set the variable name (x-axis) text size to 20
  axis.text.y = element_text(size = 16),  # Set the variable name (y-axis) text size to 20
  strip.text = element_text(size = 20),  # Set the size of the variable name in the title (facet labels)
)



conditional_effects(vary_slope_fit) 



#9. 
###tiny model - two level varying slope model 

form3 <- as.formula('Winning.party ~ 1 + urbanindex + Pct.Retirees.65 +  (0 + urbanindex | State / Region)')

int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -2, 1)', class = 'b', coef = 'Pct.Retirees.65')

sd_urb_prior <- set_prior('cauchy(0,10)', class = 'sd')

set.seed(1234)

two_level_vary_slope_fit <- brm(form3, family = 'bernoulli',  
                                prior = int_prior + urb_prior + pct_ret_prior + 
                                  sd_urb_prior, chains = 4, iter = 2000,
                                data = df_scaled)

summary(two_level_vary_slope_fit)

mcmc_plot(two_level_vary_slope_fit, type = "trace") + theme(
  strip.background = element_blank(),  # Remove background for variable names (strip labels)
  axis.text.x = element_text(size = 16),  # Set the variable name (x-axis) text size to 20
  axis.text.y = element_text(size = 16),  # Set the variable name (y-axis) text size to 20
  strip.text = element_text(size = 20),  # Set the size of the variable name in the title (facet labels)
)

mcmc_plot(two_level_vary_slope_fit, type = "hist") + theme(
  strip.background = element_blank(),  # Remove background for variable names (strip labels)
  axis.text.x = element_text(size = 16),  # Set the variable name (x-axis) text size to 20
  axis.text.y = element_text(size = 16),  # Set the variable name (y-axis) text size to 20
  strip.text = element_text(size = 20),  # Set the size of the variable name in the title (facet labels)
)


conditional_effects(two_level_vary_slope_fit, effects = "Pct.Retirees.65")
conditional_effects(two_level_vary_slope_fit, effects = "urbanindex")


conditional_effects(two_level_vary_slope_fit, effects = "urbanindex",make_conditions(df_scaled, vars = "State"))
conditional_effects(two_level_vary_slope_fit, effects = "urbanindex",make_conditions(df_scaled, vars = "Region")) 
conditional_effects(two_level_vary_slope_fit, effects = "urbanindex",make_conditions(df_scaled, vars = "State:Region")) 


#10. 
###tiny model - two level varying slope model 

form3 <- as.formula('Winning.party ~ 1 + urbanindex + Pct.Retirees.65 +  (0 + urbanindex | State / Region)')

int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -6, 1)', class = 'b', coef = 'Pct.Retirees.65')

sd_urb_prior <- set_prior('cauchy(0,10)', class = 'sd')

set.seed(1234)

two_level_vary_slope_fit <- brm(form3, family = 'bernoulli',  
                                prior = int_prior + urb_prior + pct_ret_prior + 
                                  sd_urb_prior, chains = 4, iter = 2000,
                                data = df_scaled)

summary(two_level_vary_slope_fit)


mcmc_plot(two_level_vary_slope_fit, type = "trace") + theme(
  strip.background = element_blank(),  # Remove background for variable names (strip labels)
  axis.text.x = element_text(size = 16),  # Set the variable name (x-axis) text size to 20
  axis.text.y = element_text(size = 16),  # Set the variable name (y-axis) text size to 20
  strip.text = element_text(size = 20),  # Set the size of the variable name in the title (facet labels)
)

mcmc_plot(two_level_vary_slope_fit, type = "hist") + theme(
  strip.background = element_blank(),  # Remove background for variable names (strip labels)
  axis.text.x = element_text(size = 16),  # Set the variable name (x-axis) text size to 20
  axis.text.y = element_text(size = 16),  # Set the variable name (y-axis) text size to 20
  strip.text = element_text(size = 20),  # Set the size of the variable name in the title (facet labels)
)



#11. 
###tiny model - two level varying slope model 

form3 <- as.formula('Winning.party ~ 1 + urbanindex + Pct.Retirees.65 +  (0 + urbanindex | State / Region)')

int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -2, 1)', class = 'b', coef = 'Pct.Retirees.65')

sd_urb_prior <- set_prior('gamma(5,2)', class = 'sd')

set.seed(1234)

two_level_vary_slope_fit <- brm(form3, family = 'bernoulli',  
                                prior = int_prior + urb_prior + pct_ret_prior + 
                                  sd_urb_prior, chains = 4, iter = 2000,
                                data = df_scaled)


#12. 
###tiny model - two level varying slope model 

form3 <- as.formula('Winning.party ~ 1 + urbanindex + Pct.Retirees.65 +  (0 + urbanindex | State / Region)')

int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -2, 1)', class = 'b', coef = 'Pct.Retirees.65')

sd_urb_prior <- set_prior('gamma(2,0.5)', class = 'sd')

set.seed(1234)

two_level_vary_slope_fit <- brm(form3, family = 'bernoulli',  
                                prior = int_prior + urb_prior + pct_ret_prior + 
                                  sd_urb_prior, chains = 4, iter = 2000,
                                data = df_scaled)

summary(two_level_vary_slope_fit)


mcmc_plot(two_level_vary_slope_fit, type = "trace") + theme(
  strip.background = element_blank(),  # Remove background for variable names (strip labels)
  axis.text.x = element_text(size = 16),  # Set the variable name (x-axis) text size to 20
  axis.text.y = element_text(size = 16),  # Set the variable name (y-axis) text size to 20
  strip.text = element_text(size = 20),  # Set the size of the variable name in the title (facet labels)
)

mcmc_plot(two_level_vary_slope_fit, type = "hist") + theme(
  strip.background = element_blank(),  # Remove background for variable names (strip labels)
  axis.text.x = element_text(size = 16),  # Set the variable name (x-axis) text size to 20
  axis.text.y = element_text(size = 16),  # Set the variable name (y-axis) text size to 20
  strip.text = element_text(size = 20),  # Set the size of the variable name in the title (facet labels)
)

#13. 
###tiny model - two level varying slope model 

form3 <- as.formula('Winning.party ~ 1 + urbanindex + Pct.Retirees.65 +  (0 + urbanindex | State / Region)')

int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -2, 1)', class = 'b', coef = 'Pct.Retirees.65')

sd_urb_prior <- set_prior('gamma(2,2)', class = 'sd')

set.seed(1234)

two_level_vary_slope_fit <- brm(form3, family = 'bernoulli',  
                                prior = int_prior + urb_prior + pct_ret_prior + 
                                  sd_urb_prior, chains = 4, iter = 2000,
                                data = df_scaled)



#14. 
###tiny model - two level varying slope model 

form3 <- as.formula('Winning.party ~ 1 + urbanindex + Pct.Retirees.65 +  (0 + urbanindex | State / Region)')

int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -6, 1)', class = 'b', coef = 'Pct.Retirees.65')

sd_urb_prior <- set_prior('gamma(5,2)', class = 'sd')

set.seed(1234)

two_level_vary_slope_fit <- brm(form3, family = 'bernoulli',  
                                prior = int_prior + urb_prior + pct_ret_prior + 
                                  sd_urb_prior, chains = 4, iter = 2000,
                                data = df_scaled)


#15. 
###tiny model - two level varying slope model 

form3 <- as.formula('Winning.party ~ 1 + urbanindex + Pct.Retirees.65 +  (0 + urbanindex | State / Region)')

int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -6, 1)', class = 'b', coef = 'Pct.Retirees.65')

sd_urb_prior <- set_prior('gamma(2,0.5)', class = 'sd')

set.seed(1234)

two_level_vary_slope_fit <- brm(form3, family = 'bernoulli',  
                                prior = int_prior + urb_prior + pct_ret_prior + 
                                  sd_urb_prior, chains = 4, iter = 2000,
                                data = df_scaled)

summary(two_level_vary_slope_fit)


mcmc_plot(two_level_vary_slope_fit, type = "trace") + theme(
  strip.background = element_blank(),  # Remove background for variable names (strip labels)
  axis.text.x = element_text(size = 16),  # Set the variable name (x-axis) text size to 20
  axis.text.y = element_text(size = 16),  # Set the variable name (y-axis) text size to 20
  strip.text = element_text(size = 20),  # Set the size of the variable name in the title (facet labels)
)

mcmc_plot(two_level_vary_slope_fit, type = "hist") + theme(
  strip.background = element_blank(),  # Remove background for variable names (strip labels)
  axis.text.x = element_text(size = 16),  # Set the variable name (x-axis) text size to 20
  axis.text.y = element_text(size = 16),  # Set the variable name (y-axis) text size to 20
  strip.text = element_text(size = 20),  # Set the size of the variable name in the title (facet labels)
)

#16. 
###tiny model - two level varying slope model 

form3 <- as.formula('Winning.party ~ 1 + urbanindex + Pct.Retirees.65 +  (0 + urbanindex | State / Region)')

int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -6, 1)', class = 'b', coef = 'Pct.Retirees.65')

sd_urb_prior <- set_prior('gamma(2,2)', class = 'sd')

set.seed(1234)

two_level_vary_slope_fit <- brm(form3, family = 'bernoulli',  
                                prior = int_prior + urb_prior + pct_ret_prior + 
                                  sd_urb_prior, chains = 4, iter = 2000,
                                data = df_scaled)







=======
rm(list = ls())
df <- read.csv('C:/Users/anarm/OneDrive/Documents/R Scripts/bayesian/data_with_region_indices.csv')[,-1]

library(ggplot2)
library(dplyr)
library(tibble)
library(brms)
library(tidybayes)
library(stargazer)
library(modelsummary)

set.seed(1234)

df <- df %>%
  rename(
    Winning.party = Winning.party,
    urbanindex = urbanindex,
    Pct.Women = Percentage.Women,
    Median.Income = Median.Household.Income,
    Pct.Retirees.65 = Percentage.Retirees..65.. ,
    Unempl.16plus = Unemployment.Rate..16.and.over. ,
    Median.Age = Median.Age,
    Pct.Bsc.25plus = Percentage.Bachelors.Degree.in.Population.over.25 ,
    Total.Pop = Total.Population ,
    urban.cat = grouping
  )

df_scaled <- df
df_scaled$Total.Pop <- df_scaled$Total.Pop/1000000
df_scaled$Median.Income <- df_scaled$Median.Income/100000


# Set a global theme to increase text sizes and remove rectangles around titles
theme_set(
  theme_classic(base_size = 20) +       # Adjust base size
    theme(
      plot.title = element_text(
        size = 18, face = "bold", hjust = 0.5,  # Center and style the title
        margin = margin(t = 10, b = 10),       # Add some spacing
        color = "black"                        # Ensure normal text color
      ),
      panel.background = element_rect(fill = NA, color = NA), # No panel background
      legend.background = element_rect(fill = NA, color = NA), # No legend background
      axis.title = element_text(size = 16, background = element_blank()), # No background for axis titles
      axis.text = element_text(size = 14),   # Axis tick text size
      legend.title = element_text(size = 16, background = element_blank()), # No background for legend titles
      legend.text = element_text(size = 14)  # Legend text size
    )
)




#1. 
###state level only -  single level varying slope model
form1 <- as.formula('Winning.party ~ 1 + urbanindex + Pct.Retirees.65 +  (0 + urbanindex|| State)')
int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -2, 1)', class = 'b', coef = 'Pct.Retirees.65')

sd_urb_prior <- set_prior('cauchy(0,10)', class = 'sd')


vary_slope_fit <- brm(form1, family = 'bernoulli',  
                      prior = int_prior + urb_prior + pct_ret_prior + 
                        sd_urb_prior, chains = 4, iter = 2000,
                      data = df_scaled)



# Call the plot function
plot(vary_slope_fit, combo = c('trace', 'hist')) 



mcmc_plot(vary_slope_fit, type = "trace") + theme(
  strip.background = element_blank(),  # Remove background for variable names (strip labels)
  axis.text.x = element_text(size = 16),  # Set the variable name (x-axis) text size to 20
  axis.text.y = element_text(size = 16),  # Set the variable name (y-axis) text size to 20
  strip.text = element_text(size = 20),  # Set the size of the variable name in the title (facet labels)
)

mcmc_plot(vary_slope_fit, type = "hist") + theme(
  strip.background = element_blank(),  # Remove background for variable names (strip labels)
  axis.text.x = element_text(size = 16),  # Set the variable name (x-axis) text size to 20
  axis.text.y = element_text(size = 16),  # Set the variable name (y-axis) text size to 20
  strip.text = element_text(size = 20),  # Set the size of the variable name in the title (facet labels)
)

modelsummary(vary_slope_fit)

conditional_effects(vary_slope_fit, effects = "urbanindex", points = TRUE)


#2. 
###state level only -  single level varying slope model
form1 <- as.formula('Winning.party ~ 1 + urbanindex + Pct.Retirees.65 +  (0 + urbanindex|| State)')
int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -6, 1)', class = 'b', coef = 'Pct.Retirees.65')

sd_urb_prior <- set_prior('cauchy(0,10)', class = 'sd')


vary_slope_fit <- brm(form1, family = 'bernoulli',  
                      prior = int_prior + urb_prior + pct_ret_prior + 
                        sd_urb_prior, chains = 2, iter = 2000,
                      data = df_scaled)


#3. 
###state level only -  single level varying slope model
form1 <- as.formula('Winning.party ~ 1 + urbanindex + Pct.Retirees.65 +  (0 + urbanindex|| State)')
int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -2, 1)', class = 'b', coef = 'Pct.Retirees.65')

sd_urb_prior <- set_prior('gamma(5,2)', class = 'sd')


vary_slope_fit <- brm(form1, family = 'bernoulli',  
                      prior = int_prior + urb_prior + pct_ret_prior + 
                        sd_urb_prior, chains = 2, iter = 2000,
                      data = df_scaled)

#4. 
###state level only -  single level varying slope model
form1 <- as.formula('Winning.party ~ 1 + urbanindex + Pct.Retirees.65 +  (0 + urbanindex|| State)')
int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -2, 1)', class = 'b', coef = 'Pct.Retirees.65')

sd_urb_prior <- set_prior('gamma(2, 0.5)', class = 'sd')


vary_slope_fit <- brm(form1, family = 'bernoulli',  
                      prior = int_prior + urb_prior + pct_ret_prior + 
                        sd_urb_prior, chains = 2, iter = 2000,
                      data = df_scaled)


#5. 
###state level only -  single level varying slope model
form1 <- as.formula('Winning.party ~ 1 + urbanindex + Pct.Retirees.65 +  (0 + urbanindex|| State)')
int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -2, 1)', class = 'b', coef = 'Pct.Retirees.65')

sd_urb_prior <- set_prior('gamma(2, 2)', class = 'sd')


vary_slope_fit <- brm(form1, family = 'bernoulli',  
                      prior = int_prior + urb_prior + pct_ret_prior + 
                        sd_urb_prior, chains = 2, iter = 2000,
                      data = df_scaled)


#6. 
###state level only -  single level varying slope model
form1 <- as.formula('Winning.party ~ 1 + urbanindex + Pct.Retirees.65 +  (0 + urbanindex|| State)')
int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -6, 1)', class = 'b', coef = 'Pct.Retirees.65')

sd_urb_prior <- set_prior('gamma(5, 2)', class = 'sd')


vary_slope_fit <- brm(form1, family = 'bernoulli',  
                      prior = int_prior + urb_prior + pct_ret_prior + 
                        sd_urb_prior, chains = 2, iter = 2000,
                      data = df_scaled)


#7. 
###state level only -  single level varying slope model
form1 <- as.formula('Winning.party ~ 1 + urbanindex + Pct.Retirees.65 +  (0 + urbanindex|| State)')
int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -6, 1)', class = 'b', coef = 'Pct.Retirees.65')

sd_urb_prior <- set_prior('gamma(2, 0.5)', class = 'sd')


vary_slope_fit <- brm(form1, family = 'bernoulli',  
                      prior = int_prior + urb_prior + pct_ret_prior + 
                        sd_urb_prior, chains = 2, iter = 2000,
                      data = df_scaled)


#8. 
###state level only -  single level varying slope model
form1 <- as.formula('Winning.party ~ 1 + urbanindex + Pct.Retirees.65 +  (0 + urbanindex|| State)')
int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -6, 1)', class = 'b', coef = 'Pct.Retirees.65')

sd_urb_prior <- set_prior('gamma(2, 2)', class = 'sd')


vary_slope_fit <- brm(form1, family = 'bernoulli',  
                      prior = int_prior + urb_prior + pct_ret_prior + 
                        sd_urb_prior, chains = 2, iter = 2000,
                      data = df_scaled)


#9. 
###tiny model - two level varying slope model 

form3 <- as.formula('Winning.party ~ 1 + urbanindex + Pct.Retirees.65 +  (0 + urbanindex | State / Region)')

int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -2, 1)', class = 'b', coef = 'Pct.Retirees.65')

sd_urb_prior <- set_prior('cauchy(0,10)', class = 'sd')


two_level_vary_slope_fit <- brm(form3, family = 'bernoulli',  
                                prior = int_prior + urb_prior + pct_ret_prior + 
                                  sd_urb_prior, chains = 2, iter = 2000,
                                data = df_scaled)


#10. 
###tiny model - two level varying slope model 

form3 <- as.formula('Winning.party ~ 1 + urbanindex + Pct.Retirees.65 +  (0 + urbanindex | State / Region)')

int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -6, 1)', class = 'b', coef = 'Pct.Retirees.65')

sd_urb_prior <- set_prior('cauchy(0,10)', class = 'sd')


two_level_vary_slope_fit <- brm(form3, family = 'bernoulli',  
                                prior = int_prior + urb_prior + pct_ret_prior + 
                                  sd_urb_prior, chains = 2, iter = 2000,
                                data = df_scaled)


#11. 
###tiny model - two level varying slope model 

form3 <- as.formula('Winning.party ~ 1 + urbanindex + Pct.Retirees.65 +  (0 + urbanindex | State / Region)')

int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -2, 1)', class = 'b', coef = 'Pct.Retirees.65')

sd_urb_prior <- set_prior('beta(5,2)', class = 'sd')


two_level_vary_slope_fit <- brm(form3, family = 'bernoulli',  
                                prior = int_prior + urb_prior + pct_ret_prior + 
                                  sd_urb_prior, chains = 2, iter = 2000,
                                data = df_scaled)


#12. 
###tiny model - two level varying slope model 

form3 <- as.formula('Winning.party ~ 1 + urbanindex + Pct.Retirees.65 +  (0 + urbanindex | State / Region)')

int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -2, 1)', class = 'b', coef = 'Pct.Retirees.65')

sd_urb_prior <- set_prior('beta(2,0.5)', class = 'sd')


two_level_vary_slope_fit <- brm(form3, family = 'bernoulli',  
                                prior = int_prior + urb_prior + pct_ret_prior + 
                                  sd_urb_prior, chains = 2, iter = 2000,
                                data = df_scaled)


#13. 
###tiny model - two level varying slope model 

form3 <- as.formula('Winning.party ~ 1 + urbanindex + Pct.Retirees.65 +  (0 + urbanindex | State / Region)')

int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -2, 1)', class = 'b', coef = 'Pct.Retirees.65')

sd_urb_prior <- set_prior('beta(2,2)', class = 'sd')


two_level_vary_slope_fit <- brm(form3, family = 'bernoulli',  
                                prior = int_prior + urb_prior + pct_ret_prior + 
                                  sd_urb_prior, chains = 2, iter = 2000,
                                data = df_scaled)



#14. 
###tiny model - two level varying slope model 

form3 <- as.formula('Winning.party ~ 1 + urbanindex + Pct.Retirees.65 +  (0 + urbanindex | State / Region)')

int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -6, 1)', class = 'b', coef = 'Pct.Retirees.65')

sd_urb_prior <- set_prior('beta(5,2)', class = 'sd')


two_level_vary_slope_fit <- brm(form3, family = 'bernoulli',  
                                prior = int_prior + urb_prior + pct_ret_prior + 
                                  sd_urb_prior, chains = 2, iter = 2000,
                                data = df_scaled)


#15. 
###tiny model - two level varying slope model 

form3 <- as.formula('Winning.party ~ 1 + urbanindex + Pct.Retirees.65 +  (0 + urbanindex | State / Region)')

int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -6, 1)', class = 'b', coef = 'Pct.Retirees.65')

sd_urb_prior <- set_prior('beta(2,0.5)', class = 'sd')


two_level_vary_slope_fit <- brm(form3, family = 'bernoulli',  
                                prior = int_prior + urb_prior + pct_ret_prior + 
                                  sd_urb_prior, chains = 2, iter = 2000,
                                data = df_scaled)


#16. 
###tiny model - two level varying slope model 

form3 <- as.formula('Winning.party ~ 1 + urbanindex + Pct.Retirees.65 +  (0 + urbanindex | State / Region)')

int_prior <- set_prior('normal(0, 0.5)', class = 'Intercept')
urb_prior <- set_prior('normal(0, 1)', class = 'b', coef = 'urbanindex')
pct_ret_prior <- set_prior('student_t(1, -6, 1)', class = 'b', coef = 'Pct.Retirees.65')

sd_urb_prior <- set_prior('beta(2,2)', class = 'sd')


two_level_vary_slope_fit <- brm(form3, family = 'bernoulli',  
                                prior = int_prior + urb_prior + pct_ret_prior + 
                                  sd_urb_prior, chains = 2, iter = 2000,
                                data = df_scaled)







>>>>>>> 36d7bd66610d8d7629bff89cac1dc5f5e286d7ea
