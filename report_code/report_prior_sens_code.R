library(ggplot2)
library(dplyr)
library(tibble)
library(brms)
library(tidybayes)
library(stargazer)


df <- read.csv("./report_code/final_data_for_report.csv")

### Change catagorical variables to factors in R
df$stcd <- as.factor(df$stcd)
df$State <- as.factor(df$State)
df$urban.cat <- as.factor(df$urban.cat)  # this is urban category
df$Region <- as.factor(df$Region)

## we will set priors model by model and save the results
#models 1 to 3

# prior set 1 = original
# prior set 2 = half cauchy to half normal (0, 1)
# prior set 3 = beta urban to normal (0, 5)
# prior set 4 = intercept to normal (0, 100)
# prior set 5 = half cauchy to half normal (0, 1) AND beta urban to normal (0, 5) 


#model 1 priors - first is always original
model1_prior_list <- list(
  prior_set1 = c(
    set_prior("normal(0,10)", class = "Intercept"), 
    set_prior("normal(0,1)", class = "b", coef = "urbanindex"),
    set_prior("cauchy(0,10)", class = "sd", coef = "urbanindex", group = "State"),
    set_prior("student_t(1,-2,1)", class = "b", coef = "pct.retirees")
  ),       
  
  prior_set2 = c(
    set_prior("normal(0,10)", class = "Intercept"), 
    set_prior("normal(0,1)", class = "b", coef = "urbanindex"),
    set_prior("normal(0,1)", class = "sd", coef = "urbanindex", group = "State"),
    set_prior("student_t(1,-2,1)", class = "b", coef = "pct.retirees")
  ),
  
  prior_set3 = c(
    set_prior("normal(0,10)", class = "Intercept"), 
    set_prior("normal(0,5)", class = "b", coef = "urbanindex"),
    set_prior("cauchy(0,10)", class = "sd", coef = "urbanindex", group = "State"),
    set_prior("student_t(1,-2,1)", class = "b", coef = "pct.retirees")
  ), 
  
  prior_set4 = c(
    set_prior("normal(0,100)", class = "Intercept"), 
    set_prior("normal(0,1)", class = "b", coef = "urbanindex"),
    set_prior("cauchy(0,10)", class = "sd", coef = "urbanindex", group = "State"),
    set_prior("student_t(1,-2,1)", class = "b", coef = "pct.retirees")
  ),
  
  prior_set5 = c(
    set_prior("normal(0,10)", class = "Intercept"), 
    set_prior("normal(0,5)", class = "b", coef = "urbanindex"),
    set_prior("normal(0,1)", class = "sd", coef = "urbanindex", group = "State"),
    set_prior("student_t(1,-2,1)", class = "b", coef = "pct.retirees")
  )

)

model1_coef_shelf <- matrix(nrow = 3, ncol = length(model1_prior_list))  ##this only stores betas not sd



for(i in seq_along(model1_prior_list)){
  
  set.seed(1234)
  mod1 <- as.formula("Winning.party ~ urbanindex + (0 + urbanindex | State ) + pct.retirees") 
  
  mod1.priors <- model1_prior_list[[i]]
  
  mod1.final <- brm(mod1, family = "bernoulli", prior = mod1.priors, 
                    data = df, chains = 4, iter = 4000, save_pars = save_pars(all = TRUE))

  model1_coef_shelf[, i] <- fixef(mod1.final)[, 1]
  print(summary(mod1.final))  #we need this to extract the sd
}


#write.csv(model1_coef_shelf, 'C:/Users/anarm/OneDrive/Documents/R Scripts/bayesian/report/mod1_prior_results.csv')




#model 2 priors - first is always original
model2_prior_list <- list(
  prior_set1 = c(
    set_prior("normal(0,10)", class = "Intercept"), 
    set_prior("normal(0,1)", class = "b", coef = "urbanindex"),
    set_prior("cauchy(0,10)", class = "sd", coef = "urbanindex", group = "Region"),
    set_prior("student_t(1,-2,1)", class = "b", coef = "pct.retirees")
  )
  ,       
  
  prior_set2 = c(
    set_prior("normal(0,10)", class = "Intercept"), 
    set_prior("normal(0,1)", class = "b", coef = "urbanindex"),
    set_prior("normal(0,1)", class = "sd", coef = "urbanindex", group = "Region"),
    set_prior("student_t(1,-2,1)", class = "b", coef = "pct.retirees")
  )
  ,
  
  prior_set3 = c(
    set_prior("normal(0,10)", class = "Intercept"), 
    set_prior("normal(0,5)", class = "b", coef = "urbanindex"),
    set_prior("cauchy(0,10)", class = "sd", coef = "urbanindex", group = "Region"),
    set_prior("student_t(1,-2,1)", class = "b", coef = "pct.retirees")
  )
  , 
  
  prior_set4 = c(
    set_prior("normal(0,100)", class = "Intercept"), 
    set_prior("normal(0,1)", class = "b", coef = "urbanindex"),
    set_prior("cauchy(0,10)", class = "sd", coef = "urbanindex", group = "Region"),
    set_prior("student_t(1,-2,1)", class = "b", coef = "pct.retirees")
  )
  ,
  
  prior_set5 = c(
    set_prior("normal(0,10)", class = "Intercept"), 
    set_prior("normal(0,5)", class = "b", coef = "urbanindex"),
    set_prior("cauchy(0,10)", class = "sd", coef = "urbanindex", group = "Region"),
    set_prior("student_t(1,-2,1)", class = "b", coef = "pct.retirees")
  )
)


model2_coef_shelf <- matrix(nrow = 3, ncol = length(model2_prior_list))

for(i in seq_along(model2_prior_list)){
  
  set.seed(1234)
  mod2 <- as.formula("Winning.party ~ urbanindex + (0 + urbanindex | Region ) + pct.retirees") 
  
  mod2.priors <- model2_prior_list[[i]]
  
  mod2.final <- brm(mod2, family = "bernoulli", prior = mod2.priors, 
                    data = df, chains = 4, iter = 4000, save_pars = save_pars(all = TRUE))
  
  model2_coef_shelf[, i] <- fixef(mod2.final)[, 1]
  print(summary(mod2.final))  #we need this to extract the sd
}


#write.csv(model2_coef_shelf, 'C:/Users/anarm/OneDrive/Documents/R Scripts/bayesian/report/mod2_prior_results.csv')


#model 3 priors - first is always original

model3_prior_list <- list(
  prior_set1 = c(
    set_prior("normal(0,10)", class = "Intercept"), 
    set_prior("normal(0,1)", class = "b", coef = "urbanindex"),
    set_prior('cauchy(0,10)', class = 'sd'), 
    set_prior("student_t(1,-2,1)", class = "b", coef = "pct.retirees")
  )
  ,       
  
  prior_set2 = c(
    set_prior("normal(0,10)", class = "Intercept"), 
    set_prior("normal(0,1)", class = "b", coef = "urbanindex"),
    set_prior('normal(0,1)', class = 'sd'), 
    set_prior("student_t(1,-2,1)", class = "b", coef = "pct.retirees")
  )
  ,
  
  prior_set3 = c(
    set_prior("normal(0,10)", class = "Intercept"), 
    set_prior("normal(0,5)", class = "b", coef = "urbanindex"),
    set_prior('cauchy(0,10)', class = 'sd'), 
    set_prior("student_t(1,-2,1)", class = "b", coef = "pct.retirees")
  )
  , 
  
  prior_set4 = c(
    set_prior("normal(0,100)", class = "Intercept"), 
    set_prior("normal(0,1)", class = "b", coef = "urbanindex"),
    set_prior('cauchy(0,10)', class = 'sd'), 
    set_prior("student_t(1,-2,1)", class = "b", coef = "pct.retirees")
  )
  ,
  
  prior_set5 = c(
    set_prior("normal(0,10)", class = "Intercept"), 
    set_prior("normal(0,5)", class = "b", coef = "urbanindex"),
    set_prior('normal(0,1)', class = 'sd'), 
    set_prior("student_t(1,-2,1)", class = "b", coef = "pct.retirees")
  )
)


model3_coef_shelf <- matrix(nrow = 3, ncol = length(model3_prior_list))


for(i in seq_along(model3_prior_list)){
  
  set.seed(1234)
  mod3 <- as.formula("Winning.party ~ urbanindex + (0 + urbanindex | Region / State)  +  pct.retirees") 
  
  mod3.priors <- model3_prior_list[[i]]
  
  mod3.final <- brm(mod3, family = "bernoulli", prior = mod3.priors, 
                    data = df, chains = 4, iter = 4000, save_pars = save_pars(all = TRUE))
  
  model3_coef_shelf[, i] <- fixef(mod3.final)[, 1]
  print(summary(mod2.final))  #we need this to extract the sd
}


#write.csv(model3_coef_shelf, 'C:/Users/anarm/OneDrive/Documents/R Scripts/bayesian/report/mod3_prior_results.csv')

##save the trace plots - run the prior lists but not the loops first
#run global theme from report_trace_plots_code.r first

#doing it individually so we can easily adjust settings like text size 



save_dir <- "~/R Scripts/bayesian/report/prior_sens_trace_plots"

#model 1
for (i in seq_along(model1_prior_list)) {
  
  set.seed(1234)
  mod1 <- as.formula("Winning.party ~ urbanindex + (0 + urbanindex | State ) + pct.retirees") 
  
  mod1.priors <- model1_prior_list[[i]]
  
  mod1.final <- brm(mod1, family = "bernoulli", prior = mod1.priors, 
                    data = df, chains = 4, iter = 4000, save_pars = save_pars(all = TRUE))
  
  
  # Generate trace plot for the current model
  trace_plot <- mcmc_plot(mod1.final, type = "trace") + 
    scale_x_continuous(breaks = c(0, 1000, 2000)) +
    theme(
      strip.background = element_blank(),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      strip.text = element_text(size = 12)
    )
  
  # Display the plot
  print(trace_plot)
  
  # Define the filename
  plot_filename <- paste0(save_dir, "/trace_plot_mod1_prior_set_", i, ".png")
  
  # Save the plot
  ggsave(filename = plot_filename, plot = trace_plot, width = 8, height = 6, dpi = 300)
}




#model 2
for (i in seq_along(model2_prior_list)) {
  
  set.seed(1234)
  mod2 <- as.formula("Winning.party ~ urbanindex + (0 + urbanindex | Region ) + pct.retirees") 
  
  mod2.priors <- model2_prior_list[[i]]
  
  mod2.final <- brm(mod2, family = "bernoulli", prior = mod2.priors, 
                    data = df, chains = 4, iter = 4000, save_pars = save_pars(all = TRUE))
  
  # Generate trace plot for the current model
  trace_plot <- mcmc_plot(mod2.final, type = "trace") + 
    scale_x_continuous(breaks = c(0, 1000, 2000)) +
    theme(
      strip.background = element_blank(),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      strip.text = element_text(size = 9.25)
    )
  
  # Display the plot
  print(trace_plot)
  
  # Define the filename
  plot_filename <- paste0(save_dir, "/trace_plot_mod2_prior_set_", i, ".png")
  
  # Save the plot
  ggsave(filename = plot_filename, plot = trace_plot, width = 8, height = 6, dpi = 300)
}














