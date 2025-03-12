#run modeling.r first

library(ggplot2)
library(dplyr)
library(tibble)
library(brms)
library(tidybayes)
library(stargazer)

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


#trace plots 

#this loop can be used to generate all plots at once, but without individual
# text size adjustment some of them are not very pretty
# for (i in 1:4) {
#   model_name <- paste0("model", i, ".final")  # Construct the model name
#   model <- get(model_name)  # Retrieve the model object
# 
#   plot <- mcmc_plot(model, type = "trace") +
#     scale_x_continuous(breaks = c(0, 500, 1000)) +
#     theme(
#       strip.background = element_blank(),  # Remove background for variable names (strip labels)
#       axis.text.x = element_text(size = 12),  # Adjust x-axis text size
#       axis.text.y = element_text(size = 12),  # Adjust y-axis text size
#       strip.text = element_text(size = 12)  # Adjust facet label size
#     )
# 
#   print(plot)  # Ensure the plot is displayed
# }

## model 1 and 2 look okay with some size adjustment
mcmc_plot(model1.final, type = "trace") + 
  scale_x_continuous(breaks = c(0, 1000, 2000)) +
  theme(
    strip.background = element_blank(),  # Remove background for variable names (strip labels)
    axis.text.x = element_text(size = 12),  # Set the variable name (x-axis) text size to 20
    axis.text.y = element_text(size = 12),  # Set the variable name (y-axis) text size to 20
    strip.text = element_text(size = 12),  # Set the size of the variable name in the title (facet labels)
  )


mcmc_plot(model2.final, type = "trace") + 
  scale_x_continuous(breaks = c(0, 1000, 2000)) +
  theme(
    strip.background = element_blank(),  # Remove background for variable names (strip labels)
    axis.text.x = element_text(size = 12),  # Set the variable name (x-axis) text size to 20
    axis.text.y = element_text(size = 12),  # Set the variable name (y-axis) text size to 20
    strip.text = element_text(size = 9.25),  # Set the size of the variable name in the title (facet labels)
  )


#model 3 and 4 have too many coefs

#model 3 part 1         ##sd_State__urbanindex is too long a name
mcmc_plot(model3.final, type = "trace", variable  = c('b_Intercept', 
                                                      'b_urbanindex', 
                                                      'b_pct.retirees', 
                                                      'sd_Region__urbanindex')) + 
  scale_x_continuous(breaks = c(0, 1000, 2000)) +
  theme(
    strip.background = element_blank(),  # Remove background for variable names (strip labels)
    axis.text.x = element_text(size = 12),  # Set the variable name (x-axis) text size to 20
    axis.text.y = element_text(size = 12),  # Set the variable name (y-axis) text size to 20
    strip.text = element_text(size = 12.5),  # Set the size of the variable name in the title (facet labels)
  )


#model 3 part 2     ##this one is fine
mcmc_plot(model3.final, type = "trace", variable  = c(
                                                      'sd_Region:State__urbanindex')) + 
  scale_x_continuous(breaks = c(0, 1000, 2000)) +
  theme(
    strip.background = element_blank(),  # Remove background for variable names (strip labels)
    axis.text.x = element_text(size = 12),  # Set the variable name (x-axis) text size to 20
    axis.text.y = element_text(size = 12),  # Set the variable name (y-axis) text size to 20
    strip.text = element_text(size = 8),  # Set the size of the variable name in the title (facet labels)
  )


#model 4 part 1 
mcmc_plot(model4.final, type = "trace", variable  = c('b_Intercept', 
                                                      'b_urbanindex', 
                                                      'b_pct.women', 
                                                      'b_pct.bach')) + 
  scale_x_continuous(breaks = c(0,  1000, 2000)) +
  theme(
    strip.background = element_blank(),  # Remove background for variable names (strip labels)
    axis.text.x = element_text(size = 12),  # Set the variable name (x-axis) text size to 20
    axis.text.y = element_text(size = 12),  # Set the variable name (y-axis) text size to 20
    strip.text = element_text(size = 12.5),  # Set the size of the variable name in the title (facet labels)
  )

#model 4 part 2
mcmc_plot(model4.final, type = "trace", variable  = c('b_Median.Household.Income', 
                                                      'b_pct.retirees', 
                                                      'sd_Region__pct.bach', 
                                                      'sd_State__urbanindex')) + 
  scale_x_continuous(breaks = c(0, 1000,  2000)) +
  theme(
    strip.background = element_blank(),  # Remove background for variable names (strip labels)
    axis.text.x = element_text(size = 12),  # Set the variable name (x-axis) text size to 20
    axis.text.y = element_text(size = 12),  # Set the variable name (y-axis) text size to 20
    strip.text = element_text(size = 7.5),  # Set the size of the variable name in the title (facet labels)
  )

#model 4 part 3
mcmc_plot(model4.final, type = "trace", variable  = c('sd_State__Median.Household.Income')) + 
  scale_x_continuous(breaks = c(0, 1000, 2000)) +
  theme(
    strip.background = element_blank(),  # Remove background for variable names (strip labels)
    axis.text.x = element_text(size = 12),  # Set the variable name (x-axis) text size to 20
    axis.text.y = element_text(size = 12),  # Set the variable name (y-axis) text size to 20
    strip.text = element_text(size = 7.5),  # Set the size of the variable name in the title (facet labels)
  )



