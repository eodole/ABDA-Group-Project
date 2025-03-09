library(xtable)


### Model Summaries 
sum1 <- summary(model1.final)
sum_table1 <- rbind((sum1$fixed), (sum1$random$State))
xtable(sum_table1, digits = 2)

sum2 <- summary(model2.final)
sum_table2 <- rbind((sum2$fixed), (sum2$random$Region))
xtable(sum_table2, digits = 2)



sum3 <- summary(model3.final)
sum_table3 <- rbind((sum3$fixed), (sum3$random$Region), sum3$random$`Region:State`)
xtable(sum_table3, digits = 2)



sum4 <- summary(model4.final)
sum_table4 <- rbind((sum4$fixed), (sum4$random$Region), sum4$random$State)
xtable(sum_table4, digits = 2)
################################################################################
#Conditional Effects

plot(conditional_effects(model1.final, re_formula = NULL), points = TRUE)

plot(conditional_effects(model2.final, re_formula = NULL), points = TRUE)

plot(conditional_effects(model3.final, re_formula = NULL), points = TRUE)

plot(conditional_effects(model4.final, re_formula = NULL), points = TRUE)



conditions1 <- data.frame(State = unique(df$State)) 
rownames(conditions1) <- unique(df$State)
me1_state <- conditional_effects(model1.final, conditions = conditions1, effects = "urbanindex", re_formula = NULL) 
plot(me1_state, ncol = 7, points = TRUE)


conditions2 <- data.frame(Region = unique(df$Region)) 
rownames(conditions2) <- unique(df$Region)
me2_region <- conditional_effects(model2.final, conditions = conditions2, effects = "urbanindex", re_formula = NULL) 
plot(me2_region, ncol = 2, points = TRUE)


conditions3 <- data.frame(Region = unique(df$Region:df$State)) 
rownames(conditions3) <- unique(df$Region:df$State)
me3_region <- conditional_effects(model3.final, conditions = conditions2, effects = "urbanindex", re_formula = NULL) 
plot(me3_region, ncol = 2, points = TRUE)

me3_state <- conditional_effects(model3.final, conditions = conditions3, effects = "urbanindex", re_formula = NULL) 
plot(me3_state, ncol = 7, points = TRUE)



me4_region <- conditional_effects(model4.final, conditions = conditions2, effects = "urbanindex", re_formula = NULL) 
plot(me4_region, ncol = 2, points = TRUE)

me4_state <- conditional_effects(model4.final, conditions = conditions1, effects = "urbanindex", re_formula = NULL) 
plot(me4_state, ncol = 7, points = TRUE)




##################################################################3
## Data Summaries Table 

library(dplyr)
library(vtable)

df <- read.csv("./report_code/final_data_for_report.csv")


# # rescale total population and household income
# joined_final$Total.Population <- joined_final$Total.Population/1000000
# joined_final$Median.Household.Income <- joined_final$Median.Household.Income/100000

df$Total.Population <- df$Total.Population*1000000
df$Median.Household.Income <- df$Median.Household.Income*100000

sumtable(df, 
         vars = c("urbanindex", "Total.Population", "pct.women", "Median.Household.Income","Mean.Household.Income", "pct.retirees", "pct.bach"),
         out = "latex")



