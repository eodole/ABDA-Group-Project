library(xtable)

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


