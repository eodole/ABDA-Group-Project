rm(list = ls())
df <-read.csv(file = 'C:/Users/anarm/OneDrive/Documents/R Scripts/bayesian/final_data_with_regions_and_winners.csv')

# swap values to finally have DEM as 1 and REP as 0

df$incumbent_party <- 1 - df$incumbent_party
df$Winning.party <- 1 - df$Winning.party

#now make the states numeric as well 

dist_number <- data.frame(
  index = seq_along(unique(df$State)),  
  value = unique(df$State)             
)


df_with_state_index <- merge(df, dist_number, by.x = "State", by.y = "value", all.x = TRUE)


write.csv(df_with_state_index,'C:/Users/anarm/OneDrive/Documents/R Scripts/bayesian/data_with_state_indices.csv')
write.csv(dist_number, 'C:/Users/anarm/OneDrive/Documents/R Scripts/bayesian/dim_state_index.csv')










