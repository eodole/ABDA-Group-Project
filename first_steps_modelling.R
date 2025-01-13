rm(list = ls())

df <-read.csv(file = './data/final_data_with_regions.csv')

library(dplyr)
library(brms)


head(df)


#replace DEM with 0 and REP with 1
df <- df %>%
  mutate(incumbent_party = ifelse(incumbent_party == "DEM", 0L, ifelse(incumbent_party == "REP", 1L, incumbent_party))) %>%
  mutate(Winning.party = ifelse(Winning.party == "DEM", 0L, ifelse(Winning.party == "REP", 1L, Winning.party)))

# change column names to numeric 
df$Winning.party <- as.integer(df$Winning.party)
df$incumbent_party <- as.integer(df$incumbent_party)

#for now i'm going to ignore the non-numeric rows, notably the new districts 

# df_filtered <- df %>%
#   filter(incumbent_party %in% c('0', '1')) %>%
#   filter(Winning.party %in% c('0', '1'))
# 
# df_filtered$incumbent_party <- as.numeric(df_filtered$incumbent_party)
# df_filtered$Winning.party <- as.numeric(df_filtered$Winning.party)
# 


colnames(df)

# Specify the columns to exclude (non numeric)
exclude_cols <- c("stcd", "state_code", "district", "grouping", "Region", "State")

df_filters <- df %>% select(!exclude_cols)

# Create a formula dynamically excluding the specified columns
# form1 <- as.formula(paste("Winning.party ~", paste(setdiff(colnames(df), c("Winning.party", exclude_cols)), collapse = " + ")))


brm_lm1 <- brm(Winning.party ~ ., df_filters)

summary(brm_lm1)
