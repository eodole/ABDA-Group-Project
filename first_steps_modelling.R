rm(list = ls())

df <-read.csv(file = 'C:/Users/anarm/Downloads/joined_final_with_winner_party2.csv')

library(dplyr)
library(brms)


head(df)


#replace DEM with 0 and REP with 1
df <- df %>%
  mutate(incumbent_party = ifelse(incumbent_party == "DEM", 0, ifelse(incumbent_party == "REP", 1, incumbent_party))) %>%
  mutate(Winning.party = ifelse(Winning.party == "DEM", 0, ifelse(Winning.party == "REP", 1, Winning.party)))


#for now i'm going to ignore the non-numeric rows, notably the new districts 

df_filtered <- df %>%
  filter(incumbent_party %in% c('0', '1')) %>%
  filter(Winning.party %in% c('0', '1'))

df_filtered$incumbent_party <- as.numeric(df_filtered$incumbent_party)
df_filtered$Winning.party <- as.numeric(df_filtered$Winning.party)



colnames(df)

# Specify the columns to exclude
exclude_cols <- c("stcd", "state_code", "district", "grouping")

# Create a formula dynamically excluding the specified columns
form1 <- as.formula(paste("Winning.party ~", paste(setdiff(colnames(df), c("Winning.party", exclude_cols)), collapse = " + ")))


brm_lm1 <- brm(form1, df_filtered)

