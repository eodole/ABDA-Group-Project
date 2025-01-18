rm(list = ls())
df <- read.csv('C:/Users/anarm/OneDrive/Documents/R Scripts/bayesian/data_with_state_indices.csv')[, -1]
dim_states <- read.csv('C:/Users/anarm/OneDrive/Documents/R Scripts/bayesian/dim_state_index.csv')[, -1]

states_regions <- read.csv('C:/Users/anarm/OneDrive/Documents/R Scripts/bayesian/regions.csv')

dim_regions <- data.frame(
  value = unique(states_regions$Region),               
  index = seq_along(unique(states_regions$Region))     
)


#clean up the regions problem in the original df
library(dplyr)
df <- df %>% select(-Region)

df <- df %>% rename(state_index = index) 

df_with_region <- merge(df, states_regions, by = "State", all.x = TRUE)


# Remove leading/trailing spaces and convert both columns to lowercase
df$State <- tolower(trimws(df$State))
states_regions$State <- tolower(trimws(states_regions$State))

# Merge the region information into the original data frame
df_with_region <- merge(df, states_regions, by = "State", all.x = TRUE)

dim_regions <- dim_regions %>% rename(Region = value) 

df_with_region_index <- merge(df_with_region, dim_regions, by = "Region", all.x = TRUE)

df_with_region_index <- df_with_region_index %>% rename(region_index = index) 



write.csv(df_with_region_index,'C:/Users/anarm/OneDrive/Documents/R Scripts/bayesian/data_with_region_indices.csv')
write.csv(dim_regions, 'C:/Users/anarm/OneDrive/Documents/R Scripts/bayesian/dim_regions_index.csv')







