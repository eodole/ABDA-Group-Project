
library(dplyr)
library(stringr)


### Read the CSV files
# df_elect_res <- read.csv("./data/election_results_house.csv")
df_races <- read.csv("./data/races.csv")
df_urb_index <- read.csv("./data/urbanization-index-2022.csv")
df_demographics <- read.csv("./data/us_demographic_data_clean.csv")

### Filter specifically for races involved in analysis
df_house_races_2022 = df_races %>% filter(cycle == 2022, 
                                          type == "HouseRace",
                                          stage == "general" | stage == "runoff", 
                                          special == "false") 

# Ensure data types are correctly interpretted
df_house_races_2022$date = as.Date(df_house_races_2022$date,"%m/%d/%y")

### Join the datasets by 'state' and 'district'

# Adjust Column names for consistency, state, district etc. should be names consistently
df2 <- df_house_races_2022 %>%
  mutate(district = as.double(str_remove(office_seat_name, "^District ")), state_code = state_abbrev) 

df3 <- df_urb_index %>%
  rename(
    district = cd,
    state_code = state
  )

df_demographics <- df_demographics %>%
  rename(
    district = Congressional.District.Number,
  )



### Join Datasets of interest

# df_urbanization_and_election_res
df_urbanization_and_races <- df3 %>%
  inner_join(df2, by = c("state_code", "district")) 


df_all_vars <- df_urbanization_and_races %>%
  inner_join(df_demographics, by = c("State", "district")) 


### Refine Dataset to only columns of interest

# Define the columns to keep
cols_to_keep <- c("stcd","State", "district","urbanindex",
                  "grouping", "Winning.party",
                  "Total.Population", "Percentage.Women", "Median.Household.Income",
                  "Mean.Household.Income", "Median.Age",
                  "Percentage.Retirees..65..",
                  "Percentage.Bachelors.Degree.in.Population.over.25",
                  "Unemployment.Rate..16.and.over.")

# Keep only the specified columns
df_all_vars <- df_all_vars %>%
  select(all_of(cols_to_keep))


### Add Regions Data 

# import region data 
regions <- read.csv("./data/regions.csv")
# join data 
joined_final <- df_all_vars %>%
  left_join(regions, by = "State") 



##### Data Cleaning ###### 

# Logistic Encoding: Replace DEM with 1 and REP with 0
joined_final <- joined_final %>%
  mutate(Winning.party = ifelse(Winning.party == "DEM", 1L, ifelse(Winning.party == "REP", 0L, Winning.party)))

# change columns to numeric 
joined_final$Winning.party <- as.integer(joined_final$Winning.party)

# rename columns for convience 
joined_final = joined_final %>% 
  rename(
    urban.cat = grouping,
    pct.retirees = Percentage.Retirees..65.., 
    pct.unemployed = Unemployment.Rate..16.and.over.,
    pct.bach = Percentage.Bachelors.Degree.in.Population.over.25,
    pct.women = Percentage.Women
  )

# rescale total population and household income
joined_final$Total.Population <- joined_final$Total.Population/1000000
joined_final$Median.Household.Income <- joined_final$Median.Household.Income/100000

#rescale percentage variables 
joined_final$pct.bach <- joined_final$pct.bach*100
joined_final$pct.retirees <- joined_final$pct.retirees*100
joined_final$pct.unemployed <- joined_final$pct.unemployed*100
joined_final$pct.women <- joined_final$pct.women*100



write.csv(joined_final, "final_data_for_report.csv", row.names = FALSE)

