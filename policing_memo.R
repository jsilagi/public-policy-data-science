library(dplyr)
library(ggplot2)
library(tidyr)

# Load in the data from MA State Patrol stops
df <- readRDS("C:/Users/silag/OneDrive/Desktop/Data Science/ma_state_patrol.rds")

# What timeframe is covered?
min(df$date)
max(df$date)
years <- seq(from=2007,to=2015, by=1)

# How many stops do we have?
nrow(df)

# What columns do we have?
colnames(df)

# Do any of those columns contain more than 1% NA values?
names(which(colSums(is.na(df))>(nrow(df)*0.01)))
#nacols <- names(which(colSums(is.na(df))>(nrow(df)*0.01)))

# We'll exclude these columns from further analysis
df2 <- df %>% select(-c(subject_age, contraband_found, contraband_drugs, contraband_weapons,
                        contraband_other, frisk_performed, search_basis, reason_for_stop))

# Get just the years from the dates
df2$year <- as.integer(format(as.Date(df2$date, format="%Y-%m-%d"),"%Y"))

# We just want to look at vehicular stops for this analysis
table(df2['type'])
# Seems like there aren't any pedestrian stops anyways!

# Let's look at stops by race over time
df2 %>% count(subject_race)
# We want to omit those NA and unknown values
df3 <- df2[!is.na(df2$subject_race),]
df3 <- df3[df3$subject_race!='unknown',]
df3 %>% count(subject_race)
# That's better
nrow(df3)

# Plot those counts
df3 %>% 
  count(year, subject_race) %>% 
  ggplot(aes(x = year, y = n, color = subject_race)) +
  geom_point() +
  geom_line() +
  labs(x="Year", y="Number of Stops", title="Number of Stops by Race Over Time") +
  scale_color_discrete(name="Races", labels = c("Asian/Pacific Islander", "Black", "Hispanic", "White", "Other")) +
  theme(plot.title = element_text(hjust = 0.5))


# How do search rates differ across races?
white_rates <- table(df3$search_conducted[df3$subject_race=='white'])['TRUE'] / table(df3$subject_race)['white']
black_rates <- table(df3$search_conducted[df3$subject_race=='black'])['TRUE'] / table(df3$subject_race)['black']
hispanic_rates <- table(df3$search_conducted[df3$subject_race=='hispanic'])['TRUE'] / table(df3$subject_race)['hispanic']
api_rates <- table(df3$search_conducted[df3$subject_race=='asian/pacific islander'])['TRUE'] / table(df3$subject_race)['asian/pacific islander']
other_rates <- table(df3$search_conducted[df3$subject_race=='other'])['TRUE'] / table(df3$subject_race)['other']

white_rates
black_rates
hispanic_rates
api_rates
other_rates


# What about across years? (still divided by races)
white_rates_year <- c()
for (i in 2007:2015){
  rate <- table(df3$search_conducted[df3$subject_race=='white' & df3$year==i])['TRUE'] / table(df3$subject_race[df3$year==i])['white']
  white_rates_year <- append(white_rates_year, rate)
}
black_rates_year <- c()
for (i in 2007:2015){
  rate <- table(df3$search_conducted[df3$subject_race=='black' & df3$year==i])['TRUE'] / table(df3$subject_race[df3$year==i])['black']
  black_rates_year <- append(black_rates_year, rate)
}
hispanic_rates_year <- c()
for (i in 2007:2015){
  rate <- table(df3$search_conducted[df3$subject_race=='hispanic' & df3$year==i])['TRUE'] / table(df3$subject_race[df3$year==i])['hispanic']
  hispanic_rates_year <- append(hispanic_rates_year, rate)
}
api_rates_year <- c()
for (i in 2007:2015){
  rate <- table(df3$search_conducted[df3$subject_race=='asian/pacific islander' & df3$year==i])['TRUE'] / table(df3$subject_race[df3$year==i])['asian/pacific islander']
  api_rates_year <- append(api_rates_year, rate)
}
other_rates_year <- c()
for (i in 2007:2015){
  rate <- table(df3$search_conducted[df3$subject_race=='other' & df3$year==i])['TRUE'] / table(df3$subject_race[df3$year==i])['other']
  other_rates_year <- append(other_rates_year, rate)
}

rates_year <- c(white_rates_year, black_rates_year, hispanic_rates_year, api_rates_year, other_rates_year)


# Plot those rates
# First convert to df so ggplot can work with it
df_rates <- data.frame(white_rates_year, black_rates_year, hispanic_rates_year, api_rates_year, other_rates_year)
df_rates_long <- df_rates %>% 
  mutate(id = row_number()) %>% 
  gather(key, value, -id)

# Edit those id values to years
for (i in 1:9){
  df_rates_long["id"][df_rates_long["id"] == i] <- years[i]
  }

# Plot
ggplot(df_rates_long, aes(x=id, y=value, color=key)) +
  geom_point() +
  geom_line() +
  ylim(0,0.075) +
  scale_color_discrete(name="Races", labels = c("Asian/Pacific Islander", "Black", "Hispanic", "Other", "White")) +
  labs(x="Year", y="Proportion of stops resulting in a search", title="Searches by Race Over Time") +
  theme(plot.title = element_text(hjust = 0.5))


# Does this change for in-state vs. out-of-state stops?
# In-state rates
white_rates_year_in <- c()
for (i in 2007:2015){
  rate <- table(df3$search_conducted[df3$subject_race=='white' & df3$year==i & df3$vehicle_registration_state=="MA"])['TRUE'] / table(df3$subject_race[df3$year==i & df3$vehicle_registration_state=="MA"])['white']
  white_rates_year_in <- append(white_rates_year_in, rate)
}
black_rates_year_in <- c()
for (i in 2007:2015){
  rate <- table(df3$search_conducted[df3$subject_race=='black' & df3$year==i & df3$vehicle_registration_state=="MA"])['TRUE'] / table(df3$subject_race[df3$year==i & df3$vehicle_registration_state=="MA"])['black']
  black_rates_year_in <- append(black_rates_year_in, rate)
}
hispanic_rates_year_in <- c()
for (i in 2007:2015){
  rate <- table(df3$search_conducted[df3$subject_race=='hispanic' & df3$year==i & df3$vehicle_registration_state=="MA"])['TRUE'] / table(df3$subject_race[df3$year==i & df3$vehicle_registration_state=="MA"])['hispanic']
  hispanic_rates_year_in <- append(hispanic_rates_year_in, rate)
}
api_rates_year_in <- c()
for (i in 2007:2015){
  rate <- table(df3$search_conducted[df3$subject_race=='asian/pacific islander' & df3$year==i & df3$vehicle_registration_state=="MA"])['TRUE'] / table(df3$subject_race[df3$year==i & df3$vehicle_registration_state=="MA"])['asian/pacific islander']
  api_rates_year_in <- append(api_rates_year_in, rate)
}
other_rates_year_in <- c()
for (i in 2007:2015){
  rate <- table(df3$search_conducted[df3$subject_race=='other' & df3$year==i & df3$vehicle_registration_state=="MA"])['TRUE'] / table(df3$subject_race[df3$year==i & df3$vehicle_registration_state=="MA"])['other']
  other_rates_year_in <- append(other_rates_year_in, rate)
}

# Out-of-state rates
white_rates_year_out <- c()
for (i in 2007:2015){
  rate <- table(df3$search_conducted[df3$subject_race=='white' & df3$year==i & df3$vehicle_registration_state!="MA"])['TRUE'] / table(df3$subject_race[df3$year==i & df3$vehicle_registration_state!="MA"])['white']
  white_rates_year_out <- append(white_rates_year_out, rate)
}
black_rates_year_out <- c()
for (i in 2007:2015){
  rate <- table(df3$search_conducted[df3$subject_race=='black' & df3$year==i & df3$vehicle_registration_state!="MA"])['TRUE'] / table(df3$subject_race[df3$year==i & df3$vehicle_registration_state!="MA"])['black']
  black_rates_year_out <- append(black_rates_year_out, rate)
}
hispanic_rates_year_out <- c()
for (i in 2007:2015){
  rate <- table(df3$search_conducted[df3$subject_race=='hispanic' & df3$year==i & df3$vehicle_registration_state!="MA"])['TRUE'] / table(df3$subject_race[df3$year==i & df3$vehicle_registration_state!="MA"])['hispanic']
  hispanic_rates_year_out <- append(hispanic_rates_year_out, rate)
}
api_rates_year_out <- c()
for (i in 2007:2015){
  rate <- table(df3$search_conducted[df3$subject_race=='asian/pacific islander' & df3$year==i & df3$vehicle_registration_state!="MA"])['TRUE'] / table(df3$subject_race[df3$year==i & df3$vehicle_registration_state!="MA"])['asian/pacific islander']
  api_rates_year_out <- append(api_rates_year_out, rate)
}
other_rates_year_out <- c()
for (i in 2007:2015){
  rate <- table(df3$search_conducted[df3$subject_race=='other' & df3$year==i & df3$vehicle_registration_state!="MA"])['TRUE'] / table(df3$subject_race[df3$year==i & df3$vehicle_registration_state!="MA"])['other']
  other_rates_year_out <- append(other_rates_year_out, rate)
}

rates_year_in_out <- c(white_rates_year_in, black_rates_year_in, hispanic_rates_year_in, api_rates_year_in, other_rates_year_in,
                       white_rates_year_out, black_rates_year_out, hispanic_rates_year_out, api_rates_year_out, other_rates_year_out)

# Plot in-state rates
df_rates_in <- data.frame(white_rates_year_in, black_rates_year_in, hispanic_rates_year_in, api_rates_year_in, other_rates_year_in)
df_rates_in_long <- df_rates_in %>% 
  mutate(id = row_number()) %>% 
  gather(key, value, -id)

for (i in 1:9){
  df_rates_in_long["id"][df_rates_in_long["id"] == i] <- years[i]
}

ggplot(df_rates_in_long, aes(x=id, y=value, color=key)) +
  geom_point() +
  geom_line() +
  ylim(0,0.075) +
  scale_color_discrete(name="Races", labels = c("Asian/Pacific Islander", "Black", "Hispanic", "Other", "White")) +
  labs(x="Year", y="Proportion of stops resulting in a search", title="Searches by Race Over Time for In-State Stops") +
  theme(plot.title = element_text(hjust = 0.5))

# Plot out-of-state rates
df_rates_out <- data.frame(white_rates_year_out, black_rates_year_out, hispanic_rates_year_out, api_rates_year_out, other_rates_year_out)
df_rates_out_long <- df_rates_out %>% 
  mutate(id = row_number()) %>% 
  gather(key, value, -id)

for (i in 1:9){
  df_rates_out_long["id"][df_rates_out_long["id"] == i] <- years[i]
}

ggplot(df_rates_out_long, aes(x=id, y=value, color=key)) +
  geom_point() +
  geom_line() +
  ylim(0,0.075) +
  scale_color_discrete(name="Races", labels = c("Asian/Pacific Islander", "Black", "Hispanic", "Other", "White")) +
  labs(x="Year", y="Proportion of stops resulting in a search", title="Searches by Race Over Time for Out-of-State Stops") +
  theme(plot.title = element_text(hjust = 0.5))


# What about across genders?
# Male rates
white_rates_year_male <- c()
for (i in 2007:2015){
  rate <- table(df3$search_conducted[df3$subject_race=='white' & df3$year==i & df3$subject_sex=="male"])['TRUE'] / table(df3$subject_race[df3$year==i & df3$subject_sex=="male"])['white']
  white_rates_year_male <- append(white_rates_year_male, rate)
}
black_rates_year_male <- c()
for (i in 2007:2015){
  rate <- table(df3$search_conducted[df3$subject_race=='black' & df3$year==i & df3$subject_sex=="male"])['TRUE'] / table(df3$subject_race[df3$year==i & df3$subject_sex=="male"])['black']
  black_rates_year_male <- append(black_rates_year_male, rate)
}
hispanic_rates_year_male <- c()
for (i in 2007:2015){
  rate <- table(df3$search_conducted[df3$subject_race=='hispanic' & df3$year==i & df3$subject_sex=="male"])['TRUE'] / table(df3$subject_race[df3$year==i & df3$subject_sex=="male"])['hispanic']
  hispanic_rates_year_male <- append(hispanic_rates_year_male, rate)
}
api_rates_year_male <- c()
for (i in 2007:2015){
  rate <- table(df3$search_conducted[df3$subject_race=='asian/pacific islander' & df3$year==i & df3$subject_sex=="male"])['TRUE'] / table(df3$subject_race[df3$year==i & df3$subject_sex=="male"])['asian/pacific islander']
  api_rates_year_male <- append(api_rates_year_male, rate)
}
other_rates_year_male <- c()
for (i in 2007:2015){
  rate <- table(df3$search_conducted[df3$subject_race=='other' & df3$year==i & df3$subject_sex=="male"])['TRUE'] / table(df3$subject_race[df3$year==i & df3$subject_sex=="male"])['other']
  other_rates_year_male <- append(other_rates_year_male, rate)
}

# Female rates
white_rates_year_female <- c()
for (i in 2007:2015){
  rate <- table(df3$search_conducted[df3$subject_race=='white' & df3$year==i & df3$subject_sex=='female'])['TRUE'] / table(df3$subject_race[df3$year==i & df3$subject_sex=='female'])['white']
  white_rates_year_female <- append(white_rates_year_female, rate)
}
black_rates_year_female <- c()
for (i in 2007:2015){
  rate <- table(df3$search_conducted[df3$subject_race=='black' & df3$year==i & df3$subject_sex=='female'])['TRUE'] / table(df3$subject_race[df3$year==i & df3$subject_sex=='female'])['black']
  black_rates_year_female <- append(black_rates_year_female, rate)
}
hispanic_rates_year_female <- c()
for (i in 2007:2015){
  rate <- table(df3$search_conducted[df3$subject_race=='hispanic' & df3$year==i & df3$subject_sex=='female'])['TRUE'] / table(df3$subject_race[df3$year==i & df3$subject_sex=='female'])['hispanic']
  hispanic_rates_year_female <- append(hispanic_rates_year_female, rate)
}
api_rates_year_female <- c()
for (i in 2007:2015){
  rate <- table(df3$search_conducted[df3$subject_race=='asian/pacific islander' & df3$year==i & df3$subject_sex=='female'])['TRUE'] / table(df3$subject_race[df3$year==i & df3$subject_sex=='female'])['asian/pacific islander']
  api_rates_year_female <- append(api_rates_year_female, rate)
}
other_rates_year_female <- c()
for (i in 2007:2015){
  rate <- table(df3$search_conducted[df3$subject_race=='other' & df3$year==i & df3$subject_sex=='female'])['TRUE'] / table(df3$subject_race[df3$year==i & df3$subject_sex=='female'])['other']
  other_rates_year_female <- append(other_rates_year_female, rate)
}

# Plot male rates
df_rates_male <- data.frame(white_rates_year_male, black_rates_year_male, hispanic_rates_year_male, api_rates_year_male, other_rates_year_male)
df_rates_male_long <- df_rates_male %>% 
  mutate(id = row_number()) %>% 
  gather(key, value, -id)

for (i in 1:9){
  df_rates_male_long["id"][df_rates_male_long["id"] == i] <- years[i]
}

ggplot(df_rates_male_long, aes(x=id, y=value, color=key)) +
  geom_point() +
  geom_line() +
  ylim(0,0.075) +
  scale_color_discrete(name="Races", labels = c("Asian/Pacific Islander", "Black", "Hispanic", "Other", "White")) +
  labs(x="Year", y="Proportion of stops resulting in a search", title="Searches by Race Over Time for Males") +
  theme(plot.title = element_text(hjust = 0.5))

# Plot female rates
df_rates_female <- data.frame(white_rates_year_female, black_rates_year_female, hispanic_rates_year_female, api_rates_year_female, other_rates_year_female)
df_rates_female_long <- df_rates_female %>% 
  mutate(id = row_number()) %>% 
  gather(key, value, -id)

for (i in 1:9){
  df_rates_female_long["id"][df_rates_female_long["id"] == i] <- years[i]
}

ggplot(df_rates_female_long, aes(x=id, y=value, color=key)) +
  geom_point() +
  geom_line() +
  ylim(0,0.075) +
  scale_color_discrete(name="Races", labels = c("Asian/Pacific Islander", "Black", "Hispanic", "Other", "White")) +
  labs(x="Year", y="Proportion of stops resulting in a search", title="Searches by Race Over Time for Females") +
  theme(plot.title = element_text(hjust = 0.5))
