# download data here: https://stacks.stanford.edu/file/druid:yg821jf8611/yg821jf8611_pa_philadelphia_2020_04_01.rds

library(dplyr)
library(lubridate)
library(ggplot2)
library(magrittr)
library(plyr)

#####################
# PART 1: REAL DATA #
#####################

# Load the data
pa.df <- readRDS("C:/Users/silag/OneDrive/Desktop/Data Science/pa.rds")

# Imagine this file was too big to fit in ram, how would you sample a random subset of it?
subsetSize = 10
random.subset <- pa.df[sample(nrow(pa.df), subsetSize), ]

# Inspect the data. How many rows does it contain? How many columns? What are
# the columns? What date range is covered?
nrow(pa.df) # 1865096 rows
ncol(pa.df) # 22 cols

colnames(pa.df)
min(pa.df$date) # 2014-01-01
max(pa.df$date) # 2018-04-14

# Note we only have a few months for 2018. Drop stops occurring in 2018. Note that you 
# can use the year() function in the lubridate package to get the year from the date. Install
# that package and use that function.
pa.df <- subset(pa.df, year(pa.df$date)<2018, drop = FALSE)

# I claim that raw_row_number is an unnecessary column and want to drop it.
# Gechun wonders if perhaps raw_row_number will let us link rows where an individual
# was stopped several times. Confirm that each row has a unique raw_row_number, and if so,
# drop raw_row_number
anyDuplicated(pa.df$raw_row_number) # no duplicate values
pa.df <- subset(pa.df, select = -raw_row_number)

# What proportion of the stops are pedestrian versus vehicular?
ped <- table(pa.df['type'])['pedestrian']
veh <- table(pa.df['type'])['vehicular']
pedprop <- ped / (ped+veh) # 0.385
vehprop <- veh / (ped+veh) # 0.615

# Create a new object that contains only pedestrian stops. Delete the object to
# keep ram usage down.
ped.df <- pa.df[pa.df$type == 'pedestrian', ]
rm(ped.df)

# Are stop increasing over time? Using a for loop, make a table with the number of stops per year
pa.df$year <- year(pa.df$date)

year.counts <- data.frame(year = unique(pa.df$year), counts = rep(NA, length(unique(pa.df$year))))
for (yr in unique(pa.df$year)){
  count <- sum(pa.df$year == yr)
  year.counts[year.counts$year == yr, 'counts'] <- count
}

# That is a very unnecessary for loop. Make the table without using a for loop.
table(pa.df$year)

# Now make a table of the number of stops per month. HINT: lubridate also has a month() function.
pa.df$year.month <- paste0(pa.df$year, '-', pa.df$month)
month.counts <- table(pa.df$year.month)
month.counts

# That's hard to read, turn the table into a plot and label all axes. You can use ggplot or base R
month.counts.df <- as.data.frame(month.counts)
colnames(month.counts.df) <- c('Month', 'Stops')

stops.plot <- ggplot(month.counts.df, aes(x = Month, y = Stops)) + geom_col()
stops.plot

# Now let's make a plot with the number of stops by race per year
race.year <- as.data.frame(table(pa.df$year, pa.df$subject_race))
names(race.year)[names(race.year) == 'Var1'] <- 'year'
names(race.year)[names(race.year) == 'Var2'] <- 'race'
race.year$year <- as.numeric(as.character(race.year$year))

race.plot <- ggplot(data=race.year, aes(x=year, y=Freq, colour = race)) + geom_line()
race.plot

# Make a second plot with the number of stops by race per year, with separate plots for pedestrian and vehicular stops. With dplyr, we can easily rename multiple variables.
race.year.type <- as.data.frame(table(pa.df$year, pa.df$subject_race, pa.df$type))
race.year.type %>% rename(year = Var1, race = Var2, type = Var3)

# What did this pipe do?
rename(race.year.type, year = Var1, race = Var2, type = Var3)
race.year.type <- rename(race.year.type, year = Var1, race = Var2, type = Var3)

ggplot(data=race.year.type,
       aes(x=year,
           y=Freq,
           colour=race,
           shape = type,
           group = interaction(race, type),
           linetype=type)
) + geom_line()

# Counts aren't that interesting. Let's adjust by population with the data below, from 2017.
# On the assumption that only residents are stopped, what proportion of the different 
# racial identities were stopped in 2017 for pedestrian and vehicular stops?

population.2017 <- data.frame(
  race = c(
    "asian/pacific islander", "black", "hispanic", "other/unknown","white"
  ),
  num.people = c(110864, 648846, 221777, 39858, 548312)
)

# This sort of test is called a "benchmark comparison"
race2017 <- race.year[race.year == 2017,]
race2017 <- left_join(race2017, population.2017, by = 'race')
race2017$Freq/race2017$num.people

# Same for arrest rate
pa.df %>% group_by(subject_race) %>% summarize(arrest_rate = mean(arrest_made))

# If a search found contraband
pa.df[pa.df$type == 'vehicular',] %>% 
  filter(search_conducted) %>% 
  group_by(subject_race) %>% 
  summarize(
    hit_rate = mean(contraband_found, na.rm = T)
  )

# Let's make a map
locations <- pa.df[,c('lat', 'lng', 'subject_race')]
locations <- locations[!is.na(locations$lat),]

philly.map <- ggplot() +
  geom_point(data = locations,
             aes(x = lng, lat, color = subject_race),
             alpha = 1,
             size = .2) +
  labs(title="Geographic Distribution of Citations",
       x ="Longitude", y = "Latitude")
ggsave('philly_map.png')
