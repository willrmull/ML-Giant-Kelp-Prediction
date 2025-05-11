library(dplyr)
library(ggplot2)
library(readr)
library(reshape2)
library(scales)
library(tcltk2)
library(tidyverse)
library(zoo)
library(lubridate)
library(sf)
library(data.table)
library(readr)

# Program reads in three datasets and combines them into one
# Read in necessary files
# loc: latatude and longitude information for sites
# sst: sea surface temperature 
# drivers: environmental drivers (waves, no3, and biomass)
loc <- read_csv("https://raw.githubusercontent.com/willrmull/ML-Giant-Kelp-Prediction/refs/heads/main/Datasets/SiteKey.csv")
sst <- read.csv("https://raw.githubusercontent.com/willrmull/ML-Giant-Kelp-Prediction/refs/heads/main/Datasets/SST_update2023.csv")
drivers <- read.csv('https://raw.githubusercontent.com/willrmull/ML-Giant-Kelp-Prediction/refs/heads/main/Datasets/SST_update2023.csv', header=TRUE)

#Assigning coordinates to the dataset of biomass and environmental drivers
drivers <- merge(drivers, loc, by=c("site_id"))

#Convert dates to year and quarter
#Find overlapping points
min_lat <- max(min(sst$latitude), min(drivers$lat))
max_lat <- min(max(sst$latitude), max(drivers$lat))
min_lon <- max(min(sst$longitude), min(drivers$lon))
max_lon <- min(max(sst$longitude), max(drivers$lon))
drivers <- drivers %>% filter(between(lat, min_lat, max_lat),
                              between(lon, min_lon, max_lon))
sst <- sst %>% filter(between(latitude, min_lat, max_lat),
                      between(longitude, min_lon, max_lon))

#Adjust SST Dates
sst$date <- as.yearqtr(sst$date)
sst$date <- as.Date(sst$date)
sst <- sst %>% group_by(date, latitude, longitude) %>% 
  summarize(temp = mean(temp))

#Adjust DRIVERS Dates
drivers <- drivers %>% mutate(date = paste(year, quarter))
drivers$date <- as.yearqtr(drivers$date, "%Y%q")
drivers$date <- as.Date(drivers$date)
drivers <- drivers %>% select(-c(year, quarter))

complete_rows <- complete.cases(sst)
# Selecting only complete rows for analysis
sst <- sst[complete_rows, ]
complete_rows <- complete.cases(drivers)
# Selecting only complete rows for analysis
drivers <- drivers[complete_rows, ]

#Converting latitude and longitude to coordinates
df1 <- sst %>%
  st_as_sf(coords = c("longitude", "latitude"), remove = FALSE) %>%
  st_set_crs(2193)
df1 <- filter(df1, date >= "1987-01-01")
df1 <- filter(df1, date <= "2019-10-01")

df2 <- drivers %>% 
  st_as_sf(coords = c("lon", "lat"), remove = FALSE) %>%
  st_set_crs(2193)

#Merging the datasets based on nearest corrdnates
res <- do.call('rbind', lapply(split(df2, 1:nrow(df2)), function(x) {
  st_join(x, df1[df1$date == unique(x$date),], join = st_nearest_feature)
}))

#Selecting Nnecessary rows
df_final <- select(res, c("kelp", "no3", "waves", "temp", "date.x"))
df_final <- df_final %>% drop_na()

#Creating year and date collumns
df_final$date.x <- as.yearqtr(df_final$date.x)
names(df_final)[names(df_final) == 'date.x'] <- 'date'

