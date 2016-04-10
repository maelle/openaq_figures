library("dplyr")
library("ggplot2")
library("Ropenaq")
library("scales")

blue <- "#55828B"
orange <- "#C75233"
theme_set(theme_bw(base_size = 18)) 

# find all locations

locs <- NULL
for (page in 1:3){
  locs <- rbind(locs,
                aq_locations(limit = 1000,
                             page = page))
}

# daily counts
dailyData <- readr::read_csv("data/openaq_daily_numbers.csv") 