library("dplyr")
library("ggplot2")
library("ropenaq")
library("scales")

blue <- "#55828B"
orange <- "#C75233"
theme_set(theme_bw(base_size = 18)) 

# find all locations

locs <- NULL
for (page in 1:3){
  locs <- rbind(locs,
                aq_locations(limit = 1000,
                             page = page)$results)
}

# daily counts
dailyData <- readr::read_csv("data/openaq_daily_numbers.csv") 
dailyData <- dailyData %>% 
  mutate(addDay = date %in% as.Date(locs$firstUpdated)) %>%
  mutate(addDay = as.Date(ifelse(addDay, date, NA), origin = "1970-01-01")) %>%
  mutate(noNew = 0)

for(i in 1:nrow(dailyData)){
  if(!is.na(dailyData$addDay[i])){
    dailyData$noNew[i] <- sum(as.Date(locs$firstUpdated) == dailyData$addDay[i])
  }
}
  
# graph with transitions as vertical lines
dailyData %>% ggplot() +
  geom_point(aes(x = date, y = count),
             size = 2,
             col = blue) +
  geom_vline(aes(xintercept = as.numeric(addDay),
                 alpha = log(noNew)),
             size = 1,
             col = orange)
ggsave(file = "figures/transitions.png", height = 6, width = 12)

# now compare to last count if it wasn't already an outlier, so iteratively
# I cannot use lead/lag things because the last count could be an outlier
for(limit in seq(0.1, 1, by = 0.1)){
  dailyData <- dailyData %>% mutate(toolow = FALSE)
  
  for (i in 2:nrow(dailyData)){
    comparisonPoint <- max(which(dailyData[1:(i-1), "toolow"] == FALSE))
    if (dailyData$count[i] < limit*dailyData$count[comparisonPoint]){
      dailyData$toolow[i] <- TRUE
    }
  }
  
  dailyData %>% ggplot() +
    geom_point(aes(x = date, y = count,
                   col = toolow),
               size = 2) +
    scale_colour_manual(values = c(blue, orange))
  ggsave(file = paste0("figures/toolow",limit,".png"), height = 6, width = 12)
}

