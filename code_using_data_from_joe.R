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

# prepare data with cumsum of no. of locations
sumLoc <- locs[,c("firstUpdated")] %>% group_by(firstUpdated) %>%
  summarize(n = n()) %>%
  mutate(cumsum = cumsum(n), 
         type = "locations",
         date = as.Date(firstUpdated)) %>%
  select(date, cumsum, type)

# prepare data with cumsum of no. of measurements
dailyData <- readr::read_csv("data/openaq_daily_numbers.csv") %>%
  mutate(cumsum = cumsum(count), 
         type = "measurements") %>%
  select(date, cumsum, type)

# data for plot
dataPlot <- bind_rows(sumLoc, dailyData)



dataPlot %>%
  ggplot() +
  geom_line(aes(x = date,
                y = cumsum),
            col = blue, size = 2) +
  ylab("Cumulative count")+ 
  #scale_y_log10() +
  scale_x_date(breaks = date_breaks("8 weeks"), date_labels = "%b %y") +
  ggtitle("OpenAQ growth") +
  xlab("Date") +
  facet_grid(type ~ ., scales = "free")

ggsave(file = "figures/growth.png", height = 6, width = 8)
