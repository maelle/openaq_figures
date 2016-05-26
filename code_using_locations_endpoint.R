#' ---
#' title: "Figures for Christa"
#' author: "M. Salmon"
#' ---

library("knitr")
opts_chunk$set(cache = TRUE)
opts_chunk$set(echo = FALSE)
opts_chunk$set(warning = FALSE)
opts_chunk$set(message = FALSE)


#'
library("ropenaq")
library("dplyr")
library("tidyr")
library("ggplot2")
library("scales")

Sys.setlocale("LC_ALL","English")
blue <- "#55828B"
orange <- "#C75233"

# find all locations
locs <- NULL
for (page in 1:3){
  locs <- rbind(locs,
                aq_locations(limit = 1000,
                             page = page)$results)
}

#' # growth of platform
#' I'm not sure whether having both no. of countries and no. of locations
#' is that interesting?
#' Regarding no. of measurements for now I've done nothing. I'm not sure how to do it 
#' and whether it would be interesting
theme_set(theme_bw(base_size = 17)) 
locs[,c("firstUpdated")] %>% group_by(firstUpdated) %>%
  summarize(n = n()) %>%
  mutate(cumsum = cumsum(n)) %>%
  ggplot() +
  geom_line(aes(x = firstUpdated,
                 y = cumsum),
            col = blue, size = 2) +
  ylab("No. of locations")+ 
  scale_x_datetime(breaks = date_breaks("8 weeks"), date_labels = "%b %y") +
  ggtitle("OpenAQ locations") +
  xlab("Date of inclusion in the platform")
ggsave(file = "figures/locations_growth.png", width = 8, height = 5)


locs[,c("firstUpdated", "country")] %>% group_by(country) %>%
  summarize(firstUpdated = min(firstUpdated)) %>%
  ungroup()%>% group_by(firstUpdated) %>%
  summarize(n = n()) %>%
  mutate(cumsum = cumsum(n)) %>%
  ggplot() +
  geom_line(aes(x = firstUpdated,
                y = cumsum),
            col = blue, size = 2)+
  ylab("No. of countries")+ 
  scale_x_datetime(breaks = date_breaks("8 weeks"), date_labels = "%b %y") +
  ggtitle("OpenAQ countries") +
  xlab("Date of inclusion in the platform")
ggsave(file = "figures/countries_growth.png", width = 7, height = 5)

#' # Parameters
#' I first add to change some things in code, now it's easier to do.
#' Here we can change the order on the graph if you want!

locs %>% gather(parameter, present, pm25:bc) %>%
  filter(present == TRUE) %>%
  ggplot() +
  geom_bar(aes(x = parameter), fill = orange,
           size = 1) +
  ylab("No. of locations monitoring the parameter")+
  xlab("Parameter")+
  scale_x_discrete(breaks=c("bc", "co", "no2", "so2",
                            "o3", "pm10", "pm25",
                            "so2"),
                   labels=c("BC", "CO", expression(NO[2]),
                            expression(O[3]),
                            "PM10", "PM2.5",
                            expression(SO[2]))) +
  ggtitle("Monitored parameters")
ggsave(file = "figures/parameters1.png", width = 6, height = 5)



#' combinations?

locs %>% group_by(location) %>%
  summarize(noOfPar = o3 + pm25 + pm10 + so2 + no2 + co + bc) %>%
  ungroup() %>% left_join(locs) %>%
  gather(parameter, present, pm25:bc) %>%
  filter(present == TRUE) %>% 
  group_by(location) %>%
  summarize(parameters = toString(parameter)) %>%
  select(parameters)  %>%
  group_by(parameters) %>%
  summarize(count = n()) %>%
  mutate(parameters = ordered(parameters,
                              levels = parameters[order(-count)])) %>%
  arrange(-count) %>% readr::write_csv("combination.csv")