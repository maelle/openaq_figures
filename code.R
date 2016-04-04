#' ---
#' title: "Figures for Christa"
#' author: "M. Salmon"
#' ---

library("knitr")
opts_chunk$set(cache = TRUE)
opts_chunk$set(echo = FALSE)

library("Ropenaq")
library("dplyr")
library("tidyr")
library("ggplot2")

# find all locations
locs <- NULL
for (page in 1:3){
  locs <- rbind(locs,
                aq_locations(limit = 1000,
                             page = page))
}

#' # growth of platform

locs[,c("firstUpdated")] %>% group_by(firstUpdated) %>%
  summarize(n = n()) %>%
  mutate(cumsum = cumsum(n)) %>%
  ggplot() +
  geom_line(aes(x = firstUpdated,
                 y = log(cumsum))) +
  ylab("Log of no. of locations")

locs[,c("firstUpdated", "country")] %>% group_by(country) %>%
  summarize(firstUpdated = min(firstUpdated)) %>%
  ungroup()%>% group_by(firstUpdated) %>%
  summarize(n = n()) %>%
  mutate(cumsum = cumsum(n)) %>%
  ggplot() +
  geom_line(aes(x = firstUpdated,
                y = cumsum))+
  ylab("No. of countries")