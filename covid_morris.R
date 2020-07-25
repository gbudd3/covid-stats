library(tidyverse)
library(ggplot2)
library(zoo)

# Setup states and specifically NJ
states <- read.csv("covid-19-data/us-states.csv")
states$date_p <- as.POSIXct(states$date)

nj <- states %>% filter(state == "New Jersey") %>% arrange(date_p)
nj <- nj %>% mutate(delta_cases = cases - lag(cases))
nj <- nj %>% mutate(delta_deaths = deaths - lag(deaths))
nj$mean7_delta_cases <- floor(rollmeanr(nj$delta_cases, 7, fill=NA))
nj$mean7_delta_deaths <- floor(rollmeanr(nj$delta_deaths, 7, fill=NA))

# Setup counties, specifically Morris county NJ
counties <- read.csv("covid-19-data/us-counties.csv") 
counties$date_p <- as.POSIXct(counties$date)
morris <- counties %>%
	filter(county == "Morris") %>%
	filter(state == "New Jersey") %>%
	arrange(date_p)

morris <- morris %>% mutate(delta_cases = cases - lag(cases))
morris <- morris %>% mutate(delta_deaths = deaths - lag(deaths))
morris$mean7_delta_cases <- floor(rollmeanr(morris$delta_cases, 7, fill=NA))
morris$mean7_delta_deaths <- floor(rollmeanr(morris$delta_deaths, 7, fill=NA))
