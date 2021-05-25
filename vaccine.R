library(tidyverse)
library(lubridate)
library(ggplot2)
library(zoo)
 
vaccine <- read.csv("owid/us_state_vaccinations.csv")
pdf("output/vaccine.pdf", width = 11, height=8.5)

# Adjust theme to center title
theme_update(plot.title = element_text(hjust = 0.5))
theme_update(plot.subtitle = element_text(hjust = 0.5))
theme_update(legend.position = c(0.1, 0.9))

vaccine$date_p <- as.POSIXct(vaccine$date) 
vaccine <- vaccine %>% group_by(location) %>% mutate(d_fullpct = people_fully_vaccinated_per_hundred - lag(people_fully_vaccinated_per_hundred), d_partpct = people_vaccinated_per_hundred  - lag(people_vaccinated_per_hundred))

d <- vaccine %>% filter(location == "New Jersey")

g <- ggplot(d, aes(x = date_p)) +
    geom_line(stat = "identity", aes(y = people_fully_vaccinated_per_hundred), color = "green", size = 2) +
    geom_line(stat = "identity", aes(y = people_vaccinated_per_hundred), color = "blue", size = 2) +
    geom_line(stat = "identity", aes(y = people_vaccinated_per_hundred - people_fully_vaccinated_per_hundred), color = "grey", size = 2) +
    theme(panel.grid.major = element_line(color = "black", size = 0.1)) +
    labs(title = "NJ Vaccination Percent")

print(g)

d<- vaccine %>% filter(location=="New Jersey" | location == "Kansas" | location == "Texas" | location == "Missouri")
g <- ggplot(d, aes(x = date_p)) +
    geom_line(stat = "identity", aes(y = people_fully_vaccinated_per_hundred), color = "green", size = 2) +
    geom_line(stat = "identity", aes(y = people_vaccinated_per_hundred), color = "blue", size = 2) +
    geom_line(stat = "identity", aes(y = people_vaccinated_per_hundred - people_fully_vaccinated_per_hundred), color = "grey", size = 2) +
    facet_wrap(~location) +
    theme(panel.grid.major = element_line(color = "black", size = 0.1)) +
    labs(title = "Firemon States Vaccination Percent")
print(g)

max_pct <- max(d$people_vaccinated_per_hundred, na.rm = TRUE)
max_million <- max(d$daily_vaccinations_per_million, na.rm = TRUE)

v <- vaccine %>% filter(date_p >= today()-days(30)) 

g <- ggplot(v, aes(x = date_p)) +
    geom_line(stat = "identity", aes(y = people_fully_vaccinated_per_hundred), color = "green", size = 2) +
    geom_line(stat = "identity", aes(y = people_vaccinated_per_hundred), color = "blue", size = 2) +
    geom_line(stat = "identity", aes(y = people_vaccinated_per_hundred - people_fully_vaccinated_per_hundred), color = "grey", size = 2) +
    geom_hline(yintercept = max_pct) +
    facet_wrap(~location) +
    labs(title = "Vaccination Percent by State")

print(g)

g <- ggplot(v, aes(x = date_p)) +
    geom_bar(stat = "identity", aes(y = daily_vaccinations_per_million)) +
    geom_hline(yintercept = max_million) +
    facet_wrap(~location) +
    labs(title = "Vaccinations Per Day (per Million)")

print(g)

g <- ggplot(d, aes(x = date_p)) +
    geom_line(stat = "identity", aes(y = d_fullpct), color = "green", size = 2) +
    geom_line(stat = "identity", aes(y = d_partpct), color = "blue", size = 2) +
    facet_wrap(~location) +
    labs(title = "Delta Percent Vaccinations Per Day")

print(g)
dev.off()

print(vaccine %>%
    filter(date_p >= today()-days(30)) %>%
    group_by(location) %>%
    summarize( fully_vaccinated_pct = max(people_fully_vaccinated_per_hundred), vaccinated_pct = max(people_vaccinated_per_hundred)) %>%
    arrange(desc(vaccinated_pct))
, n = 100) 
