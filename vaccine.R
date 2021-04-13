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

d <- vaccine %>% filter(location == "New Jersey")

g <- ggplot(d, aes(x = date_p)) +
    geom_line(stat = "identity", aes(y = people_fully_vaccinated_per_hundred), color = "green", size = 2) +
    geom_line(stat = "identity", aes(y = people_vaccinated_per_hundred), color = "blue", size = 2) +
    labs(title = "NJ Vaccination Percent")

print(g)

max_pct <- max(d$people_vaccinated_per_hundred, na.rm = TRUE)
max_million <- max(d$daily_vaccinations_per_million, na.rm = TRUE)

v <- vaccine %>% filter(date_p >= today()-days(30)) 

g <- ggplot(v, aes(x = date_p)) +
    geom_line(stat = "identity", aes(y = people_fully_vaccinated_per_hundred), color = "green", size = 2) +
    geom_line(stat = "identity", aes(y = people_vaccinated_per_hundred), color = "blue", size = 2) +
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
dev.off()
