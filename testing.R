library(tidyverse)
library(lubridate)
library(ggplot2)
library(zoo)
 
testing <- read.csv("owid/covid-testing-all-observations.csv")
testing$date_p <- as.POSIXct(testing$date) 
testing <- testing %>%
    pivot_wider(names_from = overall_outcome, values_from = c(new_results_reported), id_cols = c(state_name,date_p)) %>%
    mutate(positive_pct = 100 * ( Positive / ( Positive + Negative + Inconclusive)))
testing$positive_pct[testing$positive_pct==Inf] <- NA

pdf("output/testing.pdf", width = 11, height=8.5)

# Adjust theme to center title
theme_update(plot.title = element_text(hjust = 0.5))
theme_update(plot.subtitle = element_text(hjust = 0.5))
theme_update(legend.position = c(0.1, 0.9))

d <- testing %>% filter(state_name == "New Jersey") %>%
    group_by(state_name) %>% mutate( m7 = rollapply(positive_pct, 7, mean, fill = NA, align = "right"))

g <- ggplot(d, aes(x = date_p)) +
    geom_bar(stat = "identity", aes(y = positive_pct), color = "grey", fill = "white") +
    geom_line(stat="identity", aes(y = m7, lty = "7 Day Average"),color = "blue", size = 2) +
    theme(panel.grid.major = element_line(color = "black", size = 0.1)) +
    labs(title = "NJ Positivity Percent")
print(g)
 
d <- testing %>% filter(state_name=="New Jersey" | state_name == "Kansas" | state_name == "Texas" | state_name == "Missouri") %>%
    group_by(state_name) %>% mutate( m7 = rollapply(positive_pct, 7, mean, fill = NA, align = "right"))

g <- ggplot(d, aes(x = date_p)) +
    geom_bar(stat = "identity", aes(y = positive_pct), color = "grey", fill = "white") +
    geom_line(stat="identity", aes(y = m7, lty = "7 Day Average"),color = "blue", size = 2) +
    theme(panel.grid.major = element_line(color = "black", size = 0.1)) +
    facet_wrap(~state_name) +
    labs(title = "Firemon State Positivity Percent")
print(g)

d <- testing %>% filter(date_p >= today()-days(30))  %>%
    group_by(state_name) %>% mutate( m7 = rollapply(positive_pct, 7, mean, fill = NA, align = "right"))

g <- ggplot(d, aes(x = date_p)) +
    #geom_bar(stat = "identity", aes(y = positive_pct), color = "grey", fill = "white") +
    geom_line(stat="identity", aes(y = m7),color = "blue", size = 2) +
    theme(panel.grid.major = element_line(color = "black", size = 0.1)) +
    facet_wrap(~state_name) +
    labs(title = "All States Positivity Percent") +
    labs(subtitle = "Last 30 Days")
print(g)
 
dev.off()

print(testing %>%
    filter(date_p >= today()-days(30)) %>%
    group_by(state_name) %>%
    summarize( positive_pct = mean(positive_pct)) %>%
    arrange(desc(positive_pct))
, n = 100)
