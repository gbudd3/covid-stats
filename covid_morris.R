library(tidyverse)
library(ggplot2)
library(zoo)

graph_covid <- function(data_frame, title) {
# Graph Stats.  Cases / day (with rolling average) and Deaths / day

	print(ggplot(data_frame, aes(x=date_p))+
		geom_bar(stat="identity", aes(y=delta_cases), color="blue", fill="white")+
		geom_line(stat="identity",aes(y=mean7_delta_cases), color="red", size=2)+
		labs(title=paste(title, "Cases / Day"))+
		labs(x="Date")+
		labs(caption="Data from NY Times")+
		labs(y="Cases/Day")
		)

	print(ggplot(data_frame, aes(x=date_p))+
		geom_bar(stat="identity", aes(y=delta_deaths), color="blue", fill="white")+
		geom_line(stat="identity",aes(y=mean7_delta_deaths), color="red", size=2)+
		labs(title=paste(title, "Deaths / Day"))+
		labs(x="Date")+
		labs(caption="Data from NY Times")+
		labs(y="Deaths/Day")
		)
}

# Setup states and specifically NJ
states <- read.csv("covid-19-data/us-states.csv")
states$date_p <- as.POSIXct(states$date)

nj <- states %>% filter(state == "New Jersey") %>% arrange(date_p)
nj <- nj %>% mutate(delta_cases = cases - lag(cases))
nj <- nj %>% mutate(delta_deaths = deaths - lag(deaths))
nj$mean7_delta_cases <- floor(rollmeanr(nj$delta_cases, 7, fill=NA))
nj$mean7_delta_deaths <- floor(rollmeanr(nj$delta_deaths, 7, fill=NA))
nj$wday <- weekdays(nj$date_p)

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
morris$wday <- weekdays(morris$date_p)

pdf("output/covid_nj.pdf", width = 11, height=8.5)

# Adjust theme to center title
theme_update(plot.title = element_text(hjust = 0.5))
theme_update(plot.subtitle = element_text(hjust = 0.5))

graph_covid(nj, "New Jersey")
graph_covid(morris, "Morris County")

dev.off()
