library(tidyverse)
library(ggplot2)
library(zoo)

graph_covid <- function(data_frame, title) {
# Graph Stats.  Cases / day (with rolling average) and Deaths / day
	data_frame$date_p <- as.POSIXct(data_frame$date)
	data_frame$wday <- weekdays(data_frame$date_p)
	data_frame <- data_frame %>% arrange(date_p)
	data_frame <- data_frame %>% mutate(delta_cases = cases - lag(cases))
	data_frame <- data_frame %>% mutate(delta_deaths = deaths - lag(deaths))
	data_frame$mean7_delta_cases <- floor(rollmeanr(data_frame$delta_cases, 7, fill=NA))
	data_frame$mean7_delta_deaths <- floor(rollmeanr(data_frame$delta_deaths, 7, fill=NA))

	n <- length(data_frame$date_p)
 
	print(ggplot(data_frame, aes(x=date_p))+
		geom_bar(stat="identity", aes(y=delta_cases), color="blue", fill="white")+
		geom_line(stat="identity",aes(y=mean7_delta_cases), color="red", size=2)+
		annotate("text", x=data_frame$date_p[n], y=data_frame$mean7_delta_cases[n], size=5, label=sprintf("%.0f",data_frame$mean7_delta_cases[n]))+
		labs(title=paste(title, "Cases / Day"))+
		labs(x="Date")+
		labs(caption="Data from NY Times")+
		labs(y="Cases/Day")
		)

	print(ggplot(data_frame, aes(x=date_p))+
		geom_bar(stat="identity", aes(y=delta_deaths), color="blue", fill="white")+
		geom_line(stat="identity",aes(y=mean7_delta_deaths), color="red", size=2)+
		annotate("text", x=data_frame$date_p[n], y=data_frame$mean7_delta_deaths[n], size=5, label=sprintf("%.0f",data_frame$mean7_delta_deaths[n]))+
		labs(title=paste(title, "Deaths / Day"))+
		labs(x="Date")+
		labs(caption="Data from NY Times")+
		labs(y="Deaths/Day")
		)
}

# Setup states and specifically NJ
states <- read.csv("covid-19-data/us-states.csv")

# Setup counties, specifically Morris county NJ
counties <- read.csv("covid-19-data/us-counties.csv") 

pdf("output/covid_nj.pdf", width = 11, height=8.5)

# Adjust theme to center title
theme_update(plot.title = element_text(hjust = 0.5))
theme_update(plot.subtitle = element_text(hjust = 0.5))

graph_covid(states %>% filter(state == "New Jersey"), "New Jersey")
graph_covid(counties %>% filter(county == "Morris" & state == "New Jersey"), "Morris County")
graph_covid(states %>% filter(state == "Texas"), "Texas")
graph_covid(states %>% filter(state == "Florida"), "Florida")
graph_covid(states %>% filter(state == "Arizona"), "Arizona")

dev.off()
