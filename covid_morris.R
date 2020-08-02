library(tidyverse)
library(lubridate)
library(ggplot2)
library(zoo)

graph_covid <- function(data_frame, title, num_days=0) {
# Graph Stats.  Cases / day (with rolling average) and Deaths / day
	data_frame$date_p <- as.POSIXct(data_frame$date)
	data_frame$wday <- weekdays(data_frame$date_p)
	data_frame <- data_frame %>% arrange(date_p)
	data_frame <- data_frame %>% mutate(delta_cases = cases - lag(cases))
	data_frame <- data_frame %>% mutate(delta2_cases = delta_cases - lag(delta_cases))
	data_frame <- data_frame %>% mutate(delta_deaths = deaths - lag(deaths))
	data_frame <- data_frame %>% mutate(delta2_deaths = delta_deaths - lag(delta_deaths))
	data_frame$mean7_delta_cases <- floor(rollmeanr(data_frame$delta_cases, 7, fill=NA))
	data_frame$mean7_delta_deaths <- floor(rollmeanr(data_frame$delta_deaths, 7, fill=NA))
	data_frame$mean7_delta2_cases <- floor(rollmeanr(data_frame$delta2_cases, 7, fill=NA))
	data_frame$mean7_delta2_deaths <- floor(rollmeanr(data_frame$delta2_deaths, 7, fill=NA))

	if (num_days > 0) {
		data_frame <- data_frame %>% filter(date_p >= today()-days(num_days))
	}

	n <- length(data_frame$date_p)
 
	g <- ggplot(data_frame, aes(x=date_p))+
		geom_bar(stat="identity", aes(y=delta_cases), color="blue", fill="white")+
		geom_line(stat="identity",aes(y=mean7_delta_cases, lty="7 day average"), color="blue", size=2)+
		scale_linetype("")+
		annotate("text", x=data_frame$date_p[n], y=data_frame$mean7_delta_cases[n], size=5, label=sprintf("%.0f",data_frame$mean7_delta_cases[n]))+
		labs(title=paste(title, "Cases / Day"))+
		labs(x="Date")+
		labs(caption="Data from NY Times")+
		labs(y="Cases/Day")

	if (num_days >0 ) { g <- g + labs(subtitle=(sprintf("For last %d days",num_days))) }
	print(g)

	g <- ggplot(data_frame, aes(x=date_p))+
		geom_bar(stat="identity", aes(y=delta_deaths), color="blue", fill="white")+
		geom_line(stat="identity",aes(y=mean7_delta_deaths, lty="7 day average"), color="red", size=2)+
		scale_linetype("")+
		annotate("text", x=data_frame$date_p[n], y=data_frame$mean7_delta_deaths[n], size=5, label=sprintf("%.0f",data_frame$mean7_delta_deaths[n]))+
		labs(title=paste(title, "Deaths / Day"))+
		labs(x="Date")+
		labs(caption="Data from NY Times")+
		labs(y="Deaths/Day")

	if (num_days >0 ) { g <- g + labs(subtitle=(sprintf("For last %d days",num_days))) }
	print(g)

 }

graph_counties_for_state <- function(data_frame, in_state) {
	data_frame$date_p <- as.POSIXct(data_frame$date)
	state <- data_frame %>%
		filter(state == in_state) %>%
		filter(county != "Unknown") %>%
		arrange(county,date_p) %>%
		group_by(county) %>%
		mutate(delta_cases = cases - lag(cases)) %>%
		mutate(delta_deaths = deaths - lag(deaths)) %>%
		mutate(mean7_delta_cases = floor(rollmeanr(delta_cases, 7, fill=NA))) %>%
		mutate(mean7_delta_deaths = floor(rollmeanr(delta_deaths, 7, fill=NA)))

	print(ggplot(state, aes(x=date_p))+
			geom_bar(stat="identity", aes(y=delta_cases), color="blue", fill="white")+
			facet_wrap(~county)+
			scale_linetype("")+
			labs(title=paste(in_state, "Cases / Day by County"))+
			labs(x="Date")+
			labs(caption="Data from NY Times")+
			labs(y="Cases/Day")
	)             
}

graph_states <- function(data_frame, in_title) {
	data_frame$date_p <- as.POSIXct(data_frame$date)
	state <- data_frame %>%
		arrange(state,date_p) %>%
		group_by(state) %>%
		mutate(delta_cases = cases - lag(cases)) %>%
		mutate(delta_deaths = deaths - lag(deaths)) %>%
		mutate(mean7_delta_cases = floor(rollmeanr(delta_cases, 7, fill=NA))) %>%
		mutate(mean7_delta_deaths = floor(rollmeanr(delta_deaths, 7, fill=NA)))

	print(ggplot(state, aes(x=date_p))+
			geom_bar(stat="identity", aes(y=delta_cases), color="blue", fill="white")+
			geom_line(stat="identity",aes(y=mean7_delta_cases, lty="7 day average"), color="blue", size=2)+
			facet_wrap(~state)+
			scale_linetype("")+
			labs(title=in_title)+
			labs(x="Date")+
			labs(caption="Data from NY Times")+
			labs(y="Cases/Day")
	)             
}

graph_states_100k <- function(data_frame, in_title) {
	data_frame$date_p <- as.POSIXct(data_frame$date)
	n <- 100000
	l <- length(data_frame$date_p)

	state <- data_frame %>%
		arrange(state,date_p) %>%
		group_by(state) %>%
		mutate(delta_cases = (cases - lag(cases)) / (pop/n)) %>%
		mutate(delta_deaths = (deaths - lag(deaths)) / (pop/n) ) %>%
		mutate(mean7_delta_cases = rollmeanr(delta_cases, 7, fill=NA))
		
	print(ggplot(state, aes(x=date_p))+
			geom_bar(stat="identity", aes(y=delta_cases), color="blue", fill="white")+
			geom_line(stat="identity",aes(y=mean7_delta_cases, lty="7 day average"), color="blue", size=2)+
	   		annotate("text", x=data_frame$date_p[l], y=data_frame$mean7_delta_cases[n], size=5, label=sprintf("%3.1f",data_frame$mean7_delta_cases[n]))+
			facet_wrap(~state)+
			scale_linetype("")+
			labs(title=in_title)+
			labs(x="Date")+
			labs(caption="Data from NY Times")+
			labs(y="Cases/Day")
	)             
}
  
# Setup states and specifically NJ
us <- read.csv("covid-19-data/us.csv")

# Setup states and specifically NJ
states <- read.csv("covid-19-data/us-states.csv")
state_population <- read.csv("nst-est2019-alldata.csv")
state_population <- state_population %>% 
	select(NAME, STATE, POPESTIMATE2019) %>%
	rename(name=NAME, state_fips=STATE, pop=POPESTIMATE2019)

states <- states %>% left_join(state_population, by = c("fips" = "state_fips"))
states$date_p <- as.POSIXct(states$date)

# Setup counties, specifically Morris county NJ
counties <- read.csv("covid-19-data/us-counties.csv") 
county_population <- read.csv("co-est2019-alldata.csv")
county_population <- county_population %>%
	select(STATE,COUNTY,STNAME,CTYNAME,POPESTIMATE2019) %>%
	rename(state_fips=STATE, county_fips=COUNTY, state_name=STNAME, county_name=CTYNAME, pop=POPESTIMATE2019)

county_population$fips <- county_population$state_fips * 1000 + county_population$county_fips
counties <- counties %>% left_join(county_population, by = c("fips" = "fips"))



pdf("output/covid_nj.pdf", width = 11, height=8.5)

# Adjust theme to center title
theme_update(plot.title = element_text(hjust = 0.5))
theme_update(plot.subtitle = element_text(hjust = 0.5))
theme_update(legend.position = c(0.1, 0.9))

# NJ Graphs
graph_covid(states %>% filter(state == "New Jersey"), "New Jersey")
graph_covid(states %>% filter(state == "New Jersey"), "New Jersey", 30)
graph_counties_for_state(counties, "New Jersey")

graph_states(states %>% filter(state=="New York" | state=="New Jersey" | state=="Delaware" | state=="Pennsylvania"),
			 "Cases / Day for Neighboring States plus NJ")

graph_states_100k(states %>%
				filter(date_p >= today()-days(30)) %>%
				filter(state=="New York" | state=="New Jersey" | state=="Delaware" | state=="Pennsylvania"),
			 "Cases per 100K / Day for last 30 days for Neighboring States plus NJ")


graph_covid(counties %>% filter(county == "Morris" & state == "New Jersey"), "Morris County")
graph_covid(counties %>% filter(county == "Morris" & state == "New Jersey"), "Morris County", 30)

dev.off()

# Other states for comparison

pdf("output/covid_all_states.pdf", width = 11, height=8.5)
graph_covid(us, "United States")

graph_states_100k(states %>%
				filter(date_p >= today()-days(30))
			 ,"Cases per 100K / Day for last 30 days for All States")
 for (st in levels(states$state)) {
	graph_covid(states %>% filter(state == st), st)
}
dev.off()
