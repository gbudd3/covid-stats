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
	data_frame$mean7_delta_cases <- (rollmeanr(data_frame$delta_cases, 7, fill=NA))
	data_frame$mean7_delta_deaths <- (rollmeanr(data_frame$delta_deaths, 7, fill=NA))
	data_frame$mean7_delta2_cases <- (rollmeanr(data_frame$delta2_cases, 7, fill=NA))
	data_frame$mean7_delta2_deaths <- (rollmeanr(data_frame$delta2_deaths, 7, fill=NA))

	if (num_days > 0) {
		data_frame <- data_frame %>% filter(date_p >= today()-days(num_days))
	}

	n <- length(data_frame$date_p)

	df_filtered <- data_frame %>% filter(delta_cases < lag(delta_cases) * 10 & delta_cases < lead(delta_cases) * 10)
 
	g <- ggplot(data_frame, aes(x=date_p))+
		geom_bar(data=df_filtered, stat="identity", aes(y=delta_cases), color="blue", fill="white")+
		geom_line(stat="identity",aes(y=mean7_delta_cases, lty="7 day average"), color="blue", size=2)+
		scale_linetype("")+
		annotate("text", x=data_frame$date_p[n], y=data_frame$mean7_delta_cases[n], size=5, adj=0, label=sprintf("%.1f",data_frame$mean7_delta_cases[n]))+
		labs(title=paste(title, "Cases / Day"))+
		labs(x="Date")+
		labs(caption="Data from NY Times")+
		labs(y="Cases/Day")

	if (num_days >0 ) { g <- g + labs(subtitle=(sprintf("For last %d days",num_days))) }
	print(g)

	df_filtered <- data_frame %>% filter(delta_deaths < lag(delta_deaths) * 10 & delta_deaths < lead(delta_deaths) * 10)

	g <- ggplot(data_frame, aes(x=date_p))+
		geom_bar(data=df_filtered, stat="identity", aes(y=delta_deaths), color="blue", fill="white")+
		geom_line(stat="identity",aes(y=mean7_delta_deaths, lty="7 day average"), color="red", size=2)+
		scale_linetype("")+
		annotate("text", x=data_frame$date_p[n], y=data_frame$mean7_delta_deaths[n], adj=0, size=5, label=sprintf("%.1f",data_frame$mean7_delta_deaths[n]))+
		labs(title=paste(title, "Deaths / Day"))+
		labs(x="Date")+
		labs(caption="Data from NY Times")+
		labs(y="Deaths/Day")

	if (num_days >0 ) { g <- g + labs(subtitle=(sprintf("For last %d days",num_days))) }
	print(g)

 }

graph_counties_for_state <- function(data_frame, in_state, num_days=0) {
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

	if (num_days > 0) {
		state <- state %>% filter(date_p >= today()-days(num_days))
	}

	g <- (ggplot(state, aes(x=date_p))+
			geom_bar(stat="identity", aes(y=delta_cases), color="blue", fill="white")+
			facet_wrap(~county)+
			scale_linetype("")+
			labs(title=paste(in_state, "Cases / Day by County"))+
			labs(x="Date")+
			labs(caption="Data from NY Times")+
			labs(y="Cases/Day")
	)             

	if (num_days >0 ) { g <- g + labs(subtitle=(sprintf("For last %d days",num_days))) }
	print(g)
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

graph_states_cases_100k <- function(data_frame, in_title, num_days=0, hline_nj=0, hline_us=0) {
	data_frame$date_p <- as.POSIXct(data_frame$date)
	n <- 100000

	state <- data_frame %>%
		arrange(state,date_p) %>%
		group_by(state) %>%
		mutate(delta_cases = (cases - lag(cases)) / (pop/n)) %>%
		mutate(delta_cases_pos = ifelse(delta_cases > 0, delta_cases, 0)) %>%
		mutate(delta_deaths = (deaths - lag(deaths)) / (pop/n) ) %>%
		mutate(delta_deaths_pos = ifelse(delta_deaths > 0, delta_deaths, 0)) %>%
		mutate(mean7_delta_cases = rollmeanr(delta_cases, 7, fill=NA))


    if (num_days > 0) {
		state <- state %>% filter(date_p >= today()-days(num_days))
	}
 
	state_filtered <- state %>% filter(delta_cases < lag(delta_cases) * 10 & delta_cases < lead(delta_cases) * 10)

	l <- length(data_frame$date_p)

	g <- (ggplot(state, aes(x=date_p))+
			geom_bar(data=state_filtered, stat="identity", aes(y=delta_cases_pos), color="steelblue", fill="white")+
			geom_line(stat="identity",aes(y=mean7_delta_cases), color="blue", size=2)+
	   		annotate("text", x=data_frame$date_p[l], y=data_frame$mean7_delta_cases[n], size=5, label=sprintf("%3.1f",data_frame$mean7_delta_cases[n]))+
			facet_wrap(~state)+
			scale_linetype("")+
			labs(title=in_title)+
			labs(subtitle = "Horizontal Line is NJ/US 7 Day Mean baseline")+
			labs(x="Date")+
			labs(caption="Data from NY Times")+
			labs(y="Cases/Day per 100K population")
	)             

	if (num_days >0 ) { g <- g + labs(subtitle=(sprintf("For last %d days",num_days))) }

	if (hline_nj >0 ) { g <- g + geom_hline(yintercept=hline_nj) }
	if (hline_us >0 ) { g <- g + geom_hline(yintercept=hline_us) }

	print(g)	
}

graph_states_deaths_100k <- function(data_frame, in_title, num_days=0, hline_nj=0, hline_us=0) {
	data_frame$date_p <- as.POSIXct(data_frame$date)
	n <- 100000

	state <- data_frame %>%
		arrange(state,date_p) %>%
		group_by(state) %>%
		mutate(delta_cases = (cases - lag(cases)) / (pop/n)) %>%
		mutate(delta_deaths = (deaths - lag(deaths)) / (pop/n) ) %>%
		filter(delta_cases > 0 & delta_deaths > 0) %>%
		mutate(mean7_delta_deaths = rollmeanr(delta_deaths, 7, fill=NA))

    if (num_days > 0) {
		state <- state %>% filter(date_p >= today()-days(num_days))
	}
 
	state_filtered <- state %>% filter(delta_deaths < lag(delta_deaths) * 10 & delta_deaths < lead(delta_deaths) * 10)

	l <- length(data_frame$date_p)

	g <- (ggplot(state, aes(x=date_p))+
			geom_bar(data=state_filtered, stat="identity", aes(y=delta_deaths), color="indianred", fill="white")+
			geom_line(stat="identity",aes(y=mean7_delta_deaths), color="red", size=2)+
	   		annotate("text", x=data_frame$date_p[l], y=data_frame$mean7_delta_deaths[n], size=5, label=sprintf("%3.1f",data_frame$mean7_delta_deaths[n]))+
			facet_wrap(~state)+
			scale_linetype("")+
			labs(title=in_title)+
			labs(subtitle = "Horizontal Lines are NJ/US 7 Day Mean baseline")+
			labs(x="Date")+
			labs(caption="Data from NY Times")+
			labs(y="Deaths/Day per 100K population")
	)             

	if (num_days >0 ) { g <- g + labs(subtitle=(sprintf("For last %d days",num_days))) }

	if (hline_nj >0 ) { g <- g + geom_hline(yintercept=hline_nj) }
	if (hline_us >0 ) { g <- g + geom_hline(yintercept=hline_us) }

	print(g)	
}
   
# Setup states and specifically NJ
us <- read.csv("covid-19-data/us.csv")
us$date_p <- as.POSIXct(us$date)

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
graph_covid(states %>% filter(state == "New Jersey"), "New Jersey", 90)
graph_covid(states %>% filter(state == "New Jersey"), "New Jersey")
graph_counties_for_state(counties, "New Jersey", 30)

x <- states %>%
	filter(state=="New Jersey") %>%
	arrange(state,date_p) %>%
	group_by(state) %>%
	mutate(delta_cases = (cases - lag(cases)) / (pop/100000)) %>%
	mutate(delta_deaths = (deaths - lag(deaths)) / (pop/100000) ) %>%
	mutate(mean7_delta_cases = rollmeanr(delta_cases, 7, fill=NA)) %>%
	mutate(mean7_delta_deaths = rollmeanr(delta_deaths, 7, fill=NA))
 
nj_cases <- x$mean7_delta_cases[length(x$mean7_delta_cases)]
nj_deaths <- x$mean7_delta_deaths[length(x$mean7_delta_deaths)]

us_pop <-  state_population %>% filter(name=="United States") %>% select(pop)

x <- us %>%
	arrange(date_p) %>%
	mutate(delta_cases = (cases - lag(cases)) / (us_pop$pop/100000)) %>%
	mutate(delta_deaths = (deaths - lag(deaths)) / (us_pop$pop/100000) ) %>%
	mutate(mean7_delta_cases = rollmeanr(delta_cases, 7, fill=NA)) %>%
	mutate(mean7_delta_deaths = rollmeanr(delta_deaths, 7, fill=NA))
 
us_cases <- x$mean7_delta_cases[length(x$mean7_delta_cases)]
us_deaths <- x$mean7_delta_deaths[length(x$mean7_delta_deaths)]


graph_states_cases_100k(states %>% filter(state=="New York" | state=="New Jersey" | state=="Delaware" | state=="Pennsylvania"),
			 "Cases per 100K / Day for Neighboring States plus NJ", hline_nj=nj_cases)

graph_states_deaths_100k(states %>% filter(state=="New York" | state=="New Jersey" | state=="Delaware" | state=="Pennsylvania"),
			 "Deaths per 100K / Day for Neighboring States plus NJ", 30, hline_nj=nj_deaths)

graph_covid(counties %>% filter(county == "Morris" & state == "New Jersey"), "Morris County")
graph_covid(counties %>% filter(county == "Morris" & state == "New Jersey"), "Morris County", 30)

dev.off()

# Firemon States
pdf("output/covid_firemon_states.pdf", width = 11, height=8.5)
graph_states_cases_100k(states %>% filter(state=="Kansas" | state=="New Jersey" | state=="Missouri" | state=="Texas"),
			 "Cases per 100K / Day for Firemon States", hline_us=us_cases)

graph_states_deaths_100k(states %>% filter(state=="Kansas" | state=="New Jersey" | state=="Missouri" | state=="Texas"),
			 "Deaths per 100K / Day for Firemon States", hline_us=us_deaths)
 dev.off()

# Other states for comparison

pdf("output/covid_all_states.pdf", width = 11, height=8.5)
graph_covid(us, "United States")

graph_states_cases_100k(states, "Cases per 100K / Day for All States", 30, hline_us=us_cases)
graph_states_deaths_100k(states, "Deaths per 100K / Day for All States", 30, hline_us=us_deaths)

for (st in levels(states$state)) {
	graph_covid(states %>% filter(state == st), st)
}
dev.off()

options(tibble.width=120)
options(width=120)
x <- us %>%
	mutate(date_p = as.POSIXct(date)) %>%
	mutate(name = "US") %>%
	arrange(date_p) %>%
	mutate(delta_cases = (cases - lag(cases))) %>%
	mutate(delta_deaths = (deaths - lag(deaths))) %>%
	mutate(mean7_delta_cases = rollmeanr(delta_cases, 7, fill=NA)) %>%
	mutate(mean7_delta_deaths = rollmeanr(delta_deaths, 7, fill=NA)) %>%
	select(name, date, cases, deaths, delta_cases, delta_deaths, mean7_delta_cases, mean7_delta_deaths)

print("US 15 Days")
print(tail(x,15), width=120)
 

x <- states %>%
	mutate(date_p = as.POSIXct(date)) %>%
	filter(state=="New Jersey") %>%
	arrange(state,date_p) %>%
	group_by(state) %>%
	mutate(delta_cases = (cases - lag(cases))) %>%
	mutate(delta_deaths = (deaths - lag(deaths))) %>%
	mutate(mean7_delta_cases = rollmeanr(delta_cases, 7, fill=NA)) %>%
	mutate(mean7_delta_deaths = rollmeanr(delta_deaths, 7, fill=NA)) %>%
	select(state, date, cases, deaths, delta_cases, delta_deaths, mean7_delta_cases, mean7_delta_deaths)

print("NJ 15 Days")
print(tail(x,15), width=120)

x <- counties %>%
	mutate(date_p = as.POSIXct(date)) %>%
	filter(county == "Morris" & state == "New Jersey") %>%
 	arrange(state,county,date_p) %>%
	group_by(county) %>%
	mutate(delta_cases = (cases - lag(cases))) %>%
	mutate(delta_deaths = (deaths - lag(deaths))) %>%
	mutate(mean7_delta_cases = rollmeanr(delta_cases, 7, fill=NA)) %>%
	mutate(mean7_delta_deaths = rollmeanr(delta_deaths, 7, fill=NA)) %>%
	select(county, date, cases, deaths, delta_cases, delta_deaths, mean7_delta_cases, mean7_delta_deaths)
 
print("Morris 15 Days")
print(tail(x,15), width=120)

x <- states %>%
	arrange(state,date_p) %>%
	group_by(state) %>%
	filter(row_number() == n()) %>%
	mutate(cases_100k = cases / (pop / 100000)) %>%
	mutate(deaths_100k = deaths / (pop / 100000)) %>%
	arrange(desc(deaths_100k)) %>%
	select(name,cases, deaths, cases_100k, deaths_100k)
print("Top 20 States deaths/100k")
print(x, n=20, width = 120)

x <- states %>%
	arrange(state,date_p) %>%
	group_by(state) %>%
	mutate(cases_100k = cases / (pop / 100000)) %>%
	mutate(deaths_100k = deaths / (pop / 100000)) %>%
	mutate(delta_cases = (cases - lag(cases))) %>%
	mutate(delta_deaths = (deaths - lag(deaths))) %>%
	mutate(cases_100k = rollmeanr(delta_cases, 30, fill=NA) / ( pop / 100000)) %>%
	mutate(deaths_100k = rollmeanr(delta_deaths, 30, fill=NA) / ( pop / 100000)) %>%
	mutate(fr_pct_100k = ( deaths_100k / cases_100k ) * 100 ) %>%
	mutate(fr_pct = ( deaths / cases ) * 100 ) %>%
	select(name,state, cases, deaths, fr_pct, cases_100k, deaths_100k, fr_pct_100k)

print("States by deaths/100k over last 30 days")
print(x %>% filter(row_number() == n()) %>% arrange(desc(deaths_100k)), n=100, width = 120)

x <- states %>%
	arrange(state,date_p) %>%
	group_by(state) %>%
	mutate(cases_100k = cases / (pop / 100000)) %>%
	mutate(deaths_100k = deaths / (pop / 100000)) %>%
	mutate(delta_cases = (cases - lag(cases))) %>%
	mutate(delta_deaths = (deaths - lag(deaths))) %>%
	mutate(cases_100k = rollmeanr(delta_cases, 7, fill=NA) / ( pop / 100000)) %>%
	mutate(deaths_100k = rollmeanr(delta_deaths, 7, fill=NA) / ( pop / 100000)) %>%
	mutate(fr_pct_100k = ( deaths_100k / cases_100k ) * 100 ) %>%
	mutate(fr_pct = ( deaths / cases ) * 100 ) %>%
	select(name,state, cases, deaths, fr_pct, cases_100k, deaths_100k, fr_pct_100k)

print("States by deaths/100k over last 7 days")
print(x %>% filter(row_number() == n()) %>% arrange(desc(deaths_100k)), n=100, width = 120)
  
