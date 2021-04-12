all: update output output/covid_nj.pdf

update: covid-19-data owid/us_state_vaccinations.csv
	( cd covid-19-data; git pull -v )
	( curl --output owid/us_state_vaccinations.csv https://covid.ourworldindata.org/data/vaccinations/us_state_vaccinations.csv )

output:
	mkdir output

output/covid_nj.pdf: covid-19-data/us-states.csv
	Rscript covid_morris.R | tee output/covid_nj.txt


covid-19-data:
	git clone https://github.com/nytimes/covid-19-data.git

owid/us_state_vaccinations.csv:
	mkdir owid


