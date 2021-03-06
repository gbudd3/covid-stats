all: update output output/covid_nj.pdf

update: covid-19-data owid/us_state_vaccinations.csv owid/covid-testing-all-observations.csv
	( cd covid-19-data; git pull -v )
	( curl --output owid/us_state_vaccinations.csv https://covid.ourworldindata.org/data/vaccinations/us_state_vaccinations.csv )
	( curl --output owid/covid-testing-all-observations.csv https://healthdata.gov/api/views/j8mb-icvb/rows.csv?accessType=DOWNLOAD )

output:
	mkdir output

output/covid_nj.pdf: covid-19-data/us-states.csv
	Rscript covid_morris.R | tee output/covid_nj.txt
	Rscript vaccine.R | tee output/vaccine.txt
	Rscript testing.R | tee output/testing.txt


covid-19-data:
	git clone https://github.com/nytimes/covid-19-data.git

owid/us_state_vaccinations.csv:
	mkdir owid


