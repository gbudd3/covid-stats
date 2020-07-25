all: update output output/covid_nj.pdf

update: covid-19-data
	cd covid-19-data
	git pull

output:
	mkdir output

output/covid_nj.pdf: covid-19-data/us-states.csv
	Rscript covid_morris.R


covid-19-data:
	git clone https://github.com/nytimes/covid-19-data.git



