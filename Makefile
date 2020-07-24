.PHONY: update	

update: covid-19-data
	cd covid-19-data
	git pull


covid-19-data:
	git clone https://github.com/nytimes/covid-19-data.git
