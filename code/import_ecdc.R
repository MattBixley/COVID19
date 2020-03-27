# Import from the European CDC

ecdc_url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv/"
#download the dataset from the website to a local temporary file
download.file(url = ecdc_url, destfile = "data/ecdc.csv")

coronavirus <- read_csv("data/ecdc.csv")
