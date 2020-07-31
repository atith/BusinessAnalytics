## Atith Songtham, Fabien Böckle
## COVID-19 + financial World Data interactive mapping tool 

## data extracted from Johns Hopkins data obtained from following Github repository
# https://github.com/CSSEGISandData/COVID-19

# load latest Covid-2019 data: confirmed cases
jhu_cases <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))
jhu_cases[is.na(jhu_cases)]=0
total_cases <- sum(jhu_cases[,ncol(jhu_cases)])

# load latest Covid-2019 data: deaths
jhu_deaths <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))
jhu_deaths[is.na(jhu_deaths)]=0
total_deaths <- sum(jhu_deaths[,ncol(jhu_deaths)])

# load latest Covid-2019 data: recovered
jhu_rec <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"))
jhu_rec[is.na(jhu_rec)]=0
total_rec <- sum(jhu_rec[,ncol(jhu_rec)])


# save file
write.csv(jhu_deaths, "C:\\Users\\songtham\\Documents\\Hochschule Aalen\\SS2020\\Business Analytics\\BusinessAnalytics\\CoronaAnalytics\\input_data\\corona_deaths.csv")
write.csv(jhu_rec, "C:\\Users\\songtham\\Documents\\Hochschule Aalen\\SS2020\\Business Analytics\\BusinessAnalytics\\CoronaAnalytics\\input_data\\corona_recovered.csv")
write.csv(jhu_cases, "C:\\Users\\songtham\\Documents\\Hochschule Aalen\\SS2020\\Business Analytics\\BusinessAnalytics\\CoronaAnalytics\\input_data\\corona_cases.csv", row.names=F)
