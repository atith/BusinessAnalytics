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

#get current working directory
cwd <- getwd()

#set path for corona deaths
data_path_1 <- file.path("input_data", "corona_deaths.csv")
absolut_path_1 <- paste (cwd,"/",data_path_1, sep="")

#set path for corona deaths
data_path_2 <- file.path("input_data", "corona_recovered.csv")
absolut_path_2 <- paste (cwd,"/",data_path_2, sep="")

#set path for corona deaths
data_path_3 <- file.path("input_data", "corona_cases.csv")
absolut_path_3 <- paste (cwd,"/",data_path_3, sep="")

# save file
write.csv(jhu_deaths, absolut_path_1, row.names=F)
write.csv(jhu_rec, absolut_path_2, row.names=F)
write.csv(jhu_cases, absolut_path_3, row.names=F)

