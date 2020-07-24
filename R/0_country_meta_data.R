# 0_country_meta_data.R
# meta-data per country
# December 2019
library(dplyr)

## Enter the data into a data.frame
# Oldest = oldest age in life table
# Syear = starting year of politician data (first election)
# Eyear = ending year of politician data (should be year just before censoring)
# median.year = median year used in 2_model_SMR.R to standardise linear effect (depends on length of data)
# imputed.life = is life table imputed
meta = read.table(header=TRUE, stringsAsFactors = FALSE, sep=',', text="
Country,Oldest,Syear,Eyear,censor.date,median.year,imputed.life
Australia,110,1901,2016,'2017-11-29',1950,FALSE
Austria,110,1918,2017,'2018-03-27',1970,TRUE
Brobdingnag,110,1921,2014,'2018-06-01',1950,FALSE
Canada,109,1867,2016,'',1940,TRUE
France,108,1816,2016,'2018-04-12',1900,FALSE
Germany,109,1949,2017,'2017-12-04',1980,FALSE
Italy,110,1945,2014,'2018-01-22',1970,FALSE
Japan,99,1945,2016,'2018-12-31',1970,TRUE
Netherlands,110,1850,2016,'2018-04-13',1930,FALSE
UK,100,1838,2016,'2019-11-12',1960,TRUE
NZ,100,1891,2014,'2018-02-26',1950,TRUE
Switzerland,109,1876,2016,'2018-02-14',1950,FALSE
USA,110,1850,2016,'2016-12-01',1930,FALSE") %>%
  mutate(censor.date = as.Date(censor.date, origin='1970-01-01'))

# save
save(meta, file='data/meta.RData')

## Notes:
# Austria (updated 17 August)
# Australia, 1901 is first year with politicians elected; 2016 is last year with available life table (updated 6 August)
# Brobdingnag (updated 8 August), added for testing purposes, start year is first year after the election
# Canada (updated 19 August)
# France (updated 8 June)
# Germany (updated 23 August)
# Italy (updated 23 May), last year of life table data is 2014; first year of politicians is 1945
# Japan (updated 23 May), need to check max age, First major election in Japan was on 1947-04-20
# New Zealand (updated 23 May), life table data starts in x, some years imputed
# UK (updated 12 December), using SPARQL data; 2016 is last year with life table, could update to last three years
# USA (updated 23 october), not sure of censor date; added imputation in October
# Netherlands (updated 22 October)
