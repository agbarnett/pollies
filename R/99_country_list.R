# 99_country_list.R
# simple list of countries called by other programs
# April 2021

## dropped Japan in December 2019
# all countries:
countries = c('Austria','Australia','Canada','Italy','France','Germany','Netherlands','NZ','Switzerland','UK','USA')
# countries with imputed life table data
countries.imputed = c('Austria','Canada','Germany','NZ','USA','UK')
# countries with political party data
countries_with_politics = c('Austria','Australia','Canada','Germany','Netherlands',"NZ",'Switzerland','USA','UK')

# no politics data for: France (too much missing), Italy, 