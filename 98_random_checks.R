# 98_random_checks.R
# August 2019
# randomly check 10 politicians from each country
library(dplyr)
library(ggplot2)

# get the list of countries
source('R/99_country_list.R')
countries = countries[countries!='Japan'] # remove Japan because no names

# randomly sample from each country
random = NULL
for (c in countries){
  infile = paste('data/', c, '/', c, '.RData', sep='')
  load(infile)
  pol = sample_n(politicians, 10) %>% mutate(country = c)
  if('Name' %in% names(pol) == FALSE){pol = mutate(pol, Name='')} # Add blank name if it's not there
  pol = mutate(pol, 
           Name = ifelse(country=='Australia', paste(First_name, Surname), Name),
           Name = ifelse(country=='Switzerland', paste(First_name, Surname), Name),
           Name = ifelse(country=='France', paste(Firstname, Surname), Name),
           Name = ifelse(country=='Italy', paste(First_name, Surname), Name),
           Name = ifelse(country=='NZ', paste(Firstname, Surname), Name),
           Name = ifelse(country=='UK', paste(FirstName, Surname), Name),
           Name = ifelse(country=='USA', paste(name.first, name.last), Name)
#           DOD = ifelse(Status=='Living', as.Date(''), DOD)
    ) %>%
    select(country, Name, Sex, DOB, Date_entered, DOD, Status)
  random = bind_rows(random, pol)
}

# output
write.table(random, file='random.check.txt', sep = '\t', quote=FALSE, row.names = FALSE)


## Further check of politicians alive in UK
infile = paste('data/UK/UK.RData', sep='')
load(infile)
# check those over 80 and alive
old = mutate(politicians, 
             age = floor(as.numeric((DOD - DOB)/365.25))) %>%
  filter(age > 85, Status=='Living')

# output
write.table(old, file='random.check.UK.txt', sep = '\t', quote=FALSE, row.names = FALSE)
