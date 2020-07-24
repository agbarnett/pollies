# 3_table_DIC.R
# table DICs
# August 2019
library(dplyr)

# get the list of countries
source('R/99_country_list.R')
# loop through the countries
DIC = NULL
for (country in countries){
  infile = paste('results/', country, '.DIC.RData', sep='') # from 3_model_SMR.R
  load(infile)
  dic.tab = mutate(dic.tab, country=country)
  DIC = bind_rows(DIC, dic.tab)
  remove(dic.tab) # tidy up
}

# best per model
best = group_by(DIC, country) %>%
  mutate(minD = min(DIC)) %>%
  filter(DIC == minD) %>%
  ungroup() %>%
  select(country, Model) %>%
  rename(Best = Model)

# wide to long
for.table = mutate(DIC, 
                   pD = sprintf(round(pD*10)/10, fmt='%3.1f'),
                   DIC = sprintf(round(DIC*10)/10, fmt='%4.1f'),
                   both = paste(DIC, ' (', pD, ')', sep='')) %>%
            select(-pD, -DIC) %>%
            tidyr::spread(Model, both) %>%
  select(country, Linear, everything()) %>%
  left_join(best, by='country') # add best

# tabulate, put on github
library(pander)
pander(for.table)
