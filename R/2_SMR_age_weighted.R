# 2_SMR_age_weighted.R
# Calculate annual standardised mortality ratio, works on multiple countries
# Version that stratifies by age
# Feb 2020
library(dplyr)
source('R/0_population_weights.R') # for population weights
source('R/99_country_list.R') # get list of countries

## loop through countries
for (country in countries){

  use.imputed = ifelse(country %in% countries.imputed, TRUE, FALSE)
  
# get the country's meta-data
load('data/meta.RData')
meta = filter(meta, Country==country)
# get the country's politician and life table data (imputed or not)
data.file = paste('data/', country, '/', country, '.RData', sep='')
if(use.imputed == TRUE){data.file = paste('data/', country, '/', country, '.imputed.RData', sep='')}
load(data.file) # from 0_import_data_`country`.R or 1_impute_life_tables_nonbayes.RR
politicians = mutate(politicians, entry.year = as.numeric(format(Date_entered, '%Y'))) # create entry year, needed later for fractional year in election year
if(use.imputed == FALSE){ # if not using imputed data, then create column of "No" for imputed
  life.table = mutate(life.table, imputed='No')
}

to.SMR = mutate(politicians, 
                follow.time = as.numeric(difftime(DOD, Date_entered, units='days')/365.25)) # observed follow-up time in years from entry to death (or censoring)

## Inner loop of calculating SMR
# merge a year of policitians with a year of data
SMR = NULL
for (year in meta$Syear:meta$Eyear){ 
  # get life table for this year
  this.life = ungroup(life.table) %>%
    filter(Year==year) %>% 
    dplyr::select(Age, Sex, qx, imputed)
  # get relevant politicians
  year.start = as.Date(paste(year, '-01-01', sep="")) # start of year
  year.end = as.Date(paste(year, '-12-31', sep="")) # end of year
  # calculate expected deaths
  exp = filter(to.SMR, Date_entered<=year.end & DOD>=year.start) %>% # entered some time during the year and were alive at the start of the year
    mutate(age = floor(as.numeric(difftime(year.start, DOB, units='days')/365.25)),  # calculate age at start of year ('current' age)
           age = ifelse(age > meta$Oldest, meta$Oldest, age), # change any ages over limit of life table
           age.group = case_when(age<=19 ~ 1,
                                 age>19 & age<= 24 ~ 2,
                                 age>24 & age<= 29 ~ 3,
                                 age>29 & age<= 34 ~ 4,
                                 age>34 & age<= 39 ~ 5,
                                 age>39 & age<= 44 ~ 6,
                                 age>44 & age<= 49 ~ 7,
                                 age>49 & age<= 54 ~ 8,
                                 age>54 & age<= 59 ~ 9,
                                 age>59 & age<= 64 ~ 10,
                                 age>64 & age<= 69 ~ 11,
                                 age>69 & age<= 74 ~ 12,
                                 age>74 & age<= 79 ~ 13,
                                 age>79 & age<= 84 ~ 14,
                                 age>84 ~ 15)) # add age group to match weights
  # fractional first year (could not get this to work in dplyr)
  index = exp$entry.year == year
  exp$frac = 1 
  if(any(index)){exp$frac[index] = as.numeric(difftime(year.end, exp$Date_entered[index], units='days'))/365.25}
  # merge with life table by age and sex
  exp = left_join(exp, this.life, by=c('age'='Age', 'Sex'='Sex')) %>% 
    group_by(age.group) %>% # grouped by age groups
    summarise(expected=sum(qx*frac)) %>% # expected deaths, fractional time in first year
    ungroup()
  
  # calculate observed deaths
  obs = filter(to.SMR, DOD>=year.start & DOD<=year.end & Status=='Dead') %>% # died this year (not censored)
    mutate(age = floor(as.numeric(difftime(DOD, DOB, units='days')/365.25)), # add age at death
           age.group = case_when(age<=19 ~ 1,
                                 age>19 & age<= 24 ~ 2,
                                 age>24 & age<= 29 ~ 3,
                                 age>29 & age<= 34 ~ 4,
                                 age>34 & age<= 39 ~ 5,
                                 age>39 & age<= 44 ~ 6,
                                 age>44 & age<= 49 ~ 7,
                                 age>49 & age<= 54 ~ 8,
                                 age>54 & age<= 59 ~ 9,
                                 age>59 & age<= 64 ~ 10,
                                 age>64 & age<= 69 ~ 11,
                                 age>69 & age<= 74 ~ 12,
                                 age>74 & age<= 79 ~ 13,
                                 age>79 & age<= 84 ~ 14,
                                 age>84 ~ 15)) %>% # add age group to match weights
    group_by(age.group) %>% # grouped by age groups
    summarise(observed=n()) %>%
    ungroup()
  
  # merge expected and observed, then concatenate
  frame = left_join(exp, obs, by='age.group') %>%
    mutate(observed = tidyr::replace_na(observed, 0),
           year = year,
           imputed=this.life$imputed[1])
  SMR = rbind(SMR, frame)
  
} # end of inner loop over years

# sum(x*w) / sum(w) is the weighted mean
SMR = left_join(SMR, pop.weights, by='age.group')  %>% # add population weights by age group
  group_by(year, imputed) %>% # group by years (add imputed information)
  summarise(O = sum(observed),
            E = sum(expected),
            SMR = O / E, # standard SMR
            n = n(), # number of observations used
            observed.weighted = sum(observed*weight) / sum(weight) * n, # weighted observed, times by n for sum
            expected.weighted = sum(expected*weight) / sum(weight) * n, # weighted expected
            SMRw = observed.weighted / expected.weighted) %>% # weighted SMR within each year
  ungroup()
  
## save
outfile = paste('data/', country, '/SMR.weighted.RData', sep='')
save(SMR, file=outfile)

} # end of country loop
