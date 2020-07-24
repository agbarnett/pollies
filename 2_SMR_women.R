# 2_SMR_women.R
# Calculate annual standardised mortality ratio for women only, works on multiple countries
# Extended to examine multiple follow-up times for animations
# March 2020
library(dplyr)
source('R/99_country_list.R') # get list of countries

# big loop
for (country in countries){
  # get the country's meta-data
  load('data/meta.RData')
  meta = filter(meta, Country==country)
  use.imputed = country %in% countries.imputed  # is there any imputed life table data?
  
  # get the country's politician and life table data (imputed or not)
  data.file = paste('data/', country, '/', country, '.RData', sep='')
  if(use.imputed == TRUE){data.file = paste('data/', country, '/', country, '.imputed.RData', sep='')}
  load(data.file) # from 0_import_data_`country`.R or 1_impute_life_tables_nonbayes.RR
  politicians = mutate(politicians, entry.year = as.numeric(format(Date_entered, '%Y'))) %>% # create entry year, needed later for fractional year in election year
    filter(Sex=='Female') # only women
  if(use.imputed == FALSE){ # if not using imputed data, then create column of "No" for imputed
    life.table = mutate(life.table, imputed='No')
  }
  life.table = filter(life.table, Sex=='Female')

## Loop of calculating SMR
# merge a year of policitians with a year of data
SMR = ages = NULL
for (year in meta$Syear:meta$Eyear){ 
  # get life table for this year
  this.life = filter(life.table, Year==year) %>% 
    dplyr::select(Age, Sex, qx, imputed)
  # get relevant politicians
  year.start = as.Date(paste(year, '-01-01', sep="")) # start of year
  year.end = as.Date(paste(year, '-12-31', sep="")) # end of year
  # calculate expected deaths
  eligible = filter(politicians, 
               Date_entered<=year.end & DOD>=year.start) # entered some time during the year and were alive at the start of the year
  if(nrow(eligible) > 0){ # do not run this year if there are no women
    exp = mutate(eligible, 
                 age = floor(as.numeric(difftime(year.start, DOB, units='days')/365.25)),  # calculate age at start of year ('current' age)
                 age = ifelse(age > meta$Oldest, meta$Oldest, age)) # change any ages over limit of life table
    # fractional first year (could not get this to work in dplyr)
    index = exp$entry.year == year
    exp$frac = 1 
    if(any(index)){exp$frac[index] = as.numeric(difftime(year.end, exp$Date_entered[index], units='days'))/365.25}
    # merge with life table by age and sex
    exp = left_join(exp, this.life, by=c('age'='Age', 'Sex'='Sex')) %>% 
      summarise(expected=sum(qx*frac)) # expected deaths, fractional time in first year
    
    # calculate observed deaths
    obs = filter(politicians, DOD>=year.start & DOD<=year.end & Status=='Dead') %>% # died this year (not censored)
      summarise(observed=n())
    # calculate SMR and concatenate; add imputed (yes/no) flag for life tables
    frame = data.frame(year=year, N=nrow(eligible), O=obs$observed, E=exp$expected, imputed=this.life$imputed[1])
    SMR = rbind(SMR, frame)
  }
} # end of inner loop 
SMR = mutate(SMR, SMR=O/E) # calculate SMR

## save
outfile = paste('data/', country, '/SMR_women.RData', sep='')
save(SMR, file=outfile)

} # end of countries loop