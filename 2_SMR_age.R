# 2_SMR_age.R
# Calculate annual standardised mortality ratio, works on multiple countries
# Version that follows politicians until a particular age
# August 2019
library(dplyr)

# which country and is there imputed life table data
country = 'Japan'; use.imputed = TRUE
country = 'Italy'; use.imputed = FALSE
country = 'NZ'; use.imputed = TRUE
country = 'Austria'; use.imputed = FALSE
country = 'Switzerland'; use.imputed = FALSE
country = 'France'; use.imputed = FALSE
country = 'Brobdingnag'; use.imputed = FALSE
country = 'UK'; use.imputed = TRUE
country = 'Australia'; use.imputed = FALSE
country = 'Germany'; use.imputed = FALSE
country = 'Canada'; use.imputed = TRUE
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

## Outer loop for sensitivity analysis with specific years of follow-up
SMR = NULL
max.age = 110
#if(country=='Canada'){max.age=100}
for (age.cut in c(50:max.age)){ # age at which to censor follow-up
  # Censor politicians still alive at censor date
  to.SMR = mutate(politicians, 
        age.at.entry = as.numeric(difftime(Date_entered, DOB, units='days')/365.25), # age at entry in years
        age.at.death = as.numeric(difftime(DOD, DOB, units='days')/365.25), # age at death in years
        flag = ifelse((age.cut < age.at.death) & (age.cut > age.at.entry), 1, 0), # flag whether DOD and status will change
        DODx = ifelse(flag==1, DOB + (age.cut*365.25), DOD), # there were still alive at this date
        DODx = as.Date(DODx, origin='1970-01-01'),
        Statusx = ifelse(flag==1, 'Living', Status)) %>% # update status
  select(-age.at.entry, -age.at.death, -flag, -DOD, -Status) %>% # over-write Status and date-of-death
  rename(DOD = DODx, Status=Statusx)

  ## Inner loop of calculating SMR
  # merge a year of policitians with a year of data
  for (year in meta$Syear:meta$Eyear){ 
    # get life table for this year
    this.life = filter(life.table, Year==year) %>% 
      dplyr::select(Age, Sex, qx, imputed)
    # get relevant politicians
    year.start = as.Date(paste(year, '-01-01', sep="")) # start of year
    year.end = as.Date(paste(year, '-12-31', sep="")) # end of year
    # calculate expected deaths
    exp = filter(to.SMR, Date_entered<=year.end & DOD>=year.start) %>% # entered some time during the year and were alive at the start of the year
      mutate(age = floor(as.numeric(difftime(year.start, DOB, units='days')/365.25)),  # calculate age at start of year ('current' age)
             age = ifelse(age > meta$Oldest, meta$Oldest, age)) # change any ages over limit of life table
    # fractional first year (could not get this to work in dplyr)
    index = exp$entry.year == year
    exp$frac = 1 
    if(any(index)){exp$frac[index] = as.numeric(difftime(year.end, exp$Date_entered[index], units='days'))/365.25}
    # merge with life table by age and sex
    exp = left_join(exp, this.life, by=c('age'='Age', 'Sex'='Sex')) %>% 
      summarise(expected=sum(qx*frac)) # expected deaths, fractional time in first year
    
    # calculate observed deaths
    obs = filter(to.SMR, DOD>=year.start & DOD<=year.end & Status=='Dead') %>% # died this year (not censored)
      summarise(observed=n())
    # calculate SMR and concatenate; add imputed (yes/no) flag for life tables
    frame = data.frame(age.cut=age.cut, year=year, O=obs$observed, E=exp$expected, imputed=this.life$imputed[1])
    SMR = rbind(SMR, frame)
  } # end of inner loop 
} # end of outer loop for age-cut
SMR = mutate(SMR, SMR=O/E) # calculate SMR

## save
outfile = paste('data/', country, '/SMR.Age.RData', sep='')
save(SMR, file=outfile)
