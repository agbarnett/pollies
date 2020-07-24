# 2_SMR.R
# Calculate annual standardised mortality ratio, works on multiple countries
# Extended to examine multiple follow-up times for animations
# October 2019
library(dplyr)

# which country and is there imputed life table data
country = 'Italy'; use.imputed = FALSE
country = 'NZ'; use.imputed = TRUE
country = 'Japan'; use.imputed = TRUE
country = 'Switzerland'; use.imputed = FALSE
country = 'France'; use.imputed = FALSE
country = 'Brobdingnag'; use.imputed = FALSE
country = 'Austria'; use.imputed = TRUE
country = 'Canada'; use.imputed = TRUE
country = 'Netherlands'; use.imputed = FALSE
country = 'USA'; use.imputed = TRUE
country = 'UK'; use.imputed = TRUE
country = 'Germany'; use.imputed = TRUE
country = 'Australia'; use.imputed = FALSE
# get the country's meta-data
load('data/meta.RData')
meta = filter(meta, Country==country)
# get the country's politician and life table data (imputed or not)
data.file = paste('data/', country, '/', country, '.RData', sep='')
load(data.file) # from 0_import_data_`country`.R 
if(use.imputed == TRUE){
  pols = politicians # temporary store of politicians
  data.file = paste('data/', country, '/', country, '.imputed.RData', sep='') # from 1_impute_life_tables_nonbayes.RR
  load(data.file) # for imputed life table data
  politicians = pols # over-write with politicians because of updates that happened after imputation
}
politicians = mutate(politicians, entry.year = as.numeric(format(Date_entered, '%Y'))) # create entry year, needed later for fractional year in election year
if(use.imputed == FALSE){ # if not using imputed data, then create column of "No" for imputed
  life.table = mutate(life.table, imputed='No')
}

## Outer loop for sensitivity analysis with specific years of follow-up
SMR = ages = NULL
for (follow.cut in c(10:60, 100)){ # years of follow-up after entry in scale of years ; add 100 as largest possible follow-up
  # Calculate follow-up time depending on years of follow-up
  to.SMR = mutate(politicians, 
            follow.time = as.numeric(difftime(DOD, Date_entered, units='days')/365.25), # observed follow-up time in years from entry to death (or censoring)
            DODx = ifelse(follow.time > follow.cut, Date_entered + (follow.cut*365.25), DOD), # censor time at cut-off time if observed time is longer than time to death
            DODx = as.Date(DODx, origin='1970-01-01'),
            Statusx = ifelse(follow.time > follow.cut, 'Living', 'Dead'), # change status to living if still alive at this time
            Statusx = ifelse(DODx >= censor.date, Status, Statusx), # revert back to previous status if beyond censor date
            DODx = ifelse(DODx >= censor.date, DOD, DODx), # revert back to previous time if beyond censor date
            DODx = as.Date(DODx, origin='1970-01-01')) %>%
  select(-DOD, -Status) %>% # over-write Status and date-of-death
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
    # calculate average age of politicians
    av.age = summarise(exp, mean=mean(age)) %>%
      mutate(follow=follow.cut, year = year)
    ages = rbind(ages, av.age)
    # fractional first year (could not get this to work in dplyr)
    index = exp$entry.year == year
    exp$frac = 1 
    if(any(index)){exp$frac[index] = as.numeric(difftime(year.end, exp$Date_entered[index], units='days'))/365.25}
    # merge with life table by age and sex
    exp = left_join(exp, this.life, by=c('age'='Age', 'Sex'='Sex')) %>% 
      summarise(expected=sum(qx*frac)) # expected deaths, fractional time in first year
    
    ## flag any very high qx here??
    
    # calculate observed deaths
    obs = filter(to.SMR, DOD>=year.start & DOD<=year.end & Status=='Dead') %>% # died this year (not censored)
      summarise(observed=n())
    # calculate SMR and concatenate; add imputed (yes/no) flag for life tables
    frame = data.frame(follow=follow.cut, year=year, O=obs$observed, E=exp$expected, imputed=this.life$imputed[1])
    SMR = rbind(SMR, frame)
  } # end of inner loop 
} # end of outer loop for follow-cut
SMR = mutate(SMR, SMR=O/E) # calculate SMR

## save
outfile = paste('data/', country, '/SMR.RData', sep='')
save(SMR, ages, file=outfile)

