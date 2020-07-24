# 3_plot_life_expectancy.R
# plot life expectancy of the population using life tables
# conditional on reaching age 45 and 65
# January 2020
library(demography)
library(dplyr)
library(tidyr)
library(ggplot2)
colours = c("orange",'grey',"#009317","#8d1ad2","#a3ffe6",
            "#ff47f2","#0e1c00","#0148b1","#844b00", "light green", "#ffc3bb","red")

## section 0: rob's example
plot(life.expectancy(fr.mort), ylab="Life expectancy")
# try their data, can it run from 45, answer = yes
fr.mort.45 = fr.mort
fr.mort.45$rate$total = fr.mort.45$rate$total[46:nrow(fr.mort.45$rate$total), ]
fr.mort.45$rate$male = fr.mort.45$rate$male[46:nrow(fr.mort.45$rate$male), ]
fr.mort.45$rate$female = fr.mort.45$rate$female[46:nrow(fr.mort.45$rate$female), ]
fr.mort.45$age = fr.mort.45$age[fr.mort.45$age>=45]
plot(life.expectancy(fr.mort.45), ylab="Life expectancy at 45")

# list of countries
source('R/99_country_list.R')
# loop through countries
expectancy = expectancy.sex = NULL
for (country in countries){
  use.imputed = FALSE
  if(country %in% countries.imputed){use.imputed=TRUE}

  # get the country's life table data (imputed or not); do not need politician data here
  data.file = paste('data/', country, '/', country, '.RData', sep='')
  load(data.file) # from 0_import_data_`country`.R 
  if(use.imputed == TRUE){
    data.file = paste('data/', country, '/', country, '.imputed.RData', sep='') # from 1_impute_life_tables_nonbayes.RR
    load(data.file) # for imputed life table data
  }
  remove(politicians) # not needed
  
  # remove duplicates if there are imputed and non-imputed results in the same year
  if(use.imputed==TRUE){
    life.table= group_by(life.table, Year, Age, Sex) %>%
      arrange(Year, Age, Sex, imputed) %>%
      slice(1) %>% # take not imputed if both imputed and not
      ungroup()
  }
  
  
  ## section 1: get country data and convert into Rob's format
  my.fr.mort = list()
  my.fr.mort$type = 'mortality'
  my.fr.mort$label = country
  my.fr.mort$lambda = 0 
  my.fr.mort$year = unique(life.table$Year)
  my.fr.mort$age = 45:max(life.table$Age)
  # rates for ages over 45
  males = filter(life.table, Sex=='Male', Age>=45) %>%
    mutate(rate = -log(1-qx)) %>% # transform probability to rate
    select(Year, Age, rate) %>%
    group_by(Age) %>% # age as rows ... 
    spread(Year, rate) %>% #... year as columns
    ungroup() %>%
    select(-Age)
  males = as.matrix(males); rownames(males) = my.fr.mort$age
  my.fr.mort$rate$male = as.matrix(males)
  females = filter(life.table, Sex=='Female', Age>=45) %>%
    mutate(rate = -log(1-qx)) %>% # transform probability to rate
    select(Year, Age, rate) %>%
    group_by(Age) %>% # age as rows ... 
    spread(Year, rate) %>% #... year as columns
    ungroup() %>%
    select(-Age)
  females = as.matrix(females); rownames(females) = my.fr.mort$age
  my.fr.mort$rate$female = as.matrix(females)
  # population over 45
  males = filter(life.table, Sex=='Male', Age>=45) %>%
    select(Year, Age, denom) %>%
    mutate(denom = ifelse(denom==0, 1, denom), # avoid zero denominators
           denom = round(denom)) %>% 
    group_by(Age) %>% # age as rows ... 
    spread(Year, denom) %>% #... year as columns
    ungroup() %>%
    select(-Age)
  males = as.matrix(males); rownames(males) = my.fr.mort$age
  my.fr.mort$pop$male = as.matrix(males)
  females = filter(life.table, Sex=='Female', Age>=45) %>%
    select(Year, Age, denom) %>%
    mutate(denom = ifelse(denom==0, 1, denom),  # avoid zero denominators
           denom = round(denom)) %>%
    group_by(Age) %>% # age as rows ... 
    spread(Year, denom) %>% #... year as columns
    ungroup() %>%
    select(-Age)
  females = as.matrix(females); rownames(females) = my.fr.mort$age
  my.fr.mort$pop$female = as.matrix(females)
  class(my.fr.mort) = 'demogdata'
  # calculate life expectancy and make data frame 
  e = data.frame(life.expectancy(my.fr.mort, type='period', age=45)); names(e) = 'expect'
  e = mutate(e, 
             expect = as.numeric(expect),
             years = my.fr.mort$year,
             country=country)
  expectancy = bind_rows(expectancy, e)
  
  # separate estimates for men and women
  women = my.fr.mort
  men = my.fr.mort
  men$rate$female = NULL
  men$pop$female = NULL
  women$rate$male = NULL
  women$pop$male = NULL
  # calculate life expectancy and make data frame 
  # men
  e.men = data.frame(life.expectancy(men, type='period', age=45)); names(e.men) = 'expect'
  e.men = mutate(e.men,
                 sex = 'Male',
             expect = as.numeric(expect),
             years = my.fr.mort$year,
             country=country)
  # women
  e.women = data.frame(life.expectancy(women, type='period', age=45)); names(e.women) = 'expect'
  e.women = mutate(e.women,
                 sex = 'Female',
                 expect = as.numeric(expect),
                 years = my.fr.mort$year,
                 country=country)
  expectancy.sex = bind_rows(expectancy.sex, e.men, e.women)
  
  # check against existing expectancy for countries with it available -looks good
  if(exists('ex') == TRUE){
    check.plot = ggplot(data=filter(ex, Age==45), aes(x=Year, y=ex, col=factor(Sex)))+
      geom_line()+
      geom_line(data=e, aes(x=years, y=expect), col='black', lty=2)+
      ggtitle(country)+
      ylab('Life expectancy at 45')
    check.plot
  }
  
  if(exists('ex')==TRUE){remove(ex)} # tidy up so that is.null check per country works
  
} # end of loop

# plot
eplot = ggplot(data=expectancy, aes(x=years, y=expect, col=factor(country)))+
  scale_colour_manual('Country', values=colours)+
  geom_line(size=1.1)+
  xlab('Year')+
  ylab('Life expectancy at age 45')+
  theme_bw()
jpeg('figures/LifeExpect45population.jpg', width=5, height=4, units='in', res=500, quality = 100)
print(eplot)
dev.off()

# save for combined plot
save(expectancy, expectancy.sex, file='data/ConditionalLifeExpectancyPopulation.RData')
# write to csv for An
write.csv(expectancy.sex, quote=FALSE, row.names = FALSE, file='data/life_expectancy_sex.csv')
