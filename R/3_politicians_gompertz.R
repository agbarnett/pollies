# 3_politicians_gompertz.R
# create conditional life expectancies for politicians over time using a Gompertz model fitted to observed data ignoring sex
# January 2020
library(dplyr)
library(MortalityLaws) # for Gompertz models and life tables
library(ggplot2)
colours = c("orange",'grey',"#009317","#8d1ad2","#a3ffe6",
            "#ff47f2","#0e1c00","#0148b1","#844b00", "light green", "#ffc3bb","red")

# key parameters
year.window = 9 # window length for monitoring deaths, minus one because we use Jan to December, e.g., 4 = 5 year-window
filter(availableLaws ()$table, TYPE==3) # available laws for adult deaths
law = 'gompertz0' # still very variable for NZ
law = 'makeham' # also variable in C parameter
law = 'gompertz' # very variable parameters for NZ

## country loop ##
# list of countries
source('R/99_country_list.R')
gompertz.coeff = to.plot.check = expect45 = NULL
for (country in countries){
  
## Section 1: get the data
# get the country's meta-data
load('data/meta.RData')
meta = filter(meta, Country==country)
# get the country's politician data (do not need life table data here)
data.file = paste('data/', country, '/', country, '.RData', sep='')
load(data.file) # from 0_import_data_`country`.R 
remove(life.table) # not needed

# change year window dependent on country
#year.window = ifelse(country=='NZ', 9, 4) # longer window for NZ

## Section 2: main loop over time with years varying by country
for (start.year in seq(meta$Syear, meta$Eyear-year.window-1, year.window+1)){
  ## arrange the data as deaths and expected numbers
  deaths.frame = NULL
  for (year.to.add in 0:year.window){ # loop in single years to give yearly death probabilities
    year.start = as.Date(paste(start.year + year.to.add, '-01-01', sep="")) # start of year
    year.end = as.Date(paste(start.year + year.to.add, '-12-31', sep="")) # end of year (last day in December)
    exp = filter(politicians, Date_entered<=year.end & DOD>=year.start) %>% # entered some time before the period end and were alive at start of period (have not done fractional years)
      mutate(age = floor(as.numeric(difftime(year.start, DOB, units='days')/365.25)),  # calculate age at start of period ('current' age)
             ftime = 1, # follow-up time, 1 year for all
             dead = DOD <= year.end & Status == 'Dead') %>% # died during period
      select(DOB, DOD, Date_entered, Sex, age, ftime, dead)
    deaths.frame = bind_rows(deaths.frame, exp)
  }
  # sum deaths and observed years, ignore Sex
  sums = group_by(deaths.frame, age) %>% # put Sex here if necessary
    summarise(obs = sum(ftime), deaths = sum(dead) ) %>%
    ungroup() %>%
    arrange(age) 
  
  ## Section 2b: fit the Gompertz model
  # ages at begining of interval
  ages = as.integer(sums$age)
  Dx = as.numeric(sums$deaths)
  Ex = as.numeric(sums$obs)
  M1 <- MortalityLaw(x = ages, Dx = Dx, Ex = Ex, law = law)
  
  # store the Gompertz coefficients
  death.numbers = sum(Dx) # add observed death numbers
  if(law=='gompertz'){storeg = data.frame(start.year = start.year, A=M1$coefficients[1], B=M1$coefficients[2], deaths=death.numbers, country=country, stringsAsFactors = FALSE)}
  if(law=='gompertz0'){storeg = data.frame(start.year = start.year, sigma=M1$coefficients[1], M=M1$coefficients[2], deaths=death.numbers, country=country, stringsAsFactors = FALSE)}
  if(law=='makeham'){storeg = data.frame(start.year = start.year, A=M1$coefficients[1], B=M1$coefficients[2], C=M1$coefficients[3], deaths=death.numbers, country=country, stringsAsFactors = FALSE)}
  gompertz.coeff = bind_rows(gompertz.coeff, storeg)
  
  # check estimated Gompertz rates using a plot
  ages = 30:90 # ages to estimate over
  if(law=='gompertz'){rate <- storeg$A * exp(storeg$B * ages)} # Gompertz equation
  if(law=='gompertz0'){rate <- (1/storeg$sigma) * exp((ages-storeg$M)/storeg$sigma)}
  if(law=='makeham'){rate <- (storeg$A * exp(storeg$B * ages)) + storeg$C}
  store = data.frame(start.year = start.year, ages=ages, rate=rate, country=country, stringsAsFactors = FALSE)
  to.plot.check = bind_rows(to.plot.check, store)
  
  ## Section 2c: make life table from Gompertz rates
  if(law=='gompertz'){par = matrix(c(M1$coefficients[1], M1$coefficients[2]), nrow=1, dimnames=list(1, c("A","B")))} # Gompertz estimates
  if(law=='gompertz0'){par = matrix(c(M1$coefficients[1], M1$coefficients[2]), nrow=1, dimnames=list(1, c("sigma","M")))} # 
  if(law=='makeham'){par = matrix(c(M1$coefficients[1], M1$coefficients[2], M1$coefficients[3]), nrow=1, dimnames=list(1, c("A","B","C")))} # 
  ltable = LawTable(x=ages, par=par, law=law, sex = 'total')
  life.table = mutate(ltable$lt, age=ages) # add age, missing in life table
  # now get life expectancy at age 45
  this.expect = filter(life.table, age==45) %>%
    select(ex) %>%
    mutate(age=45, start.year = start.year, country=country)
  expect45 = bind_rows(expect45, this.expect)

} # end of time loop

} # end of country loop

# plot mortality curve parameters over time
if(law=='gompertz'){parms.to.plot = tidyr::gather(gompertz.coeff, `A`, `B`, key='parm', value='est')}
if(law=='gompertz0'){parms.to.plot = tidyr::gather(gompertz.coeff, `sigma`, `M`, key='parm', value='est')}
if(law=='makeham'){parms.to.plot = tidyr::gather(gompertz.coeff, `A`, `B`, `C`, key='parm', value='est')}
tplot = ggplot(data=parms.to.plot, aes(x=start.year, y=est, col=factor(parm)))+
  scale_colour_manual('Mortality\nparameter', values=colours)+
  geom_line()+
  xlab('Year')+
  ylab('Estimate')+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  facet_wrap(~country, scales='free_x')
tplot
outfile = paste('figures/MortParms.', law, '.jpg', sep='')
jpeg(outfile, width=5, height=4, units='in', res=300, quality = 100)
print(tplot)
dev.off()

# check rates using plot
gplot = ggplot(data=to.plot.check, aes(x=ages, y=rate, col=factor(start.year)))+
  geom_line()+
  theme_bw()+
  xlab('Age')+
  scale_y_log10()+
  facet_wrap(~country)+
  theme(legend.position = 'none')
gplot

# plot life expectancy
eplot = ggplot(data=expect45, aes(x=start.year, y=ex, col=factor(country)))+
  scale_colour_manual('Country', values=colours)+
  geom_line(size=1.1)+
  xlab('Year')+
  ylab('Life expectancy after age 45')+
  theme_bw()
eplot
jpeg('figures/LifeExpect45politicians.jpg', width=5, height=4, units='in', res=500, quality = 100)
print(eplot)
dev.off()

# average death numbers by country
group_by(gompertz.coeff, country) %>%
  summarise(av = mean(deaths))

# save for combined plot in 4_combined_life_expectancy_plot.R
save(expect45, law, file='data/ConditionalLifeExpectancyPoliticians.RData')
