# 1_impute_life_tables_nonbayes.R
# imputed versions of life tables for countries with big gaps
# impute males and females separately
# non-bayesian version using two-dimensional splines; takes a few hours to run
# January 2020
library(dplyr)
library(ggplot2)
library(crs)

## which country
country = 'Japan'
country = 'Canada'
country = 'Germany' # gap is 1952 to 1956, also some years where deaths end at 89 years
country = 'UK'
country = 'NZ'
country = 'USA' 
country = 'Austria'
# load the meta-data
load('data/meta.RData')
meta = filter(meta, Country==country)
# get the country's life table data
data.file = paste('data/', country, '/', country, '.RData', sep='')
load(data.file) # from 0_import_data_`country`.R

# get the life table data, exclude probabilities that are 1 as they are too hard to model and are rare
if(country !='USA'){
life.table = filter(life.table, 
                    denom > 0,
                    qx < 1)
}
if(country =='USA'){ # no denominator data in USA
  life.table = filter(life.table, 
                      qx > 0, # some odd zero probability for very old ages
                      qx < 1)
}

# reduce data for UK as otherwise it takes too long; just use around time that needs imputing
if(country=='UK'){
  full.life.table = life.table # keep full table
  life.table = filter(life.table, Year>=1853, Year<=1950)
}
# reduce data for Canada as otherwise it takes too long; just use around time that needs imputing
if(country=='Canada'){
  full.life.table = life.table # keep full table
  life.table = filter(life.table, Year<=1950)
  # smooth probabilities over age in earlier years where multiple ages have the same qx
  early = filter(life.table, Year <= 1911) %>%
    group_by(Year, Sex) %>%
    mutate(qx = smooth.spline(x=Age, y=qx, df=4)$y) %>%
    ungroup()
  later = filter(life.table, Year > 1911)
  life.table = bind_rows(early, later) %>%
    arrange(Year, Age, Sex)
}
# reduce data for USA as otherwise it takes too long; just use around time that needs imputing
if(country=='USA'){
  full.life.table = life.table # keep full table
  life.table = filter(life.table, Year<=1930)
}
# reduce data for Germany as otherwise it takes too long; just use around time that needs imputing
if(country=='Germany'){
  full.life.table = life.table # keep full table
  life.table = filter(life.table, Year<=1970)
}

# model of log-probability with splines for Age and Year (takes a little while)
life.table = mutate(life.table, s = as.numeric(Sex=='Male'))
model <- crs(log(qx) ~ Age + Year + s, data=life.table, 
             knots = 'auto', # knot-type automatically determined
             basis = 'auto', # spline type automatically determined
             nmulti = 10, # upped from 5 to avoid local minima
             degree.max = 20, segments.max=20) # have a large number of potential knots
plot(model) # look at residuals, etc
summary(model)
# model for denominator
model.denom <- crs(log(denom) ~ Age + Year + s, data=life.table, 
             knots = 'auto', # knot-type automatically determined
             basis = 'auto', # spline type automatically determined
             nmulti = 10, # upped from 5 to avoid local minima
             degree.max = 20, segments.max=20) # have a large number of potential knots
plot(model.denom)
summary(model.denom)

## Visual checks of model fit ##
# a) plot observed and predicted ages in a particular year
y = 1950 # selected year
obs = filter(life.table, Year==y)
newdata = select(obs, Age, Year, s, qx, denom)
newdata$fit = predict(model, newdata=newdata)
gplot = ggplot(newdata, aes(x=Age, y=qx))+
  geom_line()+
  geom_point()+
  geom_line(aes(x=Age, y=exp(fit)), col='red')+
  facet_wrap(~s)
gplot
# - same for denominator
newdata$fit.denom = predict(model.denom, newdata=newdata)
gplot = ggplot(newdata, aes(x=Age, y=denom))+
  geom_line()+
  geom_point()+
  geom_line(aes(x=Age, y=exp(fit.denom)), col='red')+
  facet_wrap(~s)
gplot
# b) plot observed and predicted years for a particular age
a = 60 # selected age
obs = filter(life.table, Age==a)
newdata = select(obs, Age, Year, s, qx, denom)
newdata$fit = predict(model, newdata=newdata)
gplot = ggplot(newdata, aes(x=Year, y=qx))+
  geom_line()+
  geom_point()+
  geom_line(aes(x=Year, y=exp(fit)), col='red')+
  facet_wrap(~s)
gplot
# - same for denominator
newdata$fit.denom = predict(model.denom, newdata=newdata)
gplot = ggplot(newdata, aes(x=Year, y=denom))+
  geom_line()+
  geom_point()+
  geom_line(aes(x=Year, y=exp(fit.denom)), col='red')+
  facet_wrap(~s)
gplot

## examine residuals
life.table$fitted = predict(model)
life.table = mutate(life.table,
                    res = qx - exp(fitted))
par(mfrow=c(1,1))
hist(life.table$res)
boxplot(res ~ factor(Age), data=life.table)

## life table predictions for missing years ##
# a) find the years to impute
observed.years = unique(life.table$Year)
all.years = seq(min(life.table$Year), max(life.table$Year), 1)
to.impute = all.years[all.years %in% observed.years==FALSE]
# b) find the ages to impute
max.age = group_by(life.table, Year, Sex) %>%
  summarise(max.age = max(Age)) %>%
  filter(max.age < 100) %>% # years under 100
  tidyr::expand(s = c(0,1), Age = tidyr::full_seq(c(max.age, 100), 1)) %>%
  ungroup()
## create data frame to impute
# missing years
ages = 18:100 # 100 max in USA
newdata1 = data.frame(expand.grid(ages, to.impute, c(0,1))) %>% # ages, years, sex
  rename('Age'=Var1, 'Year'=Var2, 's'=Var3) # missing ages
newdata2 = max.age
newdata = bind_rows(newdata1, newdata2) %>%
  mutate(Sex = ifelse(s==1, 'Male', 'Female')) # convert gender from number to character

newdata$fit = predict(model, newdata=newdata) # takes a short while
newdata$fit.denom = predict(model.denom, newdata=newdata) # takes a short while
newdata = mutate(newdata, 
                 denom = exp(fit.denom),
                 qx = exp(fit)) %>%
  filter(qx <= 1) # exclude extremes
# fix very low probability for older ages in early years:
newdata = mutate(newdata, 
      qx = ifelse(Age>=100 & qx<0.01, 0.2, qx))
# fix low probability for older ages in early years in USA:
if(country =='USA'){
  newdata = mutate(newdata, 
                   qx = ifelse(Age>=80 & qx<0.2, 0.2, qx))
}
if(country == 'Germany'){
  newdata = mutate(newdata, 
                   qx = ifelse(Age>=80 & qx<0.115 & Sex=='Female', 0.115, qx),
                   qx = ifelse(Age>=80 & qx<0.135 & Sex=='Male', 0.135, qx))
}

## plot to check predictions
gplot = ggplot(newdata, aes(x=Age, y=qx, col=factor(Year)))+
  geom_line()+
  geom_point()+
  scale_y_log10()+
  theme_bw()+
  facet_wrap(~Sex)
gplot

## combine observed and imputed life data
# select variables from life table
if(country %in% c('Canada','UK','USA','Germany')){ # back to full table for these countries
  life.table = full.life.table
} 
life.table = select(life.table, Year, Age, Sex, qx, denom) %>%
  mutate(imputed = 'No')
newdata = select(newdata, Year, Age, Sex, qx, denom) %>%
  mutate(imputed = 'Yes')
life.table = bind_rows(life.table, newdata) %>%
  arrange(Year, Age, Sex)

# extra process for Germany as still some missing results for older ages in Germany
if (country=='Germany'){
  # carry forward last estimated death probability for 1850, 1851, because life tables are shorter than oldest politicians by a few years
  # need Age 89 in 1890
  max.years = group_by(life.table, Year, Sex) %>% # 
    summarise(Age = max(Age)) %>%
    filter(Age <= 90) %>%
    ungroup()
  extra = left_join(max.years, life.table, by=c('Year','Sex','Age')) %>%
    group_by(Year, Sex) %>%
    tidyr::expand(Age = tidyr::full_seq(c(Age+1, Age+10),1), qx=qx, denom=denom) %>%
    ungroup()
  life.table = bind_rows(life.table, extra)
  # last terrible fix, two missing qx's 
  extra1 = data.frame(Year=1986:1987, Age=90, Sex='Male', qx=0.22, denom=7000, imputed='Yes', stringsAsFactors = FALSE)
  extra2 = data.frame(Year=1986:1987, Age=90, Sex='Female', qx=0.19, denom=18000, imputed='Yes', stringsAsFactors = FALSE)
  life.table = bind_rows(life.table, extra1, extra2) %>%
    ungroup()
}

# extra process for USA
if (country=='USA'){
# carry forward last estimated death probability for 1850, 1851, because life tables are shorter than oldest politicians by a few years
  # need Age 89 in 1890
  max.years = group_by(life.table, Year) %>% # same for both genders
    summarise(max = max(Age)) %>%
    filter(max <= 90) %>%
    ungroup()
  extra1 = filter(life.table, Year %in% max.years$Year, Age==85) %>%
    mutate(Age = Age + 1)
  extra2 = filter(life.table, Year %in% max.years$Year, Age==85) %>%
    mutate(Age = Age + 2)
  extra3 = filter(life.table, Year %in% max.years$Year, Age==85) %>%
    mutate(Age = Age + 3)
  extra4 = filter(life.table, Year %in% max.years$Year, Age==85) %>%
    mutate(Age = Age + 4)
  life.table = bind_rows(life.table, extra1, extra2, extra3, extra4)
  
  ## if imputed and non-imputed in same year then use non-imputed
  life.table = group_by(life.table, Year, Age, Sex) %>%
    arrange(Year, Age, Sex, imputed) %>%
    slice(1) %>%
    ungroup()
}


# extra process for NZ
## if imputed and non-imputed in same year then use non-imputed
if (country=='NZ'){
  life.table = group_by(life.table, Year, Age, Sex) %>%
  arrange(Year, Age, Sex, imputed) %>%
  slice(1) %>%
  ungroup()
}

# extra process for UK
if (country=='UK'){
  life.table = ungroup(life.table) %>%
  group_by(Year, Age, Sex) %>%
  slice(1) %>% # remove duplicates in 1841
  ungroup()
# add missing ages for men for years 1856 to 1865
  missing.men = filter(life.table, Year>=1856, Year<=1864) %>%
    select(-imputed) %>%
    tidyr::spread('Sex','qx') %>%
    filter(is.na(Male)) %>% # around 100 results, 1856 to 1864, age 78 to 100
    rename('qx' = 'Female') %>% # replace missing men with Women'
    mutate(imputed = 'No', Sex='Male') %>%
    select(-Male)
  life.table = bind_rows(life.table, missing.men)
  # add missing ages for men and women for years 1857 to 1863 - use last available year of 1856
  used.to.fill = filter(life.table, Year==1856, Age>=80, Age<=100) %>%
    mutate(fill=1) %>%
    select(-Year)
  filler = NULL
  for (y in 1857:1863){ # impute these years
    this.filled = mutate(used.to.fill, Year=y)
    filler = bind_rows(filler, this.filled)
  }
  life.table = mutate(life.table, fill=0) %>%
    bind_rows(filler) %>%
    arrange(Year, Age, Sex, fill) %>%
    group_by(Year, Age, Sex) %>%
    slice(1) %>% # take first year if available (not filled)
    ungroup() %>%
    select(-fill)
}

## save life tables and politicians
out.file = paste('data/', country, '/', country, '.imputed.RData', sep='')
save(life.table, politicians, numbers, censor.date, file=out.file)
