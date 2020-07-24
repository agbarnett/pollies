# 3_lexis_diagram.R
# Lexis diagram of politicians
# May need to change to a density as lines are too thick.
# October 2019
library(Epi)
library(dplyr)
library(season) # for year fraction
library(ggplot2)

# which country
country = 'Italy'
country = 'Germany'
country = 'Japan'
country = 'NZ'
country = 'Austria'
country = 'Switzerland'
country = 'France'
country = 'USA'
country = 'Brobdingnag'
country = 'Canada'
country = 'Australia'
country = 'Netherlands'
country = 'UK'

# loop
source('R/99_country_list.R') # get list of countries
for (country in countries){

#### Section 0:  get the data ####
infile = paste('data/', country, '/', country, '.RData', sep='')
load(infile) # from 0_import_data.R
# prepare politician data for Lexis diagram
politicians = filter(politicians, is.na(DOD)==FALSE) %>% 
  mutate(ID = 1:n(),
    Status = as.numeric(factor(Status)),
    dob = as.numeric(format(DOB,'%Y')) + yrfraction(DOB),
    agein = as.numeric(difftime(Date_entered, DOB, units='days')/365.25),
    ageout = as.numeric(difftime(DOD, DOB, units='days')/365.25))

#### Section 1: Static Lexis Diagram ####
# States are 0 = voted in, 1 = dead, 2 = still alive 
polliesL <- Lexis(entry = list("period" = agein + dob, "age" = agein), exit = list("age" = ageout), 
                  entry.status=0, exit.status = Status, id = ID, data = politicians, states=c('Joined','Dead','Alive'))
# with different symbol for dead/alive:
plot(polliesL, type='b', ylim=c(20, 105), pch=c(1,16)[as.numeric(polliesL$Status==1)+1])
summary(polliesL)
# export
outfile = paste('figures/lexis/Lexis', country, '.jpg', sep='')
jpeg(outfile, width=5, height=4, units='in', res=500, quality = 100)
par(mai=c(0.9,0.9,0.05,0.1), las=1)
plot(polliesL, type='l', ylim=c(20, 100), ylab='Age, years', xlab='Year')
points(polliesL, type='p', ylim=c(20, 100), pch=c(16,1)[as.numeric(polliesL$Status==1)+1], col='red')
dev.off()

} # end of countries loop

#### Section 2: Animate - abandoned ####
library(gganimate)
library('transformr') # for lines in gganimate
to.animate = filter(politicians, Date_entered < as.Date('1920-01-01')) %>% # temporarily use small sample
  mutate(denter = as.numeric(Date_entered, '%Y')) %>% # round to years
  dplyr::select(id, Surname, DOB, agein) %>%
  filter(id %in% c(11,29)) # Temporary
# make empty frame of years
edates = as.Date(paste(1900:1920, '-01-01', sep='')) # 1st jan
empty = data.frame(Date = rep(edates, each = length(unique(to.animate$id))),
                   id = unique(to.animate$id),
                   stringsAsFactors = FALSE)
# merge years with observed data
to.animate = left_join(empty, to.animate, by='id') %>%
  mutate(ageout = as.numeric(difftime(Date, DOB, units='days')/365.25), # calculate current age
         year = as.numeric(format(Date, '%Y'))) %>%
  arrange(id, year) 
# now for every year also add initial data
initial.year = group_by(to.animate, id) %>%
  mutate(Date=min(Date), ageout=min(ageout)) %>% # in each year over-write with starting time
  ungroup()
to.animate = bind_rows(to.animate, initial.year)


# static plot
static = filter(to.animate, year==1906) %>%
  arrange(year) %>%
  ggplot(aes(x=Date, y=ageout, colour=factor(id))) +
  geom_line()+
  theme_minimal()+
  theme(legend.position = 'none')
static

g = to.animate %>%
  arrange(year) %>%
  ggplot(aes(x=Date, y=ageout, colour=factor(id))) +
  geom_line()+
  theme_bw() +
  labs(title = 'Year: {frame_time}')+
  transition_time(year)


### TO DO: Add cumalative years of follow-up and deaths to labels

animate(g, nframes = 20, fps = 2)
