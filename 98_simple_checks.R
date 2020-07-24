# 98_simple_checks.R
# June 2019
# simple checks of politician and life table data
library(dplyr)
library(ggplot2)

## Politicians
# death before birth
filter(politicians, DOD < DOB)
# entry before birth
filter(politicians, Date_entered < DOB)
# entry after death
filter(politicians, Date_entered >= DOD, DOD!=censor.date)
# entry after exit (not all datasets have exit)
filter(politicians, Date_entered >= Date_exited)
# check those over 100
old = mutate(politicians, age = as.numeric((DOD - DOB)/365.25)) %>%
  filter(age > 100)
# check those entering very early
young = mutate(politicians, age = as.numeric((Date_entered - DOB)/365.25)) %>%
  filter(age < 21)
# Any deaths after censor date (not censored)
filter(politicians, DOD > censor.date & Status !='Living')
# Multiple deaths on same day
filter(politicians, Status=='Dead') %>%
  group_by(DOD) %>%
  summarise(count = n()) %>%
  filter(count > 3)
# output list of old politicians that are still alive to check
to.check = mutate(politicians, 
                  DOD = as.Date(DOD),
                  Age = as.numeric((DOD - DOB)/365.25)) %>%
  filter(Status=='Living', Age > 105) 
#%>%  select(-First_name.1, -State)
print(to.check)
write.csv(to.check, file='FranceCheck.csv', quote = FALSE, row.names = FALSE)

## Life tables
# check one year
to.plot = filter(life.table, Year==1950 , Age<100)
lplot = ggplot(data=to.plot, aes(x=Age, y=qx, col=factor(Sex)))+
  geom_line()+
  geom_point()+
  theme_bw()
lplot

# check one age over time
to.plot = filter(life.table, Age==88)
aplot = ggplot(data=to.plot, aes(x=Year, y=qx, col=factor(Sex)))+
  geom_line(size=1.1)+
  geom_point()+
  ylab('Yearly Probability of death')+
  theme_bw()+
#  scale_color_manual('', values = cbPalette[6:7])+
  theme(legend.position=c(0.8,0.8), text=element_text(size=30))
aplot
jpeg('figures/USDeath.jpg', width=1200, height = 675, units='px', quality=100)
print(aplot)
dev.off()

# check for multiple results per year (should be empty)
group_by(life.table, Year, Age, Sex) %>%
  summarise(n = n()) %>%
  filter(n > 1)

# any gaps over time?
table(diff(unique(life.table$Year))) # should all be one
