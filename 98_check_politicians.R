# 98_check_politicians.R
# check politician numbers over time in all countries
# August 2019
library(dplyr)
library(ggplot2)
source('R/99_country_list.R') # get list of countries
# year labels
labels = seq(1820, 2020, 20)
breaks = as.numeric(as.Date(paste(labels, '-01-01', sep='')))

## get data for all countries cumulative numbers
to.plot = NULL
for (c in countries){
  infile = paste('data/', c, '/', c, '.RData', sep='')
  load(infile)
  frame = select(politicians, Date_entered) %>%
     mutate(date = as.numeric(Date_entered),
	 country = c)
  to.plot = bind_rows(to.plot, frame)
}

## plot cumulative proportion
cplot = ggplot(data=to.plot, aes(x=date))+
  stat_ecdf()+
  scale_x_continuous(breaks=breaks, labels=labels)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle=45, hjust=1, size = 6))+
  facet_wrap(~country, scales='free_x')+
  xlab('')+
  ylab('Cumulative proportion')
cplot
jpeg('figures/politicians.cumulative.jpg', width=5, height=4, units='in', res=300)
print(cplot)
dev.off()

## look at dynamic numbers ##
countries = c('Austria','UK') # just those with exit date
to.plot = NULL
for (c in countries){
  infile = paste('data/', c, '/', c, '.RData', sep='')
  load(infile)
  frame = select(politicians, Date_entered, Date_exited) %>%
    mutate(Date_exited = ifelse(is.na(Date_exited)==TRUE, max(Date_exited, na.rm=TRUE), Date_exited), # date exit is missing for those still in office
           date1 = as.numeric(Date_entered),
           date2 = as.numeric(Date_exited),
           country = c)
  starts = group_by(frame, date1) %>% # unique dates of politicians starting
    summarise(start = n()) %>%
    arrange(date1)
  stops = group_by(frame, date2) %>% # unique dates of politicians leaving
    summarise(stop = n()) %>%
    arrange(date2)
  # create full data frame of all dates
  dates = data.frame(date = seq(min(frame$date1), max(frame$date2)-1, 1)) %>%
    left_join(starts, by=c('date' = 'date1')) %>%
    left_join(stops, by=c('date' = 'date2')) %>%
    mutate(country = c,
           start = ifelse(is.na(start)==TRUE, 0, start),
           stop = ifelse(is.na(stop)==TRUE, 0, stop),
           current = NA) %>%
    filter(start>0 | stop>0) 
  # now make cumulative numbers
  dates$current[1] = dates$start[1]
  for (k in 2:nrow(dates)){
    dates$current[k] = dates$current[k-1] + dates$start[k] - dates$stop[k]
  }
  to.plot = bind_rows(to.plot, dates)
}

# plot
dplot = ggplot(data=to.plot, aes(x=date, y=current))+
  geom_line()+
  scale_x_continuous(breaks=breaks, labels=labels)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle=45, hjust=1, size = 6))+
  facet_wrap(~country, scales='free')+
  xlab('')+
  ylab('Current numbers')
dplot

# UK numbers are not working https://en.wikipedia.org/wiki/Number_of_Westminster_MPs
# exit dates are not accurate