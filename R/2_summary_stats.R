# 2_summary_stats.R
# make some simple summary stats per country
# also look at numbers lost and make a graphic for the paper
# March 2020
library(dplyr)
library(stringr)
library(shape)
options(width=1000, scipen = 999) # Wide pages and no scientific numbers

## get the data
# get the list of countries
source('R/99_country_list.R')

# loop through the countries
stats = flow = NULL
for (country in countries){
  setwd(paste('data/', country, sep=''))
  d = dir(pattern=country)
  d = d[str_detect(d, 'RData')]
  is.imputed = any(str_detect(d, 'imputed'))
  infile = ifelse(is.imputed, paste(country, '.imputed.RData',sep=''), paste(country, '.RData',sep=''))
  load(infile)   
  setwd('../..')
  ## life table
  life.table = mutate(life.table, country=country)
  # life table years
  l.years = unique(life.table$Year)
  # percent of life table years that are imputed
  percent.imputed = 0
  if(is.imputed==TRUE){
    istats = group_by(life.table, Year) %>%
      slice(1) %>%
      ungroup() %>%
      summarise(n=n(), 
                imputed = sum(imputed=='Yes'),
                perc = round(100*imputed/n))
    percent.imputed = istats$perc
  }

  ## a) politicians
  politicians = mutate(politicians, country=country,
                       follow = as.numeric(difftime(Date_entered, DOB, units = 'weeks')) ) %>% # follow-up time
    select(DOB, DOD, Sex, Date_entered, Status, country, follow) %>%
    mutate(year.entry = as.numeric(format(Date_entered, '%Y')),
           age = floor(as.numeric(difftime(Date_entered, DOB, units='days')/365.25))) %>%
    filter(year.entry>= min(l.years), # must be within life table range
           year.entry<= max(l.years))
  # concatenate stats
  s.frame = data.frame(country = country, 
                       n = nrow(politicians),
                       perc.female = round(100*sum(politicians$Sex=='Female')/nrow(politicians)),
                       n.dead = sum(politicians$Status == 'Dead'),
                       Age.elected = round(mean(politicians$age),0), # round to whole years
                       follow.years = round(sum(politicians$follow)/52), # scale to years
                       earliest.election = min(politicians$year.entry),
                       #latest.election = max(politicians$year.entry),
                       last.year.follow = max(l.years), 
                       percent.imputed = percent.imputed,
                       stringsAsFactors = FALSE)
  stats = bind_rows(stats, s.frame)
  
  ## b) EQUATOR flow  
  numbers$country = country
  numbers$start.new = nrow(politicians) # updated start number that excludes those outside life table
  flow = bind_rows(flow, numbers)
  # tidy up
  remove(numbers, life.table, politicians)
}

# output
library(pander)
options(width=1000, scipen = 999) # Wide pages and no scientific numbers
pander(stats, style='simple', big.mark='', table.split.table=Inf, table.split.cells=Inf)

## make circular flow diagram
n.countries = length(countries)
# box positions
start = c(0.25, 0.92) # start box
dob.box = c(0.75, 0.75) # lost DOB
dod.box = c(0.75, 0.5) # lost DOD
election.box = c(0.75, 0.25) # lost election - only in Japan ...
life.table.box = c(0.75, 0.25) # replaced with life table box
end = c(0.25, 0.08) # end box (0.08 for y if Japan included)
# adjustments for each country in a circle
start.o.clock = pi/8 # -pi/2 to start at 12, reverse to go clockwise
x = rev(cos(start.o.clock + 2*pi*1:n.countries/n.countries) / 5.5) 
y = rev(sin(start.o.clock + 2*pi*1:n.countries/n.countries) / 10) # bigger divider for sin makes ellipse
## start the plot
jpeg('figures/PoliticiansDataFlow.jpg', width=5, height=7, units='in', res=300)
par(mai=rep(0.03, 4))
plot.new()
# circles and text
filledellipse(rx1=0.25, ry1 = 0.12, mid=start, col='skyblue')
text(x=start[1], y=start[2], label='Start', font=2, cex=1.1) # bold at the centre of the ellipse
filledellipse(rx1=0.25, ry1 = 0.12, mid=dob.box, col='skyblue')
text(x=dob.box[1], y=dob.box[2], label='No birth\ndate', font=2, cex=1.1) # bold at the centre of the ellipse
filledellipse(rx1=0.25, ry1 = 0.12, mid=dod.box, col='skyblue')
text(x=dod.box[1], y=dod.box[2], label='No death\ndate', font=2, cex=1.1) # bold at the centre of the ellipse
# outside available life tables
filledellipse(rx1=0.25, ry1 = 0.12, mid=life.table.box, col='skyblue')
text(x=life.table.box[1], y=life.table.box[2], label='No\nmatching\nlife table\ndata', font=2, cex=1.1) # bold at the centre of the ellipse
# only in Japan
#filledellipse(rx1=0.25, ry1 = 0.12, mid=election.box, col='skyblue')
#text(x=election.box[1], y=election.box[2], label='No election\ndate', font=2, cex=1.1) # bold at the centre of the ellipse
filledellipse(rx1=0.25, ry1 = 0.12, mid=end, col='skyblue')
text(x=end[1], y=end[2], label='Final', font=2, cex=1.1) # bold at the centre of the ellipse
# text
text.cex = 0.70
for (c in 1:n.countries){
  # start
  n.start = filter(flow, country==countries[c])$n.start
  label = paste(countries[c], '\n',  n.start, sep='')
  text(x= start[1] - x[c], y=start[2]-y[c], labels=label, cex=text.cex )
  # lost DOB
  num = filter(flow, country==countries[c])$n.start - filter(flow, country==countries[c])$n.post.no.DOB
  label = paste(countries[c], '\n', num, sep='')
  text(x= dob.box[1] - x[c], y=dob.box[2]-y[c], labels=label, cex=text.cex )
  check = n.start - num # check
  # lost DOD
  num = filter(flow, country==countries[c])$n.post.no.DOB - filter(flow, country==countries[c])$n.post.no.DOD
  if(is.na(num) == TRUE){num = 0}
  label = paste(countries[c], '\n', num, sep='')
  text(x= dod.box[1] - x[c], y=dod.box[2]-y[c], labels=label, cex=text.cex )
  check = check - num # check
  # no life.table
  num = filter(flow, country==countries[c])$n.end - filter(flow, country==countries[c])$start.new
  if(is.na(num) == TRUE){num = 0}
  label = paste(countries[c], '\n', num, sep='')
  text(x= life.table.box[1] - x[c], y=life.table.box[2]-y[c], labels=label, cex=text.cex )
  # lost election (Japan only)
  #num = filter(flow, country==countries[c])$n.post.no.DOD - filter(flow, country==countries[c])$n.post.no.entry
  #if(is.na(filter(flow, country==countries[c])$n.post.no.DOD) == TRUE){num = filter(flow, country==countries[c])$n.post.no.DOB - filter(flow, country==countries[c])$n.post.no.entry}
  #if(is.na(num) == TRUE){num = 0}
  #label = paste(countries[c], '\n', num, sep='')
  #text(x= election.box[1] - x[c], y=election.box[2]-y[c], labels=label, cex=text.cex )
  #check = check - num # check
  # end/final
  num = filter(flow, country==countries[c])$start.new
  if(is.na(num) == TRUE){num = 0}
  label = paste(countries[c], '\n', num, sep='')
  text(x= end[1] - x[c], y=end[2]-y[c], labels=label, cex=text.cex )
  # quick check that numbers add
  if(check != num){cat('numbers do not add up for', countries[c], '\n')}
  
}
# arrows
arrows(x0=0.25, y0=0.8, y1=0.2, length = 0.1) # start to end, y1=0.2 if included election loss
arrows(x0=0.25, x1=0.5, y0=0.75, length = 0.1) # loss to DOB
arrows(x0=0.25, x1=0.5, y0=0.5, length = 0.1) # loss to DOD
arrows(x0=0.25, x1=0.5, y0=0.25, length = 0.1) # loss to election - Japan only or No matching life table data
dev.off()
