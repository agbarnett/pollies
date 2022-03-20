# 3_plot_multiple_countries_women.R
# Plot SMRs from multiple countries in one panel
# version for women
# does weighted and unweighted
# March 2020
library(dplyr)
library(ggplot2)
library(ggrepel) # for non-overlapping labels
# for legend labels
mid.year = 1982 # position for "politicians live longer", etc
start.year = 1926 # position for summary stats

# key decisions:
weighted = TRUE # switch to weighted or unweighted
stype = 'SMR' # SMR (ratio) or SMD (difference)

# get the list of countries
source('R/99_country_list.R')
countries = countries[countries!='NZ'] # exclude NZ as just 11 deaths

# loop through the countries
to.plot = NULL
for (country in countries){
  # a) DIC
  infile = paste('results/', country, '.DIC.women.RData', sep='') # from 3_model_SMR.R
  load(infile)
  best = filter(dic.tab, DIC==min(DIC)) %>%
    mutate(best.df = case_when(
      Model == 'Linear' ~ 1,
      Model == 'Non-linear df=2' ~ 2,
      Model == 'Non-linear df=3' ~ 3))
#  if(country=='Italy'){best$best.df=2} # use 2 df for Italy
  # b) SMR/SMD
  if(weighted==FALSE){infile = paste('results/Results', country, '.', stype, '.women.RData', sep='')} # from 3_model_SMR.R or 3_model_SMD.R
  if(weighted==TRUE){infile = paste('results/Results', country, '.', stype, '.weighted.women.RData', sep='')} # from 3_model_SMR.R or 3_model_SMD.R
  load(infile)
  results = mutate(results, country = country) %>%
    filter(df == best$best.df) # just the best model 
  # if using SMD then rename to SMR
  if(stype=='SMD'){
    results = select(results, -SMR) %>%
      rename('SMR' = 'SMD')
  }
  to.plot = bind_rows(to.plot, results)
  remove(dic.tab, results) # tidy up
}

# prepare statistics
stats.extremes = group_by(to.plot, country) %>% # stats on min/max 
  # there are observed SMRs, too many zero / extreme value
#  summarise(min = round(min(SMR),2),
#            max = round(max(SMR),2))
# there are models SMRs
summarise(min = round(min(fitted),2),
          max = round(max(fitted),2))
stats.latest = group_by(to.plot, country) %>% # stats on latest
  arrange(country, -year) %>%
  slice(1) %>%
  ungroup() %>%
  select(country, fitted) # use fitted
stats = full_join(stats.extremes, stats.latest, by='country') %>%
  mutate(
    lower = 0, # needed for plot
    upper = 0, 
    label = paste(max, '\n', min, '\n', round(fitted,2), sep=''), # without labels
  #label = paste("Max = ", max, '\nMin=', min, '\nLatest=', round(SMR,2), sep=''), # with labels
   year = start.year) # add (x,y) coordinates
if(stype=='SMR'){stats$SMR=2} # change label position depending on stats 
if(stype=='SMD'){stats$SMR=20}

## plot with country as facet ##
# add legend in bottom-right
legend = to.plot[1,] %>% # start with row in the data
  mutate(country=' ', year=NA, SMR=0, lower=0, upper=0) # missing year creates missing warning
to.plot.plus.legend = bind_rows(to.plot, legend)
mplot = ggplot(data=to.plot.plus.legend, aes(x=year, y=SMR)) + 
  geom_line(size=0.15, col=grey(0.4))+
  geom_point(col=grey(0.4), size=0.4)+
  geom_line(aes(x=year, y=fitted), col='blue', size=0.6)+ # thin lines
  xlab('Year')+
  theme_bw()+
  theme(axis.text.x = element_text(size=6))+ # reduce text size
  facet_wrap(~country, ncol=3)
if(stype=='SMR'){mplot = mplot + 
  ylab('Standardised mortality ratio')+
  scale_y_continuous(limits=c(0,2))+ # remove crazy upper limit from Italy
  geom_hline(yintercept = 1, lty=1, col='dark red', size=0.8)+ # reference line at SMR = 1
  # legend:
  geom_label(data=stats, aes(x=year, y=SMR, label=label), size=1.5, hjust=0, vjust=1)+ # alignment for top-left corner
  geom_text(data=legend, aes(x=mid.year, y=1.8, label='Politicians\nliving shorter'), size=2)+
  geom_text(data=legend, aes(x=mid.year, y=1.05, label='Politicians equal\n to general population'), col='dark red', size=2)+
  geom_text(data=legend, aes(x=mid.year, y=0.3, label='Politicians\n living longer'), size=2)+
  geom_label(data=legend, aes(x=start.year, y=2, label='Max\nMin\nLatest'), hjust=0, vjust=1, size=1.5) # labels
}
if(stype=='SMD'){mplot = mplot + 
  ylab('Standardised mortality difference')+
  scale_y_continuous(limits=c(-60,25))+ # remove crazy lower/upper limit from Italy
  geom_hline(yintercept = 0, lty=1, col='dark red', size=0.8)+ # reference line at SMD = 0
  # legend:
  geom_label(data=stats, aes(x=year, y=SMR, label=label), size=1.5, hjust=0, vjust=1)+ # alignment for top-left corner
  geom_text(data=legend, aes(x=mid.year, y=20, label='Politicians\nliving shorter'), size=2)+
  geom_text(data=legend, aes(x=mid.year, y=1.05, label='Politicians equal\n to general population'), col='dark red', size=2)+
  geom_text(data=legend, aes(x=mid.year, y=-30, label='Politicians\n living longer'), size=2)+
  geom_label(data=legend, aes(x=start.year, y=20, label='Max\nMin\nLatest'), hjust=0, vjust=1, size=1.5) # labels
}
if(stype=='SMD' & weighted==TRUE){mplot = mplot + 
  scale_y_continuous(limits=c(-32,25)) # remove crazy lower/upper limit from Italy
}
start.file = paste('MultiCountry', stype, sep='')
if(weighted==FALSE){outfile = paste('figures/', start.file, '.women.jpg', sep='')}
if(weighted==TRUE){outfile = paste('figures/', start.file, '.weighted.women.jpg', sep='')}
jpeg(outfile, width=5.5, height=4, units='in', res=400, quality=100)
print(mplot)
dev.off()
## version with confidence intervals ##
ciplot = ggplot(data=to.plot.plus.legend, aes(x=year, y=SMR, ymin=lower, ymax=upper)) + 
  geom_ribbon(alpha=0.4, fill='darkorange')+
  geom_line(size=0.15, col=grey(0.4))+
  geom_point(col=grey(0.4), size=0.4)+
  geom_line(aes(x=year, y=fitted), col='blue', size=0.6)+ 
  facet_wrap(~country, ncol=3)+
  # thin lines
  xlab('Year')+
  theme_bw()+
  theme(axis.text.x=element_text(size=8)) # reduce text size
if(stype=='SMR'){ciplot = ciplot + 
  ylab('Standardised mortality ratio')+
  scale_y_continuous(limits=c(0,2))+ # 
  geom_hline(yintercept = 1, lty=1, col='dark red', size=0.8)+ # reference line at SMR = 1
  # legend:
  geom_label(data=stats, aes(x=year, y=SMR, label=label), size=2, hjust=0, vjust=1, col='blue')+ # alignment for top-left corner
  geom_text(data=legend, aes(x=mid.year, y=1.8, label='Politicians\nliving shorter'), size=2.5)+
  geom_text(data=legend, aes(x=mid.year, y=1.05, label='Politicians equal\n to general population'), col='dark red', size=2.5)+
  geom_text(data=legend, aes(x=mid.year, y=0.3, label='Politicians\n living longer'), size=2.5)+
  geom_label(data=legend, aes(x=start.year, y=2, label='Max\nMin\nLatest'), hjust=0, vjust=1, size=2, col='blue') # labels
}
if(stype=='SMD'){ciplot = ciplot + 
  ylab('Standardised mortality difference')+
  scale_y_continuous(limits=c(-60,25))+ # remove crazy upper limit from Italy
  geom_hline(yintercept = 0, lty=1, col='dark red', size=0.8)+
  # legend:
  geom_label(data=stats, aes(x=year, y=SMR, label=label), size=1.5, hjust=0, vjust=1)+ # alignment for top-left corner
  geom_text(data=legend, aes(x=mid.year, y=20, label='Politicians\nliving shorter'), size=2)+
  geom_text(data=legend, aes(x=mid.year, y=1.05, label='Politicians equal\n to general population'), col='dark red', size=2)+
  geom_text(data=legend, aes(x=mid.year, y=-18, label='Politicians\n living longer'), size=2)+
  geom_label(data=legend, aes(x=start.year, y=20, label='Max\nMin\nLatest'), hjust=0, vjust=1, size=1.5) # labels
}
if(stype=='SMD' & weighted==TRUE){ciplot = ciplot + 
  scale_y_continuous(limits=c(-32,25)) # remove crazy lower/upper limit from Italy
}
start.file = paste('MultiCountryWithIntervals', stype, sep='')
if(weighted==FALSE){outfile = paste('figures/', start.file, '.women.jpg', sep='')}
if(weighted==TRUE){outfile = paste('figures/', start.file, '.weighted.women.jpg', sep='')}
jpeg(outfile, width=6, height=6, units='in', res=400, quality=100)
print(ciplot)
dev.off()


## plot of just the trend line with countries on same axes ##
# colours from https://medialab.github.io/iwanthue/, with some edits
colours = c("orange",
            'grey',
           "#009317",
           "#8d1ad2",
           "#a3ffe6",
           "#ff47f2",
           "#0e1c00",
           "#0148b1",
           "#844b00", 
           "light green", # switzerland
           "#ffc3bb",
           "red")
tplot = ggplot(data=to.plot, aes(x=year, y=fitted, col=factor(country))) + 
  geom_line(size=1.1)+
  xlab('Year')+
  scale_color_manual('Country', values=colours)+
  theme_bw()
if(stype=='SMR'){tplot = tplot + 
  ylab('Standardised mortality ratio')+
#  scale_y_continuous(limits=c(0.4, 2))+ # 
  geom_hline(yintercept = 1, lty=1, col='dark red') # reference line at SMR = 1
}
if(stype=='SMD'){tplot = tplot + 
  ylab('Standardised mortality difference')+
  geom_hline(yintercept = 0, lty=1, col='dark red')}
tplot
# export
outfile = paste('figures/MultiCountryMean.', stype, '.women.jpg', sep='')
jpeg(outfile, width=5.5, height=3.3, units='in', res=400, quality=100)
print(tplot)
dev.off()

##
## alternative arrangement of two 2x3 ##
c.list = c('Italy','Netherlands','NZ','Switzerland','UK','USA') # last six countries in alphabetical order
last.six = filter(to.plot, country %in% c.list)
first.five = filter(to.plot, !country %in% c.list)
last.six.stats = filter(stats, country %in% c.list)
first.five.stats = filter(stats, !country %in% c.list)
## plot with country as facet (no legend) ##
mplot2 = ggplot(data=last.six, aes(x=year, y=SMR)) + 
  geom_line(size=0.3, col=grey(0.4))+
  geom_point(col=grey(0.4), size=0.4)+
  geom_line(aes(x=year, y=fitted), col='blue', size=0.7)+ # thin lines
  xlab('Year')+
  geom_label(data=last.six.stats, aes(x=year, y=SMR, label=label), size=1.5, hjust=0, vjust=1)+ # alignment for top-left corner
  theme_bw()+
  facet_wrap(~country)
if(stype=='SMR'){mplot2 = mplot2 +
  ylab('Standardised mortality ratio')+
  scale_y_continuous(limits=c(0,2))+ # remove crazy upper limit from Italy
  geom_hline(yintercept = 1, lty=1, col='dark red', size=0.5)} # reference line at SMR = 1
if(stype=='SMD'){mplot2 = mplot2 +
  ylab('Standardised mortality difference')+
  scale_y_continuous(limits=c(-60,25))+ # remove crazy upper limit from Italy
  geom_hline(yintercept = 0, lty=1, col='dark red', size=0.5)} # reference line at SMR = 1
if(stype=='SMD' & weighted==TRUE){mplot2 = mplot2 + 
  scale_y_continuous(limits=c(-32,25)) # remove crazy lower/upper limit from Italy
}
mplot2
# version with CIs
mplot2.ci = ggplot(data=last.six, aes(x=year, y=SMR, ymin=lower, ymax=upper)) + 
  geom_ribbon(alpha=0.2)+
  geom_line(size=0.3, col=grey(0.4))+
  geom_point(col=grey(0.4), size=0.4)+
  geom_line(aes(x=year, y=fitted), col='blue', size=0.7)+ # thin lines
  xlab('Year')+
  geom_label(data=last.six.stats, aes(x=year, y=SMR, label=label), size=1.5, hjust=0, vjust=1)+ # alignment for top-left corner
  theme_bw()+
  facet_wrap(~country)
if(stype=='SMR'){mplot2.ci = mplot2.ci +
  ylab('Standardised mortality ratio')+
  scale_y_continuous(limits=c(0,2))+ # remove crazy upper limit from Italy
  geom_hline(yintercept = 1, lty=1, col='dark red', size=0.5)} # reference line at SMR = 1
if(stype=='SMD'){mplot2.ci = mplot2.ci +
  ylab('Standardised mortality difference')+
  scale_y_continuous(limits=c(-60,25))+ # remove crazy upper limit from Italy
  geom_hline(yintercept = 0, lty=1, col='dark red', size=0.5)} # reference line at SMR = 1
if(stype=='SMD' & weighted==TRUE){mplot2.ci = mplot2.ci + 
  scale_y_continuous(limits=c(-32,25)) # remove crazy lower/upper limit from Italy
}

# second plot with legend  
two.plus.legend = bind_rows(first.five, legend)
mplot1 = ggplot(data=two.plus.legend, aes(x=year, y=SMR)) + 
  geom_line(size=0.3, col=grey(0.4))+
  geom_point(col=grey(0.4), size=0.4)+
  geom_line(aes(x=year, y=fitted), col='blue', size=0.7)+ # thin lines
  xlab('Year')+
  theme_bw()+
  facet_wrap(~country)
if(stype=='SMR'){mplot1 = mplot1 +
  ylab('Standardised mortality ratio')+
  scale_y_continuous(limits=c(0,2))+ # remove crazy upper limit from Italy
  geom_hline(yintercept = 1, lty=1, col='dark red', size=0.5)+ # reference line at SMR = 1
  # legend:
  geom_label(data=first.five.stats, aes(x=year, y=SMR, label=label), size=1.5, hjust=0, vjust=1)+ # alignment for top-left corner
  geom_text(data=legend, aes(x=mid.year, y=1.8, label='Politicians\nliving shorter'), size=2)+
  geom_text(data=legend, aes(x=mid.year, y=1.03, label='Politicians equal\n to general population'), col='dark red', size=2)+
  geom_text(data=legend, aes(x=mid.year, y=0.3, label='Politicians\n living longer'), size=2)+
  geom_label(data=legend, aes(x=start.year, y=2, label='Max\nMin\nLatest'), hjust=0, vjust=1, size=1.5) # labels
}
if(stype=='SMD'){mplot1 = mplot1 +
  ylab('Standardised mortality difference')+
  geom_hline(yintercept = 0, lty=1, col='dark red', size=0.5)+ # reference line at SMR = 1
  scale_y_continuous(limits=c(-60,25))+ # remove crazy upper limit from Italy
  # legend:
  geom_label(data=first.five.stats, aes(x=year, y=SMR, label=label), size=1.5, hjust=0, vjust=1)+ # alignment for top-left corner
  geom_text(data=legend, aes(x=mid.year, y=20, label='Politicians\nliving shorter'), size=2)+
  geom_text(data=legend, aes(x=mid.year, y=1.03, label='Politicians equal\n to general population'), col='dark red', size=2)+
  geom_text(data=legend, aes(x=mid.year, y=-20, label='Politicians\n living longer'), size=2)+
  geom_label(data=legend, aes(x=start.year, y=20, label='Max\nMin\nLatest'), hjust=0, vjust=1, size=1.5) # labels
}
if(stype=='SMD' & weighted==TRUE){mplot1 = mplot1 + 
  scale_y_continuous(limits=c(-32,25)) # remove crazy lower/upper limit from Italy
}

# second plot with legend and CI
two.plus.legend = bind_rows(first.five, legend)
mplot1.ci = ggplot(data=two.plus.legend, aes(x=year, y=SMR, ymin=lower, ymax=upper)) + 
  geom_ribbon(alpha=0.2)+
  geom_line(size=0.3, col=grey(0.4))+
  geom_point(col=grey(0.4), size=0.4)+
  geom_line(aes(x=year, y=fitted), col='blue', size=0.7)+ # thin lines
  xlab('Year')+
  theme_bw()+
  facet_wrap(~country)
if(stype=='SMR'){mplot1.ci = mplot1.ci +
  ylab('Standardised mortality ratio')+
  scale_y_continuous(limits=c(0,2))+ # remove crazy upper limit from Italy
  geom_hline(yintercept = 1, lty=1, col='dark red', size=0.5)+ # reference line at SMR = 1
  # legend:
  geom_label(data=first.five.stats, aes(x=year, y=SMR, label=label), size=1.5, hjust=0, vjust=1)+ # alignment for top-left corner
  geom_text(data=legend, aes(x=mid.year, y=1.8, label='Politicians\nliving shorter'), size=2)+
  geom_text(data=legend, aes(x=mid.year, y=1.03, label='Politicians equal\n to general population'), col='dark red', size=2)+
  geom_text(data=legend, aes(x=mid.year, y=0.3, label='Politicians\n living longer'), size=2)+
  geom_label(data=legend, aes(x=start.year, y=2, label='Max\nMin\nLatest'), hjust=0, vjust=1, size=1.5) # labels
} 
if(stype=='SMD'){mplot1.ci = mplot1.ci +
  ylab('Standardised mortality difference')+
  scale_y_continuous(limits=c(-60,25))+ # remove crazy upper limit from Italy
  geom_hline(yintercept = 0, lty=1, col='dark red', size=0.5)+ # reference line at SMR = 1
  # legend:
  geom_label(data=first.five.stats, aes(x=year, y=SMR, label=label), size=1.5, hjust=0, vjust=1)+ # alignment for top-left corner
  geom_text(data=legend, aes(x=mid.year, y=20, label='Politicians\nliving shorter'), size=2)+
  geom_text(data=legend, aes(x=mid.year, y=1.03, label='Politicians equal\n to general population'), col='dark red', size=2)+
  geom_text(data=legend, aes(x=mid.year, y=-20, label='Politicians\n living longer'), size=2)+
  geom_label(data=legend, aes(x=start.year, y=20, label='Max\nMin\nLatest'), hjust=0, vjust=1, size=1.5) # labels
}
if(stype=='SMD' & weighted==TRUE){mplot1.ci = mplot1.ci + 
  scale_y_continuous(limits=c(-32,25)) # remove crazy lower/upper limit from Italy
}


# export
start.file = paste('MultiCountry1.', stype, sep='')
if(weighted==FALSE){outfile = paste('figures/', start.file, '.women.jpg', sep='')}
if(weighted==TRUE){outfile = paste('figures/', start.file, '.weighted.women.jpg', sep='')}
jpeg(outfile, width=5.5, height=4, units='in', res=400, quality=100)
print(mplot1)
dev.off()
start.file = paste('MultiCountry2.', stype, sep='')
if(weighted==FALSE){outfile = paste('figures/', start.file, '.women.jpg', sep='')}
if(weighted==TRUE){outfile = paste('figures/', start.file, '.weighted.women.jpg', sep='')}
jpeg(outfile, width=5.5, height=4, units='in', res=400, quality=100)
print(mplot2)
dev.off()
start.file = paste('MultiCountry1CI.', stype, sep='')
if(weighted==FALSE){outfile = paste('figures/', start.file, '.women.jpg', sep='')}
if(weighted==TRUE){outfile = paste('figures/', start.file, '.weighted.women.jpg', sep='')}
jpeg(outfile, width=5.5, height=4, units='in', res=400, quality=100)
print(mplot1.ci)
dev.off()
start.file = paste('MultiCountry2CI.', stype, sep='')
if(weighted==FALSE){outfile = paste('figures/', start.file, '.women.jpg', sep='')}
if(weighted==TRUE){outfile = paste('figures/', start.file, '.weighted.women.jpg', sep='')}
jpeg(outfile, width=5.5, height=4, units='in', res=400, quality=100)
print(mplot2.ci)
dev.off()

