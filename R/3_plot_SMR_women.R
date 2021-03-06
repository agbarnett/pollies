# 3_plot_SMR_women.R
# Create selected plot of the SMR data, works on multiple countries
# version for women
# March 2020
library(dplyr)
library(ggplot2)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# get the list of countries
source('R/99_country_list.R')
weighted = FALSE

# loop through the countries
to.plot = to.plot.impute = NULL
for (country in countries){
  # get the data, weighted or not
  if(weighted==FALSE){infile = paste('data/', country, '/SMR_women.RData', sep='')}
  if(weighted==TRUE){infile = paste('data/', country, '/SMR.weighted.women.RData', sep='')}
  load(infile)
  use.imputed = any(SMR$imputed == 'Yes') # is there any imputed life table data?
  
  # Weighted or not
  if(weighted==TRUE){
    SMR = select(SMR, -O, -E) %>%
      rename('O' = 'observed.weighted',
             'E' = 'expected.weighted')
  }
  
  ## Plot observed and expected deaths over time
  this.plot = mutate(SMR, 
           O = O,
           E = E) %>% # per politician
    dplyr::select(year, O, E) %>%
    tidyr::gather('which', 'Deaths', 2:3) %>%
    mutate(
      country = country,
      which = factor(which),
      which = forcats::fct_recode(which, Expected='E', Observed='O'))
  eplot = ggplot(data=this.plot, aes(x=year, y=Deaths, col=which, linetype=which))+
    geom_line(size=1.05)+
    theme_bw()+
    theme(panel.grid.minor = element_blank())+
    scale_linetype_manual('', values=1:2)+
    scale_color_manual('', values=cbPalette[1:2])+
    xlab('Year')
  if(use.imputed==TRUE){ ## add transparent area to show imputation
    show.impute = filter(SMR, imputed=='Yes') %>%
      mutate(Deaths=0, which='Observed')
    eplot = eplot +
      geom_rect(data=show.impute, mapping=aes(xmin=year, xmax=year+1, ymin=-Inf, ymax=Inf, linetype=which), fill=grey(0.8), col='transparent', alpha=0.25)
    to.plot.impute = bind_rows(to.plot.impute, show.impute)
  }
  eplot
  if(weighted==FALSE){outfile = paste('figures/OE/OE', country, '.women.jpg', sep='')}
  if(weighted==TRUE){outfile = paste('figures/OE/OE', country, '.weighted.women.jpg', sep='')}
  jpeg(outfile, width=5, height=4, units='in', res=500, quality = 100)
  print(eplot)
  dev.off()
  
  # store for big plot
  to.plot = bind_rows(to.plot, this.plot)
}

## big plot with country as facet
eplot.big = ggplot(data=to.plot, aes(x=year, y=Deaths, col=which, linetype=which))+
  geom_line(size=0.5)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position=c(0.9,0.1))+
  scale_linetype_manual('', values=c(1,6))+
  scale_color_manual('', values=cbPalette[1:2])+
  xlab('Year')+
  facet_wrap(~country, scales='free_y')
#  geom_rect(data=show.impute, mapping=aes(xmin=year, xmax=year+1, ymin=-Inf, ymax=Inf, linetype=which), fill=grey(0.8), col='transparent', alpha=0.25)
if(weighted==FALSE){outfile = paste('figures/OE/OEmultiCountry.women.jpg', sep='')}
if(weighted==TRUE){outfile = paste('figures/OE/OEmultiCountry.weighted.women.jpg', sep='')}
jpeg(outfile, width=7, height=5, units='in', res=500, quality = 100)
print(eplot.big)
dev.off()

