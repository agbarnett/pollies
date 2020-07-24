# 98_check_lifetables.R
# check life tables over time in all countries
# August 2019
library(dplyr)
library(ggplot2)
source('R/99_country_list.R') # get list of countries
# year labels
labels = seq(1820, 2020, 20)
# meta data
load('data/meta.RData')

## get life table data for all countries
all.life = NULL
for (c in countries){
  # get meta data
  this.meta = dplyr::filter(meta, Country==c)
  # get life table data (imputed or not)
  infile = paste('data/', c, '/', c, '.RData', sep='')
  if(this.meta$imputed.life == TRUE){infile = paste('data/', c, '/', c, '.imputed.RData', sep='')}
  load(infile)
  life.table = mutate(life.table, country=c) %>%
    filter(Year >= this.meta$Syear) # start plots from first year available in data

  all.life = bind_rows(all.life, life.table)
}

## plot cumulative numbers
to.plot = filter(all.life, Age %in% c(25, 45), Sex=='Male')
cplot = ggplot(data=to.plot, aes(x=Year, y=qx, col=factor(Age)))+
  geom_line()+
  scale_x_continuous(breaks=labels)+
  theme_bw()+
  scale_color_manual('Males aged', values=c('red','blue'))+
  theme(panel.grid.minor = element_blank(),
        legend.position = c(0.85,0.1),
        axis.text.x = element_text(angle=45, hjust=1, size = 6))+
  facet_wrap(~country, scales='free')+
  xlab('')+
  ylab('Death probability')
cplot
jpeg('figures/death.over.time.by.country.jpg', width=6, height=5, units='in', res=400)
print(cplot)
dev.off()

