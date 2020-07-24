# 3_animate_SMR.R
# Create animated plot of the SMR data for i) increasing years of follow-up, and ii) censoring ages
# August 2019
library(dplyr)
library(ggplot2)
library(gganimate)
library(transformr) # to animate lines
# set animiation size
options(gganimate.dev_args = list(width = 800, height = 600))

### 1) Plot observed and expected deaths over time for varying follow-up
# which country and upper SMR for plot
country.frame = read.table(stringsAsFactors = FALSE, header=TRUE, sep=',', text='
country,upper.SMR
Italy,2
Canada,2
Austria,2
Germany,2
Japan,2
Austria,2
Switzerland,2
Netherlands,2
Brobdingnag,4
NZ,2
UK,1.5
USA,1.5
France,1.7
Australia,2.5')

# Loop through countries
for (n in 1:nrow(country.frame)){
  country = country.frame$country[n]
  upper.SMR = country.frame$upper.SMR[n]

# a) get the data
infile = paste('data/', country, '/SMR.RData', sep='')
load(infile)
use.imputed = any(SMR$imputed == 'Yes') # is there any imputed life table data?
# b) plot
to.animate = filter(SMR, follow < 100) %>% # leave off 100 year follow-up (which is essentially infinity)
  mutate(follow=as.integer(follow))
fplot = ggplot(data=to.animate, aes(x=year, y=SMR))+
  geom_hline(yintercept = 1, lty=1, col='dark red')+ # reference line at SMR = 1
  geom_point(size=3, col=grey(0.4))+
#  geom_line(lwd=1.1)+ # looks better without line
  geom_smooth(method='loess', lwd=1.1, se = FALSE, col='blue')+ # add smooth
  theme_bw()+
  theme(panel.grid.major = element_blank(), text=element_text(size=19))+
  coord_cartesian(ylim=c(0, upper.SMR))+ # truncate very high SMRs (usually at left of plot)
# gganimate code:
  labs(title = 'Follow-up time in years: {frame_time}', x = 'Year', y = 'Standardised mortality ratio')+
  transition_time(follow) 
# not working:
#if(use.imputed==TRUE){ # added shaded area to highlight imputed life-table years
#  show.impute = filter(SMR, follow == 100, imputed=='Yes')
  #fplot = fplot +
  #  geom_rect(data=show.impute, mapping=aes(xmin=year, xmax=year+1, ymin=-Inf, ymax=Inf), fill='lightgreen', col='transparent', alpha=0.25)#
#}

# export to GIF
anim <- animate(fplot, fps=5) # fps = frames per second
outfile = paste('animations/', country, '.SMR.follow.gif', sep='')
anim_save(filename=outfile, animation=anim, start_pause=4)

# export to mp4
anim <- animate(fplot, renderer = av_renderer())
outfile = paste('animations/', country, '.SMR.follow.mp4', sep='')
anim_save(filename=outfile, anim)

# tidy up
remove(SMR)

} # end of country loop


### 2 Plot observed and expected deaths over time for varying follow-up
# which country and upper SMR for plot (lower upper limits for these plots)
country.frame = read.table(stringsAsFactors = FALSE, header=TRUE, sep=',', text='
country,upper.SMR
Australia,2
Austria,2
Brobdingnag,4
Canada,2
France,1.7
Germany,2
Italy,1.75
Japan,1.5
NZ,2.5
UK,1.5
Switzerland,2')

# Loop through countries
for (n in 1:nrow(country.frame)){
  country = country.frame$country[n]
  upper.SMR = country.frame$upper.SMR[n]
  
  # a) get the data
infile = paste('data/', country, '/SMR.Age.RData', sep='')
load(infile)
use.imputed = any(SMR$imputed == 'Yes') # is there any imputed life table data?
# b) plot
to.animate = mutate(SMR, age.cut = as.integer(age.cut)) %>%
  filter(age.cut <= 100) # max age of 100
fplot = ggplot(data=to.animate, aes(x=year, y=SMR))+
  geom_hline(yintercept = 1, lty=1, col='dark red')+ # reference line at SMR = 1
  geom_point(size=3, col=grey(0.4))+
#  geom_line(lwd=1.1)+ # looks better without
  geom_smooth(method='loess', lwd=1.1, se = FALSE, col='blue')+ # add smooth
  theme_bw()+
  theme(panel.grid.major = element_blank(), text = element_text(size=19))+
  coord_cartesian(ylim=c(0, upper.SMR))+ # truncate very high SMRs (usually at left of plot)
  # gganimate code:
  labs(title = 'Followed until age: {frame_time}', x = 'Year', y = 'Standardised mortality ratio')+
  transition_time(age.cut) 
# not working:
#if(use.imputed==TRUE){ # added shaded area to highlight imputed life-table years
#  show.impute = filter(SMR, age.cut == 100, imputed=='Yes')
#fplot = fplot +
#  geom_rect(data=show.impute, mapping=aes(xmin=year, xmax=year+1, ymin=-Inf, ymax=Inf), fill='lightgreen', col='transparent', alpha=0.25)#
#}

# export to GIF
anim <- animate(fplot, fps=5) # fps = frames per second
outfile = paste('animations/', country, '.SMR.ages.gif', sep='')
anim_save(filename=outfile, animation=anim, start_pause=4)

# export to mp4
anim <- animate(fplot, renderer = av_renderer())
outfile = paste('animations/', country, '.SMR.ages.mp4', sep='')
anim_save(filename=outfile, anim, codec ='libx264rgb')


} # end of country loop


## In lieu of animation, plot selected frames
country = 'Australia'
infile = paste('data/', country, '/SMR.Age.RData', sep='')
load(infile)
# 
to.facet = mutate(SMR, age.cut = as.integer(age.cut)) %>%
  filter(age.cut %in% c(50,60,70,80)) %>% # four panels
  mutate(facet = paste('Age =', age.cut))
  
facet.plot = ggplot(data=to.facet, aes(x=year, y=SMR))+
  geom_hline(yintercept = 1, lty=1, col='dark red')+ # reference line at SMR = 1
  geom_point(size=3, col=grey(0.4))+
  geom_smooth(method='loess', lwd=1.1, se = FALSE, col='blue')+ # add smooth
  theme_bw()+
  xlab('Year')+
  ylab('Standardised mortality ratio')+
  theme(panel.grid.major = element_blank(), text = element_text(size=19))+
  coord_cartesian(ylim=c(0, 2))+ # truncate very high SMRs (usually at left of plot)+
  facet_wrap(~facet)
# export for Powerpoint in 16:9
jpeg('figures/4panels.Australia.jpg', width=7.5, height=6, units='in', res=400, quality=100)
print(facet.plot)
dev.off()
