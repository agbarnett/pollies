# 3_animiate_multiple_countries.R
# abandoned
library(gganimate)
library(transformr) # to animate lines



## Animation that shows each country in turn with other countries in grey
# expand data to create other countries as background grey lines
to.animate = NULL
for (this.country in countries){
  this = filter(to.plot, country==this.country) %>% # line to highlight
    mutate(animation.frame=this.country, order=99)
  not.this = filter(to.plot, !country==this.country) %>%
    mutate(animation.frame=this.country, order=1) # other lines
  to.animate = bind_rows(to.animate, this, not.this)
}
# make colours of grey and dark red
to.animate = arrange(to.animate, animation.frame, country, order) %>% # so that red line always appears on top
  mutate(col = as.numeric(as.factor(country)),
         col = ifelse(country == animation.frame, 12, col)) # move to last colour

# Add country labels to right hand side of plot; manually jitter
to.text = group_by(to.plot, country) %>%
  arrange(-year) %>%
  slice(1) %>% # take last year
  select(fitted, country) %>%
  mutate(year = 2017, col=2,  # use common year
         fitted = ifelse(country =='Switzerland', fitted+0.03, fitted),
         fitted = ifelse(country =='NZ', fitted+0.015, fitted),
         fitted = ifelse(country =='UK', fitted-0.01, fitted),
         fitted = ifelse(country =='Austria', fitted+0.015, fitted),
         fitted = ifelse(country =='Japan', fitted+0.025, fitted),
         fitted = ifelse(country =='France', fitted-0.025, fitted),
         fitted = ifelse(country =='Australia', fitted-0.05, fitted)) %>%    
  ungroup()

colours = c(grey(1:11/12), 'orange') # grey lines with striking colour for last spot
fanim = ggplot(data=to.animate, aes(x=year, y=fitted, group=factor(country), col=factor(col))) + 
  geom_hline(yintercept = 1, lty=1, col='dark blue')+ # reference line at SMR = 1
  geom_line(size=1.1)+
  xlab('Year')+
  ylab('Standardised mortality ratio')+
  scale_color_manual(NULL, values=colours)+
  scale_y_continuous(limits=c(0.4, 1.25))+ # remove crazy upper limit from Italy
  scale_x_continuous(limits=c(1816, 2050), breaks=c(1850, 1900, 1950,2000))+ # extend y-axis to allow country labels
  theme_bw() + 
  theme(legend.position = 'none', text=element_text(size=18))+
  geom_text(data=to.text, aes(x=year, y=fitted, label=country), hjust = 0, size=5)+
  # gganimate code:
  labs(title = '{closest_state}')+
  transition_states(animation.frame, transition_length = 1, state_length = 16, wrap = TRUE)

# TO DO, make red line appear on top always
# change order from high to low countries?

# export animation
outfile = paste('animations/multi.country.gif', sep='')
anim_save(filename=outfile, animation=fanim, duration=12)


