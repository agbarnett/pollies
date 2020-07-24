# 3_model_SMR.R
# Model the standardised mortality ratio
# Can use both weighted and unweighted data
# March 2020
#source('R/3_make_Bayes_SMR.R') # makes the WinBUGS models
source('R/99_country_list.R') # get list of countries
library(dplyr)
library(stringr)
library(splines)
library(ggplot2)
library(R2WinBUGS)
# samples and thinning for MCMC:
MCMC = 5000; thin=5; n.chains=2; debug=FALSE 

weighted = TRUE # true for population weighted SMR, false otherwise

# big loop
for (country in countries){

# Some countries need an upper limit because of odd single years
upper.y.limit = ifelse(country=='UK', 1.5, NA)
upper.y.limit = ifelse(country=='Italy', 2, upper.y.limit)
upper.y.limit = ifelse(country=='Germany', 1.8, upper.y.limit)
lower.y.limit = 0.25 # same for all countries

# load the data
if(weighted==FALSE){infile = paste('data/', country, '/SMR.RData', sep='')}
if(weighted==TRUE){infile = paste('data/', country, '/SMR.weighted.RData', sep='')}
load(infile) # from 2_SMR.R or weighted version
if(weighted==FALSE){
  SMR = dplyr::filter(SMR, follow == 100) # just for follow-up time of 100 (shorter years are for animation)
}
if(weighted==TRUE){
  SMR = select(SMR, -SMR, -O, -E) %>% # drop unweighted SMR, observed and expected
    mutate(follow = 100) %>%  # add dummy follow-up time
        rename('O' = 'observed.weighted',
               'E' = 'expected.weighted',
               'SMR' = 'SMRw') # use weighted SMR
}
use.imputed = any(SMR$imputed == 'Yes') # is there any imputed life table data?
# load the meta-data
load('data/meta.RData')
meta = filter(meta, Country==country)

# Non-Bayesian Poisson model for smooth SMR over time
basis = ns(SMR$year, df=2) # add spline basis for time effect to data
non.bayes = FALSE # logical to run non-bayesian models or not
if(non.bayes==TRUE){
  model0 = glm(O ~ 1, offset=log(E), data=SMR, family=poisson()) # no change (null model)
  model = glm(O ~ basis, offset=log(E), data=SMR, family=poisson())
  AIC(model0, model)
  summary(model)
  # get fitted values for spline
  new.data = SMR; new.data$E=1
  fitted = predict(model, newdata=new.data, type = 'response', se.fit = TRUE)
  SMR$fitted = fitted$fit
  z = qnorm(0.975)
  SMR$lower = fitted$fit - (z*fitted$se.fit)
  SMR$upper = fitted$fit + (z*fitted$se.fit)
  
  # plot spline predictions over time
  tplot = ggplot(data=SMR, aes(x=year, y=SMR)) + 
    geom_line(aes(x=year, y=fitted), col='blue', size=1.05)+
    geom_line(aes(x=year, y=lower), col='blue', lty=2, size=1.05)+
    geom_line(aes(x=year, y=upper), col='blue', lty=2, size=1.05)+
    geom_line(size=0.5)+
    geom_hline(yintercept=1)+
    geom_point(col=grey(0.6))+
    xlab('Year')+
    ylab('Standardised mortality ratio')+
    theme_bw()
  tplot
} # end of non-Bayesian logical

### Bayesian models - these are the ones used ### 
# loop through 2 and 3 degrees of freedom for spline
dic.tab = results = NULL
for (df in 2:3){
# prepare the data
N = nrow(SMR) # number of years
basis = ns(SMR$year, df=df) # spline for time
X = cbind(rep(1, nrow(basis)), basis); colnames(X) = NULL
J = ncol(X) # number of parameters
bdata = list(N = N, E = SMR$E, O = SMR$O, X = X, J = J)
inits = list(beta=rep(0, J)) # initial values 
inits = rep(list(inits), n.chains) # repeat per chains

# run BUGS
bfile = 'bayes.ns.txt' # from 3_make_Bayes.R
parms = c('beta','regress')
setwd('R') # need to be in same folder for bugs model to work
bugs.ns =  bugs(data=bdata, inits=inits, parameters=parms, model.file=bfile,
                     n.chains=n.chains, n.iter=MCMC*thin*2, n.thin=thin, bugs.seed=879497, debug=debug,
                     bugs.directory="c:/Program Files/WinBUGS14")
setwd('..') # move back to main folder
# store DIC
dic.row = data.frame(country=country, Model=paste('Non-linear df=', df, sep=''), 
             pD=bugs.ns$pD, DIC= bugs.ns$DIC)
dic.tab = rbind(dic.tab, dic.row)
# get MCMC chains
columns = str_detect(pattern ='regress', dimnames(bugs.ns$sims.array)[[3]])
to.estimate = t(bugs.ns$sims.array[,1,columns])
## over-write fitted and confidence interval with Bayesian estimate
for (i in 1:nrow(SMR)){
  SMR$fitted[i] = exp(mean(to.estimate[i, ]))
  SMR$lower[i] = exp(quantile(to.estimate[i, ], probs=0.025))
  SMR$upper[i] = exp(quantile(to.estimate[i, ], probs=0.975))
}
# plot SMR with spline predictions over time
ns.plot = ggplot(data=SMR, aes(x=year, y=SMR)) + 
  geom_hline(yintercept = 1, lty=1, col='dark red')+ # reference line at SMR = 1
  geom_line(size=0.2, col=grey(0.4))+
  geom_point(col=grey(0.4), size=0.9)+
  geom_line(aes(x=year, y=fitted), col='blue', size=1.05)+
  geom_line(aes(x=year, y=lower), col='blue', lty=2, size=1.05)+
  geom_line(aes(x=year, y=upper), col='blue', lty=2, size=1.05)+
  xlab('Year')+
  ylab('Standardised mortality ratio')+
  theme_bw()+
  theme(panel.grid.minor = element_blank())
if(use.imputed==TRUE){ # added shaded area to highlight imputed life-table years
  show.impute = filter(SMR, follow == 100, imputed=='Yes')
  ns.plot = ns.plot +
    geom_rect(data=show.impute, mapping=aes(xmin=year, xmax=year+1, ymin=-Inf, ymax=Inf), fill='lightgreen', col='transparent', alpha=0.25)
}
if(is.na(upper.y.limit) == FALSE){ns.plot = ns.plot + coord_cartesian(ylim=c(lower.y.limit, upper.y.limit))} # limit for Italy/UK because of one large positive outlier
if(weighted==FALSE){outfile = paste('figures/nonlineardf', df, '/SMR', country, '.nonlinear.', df ,'df.jpg', sep='')}
if(weighted==TRUE){outfile = paste('figures/nonlineardf', df, '/SMR', country, '.nonlinear.', df ,'df.weighted.jpg', sep='')}
jpeg(outfile, width=5, height=4, units='in', res=500, quality = 100)
print(ns.plot)
dev.off()
# store results
SMR$df = df
results = bind_rows(results, SMR)
} # end of df loop

## B. Bayes model with linear time trend
bfile = 'bayes.ns.txt' # can use same winbugs file as previous model
# prepare the random data
N = nrow(SMR) # number of years
X = cbind(rep(1, nrow(basis)), (SMR$year-meta$median.year)/20); colnames(X) = NULL
J = ncol(X) # number of parameters
bdata = list(N = N, E = SMR$E, O = SMR$O, X = X, J = J)
inits = list(beta=rep(0, J)) # initial values 
inits = rep(list(inits), n.chains) # repeat per chains
# run BUGS
parms = c('beta','regress')
setwd('R') # need to be in same folder for bugs model to work
bugs.linear =  bugs(data=bdata, inits=inits, parameters=parms, model.file=bfile,
                n.chains=n.chains, n.iter=MCMC*thin*2, n.thin=thin, bugs.seed=879497, debug=debug,
                bugs.directory="c:/Program Files/WinBUGS14")
setwd('..') # move back to main folder
# get MCMC chains
cols = str_detect(pattern ='regress', dimnames(bugs.linear$sims.array)[[3]])
to.estimate = t(bugs.linear$sims.array[,1,cols])

# compare DIC
dic.row = data.frame(country=country, Model='Linear', 
             pD=bugs.linear$pD, DIC= bugs.linear$DIC)
dic.tab = rbind(dic.tab, dic.row)
dic.tab
# store dic and bugs results
if(weighted==TRUE){outfile = paste('results/', country, '.DIC.weighted.RData', sep='')}
if(weighted==FALSE){outfile = paste('results/', country, '.DIC.RData', sep='')}
save(bugs.ns, bugs.linear, dic.tab, file=outfile)

## over-write fitted and confidence interval with Bayesian estimate (linear model)
for (i in 1:nrow(SMR)){
  SMR$fitted[i] = exp(mean(to.estimate[i, ]))
  SMR$lower[i] = exp(quantile(to.estimate[i, ], probs=0.025))
  SMR$upper[i] = exp(quantile(to.estimate[i, ], probs=0.975))
}
# plot SMR with linear predictions over time
linear.plot = ggplot(data=SMR, aes(x=year, y=SMR)) + 
  geom_hline(yintercept = 1, lty=1, col='dark red')+ # reference line at SMR = 1
  geom_line(size=0.2, col=grey(0.4))+
  geom_point(col=grey(0.4), size=0.9)+
  geom_line(aes(x=year, y=fitted), col='blue', size=1.05)+
  geom_line(aes(x=year, y=lower), col='blue', lty=2, size=1.05)+
  geom_line(aes(x=year, y=upper), col='blue', lty=2, size=1.05)+
  xlab('Year')+
  ylab('Standardised mortality ratio')+
  theme_bw()+
  theme(panel.grid.minor = element_blank())
if(use.imputed==TRUE){ # added shaded area to highlight imputed imputed life-table years
  show.impute = filter(SMR, follow == 100, imputed=='Yes')
  linear.plot = linear.plot +
    geom_rect(data=show.impute, mapping=aes(xmin=year, xmax=year+1, ymin=-Inf, ymax=Inf), fill='lightgreen', col='transparent', alpha=0.25)
}
if(is.na(upper.y.limit) == FALSE){linear.plot = linear.plot +coord_cartesian(ylim=c(lower.y.limit, upper.y.limit))} # limit for Italy/UK because of one large positive outlier
# add explanatory labels for conference slide
conference = FALSE
if(conference == TRUE){
  labels = data.frame(year= 2016, SMR=c(0.7,1,1.3), colour=c(1,2,1), label=c('Politicians\nliving longer','Politicians equal\nto general public','Politicians\nliving shorter'), stringsAsFactors = FALSE)
  linear.plot = linear.plot +
    geom_label(data=labels, size=3.5, aes(x=year, y=SMR, label=label, col=factor(colour)), hjust=0, show.legend = FALSE)+
    scale_x_continuous(limits=c(1901, 2040), breaks=c(1900, 1920, 1940, 1960, 1980, 2000), minor_breaks=c(1900, 1920, 1940, 1960, 1980, 2000))+
    scale_color_manual('', values=c(grey(0.2),'dark red')) # label colours
}
if(weighted==FALSE){outfile = paste('figures/linear/SMR', country, '.linear.jpg', sep='')}
if(weighted==TRUE){outfile = paste('figures/linear/SMR', country, '.linear.weighted.jpg', sep='')}
jpeg(outfile, width=6, height=4, units='in', res=500, quality = 100)
print(linear.plot)
dev.off()
# store linear results
SMR$df = 1
results = bind_rows(results, SMR)

# save the estimates
if(weighted==FALSE){outfile = paste('results/Results', country, '.SMR.RData', sep='')}
if(weighted==TRUE){outfile = paste('results/Results', country, '.SMR.weighted.RData', sep='')}
save(results, file=outfile)

} # end of country loop

