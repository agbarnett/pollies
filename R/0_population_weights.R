# 0_population_weights.R
# population weights for weighted SMR
# weights from https://www.who.int/healthinfo/paper31.pdf
# February 2020
library(dplyr)
library(stringr)

weights = read.table(sep=' ', header=TRUE, stringsAsFactors = FALSE, text='
age_range segi scandi who
0-4 12.00 8.00 8.86
5-9 10.00 7.00 8.69
10-14 9.00 7.00 8.60
15-19 9.00 7.00 8.47
20-24 8.00 7.00 8.22
25-29 8.00 7.00 7.93
30-34 6.00 7.00 7.61
35-39 6.00 7.00 7.15
40-44 6.00 7.00 6.59
45-49 6.00 7.00 6.04
50-54 5.00 7.00 5.37
55-59 4.00 6.00 4.55
60-64 4.00 5.00 3.72
65-69 3.00 4.00 2.96
70-74 2.00 3.00 2.21
75-79 1.00 2.00 1.52
80-84 0.50 1.00 0.91
85-110 0.50 1.00 0.63') %>%
  select(age_range, who) %>%
  mutate(lower=NA, upper=NA)
# loop to get age range
for (k in 1:nrow(weights)){
  weights$lower[k] = as.numeric(str_split(weights$age_range[k], pattern='-')[[1]][1])
  weights$upper[k] = as.numeric(str_split(weights$age_range[k], pattern='-')[[1]][2])
}


# add weight groups
weights = mutate(weights,
                 age = lower, # temporary
                                 age.group = case_when(age<=19 ~ 1,
                                                         age>19 & age<= 24 ~ 2,
                                                         age>24 & age<= 29 ~ 3,
                                                         age>29 & age<= 34 ~ 4,
                                                         age>34 & age<= 39 ~ 5,
                                                         age>39 & age<= 44 ~ 6,
                                                         age>44 & age<= 49 ~ 7,
                                                         age>49 & age<= 54 ~ 8,
                                                         age>54 & age<= 59 ~ 9,
                                                         age>59 & age<= 64 ~ 10,
                                                         age>64 & age<= 69 ~ 11,
                                                         age>69 & age<= 74 ~ 12,
                                                         age>74 & age<= 79 ~ 13,
                                                         age>79 & age<= 84 ~ 14,
                                                         age>84 ~ 15)) %>%
  filter(lower> 10) %>% # remove kids
  select(-age, -lower, -upper, -age_range) %>%
  rename('weight' = 'who')

pop.weights = weights # rename
remove(weights) # tidy up
