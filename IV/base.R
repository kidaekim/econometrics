library(haven)
library(ivreg)
library(tidyverse)
library(ggplot2)

df <- read_dta(file='./data/maketable4.dta')

# I have written this based on the .do file provided on 
# Prof. Acemoglu's website. 

df %<>% 
  filter(baseco == 1) %>% 
  mutate(other_cont = as.numeric(shortnam == 'AUS' | shortnam == 'ALT' | shortnam == 'NZL'))

# Reproduction of Table 4 in AJR(2001) paper
# ==========

# IV Regressions: Panels A and B
# ==========

# Columns 1-2: Base Sample
ivreg(logpgp95 ~ avexpr | logem4, data=df) %>% summary()
ivreg(logpgp95 ~ lat_abst + avexpr | lat_abst + logem4, data=df) %>% summary()

# Columns 3-4: Base Sample w/o Neo-Europes
ivreg(logpgp95 ~ avexpr | logem4, data=df %>% filter(rich4 != 1)) %>% summary()
ivreg(logpgp95 ~ lat_abst + avexpr | lat_abst + logem4, data=df%>% filter(rich4 != 1)) %>% 
  summary()

# Columns 5-6: Base Sample w/o Africa
ivreg(logpgp95 ~ avexpr | logem4, data=df %>% filter(africa != 1)) %>% summary()
ivreg(logpgp95 ~ lat_abst + avexpr | lat_abst + logem4, data=df%>% filter(africa != 1)) %>% 
  summary()

# Columns 7-8: Base Sample w/ Continent Dummies
ivreg(logpgp95 ~ avexpr + africa + asia + other_cont | 
        logem4 + africa + asia + other_cont, data=df) %>% summary()
ivreg(logpgp95 ~ lat_abst + avexpr + africa + asia + other_cont | 
        lat_abst + logem4 + africa + asia + other_cont, data=df) %>% summary()

# Column 9: Base Sample, log GDP per worker
ivreg(loghjypl ~ avexpr | logem4, data=df) %>% summary()

# OLS Regressions: Panel C
# ==========

# Columns 1-2: Base Sample
lm(logpgp95 ~ avexpr, data=df) %>% summary()
lm(logpgp95 ~ lat_abst + avexpr, data=df) %>% summary()

# Columns 3-4: Base Sample w/o Neo-Europes
lm(logpgp95 ~ avexpr, data=df %>% filter(rich4 != 1)) %>% summary()
lm(logpgp95 ~ lat_abst + avexpr, data=df%>% filter(rich4 != 1)) %>% summary()

# Columns 5-6: Base Sample w/o Africa
lm(logpgp95 ~ avexpr, data=df %>% filter(africa != 1)) %>% summary()
lm(logpgp95 ~ lat_abst + avexpr, data=df %>% filter(africa != 1)) %>% summary()

# Columns 7-8: Base Sample w/ Continent Dummies
lm(logpgp95 ~ avexpr + africa + asia + other_cont, data=df) %>% summary()
lm(logpgp95 ~ lat_abst + avexpr + africa + asia + other_cont, data=df) %>% summary()

# Column 9: Base Sample, log GDP per worker
lm(loghjypl ~ avexpr, data=df) %>% summary()


# Plot: Base Sample, log GDP ~ avexpr w/ and w/o an instrument
# ==========
iv_fit <- ivreg(loghjypl ~ avexpr | logem4, data=df)
ols_fit <- lm(loghjypl ~ avexpr, data=df)

ggplot(data=df %>% drop_na()) + 
  geom_point(aes(x=avexpr, y=loghjypl)) + 
  geom_line(aes(x=avexpr, y=iv_fit$fitted.values, colour='IV: 2SLS')) + 
  geom_line(aes(x=avexpr, y= ols_fit$fitted.values, colour='OLS')) +
  labs(x='Average Protection Against Expropriation', y='log(output per worker)',
       title='Fitted values obtained from OLS and 2SLS model',
       colour='') + 
  theme_classic()
