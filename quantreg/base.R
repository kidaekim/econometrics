library(quantreg)
library(ggplot2)
library(gbm)
library(tidyverse)

# Load the birthweight data 
# This is from Prof. Roger Koenker's Introduction to Qunatile Regression paper. 
# ==========
load(file='./data/birthweight.rda')

rq_fit <- rq(weight * 1000 ~ boy + married + mom.age + m.wtgain, 
             tau=seq(0.05, 0.95, 0.05), data=d)

ols_fit <- lm(weight * 1000 ~ boy + married + mom.age + m.wtgain, data=d)

rq_fit_out <- summary(rq_fit, se='nid')

mom.age_est <- sapply(1:19, function(index){
  rq_fit_out[[index]]$coefficients[4, 1]
  })

mom.age_error <- sapply(1:19, function(index){
  rq_fit_out[[index]]$coefficients[4, 2]
})

df = data.frame(mom.age_est, mom.age_error, tau=seq(0.05, 0.95, 0.05))

# This plot demonstrates how the coefficient varies depending on tau. 
# Also reproducible with plot(rq_fit_out).

ggplot(data=df) +
  geom_line(aes(x=tau, y=mom.age_est)) + 
  geom_line(aes(x=tau, y=mom.age_est + mom.age_error), linetype='dashed') + 
  geom_line(aes(x=tau, y=mom.age_est - mom.age_error), linetype='dashed') + 
  geom_line(aes(x=tau, y=summary(ols_fit)$coefficients[4, 1]), colour='red') +
  geom_line(aes(x=tau, y=summary(ols_fit)$coefficients[4, 1] + 
              summary(ols_fit)$coefficients[4, 2]), colour='red', linetype='dashed') +
  geom_line(aes(x=tau, y=summary(ols_fit)$coefficients[4, 1] - 
              summary(ols_fit)$coefficients[4, 2]), colour='red', linetype='dashed') + 
  labs(x='Tau', y='Coefficient of mom\'s age variable',
       title='Quantile regression and OLS estimate on the mom\'s age') + 
  theme_classic()


# Gradient boosting with quantile loss
# ==========
# Quantile Regression also can be used in the case of gradient boosting.
# With the qunatile loss, one can recover the robust predictions
# which are free from distribution assumptions.

gbm_fit1 <- gbm(weight * 1000 ~ boy + married + mom.age + m.wtgain,
               distribution=list(name='quantile', alpha=0.05),
               data=d)

gbm_fit2 <- gbm(weight * 1000 ~ boy + married + mom.age + m.wtgain,
               distribution=list(name='quantile', alpha=0.5),
               data=d)

gbm_fit3 <- gbm(weight * 1000 ~ boy + married + mom.age + m.wtgain,
               distribution=list(name='quantile', alpha=0.95),
               data=d)

gbm_df <- data.frame(weight = d$weight * 1000, 
                     m.wtgain = d$m.wtgain, 
                     fit1 = gbm_fit1$fit, fit2=gbm_fit2$fit, fit3=gbm_fit3$fit)

gbm_df %<>% sample_frac(0.05) %>% arrange(m.wtgain)

# Plot predictions based on qunatile loss.
ggplot(data=gbm_df) +
  geom_point(aes(m.wtgain, weight, colour='Data')) + 
  geom_point(aes(m.wtgain, fit1, colour='5% qunatile prediction')) + 
  geom_point(aes(m.wtgain, fit3, colour='95% qunatile prediction')) + 
  labs(x='mother\'s weight gain(pounds)', y='birthweight(grams)',
       title='5% and 95% qunatile prediction based on the gradient boosting model') + 
  theme_classic()
