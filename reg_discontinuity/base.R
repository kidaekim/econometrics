# A few lines from rddtools package.
# I will get around this if I find a suitable data set.
# ==========
library(rddtools)

data(house)
rdd_house <- rdd_data(x=x, y=y, data=house, cutpoint=0)

dens_test(rdd_house)
rdd_bw_ik(rdd_house)

lm_mod <- rdd_reg_lm(rdd_house, order=1)
np_mod <- rdd_reg_np(rdd_house)

plot(lm_mod)
plot(np_mod)
