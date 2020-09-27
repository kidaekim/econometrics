library(tidyverse)
library(ggplot2)


# Data Prep
# =================
# I have used a few lines from Bruno Rodrigues' blog.
# https://www.brodrigues.co/blog/2019-04-28-diffindiff_part1/

codebook <- read_lines(file = './data/njmin/codebook')

variable_names <- codebook %>%
  `[`(8:59) %>%
  `[`(-c(5, 6, 13, 14, 32, 33)) %>%
  str_sub(1, 13) %>%
  str_squish() %>%
  str_to_lower()

dataset <- read_table2('./data/njmin/public.dat', col_names = FALSE)

dataset <- dataset %>%
  select(-X47) %>%
  `colnames<-`(., variable_names)

dataset <- dataset %>% 
  mutate_all(as.numeric) %>% 
  mutate(sheet = as.character(sheet))

# write.table(dataset, file = './data/njmin.csv', sep = '\t', row.names = F)


# Diff-in-Diff
# ====================
# I will not control for other variables as the original paper does.

tmp1 <- dataset %>% select(sheet, state, empft) %>% mutate(season=0)
tmp2 <- dataset %>% select(sheet, state, empft2) %>% mutate(season=1) %>% rename(empft=empft2)
merged_data <- bind_rows(tmp1, tmp2) %>% mutate(did = season * state) %>% 
  mutate_at(c('state', 'season', 'did'), as.factor)

didreg = lm(empft ~ state + season + did, data = merged_data)


# Plot
# ====================

merged_data$state <- factor(merged_data$state, labels=c('PA', 'NJ'))
  
ggplot(merged_data, aes(season, empft)) + 
  geom_boxplot() + 
  scale_x_discrete(labels=c('Before', 'After')) + 
  facet_wrap( ~ state, labeller=label_value) + 
  labs(x='Time Period', y='Full-time Employment',
       title='Employment Before and After minimum wage rise') +
  theme_bw()
