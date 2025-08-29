library(tidyverse)
library(targets)

tar_load(stats)
tar_load()

test <- stats %>% mutate(lower = outputPar - se, upper = outputPar + se, sig = ifelse(lower > 0, T, F))

glimpse(test)

test %>%
  filter(!is.na(outputPar)) %>%
  filter(upper < 100) %>% # remove really high outliers. what do those mean?
  ggplot(aes(x = factor(carcID), y = outputPar, col = interaction(type, binwt)))+
  geom_point(aes(pch = sig))+
  geom_segment(aes(y = lower, yend = upper, linetype = sig))+
  scale_shape_manual(values = c(1, 19))+
  scale_linetype_manual(values = c(2, 1))+
  theme_classic()+
  coord_flip()


# Now we want carcass characteristics, such as number of arrivals, size of carcass, and year. Do we have those stored somewhere?