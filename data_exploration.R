setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(tidyr)
library(ggplot2)

pdat <- read.csv("exploration_app/data/all_data.csv")

#### Counts, tarsus data (mean & SD) and regression coefficients for each species
pdat %>% filter(code == "GRETI") %>% slice(which.max(max_t)) # Removing GRETI outliers identified a-priori
pdat %>% filter(code == "GRETI") %>% slice(which.min(max_t))
pdat <- pdat[!(pdat$code == "GRETI" & pdat$ring_no == "Y638208"),]
pdat <- pdat[!(pdat$code == "GRETI" & pdat$ring_no == "AFB9885"),]

dat.1 <- pdat %>% 
  group_by(code) %>%
  summarise(count = length(code),
            mean_min = mean(min_t), sd_min = sd(min_t),
            mean_max = mean(max_t), sd_max = sd(max_t),
            r = cor(min_t, max_t))
pdat %>% 
  filter(code %in% c("BLABI", "BLUTI", "CHAFF", "GRETI", "PIEFL", "ROBIN")) %>% # Plot focal species
  group_by(code) %>% # manipulating the code to get counts per species for facet headers
  mutate(code_count = n()) %>%
  mutate(cor_coef = round(cor(min_t, max_t),2)) %>% # Add correlation to head
  ungroup() %>%
  mutate(code_updated = paste0(code, "; n=", code_count, "; R=", cor_coef)) %>%
  ggplot(., aes(min_t, max_t, colour=factor(code))) + 
  stat_smooth(method=lm, fullrange=FALSE) + 
  geom_point() +
  facet_wrap( ~ code_updated,  ncol=2, scales = "free") 

mtcars %>% 
  group_by(carb) %>%
  mutate(carb_count = n()) %>%
  ungroup() %>%
  mutate(carb_updated = paste0(carb, "; n=", carb_count)) %>%
  ggplot(aes(x = cyl)) + geom_bar()+
  facet_wrap(~carb_updated)

#### As above, but also split by sex
dat.2 <- pdat %>% 
  group_by(code, sex) %>%
  summarise(count = length(code),
            mean_min = mean(min_t), sd_min = sd(min_t),
            mean_max = mean(max_t), sd_max = sd(max_t),
            r = cor(min_t, max_t))

#### As in the first instance, but also split by age
dat.3 <- pdat %>% 
  group_by(code, age) %>%
  summarise(count = length(code),
            mean_min = mean(min_t), sd_min = sd(min_t),
            mean_max = mean(max_t), sd_max = sd(max_t),
            r = cor(min_t, max_t))

#### Count per species, per ringer
dat.r <- pdat %>% 
  group_by(code, ringer) %>%
  summarise(count = length(code)) %>%
  spread(ringer,count) %>%
  replace(., is.na(.), 0)
