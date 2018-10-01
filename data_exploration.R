setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(tidyr)
library(ggplot2)

pdat <- read.csv("../data_raw/all_data.csv")

#### Counts, tarsus data (mean & SD) and regression coefficients for each species
dat.1 <- pdat %>% 
  group_by(code) %>%
  summarise(count = length(code),
            mean_min = mean(min_t), sd_min = sd(min_t),
            mean_max = mean(max_t), sd_max = sd(max_t),
            r = cor(min_t, max_t))

pdat %>% filter(code == "GRETI") %>% slice(which.max(max_t)) # Removing GRETI outlier identified a-priori
pdat <- pdat[!(pdat$code == "GRETI" & pdat$ring_no == "Y638208"),]

pdat %>% filter(code %in% c("BLUTI", "GRETI", "CHAFF", "ROBIN")) %>% # Plot focal species
  ggplot(., aes(min_t, max_t, colour=factor(code))) + 
  stat_smooth(method=lm, fullrange=FALSE) + 
  geom_point() +
  facet_wrap( ~ code,  ncol=2)

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
