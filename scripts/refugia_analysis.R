## Analysis of the Refugia data
## Schoodic Institute, 2023

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#

library(tidyverse)
library(lubridate)
library(lme4)



#------------------------------------------------#
####             Read and clean               ####
#------------------------------------------------#

rd <- read.csv("data/refugia_cleaned_data_20231201.csv")
sloc <- read.csv("data/site_locations.csv") %>% 
  mutate(site.code = toupper(site.code))


refdat <- as_tibble(left_join(rd, sloc, by = c("site.code"))) %>% 
  rename(latitude = latitude.x,
         longitude = longitude.x) %>% 
  select(-c(latitude.y, longitude.y)) %>% 
  mutate(survey.date = as.Date(survey.date))
  
  
  



#------------------------------------------------#
####            Exploratory data              ####
#------------------------------------------------#

refdat %>% 
  select(site.code, survey.date) %>% 
  group_by(site.code, survey.date) %>% 
  unique() %>% 
  arrange(survey.date)



#------------------------------------------------#
####            Prelim Analysis               ####
#------------------------------------------------#

pan <- refdat %>% 
  mutate(porp.cinq = num.cells.cinquefoil.flowers / num.cells.cinquefoil,
         porp.crow = num.cells.crowberry.flowers / num.cells.crowberry)


## SHOULD ADD DATE AS FIXED EFFECT - but in the current data it is too highly
## confounded with site
glmer(porp.cinq ~ refugia + (1 | site.code), family = binomial, data = pan)
glmer(porp.crow ~ refugia + (1 | site.code), family = binomial, data = pan)








