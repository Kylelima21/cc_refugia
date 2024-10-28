## Refugia Paper 2024/2025 Analysis
## Schoodic Institute, 2024

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#
library(tidyverse)
library(lubridate)
library(lme4)
library(ggeffects)




#------------------------------------------------#
####            Read in the data              ####
#------------------------------------------------#

## Read cleaned field data
rd <- read.csv("data/refugia_cleaned_data_20240911.csv")


## Read in site location
sloc <- read.csv("data/site_locations.csv") %>% 
  mutate(site.code = toupper(site.code)) %>% 
  select(-sampled.2023)


## Combine site data with the cleaned field data
## Add some variables for modelling
refdat <- as_tibble(left_join(rd, sloc, by = c("site.code"))) %>% 
  rename(latitude = latitude.x,
         longitude = longitude.x) %>% 
  select(-c(latitude.y, longitude.y)) %>% 
  mutate(survey.date = as.Date(survey.date),
         doy = yday(survey.date),
         prop.reprod.crow = num.cells.crowberry.reprod/num.cells.crowberry,
         prop.reprod.cinq = num.cells.cinquefoil.reprod/num.cells.cinquefoil, 
         prop.brown.crow = num.cells.crowberry.brleaves/num.cells.crowberry,
         prop.brown.cinq = num.cells.cinquefoil.brleaves/num.cells.cinquefoil)




#------------------------------------------------#
####             Create models                ####
#------------------------------------------------#

### Reproduction Proportion Models
## Filter to 2024 only because we didn't collect this data in year 1 (2023)
reprod <- refdat %>% 
  filter(year == 2024)
    

## Models are binomial regressions with weights for of total cells because 
## this data is proportion data (num cells reprod/num cells present)


## Crowberry reproduction model
crow.reprod <- glmer(prop.reprod.crow ~ refugia + (1 | site.code), family = binomial, 
                     data = reprod %>% filter(prop.reprod.crow <= 1.0), 
                     weights = num.cells.crowberry)
summary(crow.reprod)


## Cinquefoil reproduction model
cinq.reprod <- glmer(prop.reprod.cinq ~ refugia + (1 | site.code), family = binomial, 
                     data = reprod %>% filter(prop.reprod.cinq <= 1.0), 
                     weights = num.cells.cinquefoil)
summary(cinq.reprod)


#------------------------------------------------#

### Browning Proportion Models
## Same model structure and binomial regression for proportion data

## Crowberry browning model
crow.brown <- glmer(prop.brown.crow ~ refugia + (1 | site.code), family = binomial, 
                     data = refdat %>% filter(prop.brown.crow <= 1.0), 
                     weights = num.cells.crowberry)
summary(crow.brown)


## Crowbwerry browning model
cinq.brown <- glmer(prop.brown.cinq ~ refugia + (1 | site.code), family = binomial, 
                     data = refdat %>% filter(prop.brown.cinq <= 1.0), 
                     weights = num.cells.cinquefoil)
summary(cinq.brown)




#------------------------------------------------#
####               Plotting                   ####
#------------------------------------------------#


predcinq.rp <- ggpredict(cinq.reprod, terms = "refugia") %>% 
  as_tibble() %>% 
  mutate(metric = "cinquefoil\nreproduction")

predcrow.rp <- ggpredict(crow.reprod, terms = "refugia") %>% 
  as_tibble() %>% 
  mutate(metric = "crowberry\nreproduction")

predcinq.br <- ggpredict(cinq.brown, terms = "refugia") %>% 
  as_tibble() %>% 
  mutate(metric = "cinquefoil\nbrowning")

predcrow.br <- ggpredict(crow.brown, terms = "refugia") %>% 
  as_tibble() %>% 
  mutate(metric = "crowberry\nbrowning")

plotdat <- bind_rows(predcinq.rp, predcrow.rp, predcinq.br, predcrow.br) %>% 
  mutate(x = ifelse(x == "yes", "Refugia", "Non-refugia"))


ggplot(plotdat, aes(metric, predicted, shape = x)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), linewidth = 0.5,
                position = position_dodge(0.4), color = "gray30", width = 0.15) +
  geom_point(position = position_dodge(0.4), color = "black", size = 2.5) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "", y = "Proportion of cells") +
  #scale_shape_manual(values = c(21, 24)) +
  #scale_fill_manual(values = c("green", "orange")) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = c(0.12, 0.89),
        axis.text = element_text(color = "black", size = "12"),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title = element_text(color = "black", size = "12"),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm")),
        strip.text.x = element_text(margin = margin(.2, 0, .2, 0, "cm"), 
                                    color = "black", size = "12"), 
        strip.background = element_rect(color = "black", fill = "gray"),
        panel.background = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.border = element_rect(color = 'black', linewidth = 0.8, fill = NA)) 


ggsave("outputs/proportion_refugia_figure.png", height = 5, width = 7, dpi = 700)



