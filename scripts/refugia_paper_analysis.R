## Refugia Paper 2024/2025 Analysis
## Schoodic Institute, 2024

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#
library(tidyverse)
library(lubridate)
library(lme4)
library(ggeffects)
library(glmmTMB)
library(DHARMa)


overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}




#------------------------------------------------#
####         Read and clean the data          ####
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
  filter(site != "schoodic_mountain") %>% 
  rename(latitude = latitude.x,
         longitude = longitude.x) %>% 
  select(-c(latitude.y, longitude.y)) %>% 
  mutate(survey.date = as.Date(survey.date),
         doy = yday(survey.date),
         refugia = ifelse(refugia == "yes", 1, 0),
         prop.reprod.crow = num.cells.crowberry.reprod/num.cells.crowberry,
         prop.reprod.cinq = num.cells.cinquefoil.reprod/num.cells.cinquefoil, 
         prop.brown.crow = num.cells.crowberry.brleaves/num.cells.crowberry,
         prop.brown.cinq = num.cells.cinquefoil.brleaves/num.cells.cinquefoil,
         prop.fruit.crow = num.cells.crowberry.fruits/num.cells.crowberry,
         prop.flower.cinq = num.cells.cinquefoil.flowers/num.cells.cinquefoil)

refdat2 <- as_tibble(left_join(rd, sloc, by = c("site.code"))) %>% 
  filter(year == 2024)




#------------------------------------------------#
####             Create models                ####
#------------------------------------------------#

### ├ Crowberry Browning Model  ------------------
## Create dataset
crbrown <- refdat %>% 
  select(site.code, refugia, survey.date, year, doy, num.cells.crowberry, 
         num.cells.crowberry.brleaves, prop.brown.crow) %>% 
  filter(prop.brown.crow <= 1) %>%
  mutate(neg.brown.crow = num.cells.crowberry - num.cells.crowberry.brleaves)


## Make glmer model with random effect of site 
crow.brown <- glmer(cbind(num.cells.crowberry.brleaves, neg.brown.crow) ~ 
                      refugia + factor(year) + (1 | site.code), 
                    family = binomial, data = crbrown)

sim.brown.1 = simulateResiduals(crow.brown, re.form = NULL)
testDispersion(sim.brown.1) # overdispersed


## To handle overdispersion, use beta-binomial model instead
crow.brown.bb <- glmmTMB(cbind(num.cells.crowberry.brleaves, neg.brown.crow) ~ 
                           refugia + factor(year) + doy + (1 | site.code),
                         family = betabinomial(link = "logit"), data = crbrown)
summary(crow.brown.bb)


## Checking model fit
sim.brown.crow = simulateResiduals(crow.brown.bb, re.form = NULL)
plot(sim.brown.crow)
testDispersion(sim.brown.crow)
testZeroInflation(sim.brown.crow)



#------------------------------------------------#


### ├ Cinquefoil Browning Model  -----------------
## Create dataset
cibrown <- refdat %>% 
  select(site.code, refugia, survey.date, year, doy, num.cells.cinquefoil, 
         num.cells.cinquefoil.brleaves, prop.brown.cinq) %>% 
  filter(prop.brown.cinq <= 1) %>%
  mutate(neg.brown.cinq = num.cells.cinquefoil - num.cells.cinquefoil.brleaves)


## Make glmer model with random effect of site 
cinq.brown <- glmer(cbind(num.cells.cinquefoil.brleaves, neg.brown.cinq) ~ refugia + factor(year) +  (1 | site.code), 
                    family = binomial, data = cibrown)

sim.brown.1 = simulateResiduals(cinq.brown, re.form = NULL)
testDispersion(sim.brown.1) # overdispersed


## To handle overdispersion, use beta-binomial model instead
cinq.brown.bb <- glmmTMB(cbind(num.cells.cinquefoil.brleaves, neg.brown.cinq) ~ 
                           refugia + factor(year) + doy + (1 | site.code),
                         family = betabinomial(link = "logit"), data = cibrown)
summary(cinq.brown.bb)


## Checking model fit
sim.brown.cinq = simulateResiduals(cinq.brown.bb, re.form = NULL)
plot(sim.brown.cinq)
testDispersion(sim.brown.cinq)
testZeroInflation(sim.brown.cinq)



#------------------------------------------------#


### ├ Crowberry Reproduction Model  --------------
## Create dataset
crreprod <- refdat %>% 
  filter(year == 2024) %>% 
  select(site.code, refugia, survey.date, year, doy, num.cells.crowberry, 
         num.cells.crowberry.reprod, prop.reprod.crow) %>% 
  filter(prop.reprod.crow <= 1) %>%
  mutate(neg.reprod.crow = num.cells.crowberry - num.cells.crowberry.reprod)


## Make glmer model with random effect of site 
crow.reprod <- glmer(cbind(num.cells.crowberry.reprod, neg.reprod.crow) ~ refugia + (1 | site.code), 
                    family = binomial, data = crreprod)

sim.brown.1 = simulateResiduals(crow.reprod, re.form = NULL)
testDispersion(sim.brown.1) # overdispersed


## To handle overdispersion, use beta-binomial model instead
crow.reprod.bb <- glmmTMB(cbind(num.cells.crowberry.reprod, neg.reprod.crow) ~ 
                            refugia + doy + (1 | site.code),
                         family = betabinomial(link = "logit"), data = crreprod)
summary(crow.reprod.bb)


## Checking model fit
sim.reprod.crow = simulateResiduals(crow.reprod.bb, re.form = NULL)
plot(sim.reprod.crow)
testDispersion(sim.reprod.crow)
testZeroInflation(sim.reprod.crow)



#------------------------------------------------#


### ├ Cinquefoil Reproduction Model  -------------
## Create dataset
cireprod <- refdat %>%
  filter(year == 2024) %>% 
  select(site.code, refugia, survey.date, year, doy, num.cells.cinquefoil, 
         num.cells.cinquefoil.reprod, prop.reprod.cinq) %>% 
  filter(prop.reprod.cinq <= 1) %>%
  mutate(neg.reprod.cinq = num.cells.cinquefoil - num.cells.cinquefoil.reprod)


## Make glmer model with random effect of site 
cinq.reprod <- glmer(cbind(num.cells.cinquefoil.reprod, neg.reprod.cinq) ~ refugia + (1 | site.code), 
                     family = binomial, data = cireprod)

sim.brown.1 = simulateResiduals(cinq.reprod, re.form = NULL)
testDispersion(sim.brown.1) # overdispersed


## To handle overdispersion, use beta-binomial model instead
cinq.reprod.bb <- glmmTMB(cbind(num.cells.cinquefoil.reprod, neg.reprod.cinq) ~ 
                            refugia + doy + (1 | site.code),
                          family = betabinomial(link = "logit"), data = cireprod)
summary(cinq.reprod.bb)


## Checking model fit
sim.reprod.cinq = simulateResiduals(cinq.reprod.bb, re.form = NULL)
plot(sim.reprod.cinq)
testDispersion(sim.reprod.cinq)
testZeroInflation(sim.reprod.cinq)



#------------------------------------------------#


### ├ Crowberry Fruiting Model  --------------
## Create dataset
crfruit <- refdat %>% 
  select(site.code, refugia, survey.date, year, doy, num.cells.crowberry, 
         num.cells.crowberry.fruits, prop.fruit.crow) %>% 
  filter(prop.fruit.crow <= 1) %>%
  mutate(neg.fruit.crow = num.cells.crowberry - num.cells.crowberry.fruits)


## Make glmer model with random effect of site 
crow.fruits <- glmer(cbind(num.cells.crowberry.fruits, neg.fruit.crow) ~ 
                       refugia + factor(year) + (1 | site.code), 
                     family = binomial, data = crfruit)

sim.brown.1 = simulateResiduals(crow.fruits, re.form = NULL)
testDispersion(sim.brown.1) # overdispersed


## To handle overdispersion, use beta-binomial model instead
crow.fruit.bb <- glmmTMB(cbind(num.cells.crowberry.fruits, neg.fruit.crow) ~ 
                            refugia + factor(year) + doy + (1 | site.code),
                          family = betabinomial(link = "logit"), data = crfruit)
summary(crow.fruit.bb)


## Checking model fit
sim.fruits = simulateResiduals(crow.fruit.bb, re.form = NULL)
plot(sim.fruits)
testDispersion(sim.fruits)
testZeroInflation(sim.fruits)



#------------------------------------------------#


### ├ Cinquefoil Flowering Model  -------------
## Create dataset
ciflower <- refdat %>%
  select(site.code, refugia, survey.date, year, doy, num.cells.cinquefoil, 
         num.cells.cinquefoil.flowers, prop.flower.cinq) %>% 
  filter(prop.flower.cinq <= 1) %>%
  mutate(neg.flower.cinq = num.cells.cinquefoil - num.cells.cinquefoil.flowers)


## Make glmer model with random effect of site 
cinq.flow <- glmer(cbind(num.cells.cinquefoil.flowers, neg.flower.cinq) ~ 
                     refugia + factor(year) + (1 | site.code), 
                     family = binomial, data = ciflower)

sim.brown.1 = simulateResiduals(cinq.flow, re.form = NULL)
testDispersion(sim.brown.1) # overdispersed


## To handle overdispersion, use beta-binomial model instead
cinq.flower.bb <- glmmTMB(cbind(num.cells.cinquefoil.flowers, neg.flower.cinq) ~ 
                            refugia + factor(year) + doy + (1 | site.code),
                          family = betabinomial(link = "logit"), data = ciflower)
summary(cinq.flower.bb)


## Checking model fit
sim.flow = simulateResiduals(cinq.flower.bb, re.form = NULL)
plot(sim.flow)
testDispersion(sim.flow)
testZeroInflation(sim.flow)




#------------------------------------------------#
####               Plotting                   ####
#------------------------------------------------#


predcinq.br <- ggpredict(cinq.brown.bb, terms = "refugia") %>% 
  as_tibble() %>% 
  mutate(metric = "cinquefoil\nbrowning")

predcinq.rp <- ggpredict(cinq.flower.bb, terms = "refugia") %>% 
  as_tibble() %>% 
  mutate(metric = "cinquefoil\nflowers")

predcrow.br <- ggpredict(crow.brown.bb, terms = "refugia") %>% 
  as_tibble() %>% 
  mutate(metric = "crowberry\nbrowning")

predcrow.rp <- ggpredict(crow.fruit.bb, terms = "refugia") %>% 
  as_tibble() %>% 
  mutate(metric = "crowberry\nfruits")



plotdat <- bind_rows(predcinq.rp, predcrow.rp, predcinq.br, predcrow.br) %>% 
  mutate(x = factor(ifelse(x == 1, "Refugia", "Non-refugia"), 
                    levels = c("Refugia", "Non-refugia")))


ggplot(plotdat, aes(metric, predicted, shape = x)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), linewidth = 0.5,
                position = position_dodge(0.4), color = "gray30", width = 0.15) +
  geom_point(position = position_dodge(0.4), color = "black", size = 2) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "", y = "Proportion of cells") +
  scale_shape_manual(values = c(16, 15)) +
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





#------------------------------------------------#
####      Quick analysis for EW report        ####
#------------------------------------------------#

prelim <- plotdat %>% 
  filter(metric == "cinquefoil\nbrowning" | metric == "crowberry\nbrowning") %>% 
  mutate(metric = ifelse(metric == "cinquefoil\nbrowning", "cinquefoil", metric)) %>% 
  mutate(metric = ifelse(metric == "crowberry\nbrowning", "crowberry", metric))

ggplot(prelim, aes(metric, predicted, shape = x)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), linewidth = 0.5,
                position = position_dodge(0.3), color = "gray30", width = 0.1) +
  geom_point(position = position_dodge(0.3), color = "black", size = 1.5) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "", y = "Proportion with browning") +
  scale_shape_manual(values = c(16, 15)) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = c(0.16, 0.89),
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


ggsave("outputs/EW_report_figure.png", height = 4, width = 4.5, dpi = 700)







#------------------------------------------------#
####                 EXTRA                    ####
#------------------------------------------------#


# refdat %>% 
#   ggplot(aes(x = doy, y = prop.reprod.cinq)) + 
#   geom_point()
# 
# 
# refdat %>% 
#   ggplot(aes(x = doy, y = prop.reprod.crow)) + 
#   geom_point()



# ## Run simplified quasi-binomial model instead because the
# ## beta-binomial model did not fix overdispersion
# crow.brown.qb <- glm(cbind(num.cells.crowberry.brleaves, neg.brown.crow) ~ refugia + site.code,
#                      family = quasibinomial, data = crbrown)
# summary(crow.brown.qb)




# library(AED)
# Tbdeer <- read_delim("data/Tbdeer.txt")
# 
# Z <- cbind(Tbdeer$OpenLand, Tbdeer$ScrubLand,
#            Tbdeer$QuercusPlants, Tbdeer$QuercusTrees,
#            Tbdeer$ReedDeerIndex, Tbdeer$EstateSize,
#            Tbdeer$Fenced)
# corvif(Z)
# DeerNegCervi <- Tbdeer$DeerSampledCervi - Tbdeer$DeerPosCervi
# Tbdeer$fFenced <- factor(Tbdeer$Fenced)
# Deer1 <- glm(cbind(Tbdeer$DeerPosCervi, DeerNegCervi) ~ OpenLand + ScrubLand + 
#                QuercusPlants + QuercusTrees + ReedDeerIndex + EstateSize + 
#                fFenced, family = binomial, data = Tbdeer)
# summary(Deer1)
# 
# Tbdeer$DeerPosProp <- Tbdeer$DeerPosCervi / Tbdeer$DeerSampledCervi
# Deer2 <- glm(DeerPosProp ~ OpenLand + ScrubLand + QuercusPlants + QuercusTrees +
#                ReedDeerIndex + EstateSize + fFenced, family = binomial, data = Tbdeer, weights = DeerSampledCervi)
# summary(Deer2)
# 
# Deer3 <- glm(cbind(Tbdeer$DeerPosCervi, DeerNegCervi) ~ OpenLand + ScrubLand + QuercusPlants + QuercusTrees +
#                ReedDeerIndex + EstateSize + fFenced, family = quasibinomial, data = Tbdeer)
# summary(Deer3)
# drop1(Deer3, test = "F")
# 
# Deer4 <- glm(cbind(DeerPosCervi,DeerNegCervi) ~ OpenLand, data = Tbdeer,
#              family = quasibinomial)
# summary(Deer4)
# drop1(Deer4, test = "F")
# 
# overdisp_fun(Deer2)


#------------------------------------------------#

### Reproduction Proportion Models
## Filter to 2024 only because we didn't collect this data in year 1 (2023)
reprod <- refdat %>% 
  filter(year == 2024)
    

## Models are binomial regressions with weights for of total cells because 
## this data is proportion data (num cells reprod/num cells present)


## Crowberry reproduction model
# crow.reprod <- glmer(prop.reprod.crow ~ refugia + (1 | site.code), family = binomial, 
#                      data = reprod %>% filter(prop.reprod.crow <= 1.0), 
#                      weights = num.cells.crowberry)
# overdisp_fun(crow.reprod) # highly overdispersed
# summary(crow.reprod)

crow.reprod <- glm(prop.reprod.crow ~ refugia, family = quasibinomial, 
                     data = reprod %>% filter(prop.reprod.crow <= 1.0))
overdisp_fun(crow.reprod) # highly overdispersed
summary(crow.reprod)


## Cinquefoil reproduction model
# cinq.reprod <- glmer(prop.reprod.cinq ~ refugia + (1 | site.code), family = binomial, 
#                      data = reprod %>% filter(prop.reprod.cinq <= 1.0), 
#                      weights = num.cells.cinquefoil)
# overdisp_fun(cinq.reprod) # highly overdispersed
# summary(cinq.reprod)

cinq.reprod <- glm(prop.reprod.cinq ~ refugia, family = quasibinomial, 
                   data = reprod %>% filter(prop.reprod.cinq <= 1.0))
overdisp_fun(cinq.reprod)
summary(cinq.reprod)



#------------------------------------------------#

### Looking at just fruits for crowberry and flowers for cinquefoil

## Crowberry fruits model
crow.fruits <- glmer(prop.fruit.crow ~ refugia + (1 | site.code), family = binomial,
                     data = refdat %>% filter(prop.fruit.crow <= 1.0),
                     weights = num.cells.crowberry)
overdisp_fun(crow.fruits) # highly overdispersed
summary(crow.fruits)


crow.fruits.qb <- glm(prop.fruit.crow ~ refugia + year, family = quasibinomial, 
                   data = refdat %>% filter(prop.fruit.crow <= 1.0))
overdisp_fun(crow.fruits.qb)
summary(crow.fruits.qb)

drop1(crow.fruits.qb, test = "F")

## Cinquefoil flowers model
cinq.flowers <- glmer(prop.flower.cinq ~ refugia + (1 | site.code), family = binomial,
                     data = reprod %>% filter(prop.reprod.cinq <= 1.0),
                     weights = num.cells.cinquefoil)
overdisp_fun(cinq.flowers) # highly overdispersed
summary(cinq.flowers)

cinq.reprod <- glm(prop.reprod.cinq ~ refugia, family = quasibinomial, 
                   data = reprod %>% filter(prop.reprod.cinq <= 1.0))
overdisp_fun(cinq.reprod)
summary(cinq.reprod)



#------------------------------------------------#

### Browning Proportion Models
## Same model structure and binomial regression for proportion data
# 
# test <- refdat %>% 
#   filter(prop.brown.crow <= 1.0) %>% 
#   mutate(neg.brown.crowberry = num.cells.crowberry - num.cells.crowberry.brleaves)


## Crowberry browning model
crow.brown <- glmer(cbind(num.cells.crowberry.brleaves, neg.brown.crow) ~ refugia + (1 | site.code), family = binomial,
                    data = refdat %>% filter(prop.brown.crow <= 1.0),
                    weights = num.cells.crowberry)
overdisp_fun(crow.brown) # highly over-dispersed
summary(crow.brown)


crow.brown.bb <- glmmTMB(cbind(num.cells.crowberry.brleaves, neg.brown.crow) ~ refugia + (1 | site.code),
        family = betabinomial(link = "logit"),
        data = refdat %>% filter(prop.brown.crow <= 1.0))
overdisp_fun(crow.brown.bb) # still highly over-dispersed
summary(crow.brown.bb)


crow.brown.qb <- glm(cbind(num.cells.crowberry.brleaves, neg.brown.crow) ~ refugia, 
                     family = quasibinomial, 
                     data = refdat %>% filter(prop.brown.crow <= 1.0))
overdisp_fun(crow.brown.qb)
summary(crow.brown.qb)




## Crowbwerry browning model
# cinq.brown <- glmer(prop.brown.cinq ~ refugia + (1 | site.code), family = binomial, 
#                     data = refdat %>% filter(prop.brown.cinq <= 1.0), 
#                     weights = num.cells.cinquefoil)
# overdisp_fun(cinq.brown) # highly overdispersed
# summary(cinq.brown)

cinq.brown <- glm(prop.brown.cinq ~ refugia + year, family = quasibinomial, 
                  data = refdat %>% filter(prop.brown.cinq <= 1.0))
overdisp_fun(cinq.brown)
summary(cinq.brown)




