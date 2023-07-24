## Script to clean the raw data exported from the refugia Survey123 data collection
## Schoodic Institute, 2023

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#

library(tidyverse)
library(lubridate)


#------------------------------------------------#
####             Read and clean               ####
#------------------------------------------------#

cleandat <- tibble(read.csv("data/refugia_export_20230716.csv")) %>%
  rename_with(., ~tolower(.)) %>%
  select(-c(objectid, creationdate:editor, survey.point.id)) %>% 
  rename(global.id = globalid,
         start.time = survey.start.time,
         group.size = how.many.people.are.in.your.group.,
         crowberry.present = is.black.crowberry.present.,
         num.cells.crowberry = how.many.of.the.grid.cells.contain.crowberry.,
         num.cells.crowberry.flowers = how.many.of.the.grid.cells.contain.crowberry.flowers.,
         num.cells.crowberry.fruits = how.many.of.the.grid.cells.contain.crowberry.fruits.,
         num.cells.crowberry.brleaves = how.many.of.the.grid.cells.contain.brown.crowberry.leaves.,
         cinquefoil.present = is.three.toothed.cinquefoil.present.,
         num.cells.cinquefoil = how.many.of.the.grid.cells.contain.cinquefoil.,
         num.cells.cinquefoil.flowers = how.many.of.the.grid.cells.contain.cinquefoil.flowers.,
         num.cells.cinquefoil.fruits = how.many.of.the.grid.cells.contain.cinquefoil.fruits.,
         num.cells.cinquefoil.brleaves = how.many.of.the.grid.cells.contain.brown.cinquefoil.leaves.,
         end.time = survey.end.time,
         crowberry.inat = did.you.take.a.picture.of.crowberry.and.upload.it.to.inaturalist.,
         cinquefoil.inat = did.you.take.a.picture.of.cinquefoil.and.upload.it.to.inaturalist.,
         notes = notes.,
         longitude = x,
         latitude = y) %>% 
  mutate(survey.date = str_remove(survey.date, "\\s\\d\\:\\d*\\:\\d*\\s\\w*$"),
         survey.date = as.Date(survey.date, format = "%m/%d/%Y"),
         year = as.integer(year(survey.date)),
         group.size = as.numeric(group.size),
         crowberry.present = tolower(crowberry.present),
         cinquefoil.present = tolower(cinquefoil.present),
         site.code = toupper(site.code),
         site.code = str_replace(site.code, "BMSI", "BMIS"),
         site.code = str_replace(site.code, "MDIS", "BMIS"),
         site.code = str_replace(site.code, "CABZ", "CABA"),
         site.code = str_replace(site.code, "CABO", "CABA"),
         grid.cell.number = as.integer(grid.cell.number)) %>% 
  select(site.code, grid.cell.number, latitude, longitude, survey.date, year, 
         start.time, end.time, group.size:notes, global.id) %>% 
  arrange(site.code, survey.date, start.time)




#------------------------------------------------#
####                  QA/QC                   ####
#------------------------------------------------#

## Ensure no duplicate grid cell numbers
cleandat %>% 
  distinct(grid.cell.number)

## Check for erroneous site.code entries
cleandat %>% 
  distinct(site.code)



#------------------------------------------------#
####              Summary Stats               ####
#------------------------------------------------#

### Number of grid cells sampled
## Total
n_distinct(cleandat$grid.cell.number)

## BMIS
bmiscells <- cleandat %>% 
  filter(site.code == "BMIS")
n_distinct(bmiscells$grid.cell.number)

## CABA
cabacells <- cleandat %>% 
  filter(site.code == "CABA")
n_distinct(cabacells$grid.cell.number)


## Average duration of survey
cleandat %>% 
  mutate(start.time = as.numeric(str_remove(start.time, "\\:")),
         end.time = as.numeric(str_remove(end.time, "\\:")),
         avg.sl = end.time - start.time) %>% 
  select(avg.sl) %>% 
  summarise(mean = mean(avg.sl))


### Number of cells with crowberry
## In refugia site
cbr <- cleandat %>% 
  filter(site.code == "BMIS")
table(cbr$crowberry.present)

## in non-refugia site
cbn <- cleandat %>% 
  filter(site.code == "CABA")
table(cbn$crowberry.present)

## Number of cells with cinquefoil
## In refugia site
cnr <- cleandat %>% 
  filter(site.code == "BMIS")
table(cnr$cinquefoil.present)

## in non-refugia site
cnn <- cleandat %>% 
  filter(site.code == "CABA")
table(cnn$cinquefoil.present)








