## Script to clean the raw data exported from the refugia Survey123 data collection
## Schoodic Institute, 2023

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#

library(tidyverse)
library(lubridate)
library(ggmap)


#------------------------------------------------#
####             Read and clean               ####
#------------------------------------------------#

rdata <- tibble(read.csv("data/refugia_export_20230918.csv"))
  
  
cleandata <- rdata %>% 
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
         site.code = str_replace(site.code, "SEMO", "SCMO"),
         site.code = str_replace(site.code, "SCHD", "SCHE"),
         site.code = str_replace(site.code, "SCHA", "SHHA"),
         site.code = str_replace(site.code, "SHHE", "SHHA"),
         site.code = str_replace(site.code, "SSHE", "SHHA"),
         site.code = str_replace(site.code, "BLHL", "BLHI"),
         grid.cell.number = as.integer(grid.cell.number)) %>% 
  select(site.code, grid.cell.number, latitude, longitude, survey.date, year, 
         start.time, end.time, group.size:notes, global.id) %>% 
  arrange(site.code, survey.date, start.time) %>%
  mutate(grid.cell.number = ifelse(global.id == "6ba77df0-86e8-4089-87e9-b9bdf108dd1a",
                                   46, grid.cell.number))




#------------------------------------------------#
####                  QA/QC                   ####
#------------------------------------------------#

## Ensure no duplicate grid cell numbers
text <- cleandata %>% 
  group_by(site.code) %>% 
  distinct(.$grid.cell.number)

cleandata %>% 
  filter(site.code == "BEMO") %>% 
  distinct(.$grid.cell.number)

t <- cleandata %>% 
  group_by(site.code) %>% 
  arrange(site.code, grid.cell.number) %>% 
  ungroup() %>% 
  group_by(site.code, grid.cell.number) %>%
  filter(n() > 1)
  

## Check for erroneous site.code entries
cleandata %>% 
  distinct(site.code)




#------------------------------------------------#
####              Export CSV                  ####
#------------------------------------------------#

write.csv(cleandata, "index/www/data/rmd_data.csv", row.names = F)



