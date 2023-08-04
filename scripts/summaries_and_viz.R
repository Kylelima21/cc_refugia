## Script to make the summaries and viz from the refugia Survey123 data
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


  
#------------------------------------------------#
####              Summary Stats               ####
#------------------------------------------------#

### Number of grid cells sampled
## Total
n_distinct(cleandat$grid.cell.number)

## Sampled this year
celldat <- cleandat %>% 
  filter(year == year(Sys.Date()))
celldat


## BMIS
bmiscells <- cleandat %>% 
  filter(site.code == "BMIS")
n_distinct(bmiscells$grid.cell.number)

## CABA
cabacells <- cleandat %>% 
  filter(site.code == "CABA")
n_distinct(cabacells$grid.cell.number)


### Average duration of survey
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


### Site info table
sitefo <- read.csv("data/refugia_site_info.csv")


### Number of refugia and non-refugia cells sampled
refcells <- cleandat %>% 
  filter(site.code == "BMIS" | site.code == "CAMO" | site.code == "OTPO" | 
           site.code == "SCHE" | site.code == "SHHA") %>% 
  filter(year == year(Sys.Date()))
refcells

nonrefcells <- cleandat %>% 
  filter(site.code == "ACMO" | site.code == "BEMO" | site.code == "CABA" | 
           site.code == "PEMO" | site.code == "SCMO") %>% 
  filter(year == year(Sys.Date()))
nonrefcells




#------------------------------------------------#
####              Maps and Graphs             ####
#------------------------------------------------#

### Map of sites surveyed
## Get basemap
acad.reg <- get_stamenmap(bbox = c(left = -68.55, bottom = 44.19,
                                   right = -67.93, top = 44.62),
                          zoom = 10)

## Determine which sites have been sampled this year from the cleaned data
sampsites <- cleandat %>%
  filter(year == year(Sys.Date())) %>% 
  distinct(site.code)

## Read in the site location data and add column to specify sampled/unsampled
mapdat <- read.csv("data/site_locations.csv") %>%
  mutate(site.code = toupper(site.code),
         color = ifelse(site.code %in% sampsites$site.code, "Sampled", "Not sampled"))

## Specify colors and shapes for the map
cols <- c("Not sampled" = "#330033", "Sampled" = "#489619")
shape <- c("Not sampled" = 23, "Sampled" = 21)

## ggmap it
ggmap(acad.reg) +
  geom_point(aes(x = longitude, y = latitude, fill = color, shape = color), size = 3.5,
             data = mapdat, color = "white") +
  theme_bw() +
  scale_fill_manual(values = cols) +
  scale_shape_manual(values = shape) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.85, 0.1),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, color = "black"),
        legend.background = element_blank())

## save as PNG for reading in
# ggsave("outputs/sitemap.png", height = 6, width = 6, dpi = 600)




### Number of grid cells sampled
## Format data
barsum <- cleandat %>% 
  filter(year == 2023) %>% 
  group_by(site.code) %>% 
  summarise(cells = length(unique(grid.cell.number)))

bardat <- read.csv("data/site_locations.csv") %>% 
  dplyr::select(site.code) %>% 
  mutate(site.code = toupper(site.code),
         cells = 0) %>% 
  left_join(barsum, by = "site.code") %>% 
  tibble() %>% 
  mutate(cells = cells.x + cells.y) %>% 
  #cells = replace(cells, is.na(cells), 0)) %>% 
  dplyr::select(site.code, cells)

## Create ggplot
bardat %>% 
  ggplot(aes(x = site.code, y = cells, fill = site.code)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = cells), vjust = -1, size = 4) +
  labs(x = "Site code", y = "Count") +
  scale_y_continuous(expand = c(0, 0, 0.15, 0)) +
  scale_fill_brewer(palette = "PRGn") +
  theme_bw() +
  theme(panel.border = element_rect(linewidth = 1),
        panel.grid = element_blank(),
        axis.text = element_text(color = "black", size = 10),
        axis.title = element_text(color = "black", size = 12),
        legend.position = "none",
        plot.margin = margin(0.5,0.5,.8,1, unit = "cm"),
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = 5))

## Save plot as PNG
# ggsave("outputs/cellbd.png", height = 6, width = 8, dpi = 600)








