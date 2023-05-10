# Basic Setup -------------------------------------------------------------
rm(list = ls())

library(tidyverse)
library(readr)

library(lubridate)
library(stringr)
library(RecordLinkage)

library(readstata13)
library(foreign)

library(geosphere)
library(rgdal)
library(rgeos)
library(sp)
library(sf)

library(rdrobust)
library(fixest)

library(ggplot2)

library(data.table)
library(feather)
library(dplyr)
library(dtplyr)
library(broom)
library(xtable)
library(texreg)

library(RecordLinkage)

path <- "~/zfs/projects/students/fangg-voters/data/"

gender <- paste0(path, "gender_politics/")
import <- paste0(path, "import/")
export <- paste0(path, "export/")
tmp <- paste0(path, "tmp/")
shp <- paste0(path, "shapefiles/")
contrib <- paste0(import, "dime/contribDB/")

# Data Sanity Checks ------------------------------------------------------
primary_elections_panel <- read_feather(paste0(gender, "01_primary_elections_final_no_na.feather")) 

check_1 <- primary_elections_panel %>% 
  group_by(state) %>% 
  summarise(n_unique_cycles = n_distinct(cycle))

check_2 <- primary_elections_panel %>% 
  group_by(state, cycle) %>% 
  summarise(n_unique_districts = n_distinct(district)) %>% 
  ungroup 

general_elections_panel <- read_feather(paste0(gender, "02_general_elections_final_no_na.feather")) 


cand_panel <- read.csv(paste0(gender, "05_cand_panel_clean.csv"))

check_3 <- cand_panel %>% 
  group_by(state) %>% 
  summarise(n_unique_cycles = n_distinct(year))



# %>% 
#   select(general_election_id:primary_election_id, bonica.rid) %>% 
#   left_join(panel_data_final, by = c("general_election_id", "primary_election_id", "bonica.rid")) %>% 
#   arrange(state, district, cycle)


