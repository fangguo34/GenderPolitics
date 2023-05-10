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

path <- "~/zfs/projects/students/fangg-voters/data/"

gender <- paste0(path, "gender_politics/")
import <- paste0(path, "import/")
export <- paste0(path, "export/")
tmp <- paste0(path, "tmp/")
shp <- paste0(path, "shapefiles/")
contrib <- paste0(import, "dime/contribDB/")

# Import Basic Data -------------------------------------------------------
files = list.files(contrib)

contribution_types_to_include <- read.csv(paste0(import, "dime/contribution_types.csv"), sep = ",", header = TRUE) %>%
  filter(!grepl("Loan|loan|Transfer|transfer", Transaction.type.description) 
         & !Transaction.type %in% c("24A", "24E", "15C"))

women_pacs_list <- read_feather(paste0(tmp, "79_women_pacs_unique.feather"))

state_names <- read.csv(paste0(import, "dime/state_names.csv")) %>% 
  mutate(full_name = tolower(full_name))

# Primary Elections Returns -----------------------------------------------
load(paste0(import, "elections/us_house_primary_1956_2010.RData"))
primary_elections_1 <- table %>% 
  select(-dollars) %>% 
  mutate(state = ifelse(state == "pennsylviana", "pennsylvania", state))

load(paste0(import, "elections/us_house_primary_2012_2018.RData"))
primary_elections_2 <- table %>% 
  mutate(state = ifelse(state == "pennsylviana", "pennsylvania", state))

primary_elections <- bind_rows(primary_elections_1, primary_elections_2) %>% 
  filter(!grepl("write-in", candidate))

# Subset ----------------------
primary_elections_fg <- primary_elections %>% 
  filter(year >= 1980) %>% 
  left_join(state_names, by = c("state" = "full_name")) %>% 
  mutate(general_election_id = paste0(year, name, substr(stcd, 3,4)),
         primary_election_id = paste0(year, name, substr(stcd, 3,4), "_", ifelse(party == 1, 100, 200)),
         candidate = toupper(gsub("_", " ", sub("_", ", ", candidate)))) %>%
  mutate(candidate_lname = sub("\\,.*", "", candidate)) %>% 
  select(general_election_id, primary_election_id, candidate, candidate_lname, gender, candpct) %>% 
  distinct() %>% 
  arrange(primary_election_id, desc(candpct)) %>%
  group_by(primary_election_id) %>% 
  mutate(n_cands = n())

# Clean recipients Data ---------------------------------------------------------
recipients_dime <- fread(paste0(import, "dime/dime_recipients_all_1979_2018.csv"),
                         select = c("bonica.rid", "name", "lname", "fname", "election", "seat", "district", "party", "state",
                                    "cand.gender", "Incum.Chall", "ran.primary", "ran.general", "winner",
                                    "recipient.cfscore", "recipient.cfscore.dyn",
                                    "dwnom1", "dwnom2", "ps.dwnom1", "ps.dwnom2"))

recipients_dime_fg <- data.table(recipients_dime) %>%
  .[seat == "federal:house"] %>%
  .[, `:=`(name = toupper(name),
           lname = toupper(lname),
           fname = toupper(fname),
           district = ifelse(nchar(district) < 4, paste0(state, district), district),
           cycle = as.numeric(str_extract(election, "\\d{4}")))] %>% 
  .[, `:=`(primary_election_id = paste0(cycle, district, "_", party))] 

# Election Returns Left Join DIME Recipients Data to get bonica.rid ------------------------------------------
primary_elections_join_fg <- primary_elections_fg %>% 
  left_join(data.frame(recipients_dime_fg), by = c("primary_election_id" = "primary_election_id", "candidate_lname" = "lname")) %>% 
  group_by(primary_election_id) %>% 
  mutate(n_cands_2 = n(),
         n_na_primary = sum(is.na(bonica.rid)))

correct <- primary_elections_join_fg %>% 
  filter(n_cands == n_cands_2)

fixes <- primary_elections_join_fg %>% 
  filter(n_cands != n_cands_2) %>%
  mutate(distance = ifelse(is.na(levenshteinDist(name, candidate)), 100, levenshteinDist(name, candidate))) %>% 
  group_by(primary_election_id, candidate) %>% 
  slice(which.min(distance)) %>% 
  select(-distance)

# Prepare for export ------------------------------------------------------
primary_elections_final <- bind_rows(correct, fixes) %>% 
  arrange(primary_election_id, desc(candpct)) %>% 
  group_by(primary_election_id) %>% 
  mutate(n_cands_primary = n(),
         n_f_cands_primary = sum(gender == 1),
         n_f_cands_top2_primary = gender[1] + ifelse(is.na(gender[2]), 0, gender[2]),
         p0 = candpct[1],
         p1 = ifelse(is.na(candpct[2]), 0, candpct[2]),
         win_margin_primary = p0 - p1) %>% 
  select(-n_cands, -n_cands_2)
write_feather(primary_elections_final, paste0(gender, "01_primary_elections_final.feather"))

primary_elections_final_no_na <- primary_elections_final %>% 
  arrange(primary_election_id, desc(candpct)) %>% 
  filter(n_na_primary == 0) 
write_feather(primary_elections_final_no_na, paste0(gender, "01_primary_elections_final_no_na.feather"))

primary_elections_battle_sexes <- primary_elections_final %>% 
  arrange(primary_election_id, desc(candpct)) %>% 
  filter(n_cands_primary > 1 & n_f_cands_top2_primary == 1) 
write_feather(primary_elections_battle_sexes, paste0(gender, "01_primary_elections_battle_sexes.feather"))
