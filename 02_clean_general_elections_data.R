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

# Import clean primary election data --------------------------------------
primary_elections <- read_feather(paste0(gender, "01_primary_elections_final.feather"))

# Import general election data --------------------------------------------
general_elections <- read.csv(paste0(import, "elections/1976-2020-house.csv")) %>% 
  filter(year >= 1980 & year <= 2018 & writein == FALSE & candidate != "OTHER") %>% 
  mutate(general_election_id = paste0(year, state_po, sprintf("%02d", district)),
         voteshare_general = candidatevotes/totalvotes,
         candidate_general = as.character(candidate),
         party_general = as.character(party)) %>% 
  rowwise() %>% 
  mutate(candidate_general_lname = str_split(candidate_general, " ", simplify = T)[, length(str_split(candidate_general, " ", simplify = T))],
         primary_election_id_tmp = paste0(general_election_id, "_", ifelse(party_general == "DEMOCRAT", "100", 
                                                                       ifelse(party_general == "REPUBLICAN", "200", "328")))) %>% 
  select(general_election_id, primary_election_id_tmp, candidate_general, candidate_general_lname, party_general, voteshare_general) %>% 
  arrange(general_election_id) %>% 
  group_by(general_election_id) %>% 
  mutate(general_winner = ifelse(max(voteshare_general) == voteshare_general, 1, 0)) 

# Combine -----------------------------------------------------------------
general_elections_final <- general_elections %>% 
  left_join(primary_elections, by = c("general_election_id" = "general_election_id", "candidate_general_lname" = "candidate_lname")) %>% 
  arrange(general_election_id, primary_election_id_tmp, voteshare_general) %>% 
  group_by(general_election_id) %>% 
  mutate(n_cands_general = n(),
         n_na_general = sum(is.na(bonica.rid))) 
write_feather(general_elections_final, paste0(gender, "02_general_elections_final.feather"))

general_elections_final_no_na <- general_elections_final %>% 
  filter(!is.na(bonica.rid)) %>% 
  group_by(general_election_id) %>% 
  mutate(n_cands_general_available = n())
write_feather(general_elections_final_no_na, paste0(gender, "02_general_elections_final_no_na.feather"))

# Summary Stats -----------------------------------------------------------
summary_stats <- general_elections_final_no_na %>% 
  group_by(state, cycle) %>% 
  summarise(n = n())

