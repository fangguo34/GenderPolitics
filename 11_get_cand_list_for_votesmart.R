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


# Data for battle of sexes --------------------------------------------------------------------
# data_for_votesmart <- read_feather(paste0(gender, "06_primary_fundraising_battle_sexes.feather")) %>%
#   group_by(general_election_id) %>%
#   mutate(n_i = sum(Incum.Chall == "I"),
#          n_c = sum(Incum.Chall == "C"),
#          n_o = sum(Incum.Chall == "O"),
#          n_blank = sum(Incum.Chall == "")) %>%
#   ungroup() %>%
#   mutate(Incum.Chall = ifelse(n_i > 0 & Incum.Chall != "I", "C",
#                               ifelse(n_i == 0 & Incum.Chall != "O", "O", Incum.Chall)),
#          partyname = ifelse(party == "100", "Democratic", "Republican")) %>%
#   select(general_election_id, primary_election_id, bonica.rid, candidate, candidate_lname, partyname, state, cycle)

# write.csv(data_for_votesmart, paste0(gender, "11_candidates_input_for_votesmart.csv"), row.names = FALSE)


# Data for all cands ------------------------------------------------------
data_for_votesmart_all <- read_feather(paste0(gender, "01_primary_elections_final.feather")) %>% 
  group_by(general_election_id) %>% 
  mutate(n_i = sum(Incum.Chall == "I"),
         n_c = sum(Incum.Chall == "C"),
         n_o = sum(Incum.Chall == "O"),
         n_blank = sum(Incum.Chall == "")) %>% 
  ungroup() %>% 
  mutate(Incum.Chall = ifelse(n_i > 0 & Incum.Chall != "I", "C",
                              ifelse(n_i == 0 & Incum.Chall != "O", "O", Incum.Chall)),
         partyname = ifelse(party == "100", "Democratic", "Republican")) %>%
  select(general_election_id, primary_election_id, bonica.rid, candidate, candidate_lname, partyname, state, cycle)

write.csv(data_for_votesmart_all, paste0(gender, "11_candidates_input_for_votesmart_all.csv"), row.names = FALSE)

# Bio Detailed Data from local python code --------------------------------
bio_data <- read.csv(paste0(gender, "11_bio_detail.csv")) %>% 
  group_by(primary_election_id) %>% 
  mutate(n_cands_available = n()) %>% 
  ungroup() %>% 
  filter(n_cands_available == 2)

bio_data_edu <- bio_data %>% 
  mutate(degree_available = ifelse(degree1 != "", 1, 0)) %>% 
  group_by(primary_election_id) %>% 
  mutate(n_cands_degree_avail = sum(degree_available)) %>% 
  ungroup() %>% 
  filter(n_cands_degree_avail == 2)

fundraising_data <- read_feather(paste0(gender, "06_primary_fundraising_battle_sexes.feather"))

bio_data_edu_final <- bio_data_edu %>% 
  left_join(fundraising_data, by = c("general_election_id", "primary_election_id", "bonica.rid", "candidate", "candidate_lname",
                                     "state", "cycle")) %>% 
  group_by(general_election_id) %>%
  mutate(n_i = sum(Incum.Chall == "I"),
         n_c = sum(Incum.Chall == "C"),
         n_o = sum(Incum.Chall == "O"),
         n_blank = sum(Incum.Chall == "")) %>%
  ungroup() %>%
  mutate(Incum.Chall = ifelse(n_i > 0 & Incum.Chall != "I", "C",
                              ifelse(n_i == 0 & Incum.Chall != "O", "O", Incum.Chall)),
         has_jd = ifelse(grepl("JD", degree1), 1, 
                         ifelse(grepl("JD", degree2), 1,
                                ifelse(grepl("JD", degree3), 1, 0))))

test <- bio_data_edu_final %>% 
  select(general_election_id, primary_election_id, bonica.rid, candidate, candidate_lname, gender, 
         candpct, p0, p1, win_margin_primary, 
         partyname, state, cycle,
         has_jd, degree1, degree2, degree3)

summary <- test %>% 
  filter(abs(win_margin_primary) <= 0.1) %>% 
  group_by(gender) %>% 
  summarise(n = n(),
            n_jd = sum(has_jd == 1))

# final_data <- data_for_votesmart %>%
#   left_join(bio_data, by = c("general_election_id", "primary_election_id", "bonica.rid", "candidate_lname"))


# primary_cands_fundraising <- read_feather(paste0(gender, "03_primary_elections_fundraising_panel_data.feather"))
# general_cands_fundraising <- read_feather(paste0(gender, "03_general_elections_fundraising_panel_data.feather"))
