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

# Basic Data --------------------------------------------------------------
files = list.files(contrib)

contribution_types_to_include <- read.csv(paste0(import, "dime/contribution_types.csv"), sep = ",", header = TRUE) %>%
  filter(!grepl("Loan|loan|Transfer|transfer", Transaction.type.description) 
         & !Transaction.type %in% c("24A", "24E", "15C"))

women_pacs_list <- read_feather(paste0(tmp, "79_women_pacs_unique.feather"))

# Import ------------------------------------------------------
rd_data_raw <- read_feather(paste0(gender, "06_primary_fundraising_battle_sexes.feather")) %>% 
  group_by(general_election_id) %>% 
  mutate(n_i = sum(Incum.Chall == "I"),
         n_c = sum(Incum.Chall == "C"),
         n_o = sum(Incum.Chall == "O"),
         n_blank = sum(Incum.Chall == "")) %>% 
  ungroup() %>% 
  mutate(Incum.Chall = ifelse(n_i > 0 & Incum.Chall != "I", "C",
                              ifelse(n_i == 0 & Incum.Chall != "O", "O", Incum.Chall))) 

rd_data_select <- rd_data_raw %>% 
  select(general_election_id, primary_election_id, cycle, candidate, bonica.rid, gender, Incum.Chall,
         candpct, p0, p1, primary_winner, female_primary_winner, female_primary_vote_margin)

# p_bw_contributions <- read_feather(paste0(tmp, "86_contributions_federal_house_battle_sexes_0128_full_info.feather")) %>% 
#   filter(contributor.category.specific != "U"
#          & (cand.gender == 0)*(contributor.category.specific == "FC") == 0 
#          & !transaction.type %in% c("24A", "24E", "15C"))
# 
# test <- length(unique(p_bw_contributions$district_mod_dime))


# Functions ---------------------------------------------------------------
subset_dime_transactions <- function(filename) {
  print(filename)
  
  # Basic Info
  cycle_year = as.numeric(str_extract(filename, "\\d{4}+"))
  days <- seq(ymd(paste0(cycle_year, "-01-01")), ymd(paste0(cycle_year, "-12-31")),by="1 day")
  election_day <- days[month(days) == 11 & weekdays(days) == "Tuesday" & as.numeric(format(days, "%d")) <= 7]
  candidates <- data.table(rd_data_raw) %>% 
    .[cycle == cycle_year & !is.na(bonica.rid)]
  
  # Import raw transaction level data
  data_raw = fread(paste0(contrib, filename), select = c("cycle", "seat", "election.type", "date",
                                                         "bonica.rid", "bonica.cid", 
                                                         "transaction.id", "transaction.type", "amount", 
                                                         "contributor.name", "contributor.gender", "contributor.type",
                                                         "contributor.state", "contributor.occupation", "contributor.employer",
                                                         "is.corp"))
  
  # subset transaction level data to relevant candidates
  data_subset <- data.table(data_raw) %>% 
    .[bonica.rid %in% candidates$bonica.rid
      & seat == "federal:house" 
      & transaction.type %in% contribution_types_to_include$Transaction.type 
      & amount > 0] %>% 
    .[, date := as.Date(date)] %>% 
    .[, bonica.cid := as.character(bonica.cid)] %>% 
    .[, mths_till_general_election := -(interval(date, election_day) %/% months(1))] %>% 
    .[, contributor.category := ifelse(contributor.type == "C", "C", 
                                       ifelse(contributor.gender == "", "U", contributor.gender))] %>%
    .[, contributor.category.specific := ifelse(contributor.name %in% women_pacs_list$most.recent.contributor.name, "FC", contributor.category)] %>% 
    .[order(bonica.rid, date)]
  
  print(class(data_subset$date))
  print(nrow(data_subset))
  
  return(data_subset)
}

# Run ---------------------------------------------------------------------
contributions_all <- bind_rows(map(files, subset_dime_transactions)) 
write_feather(contributions_all, paste0(gender, "09_contributions_all.feather"))

contributions_final <- data.frame(contributions_all) %>% 
  left_join(rd_data_select, by = c("cycle" = "cycle", "bonica.rid" = "bonica.rid")) %>% 
  filter(!is.na(bonica.rid)) 
write_feather(contributions_final, paste0(gender, "09_contributions_final.feather"))

# Donor Level -------------------------------------------------------------
donor_level_data <- contributions_final %>% 
  filter(contributor.category.specific %in% c("F", "M")) %>% 
  group_by(general_election_id, bonica.cid, contributor.category.specific) %>% 
  summarise(donation_to_winner_p = sum(amount[election.type == "P" & primary_winner == 1]),
            donation_to_loser_p = sum(amount[election.type == "P" & primary_winner == 0]),
            donation_to_winner_g = sum(amount[election.type == "G" & primary_winner == 1]),
            donation_to_loser_g = sum(amount[election.type == "G" & primary_winner == 0])) %>% 
  ungroup() %>% 
  mutate(ChangeWinner = donation_to_winner_g - donation_to_winner_p,
         DonatedLoser = as.numeric(donation_to_loser_p > 0),
         FavoredLoser = as.numeric(donation_to_loser_p > donation_to_winner_p),
         PositiveBiasLoser = donation_to_loser_p - donation_to_winner_p)

write_feather(donor_level_data, paste0(gender, "09_donor_level_data.feather"))

