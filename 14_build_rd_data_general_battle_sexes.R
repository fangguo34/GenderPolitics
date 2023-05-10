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

files <- list.files(contrib)

# Basic Data --------------------------------------------------------------
contribution_types_to_include <- read.csv(paste0(import, "dime/contribution_types.csv"), sep = ",", header = TRUE) %>%
  filter(!grepl("Loan|loan|Transfer|transfer", Transaction.type.description) 
         & !Transaction.type %in% c("24A", "24E", "15C"))

women_pacs_list <- read_feather(paste0(tmp, "79_women_pacs_unique.feather"))


# Import general Data -------------------------------------------------------
general_data_raw <- read_feather(paste0(gender, "03_general_elections_fundraising_panel_data.feather")) %>% 
  group_by(general_election_id) %>% 
  mutate(n_i = sum(Incum.Chall == "I"),
         n_c = sum(Incum.Chall == "C"),
         n_o = sum(Incum.Chall == "O"),
         n_blank = sum(Incum.Chall == "")) %>% 
  ungroup() %>% 
  mutate(Incum.Chall = ifelse(n_i > 0 & Incum.Chall != "I", "C",
                              ifelse(n_i == 0 & Incum.Chall != "O", "O", Incum.Chall)))

general_data_subset <- general_data_raw %>% 
  group_by(general_election_id) %>% 
  slice_max(order_by = voteshare_general, n = 2) %>% 
  group_by(general_election_id) %>% 
  mutate(n_cands_general = n(),
         n_f_cands_general = sum(gender)) %>% 
  ungroup() %>% 
  filter(n_cands_general == 2 & n_f_cands_general == 1) %>% 
  group_by(general_election_id) %>% 
  mutate(female_vote_margin_general = voteshare_general[gender == 1]-voteshare_general[gender == 0],
         female_winner_general = as.numeric(female_vote_margin_general > 0)) %>% 
  mutate(next_cycle = cycle + 2,
         next_general_election_id = paste0(next_cycle, district),
         next_primary_election_id = paste0(next_cycle, district, "_", party)) %>% 
  select(general_election_id:bonica.rid, gender, election:state, Incum.Chall, female_vote_margin_general:next_primary_election_id) %>% 
  filter(general_winner == 1)
  
# DIME Recipients Data -------------------------------------------------------------------
recipients_dime <- fread(paste0(import, "dime/dime_recipients_all_1979_2018.csv"),
                         select = c("bonica.rid", "name", "lname", "fname", "election", "seat", "district", "party", "state",
                                    "cand.gender", "Incum.Chall", "ran.primary", "ran.general", "winner"))

recipients_dime_fg <- data.table(recipients_dime) %>%
  .[seat == "federal:house"] %>%
  .[, `:=`(name = toupper(name),
           lname = toupper(lname),
           fname = toupper(fname),
           district = ifelse(nchar(district) < 4, paste0(state, district), district),
           cycle = as.numeric(str_extract(election, "\\d{4}")))] %>% 
  .[, general_election_id := paste0(cycle, district)] %>% 
  .[general_election_id %in% general_data_subset$next_general_election_id]

# DIME Transactions Data --------------------------------------------------
# filename = files[2]
subset_dime_transactions <- function(filename) {
  print(filename)
  
  # Basic Info
  cycle_year = as.numeric(str_extract(filename, "\\d{4}+"))
  days <- seq(ymd(paste0(cycle_year, "-01-01")), ymd(paste0(cycle_year, "-12-31")),by="1 day")
  election_day <- days[month(days) == 11 & weekdays(days) == "Tuesday" & as.numeric(format(days, "%d")) <= 7]
  candidates <- data.table(recipients_dime_fg) %>% 
    .[cycle == cycle_year]
  
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
    .[, mths_till_general_election := -(interval(date, election_day) %/% months(1))] %>% 
    .[, contributor.category := ifelse(contributor.type == "C", "C", 
                                       ifelse(contributor.gender == "", "U", contributor.gender))] %>%
    .[, contributor.category.specific := ifelse(contributor.name %in% women_pacs_list$most.recent.contributor.name, "FC", contributor.category)] %>% 
    .[order(bonica.rid, date)]
  
  print(class(data_subset$date))
  print(nrow(data_subset))
  
  # summary
  data_summary <- data_subset %>% 
    .[, .(y_p = sum(amount[election.type == "P"]),
          y_p_f = sum(amount[election.type == "P" & contributor.category.specific == "F"]),
          y_p_m = sum(amount[election.type == "P" & contributor.category.specific == "M"]),
          y_p_fc = sum(amount[election.type == "P" & contributor.category.specific == "FC"]),
          y_p_c = sum(amount[election.type == "P" & contributor.category.specific == "C"]),
          y_p_fcc = sum(amount[election.type == "P" & contributor.category.specific %in% c("FC", "C")]),
          y_g = sum(amount[election.type == "G"]),
          y_g_f = sum(amount[election.type == "G" & contributor.category.specific == "F"]),
          y_g_m = sum(amount[election.type == "G" & contributor.category.specific == "M"]),
          y_g_fc = sum(amount[election.type == "G" & contributor.category.specific == "FC"]),
          y_g_c = sum(amount[election.type == "G" & contributor.category.specific == "C"]),
          y_g_fcc = sum(amount[election.type == "G" & contributor.category.specific %in% c("FC", "C")]),
          n_p = n_distinct(bonica.cid[election.type == "P"]),
          n_p_f = n_distinct(bonica.cid[election.type == "P" & contributor.category.specific == "F"]),
          n_p_m = n_distinct(bonica.cid[election.type == "P" & contributor.category.specific == "M"]),
          n_p_fc = n_distinct(bonica.cid[election.type == "P" & contributor.category.specific == "FC"]),
          n_p_c = n_distinct(bonica.cid[election.type == "P" & contributor.category.specific == "C"]),
          n_p_fcc = n_distinct(bonica.cid[election.type == "P" & contributor.category.specific %in% c("FC", "C")]),
          n_g = n_distinct(bonica.cid[election.type == "G"]),
          n_g_f = n_distinct(bonica.cid[election.type == "G" & contributor.category.specific == "F"]),
          n_g_m = n_distinct(bonica.cid[election.type == "G" & contributor.category.specific == "M"]),
          n_g_fc = n_distinct(bonica.cid[election.type == "G" & contributor.category.specific == "FC"]),
          n_g_c = n_distinct(bonica.cid[election.type == "G" & contributor.category.specific == "C"]),
          n_g_fcc = n_distinct(bonica.cid[election.type == "G" & contributor.category.specific %in% c("FC", "C")])), by = c("cycle", "bonica.rid")]
  
  
  return(data_summary)
  
}

# candidate cycle level fundraising
contribution_summary <- bind_rows(map(files[2:20], subset_dime_transactions)) 
write_feather(contribution_summary, paste0(gender, "14_next_elections_fundraising_cand_cycle_level.feather"))

contribution_summary_final <- data.frame(recipients_dime_fg) %>% 
  left_join(data.frame(contribution_summary), by = c("cycle" = "cycle", "bonica.rid" = "bonica.rid")) %>% 
  filter(!is.na(bonica.rid)) 
# %>% 
#   replace(is.na(.), 0)
write_feather(contribution_summary_final, paste0(gender, "14_next_elections_fundraising_summary_final_cand_cycle_level.feather"))


# Final Cleaning ----------------------------------------------------------
# district cycle level fundraising
contribution_summary_final_mod <- contribution_summary_final %>% 
  replace(is.na(.), 0) %>% 
  select(general_election_id:n_g_fcc) %>% 
  group_by(general_election_id) %>% 
  summarise(total_y_p_f = sum(y_p_f),
            total_y_p_m = sum(y_p_m),
            total_y_p_fcc = sum(y_p_fcc),
            total_y_p = sum(y_p),
            pct_y_p_f = sum(y_p_f)/(sum(y_p_f) + sum(y_p_m)),
            pct_y_p_m = sum(y_p_m)/(sum(y_p_f) + sum(y_p_m)),
            total_n_p_f = sum(n_p_f),
            total_n_p_m = sum(n_p_m),
            total_n_p_fcc = sum(n_p_fcc),
            total_n_p = sum(n_p),
            pct_n_p_f = sum(n_p_f)/(sum(n_p_f) + sum(n_p_m)),
            pct_n_p_m = sum(n_p_m)/(sum(n_p_f) + sum(n_p_m))) %>% 
  ungroup() %>% 
  mutate(log_total_y_p = log1p(total_y_p),
         log_total_y_p_f = log1p(total_y_p_f),
         log_total_y_p_m = log1p(total_y_p_m),
         log_total_y_p_fcc = log1p(total_y_p_fcc),
         log_total_n_p = log1p(total_n_p),
         log_total_n_p_f = log1p(total_n_p_f),
         log_total_n_p_m = log1p(total_n_p_m),
         log_total_n_p_fcc = log1p(total_n_p_fcc))

final_data <- general_data_subset %>% 
  left_join(contribution_summary_final_mod, by = c("next_general_election_id" = "general_election_id"))
  
write_feather(final_data, paste0(gender, "14_rd_data.feather"))


