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

library(RecordLinkage)

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


# Primary Data ------------------------------------------------------------
primary_elections <- read_feather(paste0(gender, "01_primary_elections_final.feather")) %>% 
  arrange(state, district, cycle)

# test <- primary_elections %>% 
#   filter(is.na(bonica.rid))

# Yearly Contributions Data -----------------------------------------------
subset_dime_transactions <- function(filename) {
  print(filename)
  
  # Basic Info
  cycle_year = as.numeric(str_extract(filename, "\\d{4}+"))
  days <- seq(ymd(paste0(cycle_year, "-01-01")), ymd(paste0(cycle_year, "-12-31")),by="1 day")
  election_day <- days[month(days) == 11 & weekdays(days) == "Tuesday" & as.numeric(format(days, "%d")) <= 7]
  candidates <- data.table(primary_elections) %>% 
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

contribution_summary <- bind_rows(map(files, subset_dime_transactions)) 
write_feather(contribution_summary, paste0(gender, "03_primary_elections_fundraising_summary.feather"))

panel_data_final <- primary_elections %>% 
  left_join(contribution_summary, by = c("cycle" = "cycle", "bonica.rid" = "bonica.rid")) %>% 
  filter(!is.na(bonica.rid)) 
# %>% 
#   replace(is.na(.), 0)
write_feather(panel_data_final, paste0(gender, "03_primary_elections_fundraising_panel_data.feather"))

# General Elections --------------------------------------------------------
general_elections_panel <- read_feather(paste0(gender, "02_general_elections_final_no_na.feather")) %>% 
  select(general_election_id:primary_election_id, bonica.rid) %>% 
  left_join(panel_data_final, by = c("general_election_id", "primary_election_id", "bonica.rid")) %>% 
  arrange(state, district, cycle)
write_feather(general_elections_panel, paste0(gender, "03_general_elections_fundraising_panel_data.feather"))


