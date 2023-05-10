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


# Functions ---------------------------------------------------------------
create_var <- function(data = bio_data, colname = "degree", varname = "has_jd", keyword = "JD") {
  data_tmp <- data[c(paste0(colname, 1), paste0(colname, 2), paste0(colname, 3))] 
  colnames(data_tmp) <- c("col1", "col2", "col3")
  
  data_output <- data %>% 
    bind_cols(data_tmp) %>% 
    mutate(!!varname := ifelse(grepl(keyword, col1), 1, 
                               ifelse(grepl(keyword, col2), 1,
                                      ifelse(grepl(keyword, col3), 1, 0)))) %>% 
    select(-col1, -col2, -col3)
  
  return(data_output)
  
}


# Source 1: Klarner ------------------------------------------------------------------
load(paste0(import, "elections/196slers1967to2016_20180908.RData"))

state_leg_klarner <- table %>% 
  select(year, sab, v56, v58, cand, outcome) %>% 
  filter(outcome == "w")

# Source 2: DIME --------------------------------------------------------------------
recipients_dime <- fread(paste0(import, "dime/dime_recipients_all_1979_2018.csv"),
                         select = c("bonica.rid", "name", "lname", "fname", "election", "seat", "district", "party", "state",
                                    "cand.gender", "Incum.Chall", "ran.primary", "ran.general", "winner",
                                    "recipient.cfscore", "recipient.cfscore.dyn",
                                    "dwnom1", "dwnom2", "ps.dwnom1", "ps.dwnom2"))

state_leg_dime <- data.table(recipients_dime) %>%
  .[grepl("state", seat) & winner == "W"] %>%
  .[, `:=`(name = toupper(name),
           lname = toupper(lname),
           fname = toupper(fname),
           district = ifelse(nchar(district) < 4, paste0(state, district), district),
           cycle = as.numeric(str_extract(election, "\\d{4}")))] %>% 
  .[, `:=`(primary_election_id = paste0(cycle, district, "_", party))] 

state_leg_dime_clean <- data.frame(state_leg_dime) %>% 
  select(bonica.rid, seat, cycle, state, district, party, winner) %>% 
  arrange(bonica.rid, cycle) %>% 
  group_by(bonica.rid) %>% 
  mutate(earliest_state_leg_year = min(cycle)) %>% 
  select(bonica.rid, earliest_state_leg_year) %>% 
  distinct()

# Source 3: Bio Data ----------------------------------------------------------------
bio_data <- read.csv(paste0(gender, "11_bio_detail_all.csv"), stringsAsFactors = FALSE) %>% 
  distinct() %>% 
  filter((degree1 != "" | degree2 != "" | degree3 != "") 
         & (prof_title1 != "" | prof_title2 != "" | prof_title3 != "")) %>% 
  mutate(mutate(across(c(30:47), tolower))) %>% 
  mutate(age = cycle - as.numeric(str_extract(birthdate, "\\d{4}"))) %>% 
  create_var("political_title", "is_rep", "representative|senator|member|speaker|leader|chair") %>% 
  create_var("political_organization", "is_state_leg", "state senate|state house") 

# NEED TO MAKE SURE THAT is_rep IS FOR STATE LEG, NOT FEDERAL

# test <- bio_data %>% 
#   filter(is_rep == 0 & is_state_leg == 1)

# Merge -------------------------------------------------------------------
merged_data <- bio_data %>% 
  left_join(state_leg_dime_clean, by = "bonica.rid")


