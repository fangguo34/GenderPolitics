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

# Data --------------------------------------------------------------------
bio_data <- read.csv(paste0(gender, "11_bio_detail_all.csv"), stringsAsFactors = FALSE) %>% 
  distinct() %>% 
  filter((degree1 != "" | degree2 != "" | degree3 != "") 
         & (prof_title1 != "" | prof_title2 != "" | prof_title3 != "")) %>% 
  mutate(age = cycle - as.numeric(str_extract(birthdate, "\\d{4}")))

fundraising_data <- read_feather(paste0(gender, "03_primary_elections_fundraising_panel_data.feather")) %>% 
  group_by(general_election_id, primary_election_id, bonica.rid, candidate) %>%
  slice(which.max(candpct)) %>%
  filter(cycle >= 1992) %>% 
  mutate(win_primary = ifelse(candpct == p0, 1, 0)) %>% 
  select(general_election_id, primary_election_id, bonica.rid, candidate, candidate_lname, gender, candpct)

# raw_data_summary <- fundraising_data %>% 
#   group_by(gender) %>% 
#   summarise(n = n())

# move_out_state <- bio_data %>% 
#   filter(state != homestate)

college_state <- bio_data %>% 
  select(general_election_id, primary_election_id, bonica.rid, candidate, state, homecity, homestate, school1, degree1, school2, degree2, school3, degree3)

# Clean -------------------------------------------------------------------
bio_data_family <- bio_data %>% 
  mutate(children = as.numeric(ifelse(!is.na(str_extract(family, "(\\d+)(?=\\s+Children)")), str_extract(family, "(\\d+)(?=\\s+Children)"),
                                  ifelse(!is.na(str_extract(family, "(\\d+)(?=\\s+Child)")), str_extract(family, "(\\d+)(?=\\s+Child)"),
                                      ifelse(!is.na(str_extract(family, "(\\d+)(?=\\s+children)")), str_extract(family, "(\\d+)(?=\\s+children)"),
                                         ifelse(!is.na(str_extract(family, "(\\d+)(?=\\s+child)")), str_extract(family, "(\\d+)(?=\\s+child)"),
                                                0)))))) %>% 
  create_var("degree", "bachelor_flag", "BA|BS") %>% 
  create_var("degree", "jd_flag", "JD") %>% 
  create_var("degree", "mba_flag", "MBA") %>% 
  create_var("degree", "master_flag", "MA|MS|MPA|MFA|Master") %>% 
  create_var("degree", "md_flag", "MD|DDS") %>% 
  create_var("degree", "phd_flag", "PhD") %>% 
  create_var("prof_title", "law_flag", "Lawyer|Law|law|Attorney|attorney") %>% 
  mutate(ability_index = bachelor_flag + jd_flag + mba_flag + master_flag + md_flag + phd_flag + law_flag)

final_data <- bio_data_family %>% 
  left_join(fundraising_data, by = c("general_election_id", "primary_election_id", "bonica.rid", "candidate", "candidate_lname"))

data_summary <- final_data %>% 
  group_by(gender) %>% 
  summarise(n = n(),
            mean_n_children = mean(children),
            median_n_children= median(children),
            mean_aility_index = mean(ability_index),
            median_ability_index = median(ability_index)) 
