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

files = list.files(contrib)

# Basic Data --------------------------------------------------------------
files = list.files(contrib)

contribution_types_to_include <- read.csv(paste0(import, "dime/contribution_types.csv"), sep = ",", header = TRUE) %>%
  filter(!grepl("Loan|loan|Transfer|transfer", Transaction.type.description) 
         & !Transaction.type %in% c("24A", "24E", "15C"))

women_pacs_list <- read_feather(paste0(tmp, "79_women_pacs_unique.feather"))

# Outliers Data -----------------------------------------------------------
outliers <- read.csv(paste0(gender, "07_rd_data_outliers.csv"))

# Function ----------------------------------------------------------------
subset_dime_transactions <- function(filename) {
  print(filename)
  
  # Basic Info
  cycle_year = as.numeric(str_extract(filename, "\\d{4}+"))
  days <- seq(ymd(paste0(cycle_year, "-01-01")), ymd(paste0(cycle_year, "-12-31")),by="1 day")
  election_day <- days[month(days) == 11 & weekdays(days) == "Tuesday" & as.numeric(format(days, "%d")) <= 7]
  candidates <- data.table(outliers) %>% 
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
    .[bonica.rid %in% candidates$bonica.rid] %>% 
    .[, date := as.Date(date)] %>% 
    .[, bonica.cid := as.character(bonica.cid)] %>% 
    .[, mths_till_general_election := -(interval(date, election_day) %/% months(1))] %>% 
    .[, contributor.category := ifelse(contributor.type == "C", "C", 
                                       ifelse(contributor.gender == "", "U", contributor.gender))] %>%
    .[, contributor.category.specific := ifelse(contributor.name %in% women_pacs_list$most.recent.contributor.name, "FC", contributor.category)] %>% 
    .[order(bonica.rid, date)]
  
  print(class(data_subset$contributor.category))
  print(nrow(data_subset))
  
  return(data_subset)
}

# Run ---------------------------------------------------------------------
files_subset <- files[c(1:14, 16:18, 20)]
contribution_summary <- bind_rows(map(files_subset, subset_dime_transactions)) 
write_feather(contribution_summary, paste0(gender, "16_contributions_outliers.feather"))




