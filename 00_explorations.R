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

# Import Primary Data -------------------------------------------------------
primary_elections <- read_feather(paste0(gender, "01_primary_elections_final.feather")) 
missing_rid_p <- primary_elections %>% 
  filter(is.na(bonica.rid))
print(nrow(missing_rid_p)/nrow(primary_elections))

primary_fundraising <- read_feather(paste0(gender, "03_primary_elections_fundraising_panel_data.feather")) 
missing_contrib_p <- primary_fundraising %>% 
  filter(is.na(y_p))
print(nrow(missing_contrib_p)/nrow(primary_fundraising))

# Import General Data -----------------------------------------------------
general_elections <- read_feather(paste0(gender, "02_general_elections_final.feather"))
missing_rid_g <- general_elections %>% 
  filter(is.na(bonica.rid))
print(nrow(missing_rid_g)/nrow(general_elections))

general_fundraising <- read_feather(paste0(gender, "03_general_elections_fundraising_panel_data.feather")) 
missing_contrib_g <- general_fundraising %>% 
  filter(is.na(y_g))
print(nrow(missing_contrib_g)/nrow(general_fundraising))

# test <- general_fundraising %>% 
#   filter(is.na(bonica.rid))
