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

# Run ---------------------------------------------------------------------
final_data <- read_feather(paste0(gender, "14_rd_data.feather"))

data_to_run <- final_data %>% 
  filter(abs(female_vote_margin_general) <= 0.1)

outcome = "pct_y_p_f"
indep_vars = c("female_winner_general", "female_vote_margin_general", "female_winner_general:female_vote_margin_general")
fml = as.formula(paste0(outcome, "~", paste0(indep_vars, collapse="+")))

ols_result = feols(fml, data_to_run, se = "hetero")
summary(ols_result)
