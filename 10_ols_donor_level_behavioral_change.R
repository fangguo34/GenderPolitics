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
rd_data_raw <- read_feather(paste0(gender, "06_primary_fundraising_battle_sexes.feather")) %>% 
  group_by(general_election_id) %>% 
  mutate(n_i = sum(Incum.Chall == "I"),
         n_c = sum(Incum.Chall == "C"),
         n_o = sum(Incum.Chall == "O"),
         n_blank = sum(Incum.Chall == "")) %>% 
  ungroup() %>% 
  mutate(Incum.Chall = ifelse(n_i > 0 & Incum.Chall != "I", "C",
                              ifelse(n_i == 0 & Incum.Chall != "O", "O", Incum.Chall))) 

# %>% 
#   filter(n_i == 0) %>% 
#   filter(abs(female_primary_vote_margin) <= 0.1)

# Import ------------------------------------------------------------------
donor_level_data_raw <- read_feather(paste0(gender, "09_contributions_final.feather")) %>% 
  filter(contributor.category.specific %in% c("F", "M")) %>%
  filter(general_election_id %in% rd_data_raw$general_election_id) %>% 
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

donor_level_data <- donor_level_data_raw %>% 
  mutate(MaleDonor = ifelse(contributor.category.specific == "M", 1, 0),
         LogChangeWinner = log1p(donation_to_winner_g) - log1p(donation_to_winner_p))


# Plot --------------------------------------------------------------------
mean_over_the_years <- donor_level_data %>% 
  mutate(year = substr(general_election_id, 1, 4)) %>% 
  group_by(year) %>% 
  summarise(mean_amount_p = mean(donation_to_loser_p + donation_to_winner_p),
            mean_amount_g = mean(donation_to_loser_g + donation_to_winner_g))

# OLS ---------------------------------------------------------------------
donor_level_data_reg <- donor_level_data %>% 
  filter(donation_to_winner_p + donation_to_loser_p > 0)

mean(donor_level_data_reg$donation_to_winner_p + donor_level_data_reg$donation_to_loser_p)
# mean(donor_level_data_reg$donation_to_winner_g)

# test <- donor_level_data_reg %>%
#   group_by(MaleDonor) %>%
#   summarise(n_wrong_bet = sum(FavoredLoser == 1),
#             n = n())

# (1)
indep_vars = c("FavoredLoser")
outcome_var = "ChangeWinner"
fe_vars = c("general_election_id")

fml = as.formula(paste0(outcome_var, "~", paste0(indep_vars, collapse="+"), "|", paste0(fe_vars, collapse="+")))
model.1 = feols(fml, cluster = "general_election_id", donor_level_data_reg)
summary(model.1)

# (2)
indep_vars = c("FavoredLoser", "MaleDonor", "FavoredLoser:MaleDonor")
outcome_var = "ChangeWinner"
fe_vars = c("general_election_id")

fml = as.formula(paste0(outcome_var, "~", paste0(indep_vars, collapse="+"), "|", paste0(fe_vars, collapse="+")))
model.2 = feols(fml, cluster = "general_election_id", donor_level_data_reg)
summary(model.2)

# (3)
indep_vars = c("FavoredLoser")
outcome_var = "LogChangeWinner"
fe_vars = c("general_election_id")

fml = as.formula(paste0(outcome_var, "~", paste0(indep_vars, collapse="+"), "|", paste0(fe_vars, collapse="+")))
model.3 = feols(fml, cluster = "general_election_id", donor_level_data_reg)
summary(model.3)

# (4)
indep_vars = c("FavoredLoser", "MaleDonor", "FavoredLoser:MaleDonor")
outcome_var = "LogChangeWinner"
fe_vars = c("general_election_id")

fml = as.formula(paste0(outcome_var, "~", paste0(indep_vars, collapse="+"), "|", paste0(fe_vars, collapse="+")))
model.4 = feols(fml, cluster = "general_election_id", donor_level_data_reg)
summary(model.4)

texreg(list(model.1, model.2, model.3, model.4))