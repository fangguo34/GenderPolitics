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
primary_data_raw <- read_feather(paste0(gender, "03_primary_elections_fundraising_panel_data.feather")) %>% 
  group_by(general_election_id) %>% 
  mutate(n_i = sum(Incum.Chall == "I"),
         n_c = sum(Incum.Chall == "C"),
         n_o = sum(Incum.Chall == "O"),
         n_blank = sum(Incum.Chall == "")) %>% 
  ungroup() %>% 
  mutate(Incum.Chall = ifelse(n_i > 0 & Incum.Chall != "I", "C",
                              ifelse(n_i == 0 & Incum.Chall != "O", "O", Incum.Chall)))

# test <- primary_data_raw %>%
#   group_by(primary_election_id) %>%
#   summarise(n_i = sum(Incum.Chall == "I"),
#             n_c = sum(Incum.Chall == "C"),
#             n_o = sum(Incum.Chall == "O"),
#             n_blank = sum(Incum.Chall == "")) %>%
#   arrange(primary_election_id) %>% 
#   filter(n_o >0 & n_c >0)


# Subsets of Raw Data -----------------------------------------------------
# missing_data_p <- primary_data_raw %>% 
#   filter(is.na(y_p))
# # About 21% 
# 
# primary_data_has_female <- primary_data_raw %>% 
#   filter(n_f_cands_primary >= 1 & n_cands_primary >= 1)
# 
# primary_data_mix_gender <- primary_data_raw %>% 
#   group_by(general_election_id) %>% 
#   mutate(n_f_cands_general = sum(gender),
#          n_m_cands_general = sum(gender == 0)) %>% 
#   ungroup() %>% 
#   filter(n_f_cands_general >= 1 & n_m_cands_general >= 1)

primary_panel_data <- primary_data_raw %>% 
  replace(is.na(.), 0) %>% 
  group_by(primary_election_id, district, party, cycle) %>% 
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
            pct_n_p_m = sum(n_p_m)/(sum(n_p_f) + sum(n_p_m)),
            OpenSeat = as.numeric(sum(Incum.Chall == "O") > 0),
            NumberFemaleCands = sum(gender),
            NumberCands = n(),
            PrimaryMargin = mean(win_margin_primary)) %>% 
  ungroup() %>% 
  mutate(district_party = paste0(district, "_", party),
         FemaleCand = as.numeric(NumberFemaleCands > 0),
         total_y_p_ind = total_y_p_f + total_y_p_m,
         log_total_y_p = log1p(total_y_p),
         log_total_y_p_f = log1p(total_y_p_f),
         log_total_y_p_m = log1p(total_y_p_m),
         log_total_y_p_fcc = log1p(total_y_p_fcc),
         log_total_n_p = log1p(total_n_p),
         log_total_n_p_f = log1p(total_n_p_f),
         log_total_n_p_m = log1p(total_n_p_m),
         log_total_n_p_fcc = log1p(total_n_p_fcc))

write.dta(primary_panel_data, paste0(gender, "04_primary_panel_data.dta"))

# Summary Stats District Level --------------------------------------------
district_summary <- primary_panel_data %>% 
  group_by(FemaleCand) %>% 
  summarise(mean_total_y_p = mean(total_y_p))




# Histogram ---------------------------------------------------------------
ggplot(primary_panel_data, aes(x = log_total_y_p_f)) +
  geom_histogram()

# test <- primary_panel_data %>% 
#   filter(total_y_p == 0)
       
# Summary Stats -----------------------------------------------------------------
primary_person_cycle_summary_1 <- primary_data_raw %>% 
  replace(is.na(.), 0) %>% 
  group_by(gender) %>% 
  summarise(mean_y_p = mean(y_p),
            mean_y_p_f = mean(y_p_f),
            mean_y_p_m = mean(y_p_m),
            mean_y_p_fcc = mean(y_p_fcc),
            mean_primary_voteshr = mean(candpct),
            prob_win_primary = mean(candpct == p0),
            shr_incum = mean(Incum.Chall == "I"),
            shr_chall = mean(Incum.Chall == "C"),
            shr_open = mean(Incum.Chall == "O"),
            n_person_cycle = n())
write.csv(primary_person_cycle_summary_1, paste0(gender, "04_primary_person_cycle_summary.csv"), row.names = FALSE)

primary_district_cycle_summary_1 <- primary_panel_data %>% 
  group_by(FemaleCand) %>% 
  summarise(mean_total_y_p = mean(total_y_p),
            mean_total_y_p_f = mean(total_y_p_f),
            mean_total_y_p_m = mean(total_y_p_m),
            mean_total_y_p_fcc = mean(total_y_p_fcc),
            mean_primary_margin = mean(PrimaryMargin),
            mean_n_cands = mean(NumberCands),
            mean_open_seats = mean(OpenSeat),
            n_district_cycle = n())
write.csv(primary_district_cycle_summary_1, paste0(gender, "04_primary_district_cycle_summary.csv"), row.names = FALSE)

# within the primary have female districts 
primary_data_has_female_summary <- primary_data_has_female %>% 
  group_by(gender) %>% 
  summarise(mean_y_p = mean(y_p),
            mean_y_p_f = mean(y_p_f),
            mean_y_p_m = mean(y_p_m),
            mean_y_p_fcc = mean(y_p_fcc),
            mean_primary_voteshr = mean(candpct),
            prob_win_primary = mean(candpct == p0),
            shr_incum = mean(Incum.Chall == "I"),
            shr_chall = mean(Incum.Chall == "C"),
            shr_open = mean(Incum.Chall == "O"),
            n_person_cycle = n())
write.csv(primary_data_has_female_summary, paste0(gender, "04_primary_data_candidate_cycle_has_female_summary.csv"), row.names = FALSE)

# mix gender
primary_data_mix_gender_summary <- primary_data_mix_gender %>% 
  group_by(gender) %>% 
  summarise(mean_y_p = mean(y_p),
            mean_y_p_f = mean(y_p_f),
            mean_y_p_m = mean(y_p_m),
            mean_y_p_fcc = mean(y_p_fcc),
            mean_primary_voteshr = mean(candpct),
            prob_win_primary = mean(candpct == p0),
            shr_incum = mean(Incum.Chall == "I"),
            shr_chall = mean(Incum.Chall == "C"),
            shr_open = mean(Incum.Chall == "O"),
            n_person_cycle = n())
write.csv(primary_data_mix_gender_summary, paste0(gender, "04_primary_data_candidate_cycle_mix_gender_summary.csv"), row.names = FALSE)


# Summary Plots -----------------------------------------------------------
primary_person_cycle_summary_2 <- primary_data_raw %>% 
  group_by(cycle, gender) %>% 
  summarise(mean_y_p = mean(y_p),
            mean_y_p_f = mean(y_p_f),
            mean_y_p_m = mean(y_p_m),
            mean_y_p_fcc = mean(y_p_fcc),
            mean_primary_voteshr = mean(candpct),
            prob_win_primary = mean(candpct == p0),
            shr_incum = mean(Incum.Chall == "I"),
            shr_chall = mean(Incum.Chall == "C"),
            shr_open = mean(Incum.Chall == "O"),
            n_person_cycle = n()) %>% 
  arrange(cycle, gender) %>% 
  mutate(gender = ifelse(gender == 0, "M", "F"))

for (i in colnames(primary_person_cycle_summary_2)[3:12]) {
  ggplot(primary_person_cycle_summary_2, aes_string(x = "cycle", y = i, color = "gender")) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = seq(1980, 2018, by = 4)) +
    scale_y_continuous(labels = scales::comma) +
    ggtitle(i)
  ggsave(paste0(gender, "plots/04_primary_person_cycle_summary_by_year_", i, ".png"))
} 

primary_district_cycle_summary_2 <- primary_panel_data %>% 
  group_by(cycle, FemaleCand) %>% 
  summarise(n_races = n(),
            mean_total_y_p = mean(total_y_p),
            mean_total_y_p_f = mean(total_y_p_f),
            mean_total_y_p_m = mean(total_y_p_m),
            mean_total_y_p_fcc = mean(total_y_p_fcc),
            mean_primary_margin = mean(PrimaryMargin),
            mean_n_cands = mean(NumberCands),
            mean_open_seats = mean(OpenSeat)) %>% 
  arrange(cycle, FemaleCand) %>% 
  mutate(FemaleCand = ifelse(FemaleCand == 0, "M", "F"))

for (i in colnames(primary_district_cycle_summary_2)[3:10]) {
  ggplot(primary_district_cycle_summary_2, aes_string(x = "cycle", y = i, color = "FemaleCand")) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = seq(1980, 2018, by = 4)) +
    scale_y_continuous(labels = scales::comma) +
    ggtitle(i)
  ggsave(paste0(gender, "plots/04_primary_district_cycle_summary_by_year_", i, ".png"))
} 

# Create Output Table to fill up ------------------------------------------
outcomes_list <- c(colnames(primary_panel_data)[5:18], paste0("log(", colnames(primary_panel_data)[5:10], ")"))
indep_list <- c("FemaleCand", "NumberFemaleCands")
covars_list <- c(0, 1)

n = length(outcomes_list)*length(indep_list)*length(covars_list)

output_table <- data.frame(outcome_var = rep(outcomes_list, times = length(indep_list)*length(covars_list)),
                           indep_var = rep(rep(indep_list, each = length(outcomes_list)), times = length(covars_list)),
                           covars = rep(covars_list, each = length(outcomes_list )*length(indep_list)),
                           coef = numeric(n),
                           se = numeric(n),
                           tval = numeric(n),
                           pval = numeric(n),
                           nobs = numeric(n),
                           stringsAsFactors = FALSE)


# Mass Regressions for Primary Elections -------------------------------------------------------------
reg_data = primary_panel_data %>% 
  filter(total_y_p_f > 0 & total_y_p_m > 0)

run_reg <- function(outcome, indep, covars, data = reg_data) {
  model_output <- tryCatch(
    {if (covars == 0) {
      indep_vars = c(indep)
      } else if (covars == 1) {
      indep_vars = c(indep, 
                     "PrimaryMargin", paste0(indep, ":PrimaryMargin"),
                     "NumberCands", paste0(indep, ":NumberCands"))
      }
      
      fe_vars = c("district_party", "cycle")
      outcome_var = outcome
      
      fml = as.formula(paste0(outcome_var, "~", paste0(indep_vars, collapse="+"), "|", paste0(fe_vars, collapse="+")))
      model = feols(fml, cluster = "district_party", data)
      
      return(c(model$coeftable[1,], model$nobs))
    }
    ,
    error = function(cond) {
      message(paste0("Here's the original error message:", cond))
      return(c(0,0,0,0,0))
    },
    warning = function(cond) {
      message(paste0("Here's the original error message:", cond)) 
      return(c(0,0,0,0,0))
    }
  )
  
  return(model_output)
}

for (i in 1:nrow(output_table)) {
  print(i)
  output <- run_reg(output_table$outcome_var[i], output_table$indep_var[i], output_table$covars[i], reg_data)
  output_table[i,4:7] <- output[1:4]
  output_table$nobs[i] <- output[5]
  
}

output_table <- output_table %>% 
  mutate(significance = as.numeric(pval <= 0.05))

write.csv(output_table, paste0(gender, "04_primary_election_regression_outputs.csv"), row.names = FALSE)

# Specific Election-Year Regression --------------------------------------------
run_primary_election_year_reg <- function(outcome, reg_data) {
  # indep_vars = c("FemaleCand",
  #                "PrimaryMargin",
  #                "NumberCands", 
  #                "OpenSeat")
    indep_vars = c("FemaleCand",
                 "PrimaryMargin", "FemaleCand:PrimaryMargin",
                 "NumberCands", "FemaleCand:NumberCands",
                 "OpenSeat", "FemaleCand:OpenSeat")
  # indep_vars = c("FemaleCand",
  #                "NumberCands", "FemaleCand:NumberCands",
  #                "OpenSeat", "FemaleCand:OpenSeat")
  
  fe_vars = c("district_party", "cycle")
  outcome_var = outcome
  
  fml = as.formula(paste0(outcome_var, "~", paste0(indep_vars, collapse="+"), "|", paste0(fe_vars, collapse="+")))
  model = feols(fml, cluster = "district_party", reg_data)
  
  return(model) 
  
}

reg_data = primary_panel_data 

# %>% 
#   filter(party == "200")

model.1 <- run_primary_election_year_reg("log_total_y_p", reg_data)
model.2 <- run_primary_election_year_reg("log_total_y_p_f", reg_data)
model.3 <- run_primary_election_year_reg("log_total_y_p_m", reg_data)
model.4 <- run_primary_election_year_reg("log_total_y_p_fcc", reg_data)
texreg(list(model.1, model.2, model.3, model.4))

run_primary_election_year_reg("pct_y_p_f", reg_data)


model.1 <- run_primary_election_year_reg("log_total_n_p", reg_data)
model.2 <- run_primary_election_year_reg("log_total_n_p_f", reg_data)
model.3 <- run_primary_election_year_reg("log_total_n_p_m", reg_data)
model.4 <- run_primary_election_year_reg("log_total_n_p_fcc", reg_data)
texreg(list(model.1, model.2, model.3, model.4))

run_primary_election_year_reg("pct_n_p_f", reg_data)




# Specific Candidate-Year Regression --------------------------------
run_primary_candidate_year_reg <- function(outcome, ideology = "None", first_year, last_year) {
  # reg_data = primary_data_raw %>% 
  #   mutate(FemaleCand = ifelse(gender == 1, 1, 0),
  #          PrimaryMargin = win_margin_primary,
  #          NumberCands = n_cands_primary,
  #          OpenSeat = ifelse(Incum.Chall == "O", 1, 0),
  #          Incumbent = ifelse(Incum.Chall == "I", 1, 0),
  #          CFScore = recipient.cfscore,
  #          Extreme = abs(recipient.cfscore),
  #          district_party = paste0(district, "_", party)) %>% 
  #   filter(!is.na(CFScore) & cycle >= first_year & cycle <= last_year)
  
  reg_data = primary_data_raw %>% 
    filter(!is.na(recipient.cfscore)) %>%
    replace(is.na(.), 0) %>% 
    mutate(FemaleCand = ifelse(gender == 1, 1, 0),
           PrimaryMargin = win_margin_primary,
           NumberCands = n_cands_primary,
           OpenSeat = ifelse(Incum.Chall == "O", 1, 0),
           Incumbent = ifelse(Incum.Chall == "I", 1, 0),
           CFScore = recipient.cfscore,
           Extreme = abs(recipient.cfscore),
           winner_primary = as.numeric(candpct == p0),
           district_party = paste0(district, "_", party)) %>% 
    mutate(log_y_p = log1p(y_p),
           log_y_p_f = log1p(y_p_f),
           log_y_p_m = log1p(y_p_m),
           log_y_p_fcc = log1p(y_p_fcc),
           pct_y_p_m_ind = y_p_m/(y_p_m + y_p_f)) %>% 
    filter(cycle >= first_year & cycle <= last_year)
  
  if (ideology == "None") {
    # indep_vars = c("FemaleCand",
    #                "PrimaryMargin", "FemaleCand:PrimaryMargin",
    #                "NumberCands", "FemaleCand:NumberCands",
    #                "OpenSeat", "FemaleCand:OpenSeat",
    #                "Incumbent", "FemaleCand:Incumbent")
    
    indep_vars = c("FemaleCand", "Extreme", "Incumbent", "FemaleCand:Incumbent")
    
    
  } else {
    # indep_vars = c("FemaleCand",
    #                "PrimaryMargin", "FemaleCand:PrimaryMargin",
    #                "NumberCands", "FemaleCand:NumberCands",
    #                "OpenSeat", "FemaleCand:OpenSeat",
    #                "Incumbent", "FemaleCand:Incumbent",
    #                ideology, paste0("FemaleCand:", ideology))
    
    indep_vars = c("FemaleCand", "Extreme", "Incumbent", "FemaleCand:Incumbent", ideology)
  }
  
  # fe_vars = c("district_party", "cycle")
  fe_vars = c("primary_election_id")
  outcome_var = outcome
  
  fml = as.formula(paste0(outcome_var, "~", paste0(indep_vars, collapse="+"), "|", paste0(fe_vars, collapse="+")))
  model = feols(fml, cluster = "primary_election_id", reg_data)
  
  return(model) 
  
}


model_1 <- run_primary_candidate_year_reg("log_y_p", "None", 1980, 2018)
model_2 <- run_primary_candidate_year_reg("log_y_p_f", "None", 1980, 2018)
model_3 <- run_primary_candidate_year_reg("log_y_p_m", "None", 1980, 2018)
model_4 <- run_primary_candidate_year_reg("log_y_p_fcc", "None", 1980, 2018)

run_primary_candidate_year_reg("pct_y_p_m_ind", "None", 1980, 2018)
run_primary_candidate_year_reg("winner_primary", "None", 1980, 2018)


model_1 <- run_primary_candidate_year_reg("log_y_p", "Extreme", 1980, 2018)
model_2 <- run_primary_candidate_year_reg("log_y_p_f", "Extreme", 1980, 2018)
model_3 <- run_primary_candidate_year_reg("log_y_p_m", "Extreme", 1980, 2018)
model_4 <- run_primary_candidate_year_reg("log_y_p_fcc", "Extreme", 1980, 2018)

texreg(list(model_1, model_2, model_3, model_4))


# Test --------------------------------------------------------------------
# summary(run_primary_candidate_year_reg("log_y_p", "Extreme", 1980, 2018))
# 
# 
# summary(run_primary_candidate_year_reg("log_y_p", "CFScore", 1980, 2018))
# 
# summary(run_primary_candidate_year_reg("log_y_p_fcc", "None", 1980, 2018))
# summary(run_primary_candidate_year_reg("log_y_p_fcc", "Extreme", 1980, 2018))
# summary(run_primary_candidate_year_reg("log_y_p_fcc", "CFScore", 1980, 2018))




# reg_data = primary_panel_data %>% 
#   filter(total_y_p_f > 0 & total_y_p_m > 0 & cycle == 2018) %>% 
#   mutate(state = str_extract(district, "[A-Z]+"))
# 
# run_simple_reg <- function(outcome) {
#   indep_vars = c("FemaleCand", "PrimaryMargin", "FemaleCand:PrimaryMargin","NumberCands", "FemaleCand:NumberCands")
#   fe_vars = c("state", "party")
#   outcome_var = outcome
# 
#   fml = as.formula(paste0(outcome_var, "~", paste0(indep_vars, collapse="+"), "|", paste0(fe_vars, collapse="+")))
#   model = feols(fml, cluster = "state", reg_data)
#   
#   return(model) 
#   
# }
# 
# texreg(list(run_simple_reg("total_y_p_f"),
#             run_simple_reg("log(total_y_p_f)"),
#             run_simple_reg("pct_y_p_f")))
# 
# texreg(list(run_simple_reg("total_y_p_m"),
#             run_simple_reg("log(total_y_p_m)"),
#             run_simple_reg("pct_y_p_m")))

# model <- lm(NumberFemaleCands ~ PrimaryMargin, reg_data)
# summary(model)
# 
# model <- lm(PrimaryMargin ~ FemaleCand + NumberCands, reg_data)
# summary(model)

