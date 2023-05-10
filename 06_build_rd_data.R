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

# Data (Hide) --------------------------------------------------------------------
primary_data_raw <- read_feather(paste0(gender, "03_primary_elections_fundraising_panel_data.feather")) %>% 
  group_by(primary_election_id, candidate, bonica.rid) %>% 
  slice_max(order_by = candpct, n = 1)

primary_battle_sexes <- read_feather(paste0(gender, "01_primary_elections_battle_sexes.feather")) %>% 
  group_by(primary_election_id) %>% 
  slice_max(order_by = candpct, n = 2) %>% 
  select(general_election_id, primary_election_id, candidate, bonica.rid) %>% 
  distinct()

primary_fundraising_battle_sexes <- primary_battle_sexes %>% 
  left_join(primary_data_raw, by = c("general_election_id", "primary_election_id", "candidate", "bonica.rid")) %>% 
  filter(!is.na(bonica.rid)) %>% 
  group_by(primary_election_id) %>% 
  mutate(n_cands_2 = n(),
         n_f_cands_primary_2 = sum(gender == 1),
         n_m_cands_primary_2 = sum(gender == 0)) %>% 
  ungroup() %>% 
  filter(n_f_cands_primary_2 == 1 & n_m_cands_primary_2 == 1) %>% 
  mutate(primary_winner = ifelse(candpct == p0, 1, 0)) %>% 
  group_by(primary_election_id) %>% 
  mutate(female_primary_winner = sum(gender[primary_winner == 1]),
         female_primary_vote_margin = ifelse(female_primary_winner == 1, win_margin_primary, -win_margin_primary))

write_feather(primary_fundraising_battle_sexes, paste0(gender, "06_primary_fundraising_battle_sexes.feather"))


# Import ------------------------------------------------------------------
primary_fundraising_battle_sexes <- read_feather(paste0(gender, "06_primary_fundraising_battle_sexes.feather")) %>% 
  group_by(general_election_id) %>% 
  mutate(n_i = sum(Incum.Chall == "I"),
         n_c = sum(Incum.Chall == "C"),
         n_o = sum(Incum.Chall == "O"),
         n_blank = sum(Incum.Chall == "")) %>% 
  ungroup() %>% 
  mutate(Incum.Chall = ifelse(n_i > 0 & Incum.Chall != "I", "C",
                              ifelse(n_i == 0 & Incum.Chall != "O", "O", Incum.Chall))) %>% 
  group_by(primary_election_id) %>% 
  mutate(n_incum_primary = sum(Incum.Chall == "I")) %>% 
  ungroup() %>% 
  mutate(Incumbent = ifelse(Incum.Chall == "I", 1, 0)) %>%
  ungroup() %>%
  filter(n_i == 0)


# Summary Stats -----------------------------------------------------------
male_advantage <- primary_fundraising_battle_sexes %>% 
  select(primary_election_id, candidate, bonica.rid, gender, female_primary_vote_margin, y_p, y_p_f, y_p_m, y_p_fcc) %>% 
  # filter(abs(female_primary_vote_margin) <= 0.1) %>%
  replace(is.na(.), 0) %>% 
  group_by(primary_election_id, female_primary_vote_margin) %>% 
  summarise(male_advantage_y_p = y_p[gender == 0] - y_p[gender == 1],
            male_advantage_log_y_p = log1p(y_p[gender == 0]) - log1p(y_p[gender == 1]),
            male_advantage_y_p_f = y_p_f[gender == 0] - y_p_f[gender == 1],
            male_advantage_log_y_p_f = log1p(y_p_f[gender == 0]) - log1p(y_p_f[gender == 1])) %>% 
  mutate(subset_flag = ifelse(abs(female_primary_vote_margin) <= 0.1, "Subset", "Other"))

ggplot(male_advantage, aes(x = male_advantage_log_y_p_f, color = subset_flag)) +
    # geom_histogram(binwidth = 1, alpha = 0.5, position="identity") +
    geom_density(alpha=0.6)

ggplot(male_advantage, aes(x = male_advantage_log_y_p_f)) +
  # geom_histogram(binwidth = 1, alpha = 0.5, position="identity") +
  geom_density(alpha=0.6)



summary_stats <- primary_fundraising_battle_sexes %>% 
  # filter(!is.na(recipient.cfscore)) %>% 
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


summary_stats_subset <- primary_fundraising_battle_sexes %>% 
  # filter(!is.na(recipient.cfscore)) %>% 
  replace(is.na(.), 0) %>% 
  filter(abs(female_primary_vote_margin) <= 0.1) %>% 
  group_by(gender) %>% 
  summarise(mean_y_p = mean(y_p),
            median_y_p = median(y_p),
            mean_y_p_f = mean(y_p_f),
            median_y_p_f = median(y_p_f),
            mean_y_p_m = mean(y_p_m),
            median_y_p_m = median(y_p_m),
            mean_y_p_fcc = mean(y_p_fcc),
            median_y_p_fcc = median(y_p_fcc),
            mean_primary_voteshr = mean(candpct),
            prob_win_primary = mean(candpct == p0),
            shr_incum = mean(Incum.Chall == "I"),
            shr_chall = mean(Incum.Chall == "C"),
            shr_open = mean(Incum.Chall == "O"),
            n_person_cycle = n())

summary_stats_subset <- primary_fundraising_battle_sexes %>% 
  replace(is.na(.), 0) %>% 
  filter(primary_winner == 0 & abs(female_primary_vote_margin) <= 0.1) %>% 
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


# Regressions -------------------------------------------------------------
run_reg <- function(outcome = "log_y_p", drop_na = 0, m = 0.1, l = 0) {
  if (drop_na == 1) {
    reg_data = primary_fundraising_battle_sexes %>% 
      filter(!is.na(y_p)) 
      # filter(!is.na(recipient.cfscore))
    
  } else if (drop_na == 0) {
    reg_data = primary_fundraising_battle_sexes %>% 
      filter(!is.na(recipient.cfscore)) %>% 
      replace(is.na(.), 0)
    
  }
  
  reg_data <- reg_data %>% 
    # filter(female_primary_winner == 0) %>% 
    filter(abs(female_primary_vote_margin) <= m
           & abs(female_primary_vote_margin) >= l) %>% 
    mutate(log_y_p = log1p(y_p),
           log_y_p_f = log1p(y_p_f),
           log_y_p_m = log1p(y_p_m),
           log_y_p_fcc = log1p(y_p_fcc),
           extreme = abs(recipient.cfscore)) 
  
  indep_vars = c("gender", "extreme", "gender:extreme")
  fe_vars = c("primary_election_id")
  outcome_var = outcome
  fml = as.formula(paste0(outcome_var, "~", paste0(indep_vars, collapse="+"), "|", paste0(fe_vars, collapse="+")))
  model = feols(fml, cluster = "primary_election_id", reg_data) # when use cluster se, don't use se = hetero
  
  return(model)
  
}


# Run Regressions ---------------------------------------------------------
outcome_vars_list <- c("log_y_p", "log_y_p_f", "log_y_p_m", "log_y_p_fcc")
drop_na_list <- c(1, 0)
margins_list <- seq(0.02, 1, by = 0.02)

n = length(outcome_vars_list)*length(drop_na_list)*length(margins_list)
output_table <- data.frame(outcome_var = rep(outcome_vars_list, each = length(margins_list)*length(drop_na_list)),
                           drop_na_var = rep(rep(drop_na_list, each = length(margins_list)), times = length(outcome_vars_list)),
                           margin_var = rep(margins_list, times = length(drop_na_list)*length(outcome_vars_list)),
                           coef = numeric(n),
                           se = numeric(n),
                           tval = numeric(n),
                           pval = numeric(n),
                           nobs = numeric(n),
                           significance = numeric(n))


for (i in 1:nrow(output_table)) {
  print(i)
  model <- run_reg(output_table$outcome_var[i], output_table$drop_na_var[i], output_table$margin_var[i])
  output_table[i, 4:7] <- model$coeftable[1,]
  output_table$nobs[i] <- model$nobs
}

output_table$significance = as.numeric(output_table$pval <= 0.05)

write.csv(output_table, paste0(gender, "06_ols_output_open_seats_extreme_interact.csv"), row.names = FALSE)

# Specific Regressions ----------------------------------------------------
model_1 <- run_reg("log_y_p", 0, 1)
model_2 <- run_reg("log_y_p_f", 0, 1)
model_3 <- run_reg("log_y_p_m", 0, 1)
model_4 <- run_reg("log_y_p_fcc", 0, 1)
texreg(list(model_1, model_2, model_3, model_4))

model_1 <- run_reg("log_y_p", 0, 0.1)
model_2 <- run_reg("log_y_p_f", 0, 0.1)
model_3 <- run_reg("log_y_p_m", 0, 0.1)
model_4 <- run_reg("log_y_p_fcc", 0, 0.1)
texreg(list(model_1, model_2, model_3, model_4))


# Spectrum Plot -----------------------------------------------------------
# output_table <- read.csv(paste0(gender, "06_quick_reg_output.csv"))

plot_results_spectrum <- function(data_input, outcome_input, drop_na_input, lbound, ubound) {
  data <- data_input %>% 
    filter(outcome_var == outcome_input
           & drop_na_var == drop_na_input
           & margin_var >= lbound
           & margin_var <= ubound) %>% 
    mutate(ci_lower = coef - 1.96*se,
           ci_upper = coef + 1.96*se)
  
  ggplot(data, aes(x = margin_var, y = coef)) +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_ribbon(aes(ymin=ci_lower,
                    ymax=ci_upper), alpha=.3) +
    scale_x_continuous(breaks = seq(0.02, 1, by = 0.1)) +
    ggtitle(paste0("Coefficient Estimate: ", outcome_input)) + 
    xlab("Winning Margin") + ylab("Estimate")
  
  ggsave(paste0(gender, "plots/06_spectrum_open_seats_extreme_", outcome_input, "_", drop_na_input, ".png"))
  
}

outcome_vars_list <- c("log_y_p", "log_y_p_f", "log_y_p_m", "log_y_p_fcc")

for (i in outcome_vars_list) {
  plot_results_spectrum(output_table, i, 0, 0.02, 1)

}

# Test --------------------------------------------------------------------
# tmp_data <- primary_fundraising_battle_sexes  %>%
#   select(primary_election_id, candidate, bonica.rid, gender, candpct, primary_winner, female_primary_winner, female_primary_vote_margin)

# second_source <- tmp_data %>% 
#   select(primary_election_id, candidate, female_primary_vote_margin)

# second_source <- primary_fundraising_battle_sexes %>% 
#   select(primary_election_id, candidate, primary_winner)

# 
# test <- primary_fundraising_battle_sexes %>% 
#   filter(primary_election_id == "2014NC05_100")

# test <- primary_fundraising_battle_sexes %>% 
#   mutate(na_fund_flag = ifelse(is.na(y_p), 1, 0)) %>% 
#   group_by(na_fund_flag) %>% 
#   summarise(n = n(),
#             mean_gender = mean(gender),
#             mean_candpct = mean(candpct),
#             mean_p_win = mean(primary_winner))

# Check how many NAs are the losers of the primary 
# mean candpct of NAs and Non NAs


# relevant_elections <- read_feather(paste0(tmp, "86_election_returns_join_fg_0114.feather")) %>%
#   mutate(primary_winner = ifelse(candpct == p0, 1, 0)) %>%
#   mutate(female_vote_margin = ifelse(gender == 1 & primary_winner == 1, diff,
#                                      ifelse(gender == 0 & primary_winner == 0, diff, -diff))) %>%
#   select(district_mod_dime, candidate, female_vote_margin)
# 
# 
# combined <- relevant_elections %>%
#   full_join(second_source, by = c("district_mod_dime" = "primary_election_id", "candidate" = "candidate")) %>% 
#   mutate(match = as.numeric(female_vote_margin == female_primary_vote_margin))
# 
# combined_tmp <- combined %>% 
#   filter(is.na(primary_winner.x))
# 
# relevant_elections_winners <- relevant_elections %>%
#   filter(primary_winner == 1)
