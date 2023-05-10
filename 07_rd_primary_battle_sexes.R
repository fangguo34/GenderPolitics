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

# Import ------------------------------------------------------------------
rd_data_raw <- read_feather(paste0(gender, "06_primary_fundraising_battle_sexes.feather")) %>% 
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


# Functions ---------------------------------------------------------------
prepare_data <- function(rd_data_raw) {
  rd_data <- rd_data_raw %>% 
    filter(primary_winner == 1) %>% 
    # filter(!is.na(y_p)) %>%
    replace(is.na(.), 0) %>%
    mutate(log_y_p = log1p(y_p),
           log_y_p_f = log1p(y_p_f),
           log_y_p_m = log1p(y_p_m),
           log_y_p_fcc = log1p(y_p_fcc),
           log_y_g = log1p(y_g),
           log_y_g_f = log1p(y_g_f),
           log_y_g_m = log1p(y_g_m),
           log_y_g_fcc = log1p(y_g_fcc),
           dlog_y = log1p(y_g) - log1p(y_p),
           dlog_y_f = log1p(y_g_f) - log1p(y_p_f),
           dlog_y_m = log1p(y_g_m) - log1p(y_p_m),
           dlog_y_fcc = log1p(y_g_fcc) - log1p(y_p_fcc),
           dy_m = y_g_m - y_p_m,
           pct_p_f = y_p_f/y_p,
           pct_p_m = y_p_m/y_p,
           pct_p_fcc = y_p_fcc/y_p,
           pct_p_f_ind = y_p_f/(y_p_m + y_p_f),
           pct_p_m_ind = y_p_m/(y_p_m + y_p_f),
           pct_g_f = y_g_f/y_g,
           pct_g_m = y_g_m/y_g,
           pct_g_fcc = y_g_fcc/y_g,
           pct_g_f_ind = y_g_f/(y_g_m + y_g_f),
           pct_g_m_ind = y_g_m/(y_g_m + y_g_f)) 
  
  return(rd_data)
}

set_up_output_table <- function(data_input, outcome_vars_list, bws_list, specification_list) {
  n = length(outcome_vars_list)*length(bws_list)*length(specification_list)
  rdd_output <- data.frame(ID = 1:n,
                           outcome = character(n),
                           bws = numeric(n),
                           method = numeric(n),
                           specification = character(n),
                           coef = numeric(n),
                           se = numeric(n),
                           pval = numeric(n),
                           actual_bws = numeric(n),
                           mean_treated = numeric(n),
                           mean_control = numeric(n),
                           nobs = numeric(n),
                           stringsAsFactors = FALSE)
  
  rdd_output$outcome <- rep(outcome_vars_list, each = length(bws_list)*length(specification_list))
  rdd_output$bws <- rep(rep(bws_list, each = length(specification_list)), times = length(outcome_vars_list))
  rdd_output$specification <- rep(specification_list, times = length(outcome_vars_list)*length(bws_list))
  rdd_output$method <- sub("\\-.*", "", rdd_output$specification)
  
  for (i in 1:nrow(rdd_output)) {
    print(i)
    
    output_all <- run_analysis(data = data_input,
                               outcome = rdd_output$outcome[i],
                               bws = rdd_output$bws[i],
                               method = rdd_output$method[i],
                               specification = rdd_output$specification[i])
    
    rdd_output$coef[i] = output_all[1]
    rdd_output$se[i] = output_all[2]
    rdd_output$pval[i] = output_all[3]
    rdd_output$actual_bws[i] = output_all[4]
    rdd_output$mean_treated[i] = output_all[5]
    rdd_output$mean_control[i] = output_all[6]
    rdd_output$nobs[i] = output_all[7]
    
  }
  
  rdd_output$significance = as.numeric(rdd_output$pval <= 0.05)
  
  return(rdd_output)
}

run_analysis <- function(data, outcome, bws, method, specification) {
  k = "uniform"
  # k = "triangular"
  
  data_to_run <- data %>% 
    filter(abs(female_primary_vote_margin) <= bws
           & !is.na(!!sym(outcome))
           & !is.infinite(!!sym(outcome)))
  nobs = nrow(data_to_run) 
  
  x = data_to_run[["female_primary_vote_margin"]]
  y = data_to_run[[outcome]]
  
  mean_summary <- data_to_run %>% 
    group_by(female_primary_winner) %>% 
    summarise(mean_outcome = mean(!!sym(outcome)))
  
  mean_treated <- mean_summary$mean_outcome[mean_summary$female_primary_winner == 1]
  mean_control <- mean_summary$mean_outcome[mean_summary$female_primary_winner == 0]
  
  output_all <- tryCatch(
    {if (method == "RD") {
      if (bws == 1) {
        rd_result <- rdrobust(y, x, c = 0, p = 1, q = 2, kernel = k)
        
        if (specification == "RD-C") {
          coef <- rd_result$coef[1]
          se <- rd_result$se[1]
          pval <- rd_result$pv[1]
          actual_bws <- rd_result$bws[1,1]
          
        } else if (specification == "RD-BC") {
          coef <- rd_result$coef[2]
          se <- rd_result$se[2]
          pval <- rd_result$pv[2]
          actual_bws <- rd_result$bws[2,1]
          
        } else if (specification == "RD-R") {
          coef <- rd_result$coef[3]
          se <- rd_result$se[3]
          pval <- rd_result$pv[3]
          actual_bws <- rd_result$bws[2,1]
          
        }
        
      } else if (bws != 1) {
        rd_result <- rdrobust(y, x, c = 0, h = bws, p = 1, q = 2, kernel = k)
        
        if (specification == "RD-C") {
          coef <- rd_result$coef[1]
          se <- rd_result$se[1]
          pval <- rd_result$pv[1]
          actual_bws <- rd_result$bws[1,1]
          
        } else if (specification == "RD-BC") {
          coef <- rd_result$coef[2]
          se <- rd_result$se[2]
          pval <- rd_result$pv[2]
          actual_bws <- rd_result$bws[2,1]
          
        } else if (specification == "RD-R") {
          coef <- rd_result$coef[3]
          se <- rd_result$se[3]
          pval <- rd_result$pv[3]
          actual_bws <- rd_result$bws[2,1]
        }
      }
      
    } else if (method == "OLS") {
      if (specification == "OLS-simple") {
        fml = as.formula(paste0(outcome, " ~ female_primary_winner"))
        
        ols_result = feols(fml, data_to_run, se = "hetero")
        coef = ols_result$coeftable[2,1]
        se = ols_result$coeftable[2,2]
        pval = ols_result$coeftable[2,4]
        actual_bws = bws
        
      } else if (specification == "OLS-FE1") {
        indep_vars = c("female_primary_winner", "factor(party)", "factor(Incum.Chall)")
        fml = as.formula(paste0(outcome, "~", paste0(indep_vars, collapse="+")))
        
        ols_result = feols(fml, data_to_run, se = "hetero")
        coef = ols_result$coeftable[2,1]
        se = ols_result$coeftable[2,2]
        pval = ols_result$coeftable[2,4]
        actual_bws = bws
        
      } else if (specification == "OLS-FE2") {
        indep_vars = c("female_primary_winner", "factor(party)", "factor(Incum.Chall)")
        fe_vars = c("cycle")
        fml = as.formula(paste0(outcome, "~", paste0(indep_vars, collapse="+"), "|", paste0(fe_vars, collapse="+")))
        
        ols_result = feols(fml, data_to_run, se = "hetero")
        coef = ols_result$coeftable[1,1]
        se = ols_result$coeftable[1,2]
        pval = ols_result$coeftable[1,4]
        actual_bws = bws
        
      } else if (specification == "OLS-FE3") {
        indep_vars = c("female_primary_winner", "factor(party)", "factor(Incum.Chall)")
        fe_vars = c("cycle", "state")
        fml = as.formula(paste0(outcome, "~", paste0(indep_vars, collapse="+"), "|", paste0(fe_vars, collapse="+")))
        
        ols_result = feols(fml, data_to_run, se = "hetero")
        coef = ols_result$coeftable[1,1]
        se = ols_result$coeftable[1,2]
        pval = ols_result$coeftable[1,4]
        actual_bws = bws
      }
    } else if (method == "EasyRD") {
      if (specification == "EasyRD-simple") {
        indep_vars = c("female_primary_winner", "female_primary_vote_margin", "female_primary_winner:female_primary_vote_margin")
        fml = as.formula(paste0(outcome, "~", paste0(indep_vars, collapse="+")))
        
        ols_result = feols(fml, data_to_run, se = "hetero")
        coef = ols_result$coeftable[2,1]
        se = ols_result$coeftable[2,2]
        pval = ols_result$coeftable[2,4]
        actual_bws = bws
        
      } else if (specification == "EasyRD-covars1") {
        indep_vars = c("female_primary_winner", "female_primary_vote_margin", "female_primary_winner:female_primary_vote_margin",
                       "factor(party)")
        fml = as.formula(paste0(outcome, "~", paste0(indep_vars, collapse="+")))
        
        ols_result = feols(fml, data_to_run, se = "hetero")
        coef = ols_result$coeftable[2,1]
        se = ols_result$coeftable[2,2]
        pval = ols_result$coeftable[2,4]
        actual_bws = bws
        
      } else if (specification == "EasyRD-covars2") {
        indep_vars = c("female_primary_winner", "female_primary_vote_margin", "female_primary_winner:female_primary_vote_margin",
                       "factor(party)")
        fe_vars = c("cycle")
        fml = as.formula(paste0(outcome, "~", paste0(indep_vars, collapse="+"), "|", paste0(fe_vars, collapse="+")))
        
        ols_result = feols(fml, data_to_run, se = "hetero")
        coef = ols_result$coeftable[1,1]
        se = ols_result$coeftable[1,2]
        pval = ols_result$coeftable[1,4]
        actual_bws = bws
        
      } else if (specification == "EasyRD-covars3") {
        indep_vars = c("female_primary_winner", "female_primary_vote_margin", "female_primary_winner:female_primary_vote_margin",
                       "factor(party)")
        fe_vars = c("cycle", "state")
        fml = as.formula(paste0(outcome, "~", paste0(indep_vars, collapse="+"), "|", paste0(fe_vars, collapse="+")))
        
        ols_result = feols(fml, data_to_run, se = "hetero")
        coef = ols_result$coeftable[1,1]
        se = ols_result$coeftable[1,2]
        pval = ols_result$coeftable[1,4]
        actual_bws = bws
        
      }
      
    }
      return(c(coef, se, pval, actual_bws, mean_treated, mean_control, nobs))
    },
    error = function(cond) {
      message(paste0("Here's the original error message:", cond))
      return(c(0,0,0,0,0,0,0))
    },
    warning = function(cond) {
      message(paste0("Here's the original error message:", cond)) 
      return(c(0,0,0,0,0,0,0))
    })
  
  # output_all <- c(coef, se, pval, actual_bws, mean_treated, mean_control, nobs)
  return(output_all)
}

run_specific_rd <- function(outcome, bws) {
  data_to_run <- rd_data %>% 
    filter(abs(female_primary_vote_margin) <= bws) %>% 
    mutate(FemaleWin = female_primary_winner,
           FemaleVoteMargin = female_primary_vote_margin)
  
  indep_vars = c("FemaleWin", "FemaleVoteMargin", "FemaleWin:FemaleVoteMargin")
  fml = as.formula(paste0(outcome, "~", paste0(indep_vars, collapse="+")))
  
  ols_result = feols(fml, data_to_run, se = "hetero")
  return(ols_result)
}

plot_results_spectrum <- function(data_input, specification_input, outcome_input, lbound, ubound, label) {
  data <- data_input %>% 
    filter(outcome == outcome_input
           & specification == specification_input
           & bws >= lbound
           & bws <= ubound) %>% 
    mutate(ci_lower = coef - 1.96*se,
           ci_upper = coef + 1.96*se)
  
  ggplot(data, aes(x = bws, y = coef)) +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_ribbon(aes(ymin=ci_lower,
                    ymax=ci_upper), alpha=.3) +
    scale_x_continuous(breaks = seq(0.02, 0.2, by = 0.1)) +
    ggtitle(paste0("Coefficient Estimate: ", outcome_input)) + 
    xlab("Bandwidth") + ylab("Estimate")
  
  ggsave(paste0(gender, "plots/07_spectrum_", specification_input, "_", label, "_", outcome_input, ".png"))
  
}

plot_rd <- function(data_final_plot, i, bws, goal) {
  k = "uniform"
  x = data_final_plot$female_primary_vote_margin
  y = data_final_plot[[i]]
  
  png(file = paste0(gender, "plots/07_rd_plot_", goal, "_", i, ".png"), 
      width=730, height=470)
  rdplot(y, x, c = 0, h = bws, p = 1, kernel = k, nbins = 50,
         y.lim = c(-5, 5),
         x.label = "Female Vote Margin", 
         y.label = i,
         title = paste0(goal, ": ", i))
  dev.off()
}

# Data Sanity Check -----------------------------------------------------------
rd_data <- prepare_data(rd_data_raw)

# general_data <- read.csv(paste0(gender, "08_general_election_voteshare_diff.csv"))

rd_data_select <- rd_data %>%
  select(general_election_id:candpct, cycle, Incum.Chall, party, primary_winner, female_primary_winner, female_primary_vote_margin,
         y_p_m, log_y_p_m, y_g_m, log_y_g_m, dlog_y_m, y_p, y_g) %>%
  filter(abs(female_primary_vote_margin) <= 0.1) %>%
  filter(y_p_m == 0 | y_g_m == 0) %>%
  filter(y_p != 0 & y_g != 0) 
  #
left_join(general_data, by = "general_election_id") %>%
filter(!is.na(voteshare_diff_general))

write.csv(rd_data_select, paste0(gender, "07_rd_data_outliers.csv"), row.names = FALSE)

rd_data_select_2 <- rd_data %>% 
  select(general_election_id:candpct, cycle, Incum.Chall, party, primary_winner, female_primary_winner, female_primary_vote_margin,
         y_p_m, log_y_p_m, y_g_m, log_y_g_m, dlog_y_m, y_p, y_g) %>%
  filter(abs(female_primary_vote_margin) <= 0.1) %>%
  filter(y_p == 0 | y_g == 0)

# check general election competitiveness
general_competitive_summary <- rd_data_select %>% 
  group_by(female_primary_winner) %>% 
  summarise(n = n(),
            mean_votediff_g = mean(voteshare_diff_general),
            median_votediff_g = median(voteshare_diff_general))
  
data_to_run <- rd_data_select %>% 
  mutate(FemaleWin = female_primary_winner,
         FemaleVoteMargin = female_primary_vote_margin)

indep_vars = c("FemaleWin", "FemaleVoteMargin", "FemaleWin:FemaleVoteMargin")
outcome = "voteshare_diff_general"
fml = as.formula(paste0(outcome, "~", paste0(indep_vars, collapse="+")))

ols_result = feols(fml, data_to_run, se = "hetero")
summary(ols_result)

  
ggplot(rd_data_select, aes(x = female_primary_vote_margin, y = dlog_y_m)) +
  geom_point()

ggplot(rd_data, aes(x = female_primary_vote_margin, y = pct_p_m_ind)) +
  geom_point()


ggplot(rd_data, aes(x = female_primary_vote_margin, y = log_y_p_m)) +
  geom_point()

ggplot(rd_data, aes(x = female_primary_vote_margin, y = log_y_g_m)) +
  geom_point()



# Run Massive RD ---------------------------------------------------------------------
rd_data <- prepare_data(rd_data_raw) 

# %>% 
  # filter(!primary_election_id %in% rd_data_select$primary_election_id)
  # 
  # filter(y_p != 0 & y_g != 0)

# filter(y_p_m != 0 & y_g_m != 0)

outcome_vars_list <- c("log_y_p", "log_y_p_f", "log_y_p_m", "log_y_p_fcc",
                       "log_y_g", "log_y_g_f", "log_y_g_m", "log_y_g_fcc",
                       "dlog_y", "dlog_y_f", "dlog_y_m", "dlog_y_fcc",
                       "pct_p_f", "pct_p_m", "pct_p_fcc", "pct_p_f_ind", "pct_p_m_ind",
                       "pct_g_f", "pct_g_m", "pct_g_fcc", "pct_g_f_ind", "pct_g_m_ind")


bws_list <- c(seq(0.01, 0.5, by = 0.01), 1) # 1 stands for CCT optimal bandwidth 
specification_list <- c("RD-C", "RD-BC", "RD-R", "EasyRD-simple", "EasyRD-covars1", "EasyRD-covars2")
# specification_list <- c("RD-C", "RD-BC", "RD-R")

analyses_output <- set_up_output_table(rd_data, outcome_vars_list, bws_list, specification_list)
write.csv(analyses_output, paste0(gender, "07_rd_primary_battle_sexes_output_uniform_open_seats_test.csv"), row.names = FALSE)

# Run Specific RD ---------------------------------------------------------
rd_data <- prepare_data(rd_data_raw)

model.1 <- run_specific_rd("log_y_p", 0.1)
model.2 <- run_specific_rd("log_y_p_f", 0.1)
model.3 <- run_specific_rd("log_y_p_m", 0.1)
model.4 <- run_specific_rd("log_y_p_fcc", 0.1)
texreg(list(model.1, model.2, model.3, model.4))

model.1 <- run_specific_rd("log_y_g", 0.1)
model.2 <- run_specific_rd("log_y_g_f", 0.1)
model.3 <- run_specific_rd("log_y_g_m", 0.1)
model.4 <- run_specific_rd("log_y_g_fcc", 0.1)
texreg(list(model.1, model.2, model.3, model.4))

model.1 <- run_specific_rd("dlog_y", 0.1)
model.2 <- run_specific_rd("dlog_y_f", 0.1)
model.3 <- run_specific_rd("dlog_y_m", 0.1)
model.4 <- run_specific_rd("dlog_y_fcc", 0.1)
texreg(list(model.1, model.2, model.3, model.4))

model.1 <- run_specific_rd("pct_p_m_ind", 0.1)
model.2 <- run_specific_rd("pct_g_m_ind", 0.1)
texreg(list(model.1, model.2))

# Spectrum Plot -----------------------------------------------------------
analyses_output <- read.csv(paste0(gender, "07_rd_primary_battle_sexes_output_uniform_open_seats.csv"))

plot_results_spectrum(analyses_output, "EasyRD-simple", "dlog_y_m", 0.02, 0.3, "uniform_open_seats")
plot_results_spectrum(analyses_output, "EasyRD-simple", "dlog_y_f", 0.02, 0.3, "uniform_open_seats")

# RD Plot -----------------------------------------------------------------
rd_data <- prepare_data(rd_data_raw) %>% 
  filter(y_p_m != 0 & y_g_m != 0)

data_rd_plot <- rd_data %>% 
  filter(abs(female_primary_vote_margin) <= 0.1)

plot_vars_list <- c("dlog_y_m")

for (i in plot_vars_list) {
  plot_rd(data_rd_plot, i, 0.1, "oepn_seats")
  
}


