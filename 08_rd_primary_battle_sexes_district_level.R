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
clean_data <- function(filename) {
  rd_data_raw <- read_feather(paste0(gender, filename)) %>% 
    group_by(general_election_id) %>% 
    mutate(n_i = sum(Incum.Chall == "I"),
           n_c = sum(Incum.Chall == "C"),
           n_o = sum(Incum.Chall == "O"),
           n_blank = sum(Incum.Chall == "")) %>% 
    ungroup() %>% 
    mutate(Incum.Chall = ifelse(n_i > 0 & Incum.Chall != "I", "C",
                                ifelse(n_i == 0 & Incum.Chall != "O", "O", Incum.Chall))) %>% 
    filter(primary_winner == 1) %>% 
    select(general_election_id, female_primary_vote_margin, female_primary_winner, Incum.Chall, party)
  
  return(rd_data_raw)
}

summarise_at_district_level <- function(filename) {
  general_data_raw <- read_feather(paste0(gender, filename)) %>% 
    replace(is.na(.), 0) %>% 
    arrange(general_election_id, desc(voteshare_general)) %>% 
    group_by(general_election_id) %>% 
    summarise(total_y_g_f = sum(y_g_f),
              total_y_g_m = sum(y_g_m),
              total_y_g_fcc = sum(y_g_fcc),
              total_y_g = sum(y_g),
              total_y_p_f = sum(y_p_f),
              total_y_p_m = sum(y_p_m),
              total_y_p_fcc = sum(y_p_fcc),
              total_y_p = sum(y_p),
              total_n_g_f = sum(n_g_f),
              total_n_g_m = sum(n_g_m),
              total_n_g_fcc = sum(n_g_fcc),
              total_n_g = sum(n_g),
              total_n_p_f = sum(n_p_f),
              total_n_p_m = sum(n_p_m),
              total_n_p_fcc = sum(n_p_fcc),
              total_n_p = sum(n_p),
              pct_y_g_f = sum(y_g_f)/(sum(y_g_f) + sum(y_g_m)),
              pct_y_g_m = sum(y_g_m)/(sum(y_g_f) + sum(y_g_m)),
              pct_y_p_f = sum(y_p_f)/(sum(y_p_f) + sum(y_p_m)),
              pct_y_p_m = sum(y_p_m)/(sum(y_p_f) + sum(y_p_m)),
              pct_n_g_f = sum(n_g_f)/(sum(n_g_f) + sum(n_g_m)),
              pct_n_g_m = sum(n_g_m)/(sum(n_g_f) + sum(n_g_m)),
              pct_n_p_f = sum(n_p_f)/(sum(n_p_f) + sum(n_p_m)),
              pct_n_p_m = sum(n_p_m)/(sum(n_p_f) + sum(n_p_m))) %>% 
    ungroup() %>% 
    mutate(log_total_y_g = log1p(total_y_g),
           log_total_y_g_f = log1p(total_y_g_f),
           log_total_y_g_m = log1p(total_y_g_m),
           log_total_y_g_fcc = log1p(total_y_g_fcc),
           log_total_n_g = log1p(total_n_g),
           log_total_n_g_f = log1p(total_n_g_f),
           log_total_n_g_m = log1p(total_n_g_m),
           log_total_n_g_fcc = log1p(total_n_g_fcc),
           log_total_y_p = log1p(total_y_p),
           log_total_y_p_f = log1p(total_y_p_f),
           log_total_y_p_m = log1p(total_y_p_m),
           log_total_y_p_fcc = log1p(total_y_p_fcc),
           log_total_n_p = log1p(total_n_p),
           log_total_n_p_f = log1p(total_n_p_f),
           log_total_n_p_m = log1p(total_n_p_m),
           log_total_n_p_fcc = log1p(total_n_p_fcc),
           dlog_total_y = log1p(total_y_g) - log1p(total_y_p),
           dlog_total_y_f = log1p(total_y_g_f) - log1p(total_y_p_f),
           dlog_total_y_m = log1p(total_y_g_m) - log1p(total_y_p_m),
           dlog_total_y_fcc = log1p(total_y_g_fcc) - log1p(total_y_p_fcc),
           dlog_total_n = log1p(total_n_g) - log1p(total_n_p),
           dlog_total_n_f = log1p(total_n_g_f) - log1p(total_n_p_f),
           dlog_total_n_m = log1p(total_n_g_m) - log1p(total_n_p_m),
           dlog_total_n_fcc = log1p(total_n_g_fcc) - log1p(total_n_p_fcc),
           d_total_n = total_n_g - total_n_p,
           d_total_n_f = total_n_g_f - total_n_p_f,
           d_total_n_m = total_n_g_m - total_n_p_m,
           d_total_n_fcc = total_n_g_fcc - total_n_p_fcc)
  
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
  # k = "uniform" 
  k = "triangular"
  
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
                       "factor(party)", "factor(Incum.Chall)")
        fml = as.formula(paste0(outcome, "~", paste0(indep_vars, collapse="+")))
        
        ols_result = feols(fml, data_to_run, se = "hetero")
        coef = ols_result$coeftable[2,1]
        se = ols_result$coeftable[2,2]
        pval = ols_result$coeftable[2,4]
        actual_bws = bws
        
      } else if (specification == "EasyRD-covars2") {
        indep_vars = c("female_primary_winner", "female_primary_vote_margin", "female_primary_winner:female_primary_vote_margin",
                       "factor(party)", "factor(Incum.Chall)")
        fe_vars = c("cycle")
        fml = as.formula(paste0(outcome, "~", paste0(indep_vars, collapse="+"), "|", paste0(fe_vars, collapse="+")))
        
        ols_result = feols(fml, data_to_run, se = "hetero")
        coef = ols_result$coeftable[1,1]
        se = ols_result$coeftable[1,2]
        pval = ols_result$coeftable[1,4]
        actual_bws = bws
        
      } else if (specification == "EasyRD-covars3") {
        indep_vars = c("female_primary_winner", "female_primary_vote_margin", "female_primary_winner:female_primary_vote_margin",
                       "factor(party)", "factor(Incum.Chall)")
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

plot_results_spectrum <- function(data_input = analyses_output, 
                                  specification_input = "EasyRD-simple", 
                                  outcome_input = "log_total_y_p_m", 
                                  lbound = 0.02, ubound = 0.3, label = "uniform_all_sample") {
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
  
  ggsave(paste0(gender, "plots/08_spectrum_", specification_input, "_", label, "_", outcome_input, ".png"))
  
}

plot_rd <- function(data_final_plot, i, bws, goal) {
  k = "uniform"
  x = data_final_plot$female_primary_vote_margin
  y = data_final_plot[[i]]
  
  png(file = paste0(gender, "plots/08_rd_plot_", goal, "_", i, ".png"), 
      width=730, height=470)
  rdplot(y, x, c = 0, h = bws, p = 1, kernel = k, nbins = 50,
         x.label = "Female Vote Margin", 
         y.label = i,
         title = paste0(goal, ": ", i))
  dev.off()
  
  model <- rdrobust(y, x, c = 0, h = bws, p = 1, kernel = k)
  print(model$Estimate)
}


# Import ------------------------------------------------------------------
rd_data_raw <- clean_data("06_primary_fundraising_battle_sexes.feather")

general_data_raw <- summarise_at_district_level("03_general_elections_fundraising_panel_data.feather")
  
rd_data_final <- rd_data_raw %>% 
  left_join(general_data_raw, by = "general_election_id") %>%
  filter(!is.na(log_total_y_g)) # Shouldn't be deleted!!! Will modify later

# write.csv(rd_data_final, paste0(gender, "08_sanity_check_rd_data_old.csv"), row.names = FALSE)


# Descriptive Statistics --------------------------------------------------
data_summary <- rd_data_final %>% 
  filter(abs(female_primary_vote_margin) <= 0.1) %>% 
  group_by(female_primary_winner) %>% 
  summarise(across(total_y_g_f:total_y_g, mean, .name = "mean_{.col}"),
            n = n())
  
write.csv(data_summary, paste0(gender, "08_descriptives.csv"), row.names = FALSE)

# Run ---------------------------------------------------------------------
rd_data_final_subset <- rd_data_final 

# outcome_vars_list <- c("log_total_y_g", "log_total_y_g_f", "log_total_y_g_m", "log_total_y_g_fcc",
#                        "log_total_n_g", "log_total_n_g_f", "log_total_n_g_m", "log_total_n_g_fcc",
#                        "total_n_g", "total_n_g_f", "total_n_g_m", "total_n_g_fcc",
#                        "pct_y_g_f", "pct_y_g_m", "pct_n_g_f", "pct_n_g_m")

outcome_vars_list <- colnames(rd_data_final_subset[c(14:57)])
bws_list <- c(seq(0.01, 0.3, by = 0.01), 1) # 1 stands for CCT optimal bandwidth 
specification_list <- c("RD-C", "RD-BC", "RD-R", "EasyRD-simple", "OLS-simple")

analyses_output <- set_up_output_table(rd_data_final_subset, outcome_vars_list, bws_list, specification_list)
write.csv(analyses_output, paste0(gender, "08_triangluar_full_sample.csv"), row.names = FALSE)

# Specific Cases ----------------------------------------------------------
data_to_run = rd_data_final %>% 
  filter(abs(female_primary_vote_margin) <= 0.1) %>% 
  filter(party == "100")


indep_vars = c("female_primary_winner", "female_primary_vote_margin", "female_primary_winner:female_primary_vote_margin")
outcome = "pct_y_g_f"
fml = as.formula(paste0(outcome, "~", paste0(indep_vars, collapse="+")))

ols_result = feols(fml, data_to_run, se = "hetero")
summary(ols_result)

# votesmart bio data ------------------------------------------------------
bio_data_clean <- read.csv(paste0(gender, "12_bio_data_clean.csv"))

bio_data_clean_by_district <- bio_data_clean %>% 
  group_by(general_election_id) %>% 
  summarise(n_jd = sum(jd_flag == 1))

data_to_run <- rd_data_final %>% 
  left_join(bio_data_clean_by_district, by = "general_election_id") %>% 
  filter(abs(female_primary_vote_margin) <= 0.1) %>% 
  replace(is.na(.), 0)


indep_vars = c("female_primary_winner", "female_primary_vote_margin", "female_primary_winner:female_primary_vote_margin",
               "n_jd")
outcome = "pct_y_g_f"
fml = as.formula(paste0(outcome, "~", paste0(indep_vars, collapse="+")))

ols_result = feols(fml, data_to_run, se = "hetero")
summary(ols_result)


# Spectrum Plot -----------------------------------------------------------
analyses_output <- read.csv(paste0(gender, "08_tirangular_full_sample.csv"))

# plot_results_spectrum(analyses_output, "EasyRD-simple", "log_total_y_p_f", 0.02, 0.3, "uniform_all_sample")
# plot_results_spectrum(analyses_output, "EasyRD-simple", "log_total_y_p_m", 0.02, 0.3, "uniform_all_sample")
# plot_results_spectrum(analyses_output, "EasyRD-simple", "log_total_y_p_fcc", 0.02, 0.3, "uniform_all_sample")
# plot_results_spectrum(analyses_output, "EasyRD-simple", "log_total_y_p", 0.02, 0.3, "uniform_all_sample")

plot_results_spectrum(analyses_output, "EasyRD-simple", "log_total_y_g_f", 0.02, 0.3, "uniform_all_sample")
plot_results_spectrum(analyses_output, "EasyRD-simple", "log_total_n_g_f", 0.02, 0.3, "uniform_all_sample")
plot_results_spectrum(analyses_output, "EasyRD-simple", "pct_y_g_f", 0.02, 0.3, "uniform_all_sample")
plot_results_spectrum(analyses_output, "EasyRD-simple", "pct_n_g_f", 0.02, 0.3, "uniform_all_sample")

plot_results_spectrum(analyses_output, "RD-C", "log_total_y_g_f", 0.02, 0.3, "triangular_all_sample")
plot_results_spectrum(analyses_output, "RD-C", "log_total_n_g_f", 0.02, 0.3, "triangular_all_sample")
plot_results_spectrum(analyses_output, "RD-C", "pct_y_g_f", 0.02, 0.3, "triangular_all_sample")
plot_results_spectrum(analyses_output, "RD-C", "pct_n_g_f", 0.02, 0.3, "triangular_all_sample")



# RD plot -----------------------------------------------------------------
rd_data_final_plot <- rd_data_final %>% 
  filter(abs(female_primary_vote_margin) <= 0.1)

plot_vars_list <- c("log_total_y_g_f", "log_total_n_g_f", "pct_y_g_f", "pct_n_g_f")

for (i in plot_vars_list) {
  plot_rd(rd_data_final_plot, i, 0.1, "all")
  
}

plot_rd(rd_data_final_plot, i, 0.1, "all")





# General Election Competitiveness ----------------------------------------
general_data_raw <- read_feather(paste0(gender, "03_general_elections_fundraising_panel_data.feather")) %>% 
  group_by(general_election_id) %>% 
  mutate(n_i = sum(Incum.Chall == "I"),
         n_c = sum(Incum.Chall == "C"),
         n_o = sum(Incum.Chall == "O"),
         n_blank = sum(Incum.Chall == "")) %>% 
  ungroup() %>% 
  mutate(Incum.Chall = ifelse(n_i > 0 & Incum.Chall != "I", "C",
                              ifelse(n_i == 0 & Incum.Chall != "O", "O", Incum.Chall)))

general_data_relevant <- general_data_raw %>% 
  select(general_election_id:gender) %>% 
  distinct() %>% 
  group_by(general_election_id) %>% 
  slice_max(order_by = voteshare_general, n = 2) %>% 
  group_by(general_election_id) %>% 
  summarise(voteshare_diff_general = ifelse(n() == 1, 1, max(voteshare_general) - min(voteshare_general))) %>% 
  filter(voteshare_diff_general > 0)
  
write.csv(general_data_relevant, paste0(gender, "08_general_election_voteshare_diff.csv"), row.names = FALSE)

mean(general_data_relevant$voteshare_diff_general)
median(general_data_relevant$voteshare_diff_general)
  
###
rd_data_select <- rd_data_final %>% 
  select(general_election_id, female_primary_vote_margin)

general_data_relevant_1 <- general_data_relevant %>% 
  filter(general_election_id %in% rd_data_final$general_election_id) %>% 
  left_join(rd_data_select, by = "general_election_id") %>% 
  filter(abs(female_primary_vote_margin) <= 0.1)
  
mean(general_data_relevant_1$voteshare_diff_general <= 0.1)
median(general_data_relevant_1$voteshare_diff_general)
  