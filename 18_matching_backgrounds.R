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
library(MatchIt)

path <- "~/zfs/projects/students/fangg-voters/data/"

gender <- paste0(path, "gender_politics/")
import <- paste0(path, "import/")
export <- paste0(path, "export/")
tmp <- paste0(path, "tmp/")
shp <- paste0(path, "shapefiles/")
contrib <- paste0(import, "dime/contribDB/")

# Import merged bio and primary funding data ------------------------------
# data("lalonde")

raw_merged_data <- read_feather(paste0(gender, "12_bio_data_merged_with_primary_funding.feather")) %>% 
  mutate(age_mod = ifelse(is.na(age) | age < 24, 53, age),
         cfscore_mod = ifelse(is.na(recipient.cfscore), 0, recipient.cfscore)) 

# Making two assumptions here
# 53 is the average age in the sample 
# 0 cfscore is neutral.

sample_data <- raw_merged_data %>% 
  select(general_election_id:candidate_lname, 
         gender, age_mod, cfscore_mod, party, Incum.Chall, children:state_leg_flag, 
         district, state, cycle, y_p:win_primary, candpct) %>% 
  replace(is.na(.), 0) %>% 
  mutate(log_y_p = log1p(y_p))

# OLS ---------------------------------------------------------------------
outcome_var = "log1p(y_p)"
indep_vars = c("gender", "Incum.Chall")
# indep_vars = c("gender", "Incum.Chall", "cfscore_mod", "age_mod", "bachelor_flag", "jd_flag", "state_leg_flag")
indep_vars = c("gender", "Incum.Chall", "gender:Incum.Chall", "cfscore_mod", "age_mod", 
               "bachelor_flag", "jd_flag", "md_flag", "mba_flag", "master_flag", "phd_flag",
               "law_flag", "business_flag", "doctor_flag", "teacher_flag",
               "state_leg_flag")


# fe_vars = c("state", "cycle")
fe_vars = c("primary_election_id")
# fe_vars = c("district", "cycle")

fml = as.formula(paste0(outcome_var, "~", paste0(indep_vars, collapse="+"), "|", paste0(fe_vars, collapse="+")))

model = feols(fml, cluster = "primary_election_id", sample_data)
summary(model)



# Check initial balance ---------------------------------------------------
treatment_var = "gender"
control_vars = c("Incum.Chall", "party", "cfscore_mod", "age_mod", 
                 "bachelor_flag", "jd_flag", "md_flag", "mba_flag", "master_flag", "phd_flag",
                 "law_flag", "business_flag", "doctor_flag", "teacher_flag",
                 "state_leg_flag", "cycle")
fml = as.formula(paste0(treatment_var, "~", paste0(control_vars, collapse="+")))

m.out0 <- matchit(fml, data = sample_data , method = NULL, distance = "glm")
summary(m.out0)


# Matching Nearest ----------------------------------------------------------------
m.out1 <- matchit(fml, data = sample_data, method = "nearest", distance = "glm")
summary(m.out1)
summary(m.out1, un = FALSE)

plot(m.out1, type = "jitter", interactive = FALSE)

plot(m.out1, type = "qq", interactive = FALSE,
     which.xs = c("age_mod", "jd_flag", "cfscore_mod"))


# matched_data <- get_matches(m.out1)
matched_data1 <- match.data(m.out1)

# Matching Full -----------------------------------------------------------
m.out2 <-matchit(fml, data = raw_merged_data, method = "full", distance = "glm", link = "probit")
summary(m.out2, un = FALSE)


# Matching Estimate Treatment Effect -----------------------------------------------
# matched_data_summary <- matched_data1 %>% 
#   group_by(primary_election_id) %>% 
#   summarise(n = n())


# outcome_var = "log1p(y_p)"
outcome_var = "win_primary"

# indep_vars = c("gender", "Incum.Chall")
# indep_vars = c("gender", "Incum.Chall", "cfscore_mod", "age_mod", "bachelor_flag", "jd_flag", "state_leg_flag")
indep_vars = c("gender", "Incum.Chall", "party", "cfscore_mod", "age_mod", 
               "bachelor_flag", "jd_flag", "md_flag", "mba_flag", "master_flag", "phd_flag",
               "law_flag", "business_flag", "doctor_flag", "teacher_flag",
               "state_leg_flag")


# fe_vars = c("state", "cycle")
fe_vars = c("primary_election_id")
# fe_vars = c("district", "cycle")

fml = as.formula(paste0(outcome_var, "~", paste0(indep_vars, collapse="+"), "|", paste0(fe_vars, collapse="+")))

# model = feols(fml, cluster = "subclass", matched_data1, weights = matched_data1$weights)
model = feols(fml, cluster = "primary_election_id", matched_data1, weights = matched_data1$weights)
summary(model)

# model2 = lm(fml, matched_data1, weights = matched_data1$weights)
# coeftest(model2, vcov. = vcovCL, cluster = ~subclass)
# What about interaction terms???








# Generally Estimate Treatment Effect ---------------------------------------
reg_data = matched_data1
# reg_data = sample_data
# outcome_var = "log1p(y_p)"
# controls = "basic"
# fe_vars = c("district", "cycle")
# cluster_level = "primary_election_id"

run_reg <- function(reg_data, outcome_var, controls, fe_vars, cluster_level) {
  # reg_data <- reg_data %>% 
  #   filter(cycle >= 2010)
  
  if (controls == "basic") {
    indep_vars = c("gender", "Incum.Chall", "gender:Incum.Chall", "party")
  } else if (controls == "full") {
    indep_vars = c("gender", "Incum.Chall", "gender:Incum.Chall", "party", 
                   "bachelor_flag", "jd_flag", "md_flag", "mba_flag", "master_flag", "phd_flag",
                   "law_flag", "business_flag", "doctor_flag", "teacher_flag",
                   "state_leg_flag")
  }
  
  # "cfscore_mod", "age_mod", 

  fml = as.formula(paste0(outcome_var, "~", paste0(indep_vars, collapse="+"), "|", paste0(fe_vars, collapse="+")))
  model = feols(fml, cluster = cluster_level, reg_data)
  
  return(model)
}


# Present on 04/06/2022 at HRG
model1 = run_reg(matched_data1, "log1p(y_p)", "full", c("district", "cycle"), "primary_election_id")
model2 = run_reg(matched_data1, "win_primary", "full", c("district", "cycle"), "primary_election_id")

texreg(list(model1, model2))

# Present on 04/06/2022 at HRG
model1 = run_reg(matched_data1, "log1p(y_p)", "full", c("primary_election_id"), "primary_election_id")
model2 = run_reg(matched_data1, "win_primary", "full", c("primary_election_id"), "primary_election_id")

texreg(list(model1, model2))



# OLS 
model1 = run_reg(sample_data, "log1p(y_p)", "basic", c("district", "cycle"), "primary_election_id")
model2 = run_reg(sample_data, "log1p(y_p)", "basic", c("primary_election_id"), "primary_election_id")
model3 = run_reg(sample_data, "log1p(y_p)", "full", c("district", "cycle"), "primary_election_id")
model4 = run_reg(sample_data, "log1p(y_p)", "full", c("primary_election_id"), "primary_election_id")

texreg(list(model1, model2, model3, model4))

# OLS Win
model1 = run_reg(sample_data, "win_primary", "basic", c("district", "cycle"), "primary_election_id")
model2 = run_reg(sample_data, "win_primary", "basic", c("primary_election_id"), "primary_election_id")
model3 = run_reg(sample_data, "win_primary", "full", c("district", "cycle"), "primary_election_id")
model4 = run_reg(sample_data, "win_primary", "full", c("primary_election_id"), "primary_election_id")

texreg(list(model1, model2, model3, model4))

# Matching 
model1 = run_reg(matched_data1, "log1p(y_p)", "basic", c("district", "cycle"), "primary_election_id")
model2 = run_reg(matched_data1, "log1p(y_p)", "basic", c("primary_election_id"), "primary_election_id")
model3 = run_reg(matched_data1, "log1p(y_p)", "full", c("district", "cycle"), "primary_election_id")
model4 = run_reg(matched_data1, "log1p(y_p)", "full", c("primary_election_id"), "primary_election_id")

texreg(list(model1, model2, model3, model4))

# Matching Win
model1 = run_reg(matched_data1, "win_primary", "basic", c("district", "cycle"), "primary_election_id")
model2 = run_reg(matched_data1, "win_primary", "basic", c("primary_election_id"), "primary_election_id")
model3 = run_reg(matched_data1, "win_primary", "full", c("district", "cycle"), "primary_election_id")
model4 = run_reg(matched_data1, "win_primary", "full", c("primary_election_id"), "primary_election_id")

texreg(list(model1, model2, model3, model4))

# Matching subclass cluster
model1 = run_reg(matched_data1, "log1p(y_p)", "basic", c("district", "cycle"), "subclass")
model2 = run_reg(matched_data1, "log1p(y_p)", "basic", c("primary_election_id"), "subclass")
model3 = run_reg(matched_data1, "log1p(y_p)", "full", c("district", "cycle"), "subclass")
model4 = run_reg(matched_data1, "log1p(y_p)", "full", c("primary_election_id"), "subclass")

texreg(list(model1, model2, model3, model4))


# Matching subclass cluster Win
model1 = run_reg(matched_data1, "win_primary", "basic", c("district", "cycle"), "subclass")
model2 = run_reg(matched_data1, "win_primary", "basic", c("primary_election_id"), "subclass")
model3 = run_reg(matched_data1, "win_primary", "full", c("district", "cycle"), "subclass")
model4 = run_reg(matched_data1, "win_primary", "full", c("primary_election_id"), "subclass")

texreg(list(model1, model2, model3, model4))



# Treatment Effect by year ------------------------------------------------
run_reg_by_year <- function(reg_data, outcome_var, controls, fe_vars, cluster_level, year) {
  reg_data <- reg_data %>%
    filter(cycle == year)
  
  print(sum(reg_data$gender == 1))
  print(sum(reg_data$gender == 0))
  
  if (controls == "basic") {
    indep_vars = c("gender", "Incum.Chall", "gender:Incum.Chall", "party")
  } else if (controls == "full") {
    indep_vars = c("gender", "Incum.Chall", "gender:Incum.Chall", "party", 
                   "bachelor_flag", "jd_flag", "md_flag", "mba_flag", "master_flag", "phd_flag",
                   "law_flag", "business_flag", "doctor_flag", "teacher_flag",
                   "state_leg_flag")
  }
  
  # "cfscore_mod", "age_mod", 
  
  fml = as.formula(paste0(outcome_var, "~", paste0(indep_vars, collapse="+"), "|", paste0(fe_vars, collapse="+")))
  model = feols(fml, cluster = cluster_level, reg_data)
  
  return(model)
}

plot_by_year <- function(outcome_input) {
  output_table_plot = output_table %>% 
    filter(outcome == outcome_input) %>% 
    mutate(ci_lower = coef - 1.96*se,
           ci_upper = coef + 1.96*se)
  
  ggplot(output_table_plot, aes(x = year, y = coef)) +
    geom_line() + 
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_ribbon(aes(ymin=ci_lower,
                    ymax=ci_upper), alpha=.3) +
    scale_x_continuous(breaks = seq(1992, 2018, by = 4)) +
    ggtitle(paste0("Coefficient Estimate: ", outcome_input)) + 
    xlab("Year") + ylab("Estimate")
  
  ggsave(paste0(gender, "plots/18_by_year_", outcome_input, ".png"))
}

outcome_list <- c("log1p(y_p)", "win_primary")
n = length(seq(1992, 2018, 2))*length(outcome_list)
output_table <- data.frame(ID = 1:n,
                           outcome = rep(outcome_list, each = length(seq(1992, 2018, 2))),
                           year = rep(seq(1992, 2018, 2), times = 2),
                           coef = numeric(n),
                           se = numeric(n),
                           tstat = numeric(n),
                           pval = numeric(n),
                           nobs = numeric(n),
                           stringsAsFactors = FALSE)

for (i in 1:n) {
  print(i)
  model = run_reg_by_year(sample_data, output_table$outcome[i], "basic", c("district"), "district", output_table$year[i])
  output_table[i,c(4:7)] = model$coeftable[1,]
  output_table$nobs[i] = model$nobs
  
}


for (outcome_input in outcome_list) {
  plot_by_year(outcome_input)
  
}