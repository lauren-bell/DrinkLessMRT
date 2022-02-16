

###
### Pooled estimates for main paper 
## LB 07/10/2021 

rm(list = ls())

setwd("")

# read in the Dataset A 
EMEE_final <- readRDS("EMME_final.rds")
EMEE_final <- as.data.frame(EMEE_final)


table(EMEE_final$treatment, EMEE_final$subset_one)
table(EMEE_final$treatment, EMEE_final$subset_two)


str(EMEE_final)
View(EMEE_final)

#EMEE_final$age <- as.numeric(EMEE_final$age)

EMEE_final$avail <- 1

source("estimator_EMEE.R")

summary(EMEE_final)


#######
### TABLE 2 Marginal effect to receiving notification ## 
######

estimator_1 <- estimator_EMEE(
  dta = EMEE_final,
  id_varname = "ID",
  decision_time_varname = "days_since_download",
  treatment_varname = "treatment",
  outcome_varname = "primary_outcome",
  control_varname = c("age", "AUDIT_score","days_since_download", "gender", "employment_type", "after_9pm_day_before", "before_8pm"),
  moderator_varname = NULL,
  rand_prob_varname = "prob_A",
  avail_varname = "avail"
)

print(estimator_1)

marginal <- exp((estimator_1$beta_hat))
exp((estimator_1$beta_se))

upperCI <- estimator_1$beta_hat + qnorm(0.975)*(estimator_1$beta_se)
lowerCI <- estimator_1$beta_hat - qnorm(0.975)*(estimator_1$beta_se)

#Marginal 
upperCI_m1 <- exp(upperCI)
lowerCI_m1  <- exp(lowerCI)


lowerCI_m1 
marginal 
upperCI_m1



#######################
### Table 3: Change of marginal treatment over time. 


estimator_2 <- estimator_EMEE(
  dta = EMEE_final,
  id_varname = "ID",
  decision_time_varname = "days_since_download",
  treatment_varname = "treatment",
  outcome_varname = "primary_outcome",
  control_varname = c("age", "AUDIT_score","days_since_download", "gender", "employment_type", "after_9pm_day_before", "before_8pm"),
  moderator_varname = "days_since_download",
  rand_prob_varname = "prob_A",
  avail_varname = "avail"
)

print(estimator_2)

marginal <- exp((estimator_2$beta_hat))
exp((estimator_2$beta_se))

upperCI <- estimator_2$beta_hat + qnorm(0.975)*(estimator_2$beta_se)
lowerCI <- estimator_2$beta_hat - qnorm(0.975)*(estimator_2$beta_se)

#Marginal 
upperCI_m2 <- exp(upperCI)
lowerCI_m2  <- exp(lowerCI)


lowerCI_m2 
marginal 
upperCI_m2



#######################
### Table 5 Habituation: did they user receive a notification day before?   


estimator_3 <- estimator_EMEE(
  dta = EMEE_final,
  id_varname = "ID",
  decision_time_varname = "days_since_download",
  treatment_varname = "treatment",
  outcome_varname = "primary_outcome",
  control_varname = c("age", "AUDIT_score","days_since_download", "gender", "employment_type", "after_9pm_day_before", "before_8pm", "habituation"),
  moderator_varname = "habituation",
  rand_prob_varname = "prob_A",
  avail_varname = "avail"
)

print(estimator_3)

marginal_3 <- exp((estimator_3$beta_hat))


upperCI <- estimator_3$beta_hat + qnorm(0.975)*(estimator_3$beta_se)
lowerCI <- estimator_3$beta_hat - qnorm(0.975)*(estimator_3$beta_se)

#Marginal 
upperCI_m3 <- exp(upperCI)
lowerCI_m3  <- exp(lowerCI)


lowerCI_m3 
marginal_3
upperCI_m3


#######################
### Table 6: Already engaged: did they use the app the day before between 8-9pm?   


estimator_4 <- estimator_EMEE(
  dta = EMEE_final,
  id_varname = "ID",
  decision_time_varname = "days_since_download",
  treatment_varname = "treatment",
  outcome_varname = "primary_outcome",
  control_varname = c("age", "AUDIT_score","days_since_download", "gender", "employment_type", "after_9pm_day_before", "before_8pm", "already_engaged"),
  moderator_varname = "already_engaged",
  rand_prob_varname = "prob_A",
  avail_varname = "avail"
)

print(estimator_4)

marginal_4 <- exp((estimator_4$beta_hat))


upperCI <- estimator_4$beta_hat + qnorm(0.975)*(estimator_4$beta_se)
lowerCI <- estimator_4$beta_hat - qnorm(0.975)*(estimator_4$beta_se)

#Marginal 
upperCI_m4 <- exp(upperCI)
lowerCI_m4  <- exp(lowerCI)


lowerCI_m4 
marginal_4
upperCI_m4



########
# Table 7: Outcome set as 24 hour window 


estimator_5 <- estimator_EMEE(
  dta = EMEE_final,
  id_varname = "ID",
  decision_time_varname = "days_since_download",
  treatment_varname = "treatment",
  outcome_varname = "outcome_24hour",
  control_varname = c("age", "AUDIT_score","days_since_download", "gender", "employment_type", "after_9pm_day_before", "before_8pm"),
  moderator_varname = NULL ,
  rand_prob_varname = "prob_A",
  avail_varname = "avail"
)

print(estimator_5)

marginal_5 <- exp((estimator_5$beta_hat))


upperCI <- estimator_5$beta_hat + qnorm(0.975)*(estimator_5$beta_se)
lowerCI <- estimator_5$beta_hat - qnorm(0.975)*(estimator_5$beta_se)

#Marginal 
upperCI_m5 <- exp(upperCI)
lowerCI_m5  <- exp(lowerCI)


lowerCI_m5 
marginal_5
upperCI_m5


#######
### Sensitive analysis for primary outcome - no baseline adjustment ## 
######

estimator_6 <- estimator_EMEE(
  dta = EMEE_final,
  id_varname = "ID",
  decision_time_varname = "days_since_download",
  treatment_varname = "treatment",
  outcome_varname = "primary_outcome",
  control_varname = NULL,
  moderator_varname = NULL,
  rand_prob_varname = "prob_A",
  avail_varname = "avail"
)

print(estimator_6)

marginal <- exp((estimator_6$beta_hat))
exp((estimator_6$beta_se))

upperCI <- estimator_6$beta_hat + qnorm(0.975)*(estimator_6$beta_se)
lowerCI <- estimator_6$beta_hat - qnorm(0.975)*(estimator_6$beta_se)

#Marginal 
upperCI_m6 <- exp(upperCI)
lowerCI_m6  <- exp(lowerCI)


lowerCI_m6 
marginal 
upperCI_m6

