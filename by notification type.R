
###
### By notification type for main paper 
## LB 07/10/2021 

rm(list = ls())

setwd("")


EMEE_final <- readRDS("EMME_final.rds")
EMEE_final <- as.data.frame(EMEE_final)


str(EMEE_final)
View(EMEE_final)


source("estimator_multiple_trt.R")
source("WCLS_multiple_trt.R")


# subversion = Y (standard message), X (new bank of messages), Z (no message)

### construct treatment variable taking values 0, 1, 2
# 0: no message; 1: standard message; 2: new bank of messages
EMEE_final$treatment_cate <- NA
EMEE_final$treatment_cate[EMEE_final$subversion == "Z"] <- 0
EMEE_final$treatment_cate[EMEE_final$subversion == "Y"] <- 1
EMEE_final$treatment_cate[EMEE_final$subversion == "X"] <- 2

### construct variable containing randomization probability
# prob(A = 0 / 1 / 2) = 0.3 / 0.3 / 0.4
EMEE_final$rand_prob_A0 <- 0.4 # prob of A=0 (used in IPW weights when Delta >= 1)

# prob of receiving the actual treatment received
# (used in marginalization weights)
EMEE_final$rand_prob <- NA
EMEE_final$rand_prob[EMEE_final$treatment_cate == 0] <- 0.4
EMEE_final$rand_prob[EMEE_final$treatment_cate == 1] <- 0.3
EMEE_final$rand_prob[EMEE_final$treatment_cate == 2] <- 0.3

# Table 2: marginal effect (no moderation) -----------------------

fit1 <- weighted_centered_least_square_withDelta_multiple_trt(
  dta = EMEE_final,
  id_varname = "ID",
  decision_time_varname = "days_since_download",
  treatment_varname = "treatment_cate",
  outcome_varname = "primary_outcome",
  control_varname = c("age", "AUDIT_score","days_since_download", "gender", "employment_type", "after_9pm_day_before", "before_8pm"),
  moderator_varname = NULL,
  rand_prob_varname = "rand_prob",
  rand_prob_A0_varname = "rand_prob_A0",
  rand_prob_tilde_varname = NULL,
  rand_prob_tilde = c(0.4, 0.3, 0.3),
  estimator_initial_value = NULL,
  Delta = 1,
  num_trt = 2 # number of treatment categories (excluding control level)
)


summary(fit1)

marginal <- exp((fit1$beta_hat))
exp((fit1$beta_se))

upperCI <- fit1$beta_hat + qnorm(0.975)*(fit1$beta_se)
lowerCI <- fit1$beta_hat - qnorm(0.975)*(fit1$beta_se)

#Marginal 
upperCI_m1 <- exp(upperCI)
lowerCI_m1  <- exp(lowerCI)


lowerCI_m1 
marginal 
upperCI_m1



# Table 3: effect between messages change over time? ----------------

fit2 <- weighted_centered_least_square_withDelta_multiple_trt(
  dta = EMEE_final,
  id_varname = "ID",
  decision_time_varname = "days_since_download",
  treatment_varname = "treatment_cate",
  outcome_varname = "primary_outcome",
  control_varname = c("age", "AUDIT_score","days_since_download", "gender", "employment_type", "after_9pm_day_before", "before_8pm"),
  moderator_varname = "days_since_download",
  rand_prob_varname = "rand_prob",
  rand_prob_A0_varname = "rand_prob_A0",
  rand_prob_tilde_varname = NULL,
  rand_prob_tilde = c(0.4, 0.3, 0.3),
  estimator_initial_value = NULL,
  Delta = 1,
  num_trt = 2 # number of treatment categories (excluding control level)
)

fit2


estimate_moderator2 <- exp(fit2$beta_hat)

upperCI_m2 <- fit2$beta_hat + qnorm(0.975)*(fit2$beta_se)
lowerCI_m2 <- fit2$beta_hat - qnorm(0.975)*(fit2$beta_se)
upperCI_m2 <- exp(upperCI_m2)
lowerCI_m2  <- exp(lowerCI_m2)


lowerCI_m2
estimate_moderator2
upperCI_m2



#######
## Table 5: Habituation 

fit3 <- weighted_centered_least_square_withDelta_multiple_trt(
  dta = EMEE_final,
  id_varname = "ID",
  decision_time_varname = "days_since_download",
  treatment_varname = "treatment_cate",
  outcome_varname = "primary_outcome",
  control_varname = c("age", "AUDIT_score","days_since_download", "gender", "employment_type", "after_9pm_day_before", "before_8pm", "habituation"),
  moderator_varname = "habituation",
  rand_prob_varname = "rand_prob",
  rand_prob_A0_varname = "rand_prob_A0",
  rand_prob_tilde_varname = NULL,
  rand_prob_tilde = c(0.4, 0.3, 0.3),
  estimator_initial_value = NULL,
  Delta = 1,
  num_trt = 2 # number of treatment categories (excluding control level)
)


summary(fit3)

marginal <- exp((fit3$beta_hat))
exp((fit3$beta_se))

upperCI <- fit3$beta_hat + qnorm(0.975)*(fit3$beta_se)
lowerCI <- fit3$beta_hat - qnorm(0.975)*(fit3$beta_se)

#Marginal 
upperCI_m3 <- exp(upperCI)
lowerCI_m3  <- exp(lowerCI)


lowerCI_m3 
marginal 
upperCI_m3


#######
## Table Six: already engaged

fit9 <- weighted_centered_least_square_withDelta_multiple_trt(
  dta = EMEE_final,
  id_varname = "ID",
  decision_time_varname = "days_since_download",
  treatment_varname = "treatment_cate",
  outcome_varname = "primary_outcome",
  control_varname = c("age", "AUDIT_score","days_since_download", "gender", "employment_type", "after_9pm_day_before", "before_8pm", "already_engaged"),
  moderator_varname = "already_engaged",
  rand_prob_varname = "rand_prob",
  rand_prob_A0_varname = "rand_prob_A0",
  rand_prob_tilde_varname = NULL,
  rand_prob_tilde = c(0.4, 0.3, 0.3),
  estimator_initial_value = NULL,
  Delta = 1,
  num_trt = 2 # number of treatment categories (excluding control level)
)



summary(fit9)

marginal <- exp((fit9$beta_hat))
exp((fit9$beta_se))

upperCI <- fit9$beta_hat + qnorm(0.975)*(fit9$beta_se)
lowerCI <- fit9$beta_hat - qnorm(0.975)*(fit9$beta_se)

#Marginal 
upperCI_m9 <- exp(upperCI)
lowerCI_m9  <- exp(lowerCI)


lowerCI_m9 
marginal 
upperCI_m9



#######
## Table 7: 24 hour outcome (8pm to 8pm the next day) 

fit6 <- weighted_centered_least_square_withDelta_multiple_trt(
  dta = EMEE_final,
  id_varname = "ID",
  decision_time_varname = "days_since_download",
  treatment_varname = "treatment_cate",
  outcome_varname = "outcome_24hour",
  control_varname = c("age", "AUDIT_score","days_since_download", "gender", "employment_type", "after_9pm_day_before", "before_8pm"),
  moderator_varname = NULL,
  rand_prob_varname = "rand_prob",
  rand_prob_A0_varname = "rand_prob_A0",
  rand_prob_tilde_varname = NULL,
  rand_prob_tilde = c(0.4, 0.3, 0.3),
  estimator_initial_value = NULL,
  Delta = 1,
  num_trt = 2 # number of treatment categories (excluding control level)
)



summary(fit6)

marginal <- exp((fit6$beta_hat))
exp((fit6$beta_se))

upperCI <- fit6$beta_hat + qnorm(0.975)*(fit6$beta_se)
lowerCI <- fit6$beta_hat - qnorm(0.975)*(fit6$beta_se)

#Marginal 
upperCI_m6 <- exp(upperCI)
lowerCI_m6  <- exp(lowerCI)


lowerCI_m6 
marginal 
upperCI_m6


# Table 3: marginal effect (no moderation and no adjustments) -----------------------

fit7 <- weighted_centered_least_square_withDelta_multiple_trt(
  dta = EMEE_final,
  id_varname = "ID",
  decision_time_varname = "days_since_download",
  treatment_varname = "treatment_cate",
  outcome_varname = "primary_outcome",
  control_varname = c("age", "AUDIT_score","days_since_download", "gender", "employment_type", "after_9pm_day_before", "before_8pm"),
  moderator_varname = NULL,
  rand_prob_varname = "rand_prob",
  rand_prob_A0_varname = "rand_prob_A0",
  rand_prob_tilde_varname = NULL,
  rand_prob_tilde = c(0.4, 0.3, 0.3),
  estimator_initial_value = NULL,
  Delta = 1,
  num_trt = 2 # number of treatment categories (excluding control level)
)


summary(fit7)

marginal <- exp((fit7$beta_hat))
exp((fit7$beta_se))

upperCI <- fit7$beta_hat + qnorm(0.975)*(fit7$beta_se)
lowerCI <- fit7$beta_hat - qnorm(0.975)*(fit7$beta_se)

#Marginal 
upperCI_m7 <- exp(upperCI)
lowerCI_m7  <- exp(lowerCI)


lowerCI_m7 
marginal 
upperCI_m7




