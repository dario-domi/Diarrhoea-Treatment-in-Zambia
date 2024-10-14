################################################################################
#
#####     SECOND SCRIPT (STATISTICAL ANALYSES - multilevel modelling)      #####
#
################################################################################
#
# This script is part of a set of scripts written by myself (Dario Domingo) to
# undertake the project: "Role of co-packaging for diarrhoea treatment in 
# Zambian children". See the script "Data_Preparation.R" for more info.
#
# This script runs the statistical analyses whose results are reported in a 
# manuscript currently under peer review. An interactive question at the start
# allows the user to decide whether the results should be printed to output.
#
# The script can be run on its own: all needed variables for analysis are created 
# by running the script Data_Preparation.R in the background. 
#
################################################################################



#################################################################
#
#####    PRELIMINARY OPERATIONS AND CLEANED-DATA LOADING
#
#################################################################

# Script to create tables and variables needed for analysis
source("Data_Preparation.R")

# Script to load custom functions to compute ICC (Intraclass correlation coeff)
source("Intraclass_Corr_Coeff.R")

# Ask the user if they want results to be printed to standard output
cat("Do you want results of the statistical analyses to be shown? (y/n)\n")
answer <- readline()


################################################################################

#######################################################################
#
#####    RANDOM-EFFECT MODELS (ESTIMATES OF PROPORTIONS AND CIs)
#
#######################################################################

# This section trains two random-effect logistic models, one for each year,
# with facilities as random intercepts and correct dispensing as response.
# The estimated coefficients of the model are translated into estimates and CIs 
# of the correct dispensation rate, for each facility

# Load library to build mixed-effect models 
library(lme4)

# Functions to convert log odds into probabilities and vice-versa
logodds2prob <- function(odds) exp(odds)/(1+exp(odds))
prob2logodds <- function(p) log(p/(1-p))


####################################################
#    FIRST PART: 2016 DATA (NO CO-PACK AVAILABLE)
####################################################

# Logistic random-effect model on 2016 data
logit_RE_16 <- glmer(correct ~ (1|facility),
                     data = full_df %>% filter(copack=="N"), # only 2016 data
                     family=binomial)

# Look at summary, check no anomalies
summary(logit_RE_16)

# Intercept (overall log odds of correct dispensation)
intercept_16 <- as.numeric(fixef(logit_RE_16))

# Random effects (difference between each facility's and overall log odds)
randeff_16 <- as.data.frame(ranef(logit_RE_16)) %>%
              select(grp, condval, condsd)

# Log odds by facility (sum of intercept and random effects)
# (log odds can also be obtained via coef(logit_RE_16)))
logodds_16 <- randeff_16 %>% 
              mutate(facility = grp, value = intercept_16 + condval, sd = condsd) %>% 
              select(facility, value, sd)

# Initialise tibble which will contain:
# - sample proportions ('sample' field)
# - estimated proportions and CIs ('lwr', 'mid' & 'upr' fields)
props_16 <- df16 %>% mutate(sample = 0, lwr = 0, mid = 0, upr = 0) %>%
                     select(facility, sample, lwr, mid, upr) %>%
                     as.data.frame()

### Populate fields: Sample and estimated proportions
props_16$sample <- df16$CDCs/df16$tot
props_16$mid    <- logodds2prob( logodds_16$value ) 

# Populate fields: Confidence Intervals
q95 <- qnorm(0.975)
props_16$lwr <- with(logodds_16, value - q95*sd) %>% logodds2prob
props_16$upr <- with(logodds_16, value + q95*sd) %>% logodds2prob

# Transform probabilities into percentages
props_16 <- props_16 %>% mutate( across(where(is.numeric), ~ 100*.))

# Compute associated ICC
ICC_16 <- simulation_ICC(logit_RE_16, 1.e6)

# Other ways of computing ICC
# threshold_ICC(logit_RE_16)
# linearisation_ICC(logit_RE_16)
# simulation_ICC(logit_RE_16, 1.e5)


###############################################################
#    SECOND PART: 2017 DATA (CO-PACK AVAILABLE TO DISPENSE)
###############################################################

# Logistic random-effect model on 2017 data
logit_RE_17 <- glmer(correct ~ (1|facility),
                     data = full_df %>% filter(copack=="Y"), # 2017 data only
                     family=binomial)

# Look at summary, check no anomalies
summary(logit_RE_17)

# Intercept (overall log odds of correct dispensation)
intercept_17 <- as.numeric(fixef(logit_RE_17))

# Random effects (difference between each facility's and overall log odds)
randeff_17 <- as.data.frame(ranef(logit_RE_17)) %>%
              select(grp, condval, condsd)

# Log odds by facility (sum of intercept and random effects)
# (log odds can be also obtained via coef(logit_RE_16)))
logodds_17 <- randeff_17 %>% 
              mutate(facility = grp, value = intercept_17 + condval, sd = condsd) %>%
              select(facility, value, sd)

# Initialise tibble which will contain:
# - sample proportions ('sample' field)
# - estimated proportions and CIs ('lwr', 'mid' & 'upr' fields)
props_17 <- df17 %>% mutate(sample = 0, lwr = 0, mid = 0, upr = 0) %>%
                     select(facility, sample, lwr, mid, upr) %>%
                     as.data.frame()

### Populate fields: Sample and estimated proportions
props_17$sample <- with(df17, CDCs/tot)
props_17$mid    <- logodds2prob( logodds_17$logodds )

# Populate fields: Confidence Intervals
q95 <- qnorm(0.975)
props_17$lwr <- with(logodds_17, value - q95*sd) %>% logodds2prob
props_17$upr <- with(logodds_17, value + q95*sd) %>% logodds2prob

# Transform probabilities into percentages
props_17 <- props_17 %>% mutate( across(where(is.numeric), ~ 100*.))

# Compute associated ICC
ICC_17 <-  simulation_ICC(logit_RE_17, 1.e6)


################################################################################

#######################################################################
#
#####                     MIXED-EFFECT MODEL 
#
#######################################################################

# This section trains a mixed-effect logistic model, with correct dispensing as 
# response, co-pack as fixed effect and facilities as random intercepts.

# Logistic mixed-effect model
logit_ME <- glmer(correct ~ copack + (1|facility),
                     data = full_df, family=binomial)

# Fit summary
s <- summary(logit_ME)

# Model coefficients (fixed intercept and co-pack)
cfs_ME <- s$coefficient

# Odds ratio of 2017 vs 2016, and 95% CI
OR <- exp(cfs_ME[,1])
OR_CI <- confint(logit_ME, oldNames=F)

# Estimated variance of random effects
std <- attr(s$varcor[[1]], "stddev")
v <- as.numeric(std)^2


####################################################################
# Likelihood ratio test vs model with only random intercepts
####################################################################

# Random-intercept model 
logit_RE <- glmer(correct ~ (1|facility),
                  data = full_df, family=binomial)
lr <- anova(logit_ME, logit_RE, test = "LR")

# Chi squared statistic and associated p-value
chisq <- lr$Chisq[2]
p_val <- lr$`Pr(>Chisq)`[2]

# PRINT MAIN RESULTS TO OUTPUT
if (answer=="y"){
  # Model summary
  cat("Coefficient summary of mixed-effect model:\n")
  print(round(cfs_ME, 3))
  cat("\nEstimated OR for co-pack in 2017 vs 2016:", 
      sprintf("%.3g.\n", OR["copackY"]))
  cat("95% Confidence Interval:", 
      sprintf("(%.3g, %.3g).", exp(OR_CI["copackY",1]), exp(OR_CI["copackY",2])))
}

# Print results to standard output
if (answer=="y"){
  cat("Likelihood ratio test of mixed-effect model vs random-effect (random intercepts) model:\n")
  cat("Chi squared statistic:", sprintf("%.4g.\n", chisq))
  cat("p-value:", sprintf("%.2g.\n\n", p_val))
}

# Alternative function, but exact same result: library(lmtest); lrtest(logit_RE, logit_ME)



################################################################################

####################################################
#
#####         STANDARD LOGISTIC MODEL
#
####################################################

# In light of previous near-zero estimate of ICC, carry out standard logistic 
# regression: co-pack and facilities as covariates

# Logistic model with both co-pack and facility
logit_all <- glm(correct ~ copack + facility, 
                 data = full_df, family=binomial)

# Logistic model with only co-pack
logit_copack_only <- glm(correct ~ copack,
                         data = full_df, family=binomial)

# LR test to compare the two models
lr <- anova(logit_all, logit_copack_only, test = "LR")
p_val <- lr$`Pr(>Chi)`[2]

# As expected, facility factor not significant
if (answer=="y"){
  cat("Likelihood ratio test on standard logistic model.\nSignificance",
      "of factor 'facility' in presence of 'co-pack':",
      sprintf("\n\tp-value: %.3g", p_val))
}



################################################################################

######################################################################
#
#####    SEPARATE LOGISTIC MODELS ON THE TWO GROUPS OF FACILITIES
#                (Mulambwa & Nalwei vs all others)
#
######################################################################

# Facilities with high CDR before co-pack vs all others
high_baseline <- facilities[c(1,4)]
low_baseline <- facilities[-c(1,4)]

# Logistic models (co-pack and facility as covariates)
logit_high <- glm(correct ~ copack + facility,
                  data = full_df %>% filter(facility %in% high_baseline), 
                  family=binomial)

logit_low <- glm(correct ~ copack + facility,
                 data = full_df %>% filter(facility %in% low_baseline), 
                 family=binomial)

# Summaries
s_high <- summary(logit_high)
s_low <- summary(logit_low)

# DA QUI

# Coefficients
cfs_high <- s_high$coefficient
cfs_low  <- s_low$coefficient

# Model summary
cat("Coefficient summary of high-baseline logistic model:\n")
print(round(summary(logit_high)$coefficient, 3))
cat("Coefficient summary of low-baseline logistic model:\n")
print(round(summary(logit_low)$coefficient, 3))

# Odds Ratios
OR_high    <- exp(cfs_high["copackY","Estimate"])
OR_high_CI <- exp(confint(logit_high, oldNames=F)["copackY",])
OR_low    <- exp(cfs_low["copackY","Estimate"])
OR_low_CI <- exp(confint(logit_low, oldNames=F)["copackY",])

cat("Estimated OR for co-pack, high-baseline model:", 
    sprintf("%.3g (%.3g, %.3g).\n", OR_high, OR_high_CI[1], OR_high_CI[2]))
cat("Estimated OR for co-pack, low-baseline model:", 
    sprintf("%.3g (%.3g, %.3g).\n", OR_low, OR_low_CI[1], OR_low_CI[2]))
# a <- cfs_low["copackY", "Estimate"]
# b <- cfs_low["copackY", "Std. Error"]
# exp(a + q95*c(-1,0,1)*b)

