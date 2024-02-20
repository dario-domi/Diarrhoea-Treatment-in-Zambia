################################################################################
#
# This script contains the code written by myself (Dario Domingo), for the project:
# "Role of co-packaging in diarrhoea treatment for children in Zambia".
# The project is part of the "Statisticians for Society" initiative, by the 
# Royal Statistical Society. 
#
# The analyses performed here are discussed in the manuscript associated with 
# the project, in preparation for submission to the British Medical Journal.
#
# The manuscript is divided into two parts,and accordingly there are two R scripts:
# - Part 1: Analyses on the agglomerated data (Agglomerated_Data_Analysis.R)
# - Part 2: Analyses on the data as stratified over the centres (this script)
#
################################################################################



################################################################################
#
#####            STATISTICAL TESTS/ANALYSES, STRATIFIED DATA               #####
#
################################################################################

# wd <- "/Users/Durham/Desktop/Academia/Other Projects/Statisticians4Society/Code/"
# setwd(wd)

# Source script which creates all needed variables
source("Agglomerated_Data_Analysis.R")


#################################################################
#      INTERACTIVE QUESTION 

# Ask the user if they want results to be printed to output
cat("\nDo you want summarised results on the stratified analyses",
    "to be shown? (y/n)\n")
answer <- readline()


#################################################################
#      CONFIDENCE INTERVALS OF PROPORTION (FOR EACH CENTRE) 

# Number of facilities
n <- nrow(df16)

# Create one nx3 tibble per year (prop_HC_16, prop_HC_17),
# each containing the CIs of the proportion of CDCs (correctly-dispensed cases) 
# in the seven facilities 

# Initialise the two tibbles, including central estimate of proportions
props_HC_16 <- df16 %>% mutate(lwr = 0,
                               mid = 100*CDCs/tot,
                               upr = 0) %>%
                        select(lwr, mid, upr)
props_HC_17 <- df17 %>% mutate(lwr = 0,
                               mid = 100*CDCs/tot,
                               upr = 0) %>%
                        select(lwr, mid, upr)

# Convert to data frames and assign HCs as rownames
props_HC_16 <- as.data.frame(props_HC_16)
props_HC_17 <- as.data.frame(props_HC_17)
rownames(props_HC_16) <- rownames(props_HC_17) <- df16$facility


# Populate lwr and upr columns of CIs
for (i in 1:n){
  xi <- df16$CDCs[i]
  ni <- df16$tot[i]
  ci <- exactci(xi, ni, conf.level = 0.95)$conf.int
  props_HC_16[i, c("lwr", "upr")] <- as.list(100*ci)
  
  xi <- df17$CDCs[i]
  ni <- df17$tot[i]
  ci <- exactci(xi, ni, conf.level = 0.95)$conf.int
  props_HC_17[i, c("lwr", "upr")] <- as.list(100*ci)
}


#################################################################
#       CREATE TIBBLES WITH COUNTS FOR SINGLE CENTERS

# Create 2D data frame, with last variable giving counts of cases
# for each combination of (centre(7), copack(Y/N), correct_dispensing(Y/N)).
# nrows = 7x2x2=28
#
# Then transform into a 3D table (dim: 2x2x7) with counts in each cell.

library(magrittr)

# Prepare empty dataframe
n <- nrow(df16)
Treated_Cases <- data.frame(Facility = rep(   df16 %>% pull(facility) , each=4),
                            Co_pack  = rep( c("Y", "Y", "N", "N"), n),
                            Correct  = rep( c("Y", "N", "Y", "N"), n),
                            Count    = as.numeric(rep(NA, 4*n))
                            )

# Mutate each column into the corresponding factor
Treated_Cases %<>% mutate(Facility = factor(Facility, levels = unique(Facility)),
                          Co_pack  = factor(Co_pack,  levels = unique(Co_pack)),
                          Correct  = factor(Correct,  levels = unique(Correct))
                          )

# Populate the dataframe with counts of CDCs, 
# looping through facilities and before/after co-pack
for (fac in levels(Treated_Cases$Facility)){ # loop over facilities
  for (cp in c("Y", "N")){                   # loop over with/without co-pack
    
    # Select appropriate dataframe according to value of cp (co-pack)
    if (cp=="Y") df <- df17
    else df <- df16
    
    # Extract counts of CDCs and Incorrectly DCs
    vals <- df %>% filter(facility==fac) %>%
                   summarise(CDCs, tot-CDCs) %>%
                   as.numeric()
    
    # Assign 'vals' to the appropriate position in Treated_Cases$Counts
    Treated_Cases %<>% mutate(Count = replace(Count, 
                                              Facility==fac & Co_pack==cp, 
                                              vals) )
  }
}


# Transform data frame into 2x2x7 table
CDC_Table <- xtabs(Count ~ Correct + Co_pack + Facility, data=Treated_Cases)


#################################################################
#       COCHRAN-MANTEL-HAENSZEL TEST

# Perform Cochran-Mantel-Haenszel TEST
cmh <- mantelhaen.test(CDC_Table, alternative = "t")

# Print result to std output if requested
out <- paste("\nResults of Mantel-Haenszel test:\n", 
             "H0: \t\t %s = %g\n",
             "H1: \t\t %s \n",
             "p-value: \t %.1g\n",
             "OR estimate: \t %.2f\n",
             "OR 95%% CI: \t (%.2f, %.2f)"
)


if (answer=="y") cat(sprintf(out, 
                             names(cmh$null.value), cmh$null.value,
                             cmh$alternative,
                             cmh$p.value, 
                             cmh$estimate,
                             cmh$conf.int[1], cmh$conf.int[2]))



#################################################################
#       ODDS RATIO FOR INDIVIDUAL CENTRES AND TOTAL


OR_data <- tibble(mean  = rep(0,7),
                  lower = rep(0,7),
                  upper = rep(0,7),
                  facility = df16$facility,
                  CDC16 = rep("a",7),
                  CDC17 = rep("b",7),
                  OR = rep(NA, 7),
                  empty = rep(NA, 7)
)

for (centre in 1:n){
  
  # 2x2 table with CDCs and not CDCs, before and after co-pack
  Tb <- CDC_Table[ , , centre]
  
  # Extract each xi and ni
  x1 <- Tb[1, "Y"]       # CDCs in 2017
  n1 <- sum(Tb[, "Y"])   # tot  in 2017
  x2 <- Tb[1, "N"]       # CDCs in 2016
  n2 <- sum(Tb[, "N"])   # tot  in 2016
  
  # Odds ratio and Conf. Interval
  ci <- orscoreci(x1, n1, x2, n2, conf.level = 0.95)
  ci <- as.numeric(ci$conf.int)
  OR <- (x1/(n1-x1)) / (x2/(n2-x2))   #  exp(mean(log(ci)))

  # Populate the tibble
  OR_data$mean[centre]  <- OR
  OR_data$lower[centre] <- ci[1]
  OR_data$upper[centre] <- ci[2]
  OR_data$OR[centre] <- as.character(round(OR,2))
}




s <- sqrt( 1/x1 + 1/x2 + 1/(n1-x1) + 1/(n2-x2))
(log(ci[2])-log(ci[1]))/(2*s)



#################################################################
#       MIXED-EFFECTS LOGISTIC REGRESSION

library(lme4)

# Create tibble to be used in logistic regression,
# with counts of CDCs and of total cases
Props_Data <- Treated_Cases %>% 
                  group_by(Facility, Co_pack) %>%
                  mutate(Totals = sum(Count)) %>%
                  filter(Correct=="Y") %>%
                  select(-any_of("Correct"))

fit1 <- glmer(Count/Totals ~ Co_pack + (1|Facility), 
              data = Props_Data, weights=Totals, family=binomial)

fit1 <- glmer(cbind(Count, Totals-Count) ~ Co_pack + (1|Facility), 
              data = Props_Data, family=binomial,
              nAGQ = 100)
dotplot.ranef.mer(ranef(fit1))

glmer(Count/Totals ~ offset(log(Totals)) + Co_pack + (1|Facility), 
      data=Props_Data, family=poisson)


########################################
#
# PROVE CON SYNTETIC DATA
#

Tab <- Props_Data
Tab <- Tab[1:8, ]

Tab[1, "Count"] <- 35
Tab[2, "Count"] <- 12
Tab[3, "Count"] <- 26
Tab[4, "Count"] <- 2
Tab[5, "Count"] <- 189
Tab[6, "Count"] <- 46
Tab[7, "Count"] <- 24
Tab[8, "Count"] <- 9

Tab

fit2 <- glmer(Count/Totals ~ Co_pack + (1|RHC), 
             data = Tab, weights=Totals, family=binomial)

summary(fit2)
coef(fit2)

myfunc <- function(x){x[1]/x[2]}
# Computing odds ratios
Tab %>% mutate(p = Count/Totals,
               odds = p/(1-p),
               logodds = log(odds)) %>%
        group_by(RHC) %>% 
        mutate(OR = myfunc(odds)) %>%
        select(RHC, p, odds, logodds, OR)



ranef(fit2)$RHC
x <- as.numeric(unlist(ranef(fit2)$RHC))
x
var(x)
sum(x)

dotplot.ranef.mer(ranef(fit2))


# Compute CIs from ranef
dd <- as.data.frame(ranef(fit2))
dd %>% select(grp, condval, condsd) %>% 
       mutate(across(where(is.numeric), round, digits=3))


# Potentially good terminology
# https://bookdown.org/steve_midway/DAR/random-effects.html
# Gelman and Hill (2006)

fit3 <- glmer(cbind(Count, Totals-Count) ~ Co_pack + (1|RHC), 
              data = Tab, family=binomial)

summary(fit1)
summary(fit2)

x <- as.numeric(unlist(ranef(fit2)$RHC))
x
var(x)
sd(x)
################################################################


# odds ration interpretation: pag 104 of Agresti

# we test for conditional independence of correct treatment and introduction of#
# the co-pack, controlling for the different health centres


###############################################################

###############################################################


# For example, if we had a species for which there were a large number of observations and a species in which there were only one observation, under a disaggregated approach the species with a large number of observations would have a disproportionate influence on the outcome of a disaggregated model.