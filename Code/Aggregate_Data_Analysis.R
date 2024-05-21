################################################################################
#
# This script contains code written by myself (Dario Domingo) to undertake the 
# project: "Role of co-packaging in diarrhoea treatment for children in Zambia".
# The project is part of the "Statisticians for Society" initiative of the Royal
# Statistical Society. I am the lead volunteer statistician for the project.
# Github repo: https://github.com/dario-domi/Diarrhoea-Treatment-in-Zambia
#
# The statistical analyses are divided into two parts:
# - Part 1: Analysis of the aggregate data (this script)
# - Part 2: Analysis of the data as stratified across different health centres 
#           (script: Stratified_Data_Analysis.R)
#
# Results of the analyses are discussed in an associated manuscript, about to be
# submitted for publication to PLOS Global Public Health.
#
################################################################################


################################################################################
#
#####          PRELIMINARY OPERATIONS (data loading, cleaning)             #####
#
################################################################################


##################################################################
#    INTERACTIVE QUESTION TO DECIDE WHETHER TO DIPLAY RESULTS

# Ask the user if they want results to be printed to standard output
cat("Do you want summarised results of the aggregated analyses",
    "to be shown? (y/n)\n")
answer <- readline()


#############################################
#    LOAD LIBRARIES AND DATA     

library(readxl)
library(dplyr)

# Set working directory (change as appropriate)
# wd <- "/Users/Durham/Desktop/Academia/Other Projects/Statisticians4Society/Code/"
# setwd(wd)

# Read data 
filename <- "../Data/Diarrhoea_Treatments_Mongu.xlsx"
df16 <- read_xlsx(filename, 
                  sheet = 'Records 2016', range = 'B4:M11')
df17 <- read_xlsx(filename, 
                  sheet = 'Records 2017', range = 'B4:M11')

#############################################
#     TIDY UP DATA AND RENAME VARIABLES

# Reorder rows (first UHCs, then RHCs, then HPs)
new_order <- c(3, 6, 5, 1, 7, 2, 4)
df16 <- df16[new_order, ]
df17 <- df17[new_order, ]

# Remove empty columns
df16 <- df16 %>% select_if(function(x) any(x!=0))
df17 <- df17 %>% select_if(function(x) any(x!=0))

# Remove "_16" and "_17" suffices from all variable names
names(df16) <- gsub(pattern = "_16", replacement = "", x = names(df16), fixed = TRUE)
names(df17) <- gsub(pattern = "_17", replacement = "", x = names(df17), fixed = TRUE)

# Rename some variables 
df16 <- df16 %>% rename(facility = health_facility, ors = ors_alone, zinc = zinc_alone)
df17 <- df17 %>% rename(facility = health_facility, ors = ors_alone, zinc = zinc_alone)

#############################################################
#     CREATE ADDITIONAL COLUMNS
# CDCs: Correctly Dispensed Cases

# Add column of totals and of CDCs
df16 <- df16 %>% mutate(tot  = rowSums(across(where(is.numeric)), na.rm=T),
                        CDCs = ors_zinc_10 + ors_zinc_antibiotics)
df17 <- df17 %>% mutate(tot  = rowSums(across(where(is.numeric)), na.rm=T),
                        CDCs = ors_zinc_10 + co_pack + 
                               ors_zinc_antibiotics + co_pack_antibiotics)

# Add column of co-pack in 2017 data
df17 <- df17 %>% mutate(tot_co_pack = co_pack + co_pack_antibiotics)

# Swap order of columns CDCs and tot
L <- ncol(df16); df16 <- df16[, c(1:(L-2), L, L-1)]
L <- ncol(df17); df17 <- df17[, c(1:(L-3), L-1, L-2, L)]

# Remove unnecessary variables
rm(filename, L, new_order); invisible(gc())

#############################################


################################################################################
#
#####           STATISTICAL TESTS/ANALYSES, AGGLOMERATED DATA              #####
#
################################################################################

# Library to compute confidence intervals associated with binomial proportions
library(PropCIs)

#############################################
#   SHORTER NAMES FOR RELEVANT VARIABLES  

x1 <- sum(df16$CDCs)    # correctly-dispensed cases in 2016
x2 <- sum(df17$CDCs)    # correctly-dispensed cases in 2017
n1 <- sum(df16$tot)     # total cases in 2016
n2 <- sum(df17$tot)     # total cases in 2017
p1 <- x1/n1             # sample probability of correct dispensation in 2016
p2 <- x2/n2             # sample probability of correct dispensation in 2017

# Print to std output: Overview of aggregate data
s1 <- sprintf("Oct 2016:  %d correctly-dispensed cases out of %d\n", x1, n1)  #  75 out of 171
s2 <- sprintf("Oct 2017: %d correctly-dispensed cases out of %d\n\n", x2, n2) # 342 out of 394
if (answer=="y") cat("\n", s1, s2, sep = "")


#################################################
#    BY-YEAR PROPORTIONS OF CDCs AND 95% CI  

# Sample proportions and exact 95% confidence intervals (Clopper-Pearson) for
# the two years. The same results would be obtained through binom.test(x1, n1)
aggr_props_16 <- exactci(x1, n1, conf.level = 0.95)$conf.int
aggr_props_17 <- exactci(x2, n2, conf.level = 0.95)$conf.int

# Print to std output: Proportions of CTC
s1 <- sprintf("Oct 2016. Sample Proportion of CDCs: %.2f, CI: (%.2f, %.2f)\n", 
              p1, aggr_props_16[1], aggr_props_16[2])
s2 <- sprintf("Oct 2017. Sample Proportion of CDCs: %.2f, CI: (%.2f, %.2f)\n\n", 
              p2, aggr_props_17[1], aggr_props_17[2])
if (answer=="y") cat(s1, s2, sep = "") # 0.46 (0.38, 0.53)
                                       # 0.87 (0.83, 0.90)

# Create a single dataframe with aggregated CIs for 2016 and for 2017.
# The variable will be used in script Plots.R:
aggr_props <- data.frame(lwr = 100*c(aggr_props_16[1], aggr_props_17[1]),
                         mid = 100*c(p1, p2),
                         upr = 100*c(aggr_props_16[2], aggr_props_17[2]))

rownames(aggr_props) <- c("Before\nCo-Pack", "After\nCo-Pack")
aggr_props <- as.matrix(aggr_props)


############################################################
#        TEST OF HOMOGENEITY OF PROPORTIONS   

test_homog <- prop.test(c(x2, x1), c(n2, n1), alternative = "g") # p < e-16

# Print to std output: Overview of agglomerated data
if (answer=="y")  cat("To see results of test of homogeneity of proportions,",
                      "type 'test_homog'.\n\n")


############################################################
#   CONFIDENCE INTERVAL FOR DIFFERENCE OF PROPORTIONS

# Wald CI (formula of link n4 at end of script. See also Agresti 2002, pag 27, and exercise 2.15)
prop_diff_CI <- wald2ci(x2, n2, x1, n1, conf.level = 0.95, adjust = "Wald") # (0.3479, 0.5110)

# Explicit calculations behind the above Wald CI (uses that log(p1/p2) is approx normal, large sample)
v <- p1*(1-p1)/n1 + p2*(1-p2)/n2
q <- qnorm(0.975)
round(p2-p1 + c(-1,1)*q*sqrt(v), 4) # (0.3479, 0.5110)
rm(q,v)

# Print to std output: Difference between proportions
s <- sprintf("Difference between proportions: %.2f (%.2f, %.2f)\n",
             prop_diff_CI$estimate, 
             prop_diff_CI$conf.int[1], prop_diff_CI$conf.int[2])
if (answer=="y") cat(s)


############# CONFIDENCE INTERVAL FOR RATE RATIO   #############

# Rate ratio (NOT computed through lognormal approximation)
rateCI95 <- riskscoreci(x2, n2, x1, n1, conf.level = 0.95) # (1.681, 2.378)
rateCI99 <- riskscoreci(x2, n2, x1, n1, conf.level = 0.99) # (1.605, 2.528)

# Print to std output: Rate Ratio
s <- sprintf("Rate ratio between proportions: %.2f.\n\t95%% CI: (%.2f, %.2f)\n\t99%% CI: (%.2f, %.2f).",
             p2/p1, rateCI95$conf.int[1], rateCI95$conf.int[2],
             rateCI99$conf.int[1], rateCI99$conf.int[2])
if (answer=="y") cat(s, "\n")

# Alternative as described in example 2.15 of Agresti's book, not equal to the above
s <- sqrt( (1-p1)/x1 + (1-p2)/x2 )
exp(log(p2/p1) + c(-1,1) * qnorm(0.975)*s)


########################################
#             END 
#             OF 
#           SCRIPT
########################################




###############################################################################

# Below some online references and equivalent/alternative analyses to
# a couple of points above

# Same test as prop.test, a few more output details
library(rstatix)
test_homog2 <- prop_test(x = c(x2, x1), n = c(n2, n1), 
                         alternative = "g", detailed = T)


# Alternative CI for difference of proportions
diffscoreci(x1, n1, x2, n2, conf.level = 0.95) # (0.3467, 0.5087)


##########################################

#1 Link to compute confidence interval for difference of proportions. If want to perfectly replicate MachBath, Barry 2022 paper (Table 1), use continuity correction:
# https://www.statskingdom.com/two-proportions-ci-calculator.html

#2 Binom test CI theory
# https://rcompanion.org/handbook/H_02.html

#3 Rate ratios and risk ratios
#https://sphweb.bumc.bu.edu/otlt/mph-modules/ep/ep713_association/ep713_association3.html

#4 CI for rate ratio and odds ratio
#https://influentialpoints.com/Training/confidence_intervals_of_risk_ratio_odds_ratio_and_rate_ratio-principles-properties-assumptions.htm

############################################################################