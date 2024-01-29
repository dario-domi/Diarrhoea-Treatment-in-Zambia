#library(haven)
#read_dt
#data <- read_dta("cola.dta")

#1 Link to compute confidence interval for difference of proportions. If want to perfectly replicate MachBath, Barry 2022 paper (Table 1), use continuity correction:
# https://www.statskingdom.com/two-proportions-ci-calculator.html

#2 Binom test CI theory
# https://rcompanion.org/handbook/H_02.html

#3 Rate ratios and risk ratios
#https://sphweb.bumc.bu.edu/otlt/mph-modules/ep/ep713_association/ep713_association3.html

#4 CI for rate ratio
#https://influentialpoints.com/Training/confidence_intervals_of_risk_ratio_odds_ratio_and_rate_ratio-principles-properties-assumptions.htm


############################################################################

############################################################################


###################################
#    LOAD LIBRARIES AND DATA     
###################################

library(readxl)
library(dplyr)

# Set working directory and read data
wd <- "/Users/Durham/Desktop/Academia/Other Projects/Statisticians4Society/Code_and_Pics/"
setwd(wd)
filename <- "../Data/Diarrhoea Treatments Mongu.xlsx"
df <- read_xlsx(filename, range = "Data!C3:AE10")


##############################
#     PREPROCESS DATA 
##############################

# Remove empty columns
df <- df %>% select_if(function(x) !(all(is.na(x))))

# Remove additional unneeded columns
cols_to_delete <- c("total_1_16", "total_2_16", "check_16", "total_1_17", "total_2_17")
df <- df %>% select(!all_of(cols_to_delete)) # remove columns

# Rename "Health Centres" column
df <- df %>% rename(RHC = health_centre)
# Remove " RHC" suffix from names of health centres which have it, 
for (i in 1:nrow(df)){
  name <-df$RHC[i]
  L <- nchar(name)
  if (substr(name, L-3,L)==" RHC")          # if last 4 characters are " RCH"
    df[i, "RHC"] <- substr(name, 1, L-4)    # remove those characters
}


###################################
#     CREATE TWO DATAFRAMES  
###################################

# Split into two dataframes (Oct16 and Oct 17)
df16 <- df %>% select(RHC, ends_with("_16"))
df17 <- df %>% select(RHC, ends_with("_17"))

# Remove "_16" and "_17" suffices from all variable names
names(df16) <- gsub(pattern = "_16", replacement = "", x = names(df16), fixed = TRUE)
names(df17) <- gsub(pattern = "_17", replacement = "", x = names(df17), fixed = TRUE)

# Rename other vars
df16 <- df16 %>% rename(ors = ors_alone, zinc = zinc_alone)
df17 <- df17 %>% rename(ors = ors_alone, zinc = zinc_alone)

# Replace NAs with 0s
df16[is.na(df16)] <- 0
df17[is.na(df17)] <- 0


# Add column of totals
df16 <- df16 %>% mutate(tot = rowSums(across(where(is.numeric)), na.rm=T)) %>%
                 mutate(correct_treat = ors_zinc_10 + ors_zinc_lt10 + ors_zinc_antibiotics)
df17 <- df17 %>% mutate(tot = rowSums(across(where(is.numeric)), na.rm=T)) %>%
                 mutate(correct_treat = ors_zinc_10 + co_pack + 
                                        ors_zinc_antibiotics + co_pack_antibiotics)


################################################
#  STATISTICAL ANALYSES, AGGLOMERATED DATA
################################################

library(PropCIs)
library(DescTools)


########  DEFINE MAIN VARIABLES  ##########

# The following results (in 2016) count as correct cases even the ones
# where less than 10 zinc tablets were prescribed. 
corr16 <- sum(df16$correct_treat)
tot16 <- sum(df16$tot)
corr17 <- sum(df17$correct_treat)
tot17 <- sum(df17$tot)

# Overview of agglomerated data
cat("October 2016:", corr16, "out of", tot16, "correctly treated cases.\n")
cat("October 2017:", corr17, "out of", tot17, "correctly treated cases.\n")

# Shorter names, for convenience
x1 <- corr16
n1 <- tot16
x2 <- corr17
n2 <- tot17
p1 <- x1/n1
p2 <- x2/n2


############  SEPARATE PROPORTIONS AND 95% CI  ##################

# Sample proportions and exact 95% confidence intervals (Clopper-Pearson)
prop16 <- exactci(x1, n1, conf.level = 0.95)$conf.int
prop17 <- exactci(x2, n2, conf.level = 0.95)$conf.int
# Same result with binom.test(corr16, tot16)

sprintf("Oct 2016. Sample Proportion: %.2f. CI: (%.2f, %.2f)", 
        p1, prop16[1], prop16[2])
sprintf("Oct 2017. Sample Proportion: %.2f. CI: (%.2f, %.2f)", 
        p2, prop17[1], prop17[2])

# Create dataframe with CIs for the two proportions
props <- data.frame(lwr = 100*c(prop16[1], prop17[1]),
                    mid = 100*c(p1, p2),
                    upr = 100*c(prop16[2], prop17[2]))
rownames(props) <- c("Before\nCo-Packaging", 
                     "After\nCo-Packaging")
props <- as.matrix(props)


# Estimate in Oct 2016, *without* cases with less than 10 zinc tables
#exactci(66, 176, conf.level = 0.95)


############# TEST OF HOMOGENEITY OF PROPORTIONS   #############

test_homog <- prop.test(c(x1, x2), c(n1, n2), alternative = "l") # (0.3440, 0.5137), p < e-16
test_homog

# Alternative, more details
#library(rstatix)
#prop_test(x = c(x1, x2), n = c(n1, n2), 
#          alternative = "g", detailed = T)


############# CONFIDENCE INTERVAL FOR DIFFERENCE OF PROPORTIONS   #############

# Wald CI (link n4 at beginning of script. See also Agresti 2002, pag 27, and exercise 2.15)
prop_diff_CI <- wald2ci(x2, n2, x1, n1, conf.level = 0.95, adjust = "Wald") # (0.3481, 0.5095)

# Calculations behind the above Wald CI (uses that log(p1/p2) approx normal, large sample)
v <- p1*(1-p1)/n1 + p2*(1-p2)/n2
q <- qnorm(0.975)
p2-p1 + c(-1,1)*q*sqrt(v) # (0.3481, 0.5095)

# Additional CIs
# diffscoreci(x1, n1, x2, n2, conf.level = 0.95) # (0.3468, 0.5073)


############# CONFIDENCE INTERVAL FOR RATE RATIO   #############

# Rate ratio (NOT computed through lognormal approximation)
rateCI95 <- riskscoreci(x2, n2, x1, n1, conf.level = 0.95) # (1.685, 2.374)
rateCI99 <- riskscoreci(x2, n2, x1, n1, conf.level = 0.99) # (1.685, 2.374)

round(rateCI95$conf.int, 2)
round(rateCI99$conf.int, 2)
cat("Actual rate ratio of the data:", p2/p1, "\n") # 1.98


####################################################################
####################################################################


################################################
#  STATISTICAL ANALYSES, SINGLE-CENTER DATA
################################################

# Number of health centres
n <- nrow(df16)

############# CONFIDENCE INTERVALS FOR PROPORTION   #############

# BUILD TWO TIBBLES (ONE PER YEAR) WITH CONFIDENCE INTERVALS OF PROPORTION OF CTC, 
# FOR EACH CENTRE 

# Initialise the two tibbles, including central estimate of proportions
props_HC_16 <- df16 %>% mutate(lwr = 0,
                               mid = 100*correct_treat/tot,
                               upr = 0) %>% select(lwr, mid, upr)
props_HC_17 <- df17 %>% mutate(lwr = 0,
                               mid = 100*correct_treat/tot,
                               upr = 0) %>% select(lwr, mid, upr)

props_HC_16 <- as.data.frame(props_HC_16)
props_HC_17 <- as.data.frame(props_HC_17)
rownames(props_HC_16) <- rownames(props_HC_17) <- df16$RHC


# Populate lwr and upr columns of CIs
for (i in 1:n){
  xi <- df16$correct_treat[i]
  ni <- df16$tot[i]
  ci <- exactci(xi, ni, conf.level = 0.95)$conf.int
  props_HC_16[i, c("lwr", "upr")] <- as.list(100*ci)
  
  xi <- df17$correct_treat[i]
  ni <- df17$tot[i]
  ci <- exactci(xi, ni, conf.level = 0.95)$conf.int
  props_HC_17[i, c("lwr", "upr")] <- as.list(100*ci)
}

