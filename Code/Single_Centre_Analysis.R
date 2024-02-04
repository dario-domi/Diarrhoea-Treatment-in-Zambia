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

# Number of health centres
n <- nrow(df16)

# Create one nx3 tibble per year (prop_HC_16, prop_HC_17),
# each containing the CIs of the proportion of CTC in the seven centres 

# Initialise the two tibbles, including central estimate of proportions
props_HC_16 <- df16 %>% mutate(lwr = 0,
                               mid = 100*correct_treat/tot,
                               upr = 0) %>%
                        select(lwr, mid, upr)
props_HC_17 <- df17 %>% mutate(lwr = 0,
                               mid = 100*correct_treat/tot,
                               upr = 0) %>%
                        select(lwr, mid, upr)

# Convert to data frames and assign HCs as rownames
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


#################################################################
#       CREATE TIBBLES WITH COUNTS FOR SINGLE CENTERS

# Create 2D data frame, with last variable giving counts of children
# for each combination of (centre(7), copack(Y/N), correct_treatment(Y/N)).
# nrows = 7x2x2=28
#
# Then transform into a 3D table (dim: 2x2x7) with counts in each cell,
# and finally perform CMH test on it.

library(magrittr)

# Prepare empty dataframe
n <- nrow(df16)
Treated_Cases <- data.frame(RHC     = rep(   df16 %>% pull(RHC) , each=4),
                            Co_pack = rep( c("Y", "Y", "N", "N"), n),
                            Correct = rep( c("Y", "N", "Y", "N"), n),
                            Count   = as.numeric(rep(NA, 4*n))
                            )

# Mutate each column into the corresponding factor
Treated_Cases %<>% mutate(RHC     = factor(RHC,     levels = unique(RHC)),
                          Co_pack = factor(Co_pack, levels = unique(Co_pack)),
                          Correct = factor(Correct, levels = unique(Correct))
                          )

# Populate the dataframe with counts of CTC, 
# looping through health centers and before/after co-pack
for (hc in levels(Treated_Cases$RHC)){ # loop over health centers
  for (cp in c("Y", "N")){               # loop over with/without co-pack
    
    # Select appropriate dataframe according to value of cp (co-pack)
    if (cp=="Y") df <- df17
    else df <- df16
    
    # Extract values of CTC and not-CTC
    vals <- df %>% filter(RHC==hc) %>%
                   summarise(correct_treat, tot-correct_treat) %>%
                   as.numeric()
    
    # Assign 'vals' to the appropriate position in Treated_Cases$Counts
    Treated_Cases %<>% mutate(Count = replace(Count, 
                                              RHC==hc & Co_pack==cp, 
                                              vals) )
  }
}


# Transform data frame into 2x2x7 table
CTC_Table <- xtabs(Count ~ Correct + Co_pack + RHC, data=Treated_Cases)


#################################################################
#       COCHRAN-MANTEL-HAENSZEL TEST

# Perform Cochran-Mantel-Haenszel TEST
cmh <- mantelhaen.test(CTC_Table, alternative = "g")

# Print result to std output if requested
tx <- paste("\nResults of Mantel-Haenszel test:\n", 
        "H0: \t\t %s = %g\n",
        "H1: \t\t %s \n",
        "p-value: \t %.1g\n",
        "OR estimate: \t %.2f\n",
        "OR 95%% CI: \t (%.2f, %.2f)"
)

if (answer=="y") cat(sprintf(tx, 
                             names(cmh$null.value), cmh$null.value,
                             cmh$alternative,
                             cmh$p.value, 
                             cmh$estimate,
                             cmh$conf.int[1], cmh$conf.int[2]))




################################################################


# odds ration interpretation: pag 104 of Agresti

# we test for conditional independence of correct treatment and introduction of#
# the co-pack, controlling for the different health centres


###############################################################

###############################################################
