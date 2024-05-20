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


##################################################################
#    INTERACTIVE QUESTION TO DECIDE WHETHER TO DIPLAY RESULTS

# Ask the user if they want results to be printed to standard output
cat("\nDo you want summarised results on the stratified analyses",
    "to be shown? (y/n)\n")
answer <- readline()


#################################################################
#      CONFIDENCE INTERVALS OF PROPORTION (FOR EACH CENTRE) 

# For each year (2016 & 2017), create one 7x3 tibble (prop_HC_16 & prop_HC_17).
# The tibble contains the CIs of the proportion of correctly-dispensed cases (CDCs), 
# for each of the seven facilities 

# Initialise the two tibbles, including central estimate of proportions
props_HC_16 <- df16 %>% mutate(lwr = 0,
                               mid = 100*CDCs/tot,
                               upr = 0) %>%
                        select(lwr, mid, upr)
props_HC_17 <- df17 %>% mutate(lwr = 0,
                               mid = 100*CDCs/tot,
                               upr = 0) %>%
                        select(lwr, mid, upr)

# Convert to data frames and assign facility names as row names
props_HC_16 <- as.data.frame(props_HC_16)
props_HC_17 <- as.data.frame(props_HC_17)
rownames(props_HC_16) <- rownames(props_HC_17) <- df16$facility

# Populate lwr and upr columns of CIs in prop_HC_16 and prop_HC_17
n <- nrow(df16) 
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

# Show CI for different of proportions, for each facility
if (answer=="y") {
  cat("Difference of Proprortions and 95% Confidence Interval\n")
  for (j in 1:7){
    wci <- wald2ci(df17$CDCs[j], df17$tot[j], df16$CDCs[j], df16$tot[j], 
            conf.level = 0.95, adjust = "Wald")
    a <- wci$estimate
    b <- wci$conf.int[1]
    c <- wci$conf.int[2]
    cat(df16$facility[j], ":\t", sprintf("%.2f (%.3f, %.3f)\n", a,b,c), sep="")
  }
}


#################################################################
#       CREATE TIBBLES WITH COUNTS FOR SINGLE CENTERS

# Create a 2D data frame, with last variable giving counts of cases
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
             "Chi2: \t\t %.4g\n",
             "p-value: \t %.1g\n",
             "OR estimate: \t %.2f\n",
             "OR 95%% CI: \t (%.2f, %.2f)",
             "\n"
)


if (answer=="y") cat(sprintf(out, 
                             names(cmh$null.value), cmh$null.value,
                             cmh$alternative,
                             cmh$statistic,
                             cmh$p.value, 
                             cmh$estimate,
                             cmh$conf.int[1], cmh$conf.int[2]))

# Check that value of common OR in Mantel-Haenszel test is as expected
a <- 0
b <- 0
for (i in 1:7){
  a <- a + CDC_Table[1,1,i]*CDC_Table[2,2,i]/sum(CDC_Table[,,i])
  b <- b + CDC_Table[1,2,i]*CDC_Table[2,1,i]/sum(CDC_Table[,,i])
}
a/b


#################################################################
#       ODDS RATIOS FOR INDIVIDUAL CENTRES AND TOTAL

OR_data <- tibble(mean  = rep(0,7),
                  lower = rep(0,7),
                  upper = rep(0,7),
                  facility = df16$facility,
                  Odds16 = rep("a",7),
                  Odds17 = rep("b",7),
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

  # CDCs/(Not CDCs) cases, for 2016 and 2017
  s1 <- as.character(x1)
  s2 <- as.character(n1-x1)
  str17 <- paste0(s1, "/", s2)
  s1 <- as.character(x2)
  s2 <- as.character(n2-x2)
  str16 <- paste0(s1, "/", s2)

  # Populate the tibble
  OR_data$mean[centre]  <- OR
  OR_data$lower[centre] <- ci[1]
  OR_data$upper[centre] <- ci[2]
  OR_data$OR[centre] <- as.character(signif(OR,2))
  OR_data$Odds16[centre] <- str16
  OR_data$Odds17[centre] <- str17
  
}

# Fill in information for aggregate data
OR_summary <- OR_data %>% slice(1)
OR_summary$facility <- "Total"
OR_summary$OR       <- as.character(round(cmh$estimate,1))
OR_summary$mean     <- cmh$estimate
OR_summary$lower    <- cmh$conf.int[1]
OR_summary$upper    <- cmh$conf.int[2]
Tb <- apply(CDC_Table, c(1,2), sum)
OR_summary$Odds16   <- paste0( as.character(Tb[1,2]), "/", as.character(Tb[2,2]))
OR_summary$Odds17   <- paste0( as.character(Tb[1,1]), "/", as.character(Tb[2,1]))

