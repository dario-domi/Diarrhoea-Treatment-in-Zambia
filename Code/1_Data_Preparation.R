################################################################################
#
# FIRST SCRIPT (DATA LOADING, CLEANING AND FORMATTING)
#
# This script is part of a set of scripts written by myself (Dario Domingo) to
# undertake the project: 
# "Role of co-packaging in diarrhoea treatment for children in Zambia".
# The project is part of the "Statisticians for Society" initiative of the Royal
# Statistical Society. I am the lead volunteer statistician for the project.
#
# Github repo: https://github.com/dario-domi/Diarrhoea-Treatment-in-Zambia
#
# The present script loads the data and prepares them in a format useful to 
# carry out the relevant statistical analyses ("Analysis.R" script).
# Upon running the script, an interactive question pops up, asking the user 
# whether basic summaries about the data should be printed to output.
#
# Results of the analyses are discussed in an associated manuscript, currently
# under peer review for consideration in PLOS Global Public Health.
#
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
cat("Do you want summarised results of the data to be shown? (y/n)\n")
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

################################################################################


###################################################################################
# ALSO CREATE AN EQUIVALENT DATAFRAME, IN THE STANDARD FORM NEEDED FOR REGRESSION
# (rows=observations, columns=variables)

# Vector of facilities names
facilities <- df16$facility

# Number of observations (in each year, and overall)
n16 <- sum(df16$tot)
n17 <- sum(df17$tot)
n <- n16+n17

# Prepare the empty dataframe (one row for each case)
full_df <- data.frame(facility = factor(rep(NA, n), levels = facilities),
                      copack   = factor(rep(NA, n), levels = c("N", "Y")),
                      correct  = factor(rep(NA, n), levels = c(0,1))
)

### AUXILIARY FUNCTION: populate_fields
# Takes a dataframe df in input, with columns: facility, copack, correct.
# Fills (row_yes + row_no) number of rows, starting from row1. 
# The facility and copack columns are filled as specified by the first two arguments.
# The correct column is filled row_yes times with 1, and row_no times with 0.

populate_fields <- function(df, fac, copack, row1, row_yes, row_no){
  
  # Total number of rows to be filled, and final row
  totrows <- row_yes + row_no
  row2 <- row1 + totrows - 1
  
  # Fill columns 'facility' and 'copack', between rows 'row1' & 'row2'
  df$facility[row1:row2] <- rep(fac, totrows)
  df$copack[row1:row2]   <- rep(copack, totrows)

  # Fill column 'correct' with: 1 for row_yes times; 0 for row_no times
  if(row_yes>0) 
    df$correct[row1: (row1+row_yes-1)]  <- rep(1, row_yes)
  if(row_no>0)
    df$correct[(row1+row_yes):row2]  <- rep(0, row_no)
  
  return(df)
  
}

### Use previous function to fill 'full_df' dataframe (one observation per row)
row1=1
for(fac in facilities){
  for(df in list(df16, df17)){
    
    # Number of correct/incorrect dispensation cases (for one facility and one of the two years)
    n_correct <-  df %>% filter(facility==fac) %>% pull(CDCs)
    n_wrong   <- (df %>% filter(facility==fac) %>% pull(tot)) - n_correct
    
    # Fill the relevant rows of full_df (using above auxiliary function)
    full_df <- populate_fields(df = full_df, fac = fac,
                               copack = ifelse(identical(df, df16), "N", "Y"),
                               row1, n_correct, n_wrong)
    
    # Update the value of the starting row before next loop cycle starts
    row1 <- row1 + n_correct + n_wrong
  }
}


################################################################################
#
#####                     OVERVIEW OF THE DATA                             #####
#
################################################################################


#############################################
#   SHORTER NAMES FOR RELEVANT VARIABLES  

x1 <- sum(df16$CDCs)    # correctly-dispensed cases in 2016
x2 <- sum(df17$CDCs)    # correctly-dispensed cases in 2017
n1 <- sum(df16$tot)     # total cases in 2016
n2 <- sum(df17$tot)     # total cases in 2017
p1 <- x1/n1             # sample proportion of correct dispensation in 2016
p2 <- x2/n2             # sample proportion of correct dispensation in 2017

# Print to std output: Overview of aggregate data
s <- "Aggregate data\n"
s1 <- sprintf("Oct 2016:  %d correctly-dispensed cases out of %d\n", x1, n1)  #  75 out of 171
s2 <- sprintf("Oct 2017: %d correctly-dispensed cases out of %d\n\n", x2, n2) # 342 out of 394
if (answer=="y") 
  cat("\n", s, s1, s2, sep = "")

# Can also add: Correct dispensation by facility:

########################################
#             END 
#             OF 
#           SCRIPT
########################################
