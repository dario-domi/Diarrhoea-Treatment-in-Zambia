################################################################################
# This script produces the plots associated with the project
# "Role of co-packaging in diarrhoea treatment for children in Zambia".
# Project in collaboration with the UK charity ColaLife, as part of the 
# "Statisticians for Society" initiative of the Royal Statistical Society.
#
# Github repo: https://github.com/dario-domi/Diarrhoea-Treatment-in-Zambia
#
################################################################################

# Run main program to create the variables needed
source("2_Statistical_Analyses.R")

# Relative path to the 'Pictures' folder
picpath <- "../Pictures/"

#################################################################
#      INTERACTIVE QUESTION TO CHOOSE COLOUR BACKGROUND

# Ask the user if they want plots to have dark or light background
cat("\nDo you want barplots with dark- or light- background to be produced?",
    "\nType 'dark' or 'light':\n")
answer <- readline()

while (!(answer %in% c("dark", "light")) ){
  cat("Please only type 'dark' or 'light':\n")
answer <- readline()
}


#################################################################
#   SET COLOURS ACCORDING TO ANSWER IN INTERACTIVE QUESTION 

####### Dark background
if (answer=="dark") { 
  
  # Bar colors
  fill_before <- "turquoise1"
  border_before <- "deepskyblue1"
  fill_after <- "lightgreen"
  border_after <- "mediumseagreen"
  diffcol <- "orangered3"   # colour for whiskers of difference of proprortions 
  
  # Foregroud and background colors
  fgcol <- gray(0.95)
  bgcol <- rgb(61, 55, 72, maxColorValue = 255)
}

####### White background
if (answer =="light"){
  # Bar colors
  fill_before <- "deepskyblue1"
  border_before <- "royalblue2"
  fill_after <- "seagreen3"
  border_after <- "seagreen4"
  diffcol <- "brown2"   # colour for whiskers of difference of proprortions 
  
  # Foregroud and background colors
  fgcol <- gray(0.05)
  bgcol <- gray(0.98)
}



#######################################################################
#
#   BARPLOT FOR CORRECT DISPENSING RATES OF INDIVIDUAL FACILITIES
#
#######################################################################


# CREATE FUNCTION WHICH PRODUCES THE PLOT
# The plot is based on values in the dataframes props_16 and props_17

plot_facility_props <- function(){
  
  ############   GRAFICAL PARAMETERS   ##########
  
  xrng <- c(0, 26)
  yrng <- c(0, 100)
  xvals <- seq(from=1, by=3.5, len=7)
  
  # Half distance between blue and green bars
  offset <- 0.1
  
  # Set black-ish background and margins
  par(bg = bgcol, mar = c(4, 6, 4, 3))
  
  ############   ACTUAL PLOT   #############
  
  # Set x&y plot limits
  plot(1, xlab = "", ylab = "", xaxt = "n", yaxt = "n", 
       bty = "n", ty="n", xlim = xrng, ylim = yrng)
  
  # Add horizontal gridlines
  abline(h = seq(0, 100, 20), 
         lty = 2, col = fgcol, lwd = 1)
  
  # y-axis labels
  mtext(text = paste0(seq(0, 100, 20), "%"),
        side = 2, at = seq(0, 100, 20),
        cex = 1.3, las = 1, line = 1, 
        col = fgcol)
  
  # Blue bars
  rect(xleft = xvals - 1,
       ybottom = 0,
       xright = xvals - offset,
       ytop = props_16[,"mid"],
       col = fill_before, border = border_before, lwd=2)
  
  # Green bars
  rect(xleft = xvals + offset,
       ybottom = 0,
       xright = xvals + 1,
       ytop = props_17[,"mid"],
       col = fill_after, border = border_after, lwd=2)
  
  # Confidence Intervals blue bars
  arrows(x0 = xvals - 0.5 - offset/2, 
         x1 = xvals - 0.5 - offset/2, 
         y0 = props_16[,"lwr"], 
         y1 = props_16[,"upr"],
         col = border_before, lwd = 2, 
         lend = 0, ljoin = 2,
         angle = 70, length = 0.07, code=3)
  
  # Confidence Intervals green bars
  arrows(x0 = xvals + 0.5 + offset/2, 
         x1 = xvals + 0.5 + offset/2, 
         y0 = props_17[,"lwr"], 
         y1 = props_17[,"upr"],
         col = border_after, lwd = 2, 
         lend = 0, ljoin = 2,
         angle = 70, length = 0.07, code=3)
  
  # x labels (split each facility name into two lines)
  mtext(text = gsub(" ", "\n", facilities), cex=1.25,
        side = 1, at = xvals, las = 1,
        line = 1.3, col = fgcol)
  
  # Title
  mtext("Correct Dispensation Rates by Facility", 
        side = 3, font=3, col = fgcol, line = 0.7, cex=2)
  
  # Legend
  legend(23.6, 101, c("Before\nCo-pack", "After\nCo-pack"),
         pch=c(22,22), pt.cex=2.5, pt.lwd=1.7,
         col = c(border_before, border_after),
         pt.bg = c(fill_before, fill_after), 
         y.intersp=2.2, x.intersp=0.95,
         cex = 1.2,
         text.col = fgcol,
         bty = "n")  # no box
}


# SAVE THE PLOT IN PNG
png(file = paste0(picpath, "Individual_Proportions_", answer, ".png"),
    width = 12, 
    height = 6,
    units = "in",
    res = 600)
plot_facility_props()   # Make the plot
dev.off()               # Save the plot


# # SAVE THE PLOT IN PDF
# pdf(file = paste0(picpath, "Individual_Proportions_", ".pdf"),  # Specify figure size and path
#     width = 12, 
#     height = 6)
# plot_HC_props()    
# dev.off()   


# SAVE THE PLOT IN EPS
# setEPS() 
# postscript(paste0(picpath, "Individual_Proportions", answer, ".eps"),  # Specify figure size and path
#            width = 12, 
#            height = 6)
# plot_facility_props() 
# dev.off()
