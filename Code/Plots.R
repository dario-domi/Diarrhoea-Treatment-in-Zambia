######################################################################
# This script produces the plots associated with the project 
# in collaboration with Cola-Life. 
# Run the script after having run "Analyses_Zambia.R", 
# so that all needed variables are created.
######################################################################

# Run main program to create the variables needed
source("Single_Centre_Analysis.R")

# Relative path to Pictures folder
picpath <- "../Pictures/"

################################################################
#
#           BARPLOT FOR AGGLOMERATED PROPORTIONS
#
###############################################################

# Bar colors
fill_before <- "turquoise1"
border_before <- "deepskyblue1"
fill_after <- "lightgreen"
border_after <- "mediumseagreen"

# Two versions of the plot are provided. One is simple, obtained through 
# the built-in barplot function.
# The other ones is customised, with additional features

################   VERSION 1    ##################

# # Set background colour and margins
# par(bg = "white", mar = c(4, 6, 4, 3))
# 
# # Barplot
# bp <- barplot(props[,"mid"] , col=c("turquoise1" , "lightgreen"), 
#               xlim=xrng, ylim=yrng, space=c(1, 1.5))
# # Add Confidence Intervals
# arrows(x0 = bp, x1 = bp, y0 = props[,"lwr"], y1 = props[,"upr"],
#        col = c("dodgerblue4", "aquamarine4"), lwd = 2, 
#        angle = 80, length = 0.1, code=3)


################   VERSION 2    ##################

# FUNCTION TO PRODUCE PLOT
plot_agglomerated_props <- function(){
  
  ############   GRAFICAL PARAMETERS   ##########
  
  # x and y range
  xrng <- c(0, 4.5)
  yrng <- c(0, 100)
  
  # x centres of bins and bar width
  xvals <- c(1.2, 3.3)
  half_width <- 0.4
  
  # Set black-ish background and margins
  bgcol <- rgb(61, 55, 72, maxColorValue = 255)
  par(bg = bgcol, mar = c(4, 6, 4, 1))
  
  ############   ACTUAL PLOT   #############
  
  # Set x&y plot limits
  plot(1, xlab = "", ylab = "", xaxt = "n", yaxt = "n", 
       bty = "n", xlim = xrng, ylim = yrng)
  
  # Add horizontal gridlines
  abline(h = seq(0, 100, 20), 
         lty = 2, col = gray(0.95), lwd = 1)
  
  # abline(h = seq(10, 100, 20), 
  #        lty = 2, col = gray(0.95), lwd = 0.5)
  
  # y-axis labels
  mtext(text = paste0(seq(0, 100, 20), "%"),
        side = 2, at = seq(0, 100, 20),
        cex = 1.3, las = 1, line = 1, 
        col = gray(.95))
  
  # Blue bar
  rect(xleft = xvals[1] - half_width,
       ybottom = 0,
       xright = xvals[1] + half_width,
       ytop = props[1,"mid"],
       col = fill_before, border = border_before, lwd=3)
  
  # Green bar
  rect(xleft = xvals[2] - half_width,
       ybottom = 0,
       xright = xvals[2] + half_width,
       ytop = props[2,"mid"],
       col = fill_after, border = border_after, lwd=3)
  
  # Confidence Intervals
  arrows(x0 = xvals, x1 = xvals, 
         y0 = props[,"lwr"], y1 = props[,"upr"],
         col = c(border_before, border_after), lwd = 3, 
         angle = 85, length = 0.12, code=3)
  
  # x labels
  mtext(text = rownames(props), cex=1.2,
        side = 1, at = xvals, las = 1,
        line = 1.3, col = gray(.95))
  
  # y label
  mtext("Overall Proportion of CDCs", side = 3, font=3, col = "white", line = 0.7, cex=2)
}


# SAVE THE PLOT
pdf(file = paste0(picpath, "Overall_Proportions.pdf"),  # Specify figure size and path
    width = 10, # The width of the plot in inches
    height = 6)
plot_agglomerated_props()                               # Make plot
dev.off()                                               # Save plot


###############################################################
#
#           BARPLOT FOR SINGLE-CENTRE PROPORTIONS
#
###############################################################


# CREATE FUNCTION WHICH PRODUCES THE PLOT
# The plot is based on values in the tibbles props_HC_16 and props_HC_17

plot_HC_props <- function(){
  
  ############   GRAFICAL PARAMETERS   ##########
  
  xrng <- c(0, 26)
  yrng <- c(0, 100)
  xvals <- seq(from=1, by=3.5, len=7)
  
  # Half distance between blue and green bars
  offset <- 0.1
  
  # Set black-ish background and margins
  bgcol <- rgb(61, 55, 72, maxColorValue = 255)
  par(bg = bgcol, mar = c(4, 6, 4, 3))
  
  ############   ACTUAL PLOT   #############
  
  # Set x&y plot limits
  plot(1, xlab = "", ylab = "", xaxt = "n", yaxt = "n", 
       bty = "n", xlim = xrng, ylim = yrng)
  
  # Add horizontal gridlines
  abline(h = seq(0, 100, 20), 
         lty = 2, col = gray(0.95), lwd = 1)
  
  # y-axis labels
  mtext(text = paste0(seq(0, 100, 20), "%"),
        side = 2, at = seq(0, 100, 20),
        cex = 1.3, las = 1, line = 1, 
        col = gray(.95))
  
  # Blue bars
  rect(xleft = xvals - 1,
       ybottom = 0,
       xright = xvals - offset,
       ytop = props_HC_16[,"mid"],
       col = fill_before, border = border_before, lwd=2)
  
  # Green bars
  rect(xleft = xvals + offset,
       ybottom = 0,
       xright = xvals + 1,
       ytop = props_HC_17[,"mid"],
       col = fill_after, border = border_after, lwd=2)
  
  # Confidence Intervals blue bars
  arrows(x0 = xvals - 0.5 - offset/2, 
         x1 = xvals - 0.5 - offset/2, 
         y0 = props_HC_16[,"lwr"], y1 = props_HC_16[,"upr"],
         col = border_before, lwd = 2, 
         angle = 90, length = 0.05, code=3)
  
  # Confidence Intervals green bars
  arrows(x0 = xvals + 0.5 + offset/2, 
         x1 = xvals + 0.5 + offset/2, 
         y0 = props_HC_17[,"lwr"], y1 = props_HC_17[,"upr"],
         col = border_after, lwd = 2, 
         angle = 90, length = 0.05, code=3)
  
  # x labels (split each facility name into two lines)
  name <- rownames(props_HC_16)
  mtext(text = gsub(" ", "\n", name), cex=1.25,
        side = 1, at = xvals, las = 1,
        line = 1.3, col = gray(.95))
  
  # Title
  mtext("Proportion of CDCs by Facility", 
        side = 3, font=3, col = "white", line = 0.7, cex=2)
  
  # Legend
  legend(23.6, 101, c("Before\nCo-pack", "After\nCo-pack"),
         pch=c(22,22), pt.cex=2.5, pt.lwd=1.7,
         col = c(border_before, border_after),
         pt.bg = c(fill_before, fill_after), 
         y.intersp=2.2, x.intersp=0.95,
         cex = 1.2,
         text.col = gray(0.95),
         bty = "n")  # no box
}


# SAVE THE PLOT
pdf(file = paste0(picpath, "HC_Proportions.pdf"),  # Specify figure size and path
    width = 12, 
    height = 6)
plot_HC_props()                                    # Make plot
dev.off()                                          # Save plot



###############################################################
#
#     CONFIDENCE INTERVAL FOR DIFFERENCE OF PROPORTIONS
#
###############################################################

# Lower, central and upper values of CI
lwr <- 100*prop_diff_CI$conf.int[1]
mid <- 100*prop_diff_CI$estimate
upr <- 100*prop_diff_CI$conf.int[2]

plot_diff_CI <- function(){
  
  bgcol <- rgb(61, 55, 72, maxColorValue = 255)
  col <- "orangered3"
  
  par(bg = bgcol,
      #mai=c(0.4, 0.5, 0.4, 2), # small bottom&top margins
      mai=c(0.8, 0.5, 0.8, 2), # This leaves "large" bottom&top margins (old right margin=2)
      mgp=c(1,0.5,0)) # agjust mgp[2] for distance between axis and numbers

  # Plot of single point (mid estimate)
  plot(x = 0, y = mid,
       ylim = c(-100,100), 
       pch = 19, cex = 1.8, col = col,
       bty="n", xaxt = "n", yaxt="n",
       xlab="", ylab="")
  
  # Add horizontal gridlines
  abline(h = seq(-100, 100, 20), 
         lty = 3, col = gray(0.95), lwd = 0.8)
  
  # CI around estimate
  arrows(x0 = 0,   x1 = 0, 
         y0 = lwr, y1 = upr,
         col = col, lwd = 3, 
         angle = 90, length = 0.15, code=3)
  
  axis(4, las = 1, col = gray(.95),
       lwd.ticks = 0, 
       at = seq(-100, 100, 20),
       labels = paste0(seq(-100, 100, 20), "%"),
       col.axis = gray(0.95))
  
  # y label (on different lines, spaces from each other)
  label <- list("   95% CI", 
                "       for", 
                " difference", 
                "  between", 
                "proportions", 
                "  of CDCs")
  L <- length(label)           # number of lines in ylabel
  levs <- (L-1):0 - (L-1)/2    # the integer "levels" at which each line of text is written
  space <- 13.5                  # space between two lines
  for (i in 1:L)
    mtext(text = label[[i]], at = space*levs[i],
          cex=1.15, side = 4, las = 1,
          line = 3.7, col = gray(.95))
  
}


# SAVE PLOT WITH AGGLOMERATED ESTIMATES AND THE CI OF THEIR DIFFERENCE
pdf(file = paste0(picpath, "Overall_Difference_Proportions.pdf"),
    width = 10, 
    height = 6)
# Set layout of plot and save
mat_layout <- matrix(c(1,2), nrow = 1)
layout(mat = mat_layout, widths = c(2,1))
plot_agglomerated_props()
plot_diff_CI()
dev.off()



###############################################################
#
#     FOREST PLOT FOR ODDS RATIOS
#
###############################################################

centre <- 2
fisher.test(CDC_Table[,,centre])


library(forestplot)

base_data <- tibble(mean  = c(0.578, 0.165, 0.246, 0.700, 0.348, 0.139, Inf),
                    lower = c(0.372, 0.018, 0.072, 0.333, 0.083, 0.016, 0.365),
                    upper = c(0.898, 1.517, 0.833, 1.474, 1.455, 1.209, Inf),
                    facility = c("Auckland", "Block", "Doran", "Gamsu",
                              "Morrison", "Papageorgiou", "Tauesch"),
                    OR = c("0.58", "0.16", "0.25", "0.70", "0.35", "0.14", "1.02"),
                    empty = rep(NA, 7)
                    )

my_ticks <- log(c(1,2,4,8,16))
attr(my_ticks, "labels") <- c("a", "b", "cc", "d", "ee")

# Forest plot
OR_data |>
  forestplot(labeltext = c(facility, CDC16, CDC17, empty, OR),
             clip = c(0.1, 1000),
             xlog = T,
             xticks = my_ticks
             ) |>
  fp_set_style(box = "royalblue",
               line = "deepskyblue2",
               summary = "royalblue") |> 
  fp_add_header(facility = "Facility",
                CDC16 = "CDCs\n2016",
                CDC17 = "CDCs\n2017",
                empty = NA,
                OR = "OR") |>
  fp_append_row(mean  = 3,
                lower = 2,
                upper = 4.5,
                facility = "Aggregated",
                CDC16 = "a",
                CDC17 = "b",
                empty=NA,
                OR = "3",
                is.summary = TRUE) |> 
  fp_set_zebra_style(gray(0.91))




