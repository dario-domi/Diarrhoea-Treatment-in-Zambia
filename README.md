# Ensuring successful diarrhoea treatment in Zambian children via co-packaging


## Project Background
The project is a collaboration between myself and the UK charity [ColaLife](https://www.colalife.org/), as part of the [Statisticians for Society](https://rss.org.uk/membership/volunteering-and-promoting/statisticians-for-society-initiative/) initiative by the Royal Statistical Society. I am the lead volunteer statistician for the project.

<p align="center">
<img src='Pictures/Logos/ColaLife_logo.jpg' width='240' height='90'> 
&emsp; &emsp; &emsp; &emsp;
<img src='Pictures/Logos/RSS_logo.png' width='200'>
</p>

The project is concerned with the statistical assessment of the effect of co-packaging on the correct &ndash; and often life-saving &ndash; treatment of diarrhoea for Zambian children. More details below.
 

## Brief Project Overview

Diarrhoea is still a leading cause of child mortality across developing regions of the world. 
A simple and effective treatment, widely recognised to reduce child mortality, 
is given by the parallel administration of two elements: Oral Rehidratations Salts (ORS) and zinc tablets. 

In remote regions, even though both elements may be available at a medicine-dispensing centre, only either of the two (or none at all) may be given as treatment, _e.g._ due to lack of medical knowledge of the dispensing person.

The charity ColaLife has introduced a single pack (the _co-pack_) in Zambia, containing both ORS and zinc: many more details on the decade-long story, including the induced change on WHO guidelines for diarrhoea treatment, can be read [here](https://www.colalife.org/). Both ***before*** (Oct 2016) and ***after*** (Oct 2017) the co-pack introduction, the charity has gathered data on the medicines provided in different health centres as diarrhoea treatment.

This collaboration aism to investigate statistically whether a significantly higher proportion of children have been treated correctly (_i.e._, with both ORS and zinc) following the co-pack introduction.


## Documents in the Repo
   This repository contains the R code and documents produced by myself in relation to the above project.
   * **Folder Code**: R code to perform the statistical analyses of the project and produce the figures. Data not uploaded for privacy reasons.
   * **Folder Report**: Report with the results of the (in-progress) analyses. Soon taking the form of a manuscript to be submitted for peer-review.
   * **Folder Pictures**: Figures produced by the R code and used within the manuscript.


### Folder Code
Each R script is significantly commented and its purpose highlighted at the start of the script itself. A brief overview of each script is included below for convenience.  
* [`Agglomerated_Data_Analysis.R`](https://github.com/dario-domi/Diarrhoea-Treatment-in-Zambia/blob/master/Code/Agglomerated_Data_Analysis.R): Statistical analyses performed on the data as aggregated over all health centres.   
* [`Single_Centre_Analysis.R`](https://github.com/dario-domi/Diarrhoea-Treatment-in-Zambia/blob/master/Code/Single_Centre_Analysis.R): Statistical tests and modelling accounting for the stratification of the data across different health centres.   
* [`Plots.R`](https://github.com/dario-domi/Diarrhoea-Treatment-in-Zambia/blob/master/Code/Plots.R) Code creating the figures to be used in the manuscript.
