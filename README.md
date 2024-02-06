# Ensuring the success of diarrhoea treatment in Zambian children


## Project Background
The project is a collaboration between myself and the UK charity [ColaLife](https://www.colalife.org/), as part of the [Statisticians for Society](https://rss.org.uk/membership/volunteering-and-promoting/statisticians-for-society-initiative/) initiative by the Royal Statistical Society. I am the lead volunteer statistician for the project.

</br>
<p align="center">
<img src='Pictures/Logos/ColaLife_logo.jpg' width='240' height='90'> 
&emsp; &emsp; &emsp; &emsp;
<img src='Pictures/Logos/RSS_logo.png' width='200'>
</p>
</br>
The project is concerned with the statistical assessment of the effect of co-packaging on the correct &ndash; and often life-saving &ndash; treatment of diarrhoea for Zambian children. More details below.


## Project Overview

Diarrhoea is a leading cause of child mortality across developing regions of the world. 
A simple treatment, widely recognised as effective in reducing child mortality, 
is given by the parallel administration of two elements: Oral Rehidratations Salts (ORS) and zinc tablets. 

In remote regions of the world, even though both ORS and zinc may be available at a medicine-dispensing centre, only either of the two (or none at all) may be given as treatment, _e.g._ due to lack of medical knowledge of the dispensing person.

In Zambia, the charity ColaLife has designed and introduced a _single_ pack containing both ORS and zinc: the ***co-pack***.
Many more details on the decade-long story, including the induced change on WHO guidelines for diarrhoea treatment, can be read [here](https://www.colalife.org/). 
The charity has also gathered data on the medicines administered by different health centres as diarrhoea treatment, both *before* (Oct 2016) and *after* (Oct 2017) the co-pack introduction.

<ins>**Objective**</ins>:
This collaboration uses the above data to investigate whether a significantly higher proportion of children have been treated correctly (_i.e._, with both ORS and zinc) following the co-pack introduction.

<!---
[^1]: Many more details on the decade-long story, including the induced change on WHO guidelines for diarrhoea treatment, can be read [here](https://www.colalife.org/). 
--->

***

## Repository Structure
   This repository contains the following three folders.
   * [`Code`](https://github.com/dario-domi/Diarrhoea-Treatment-in-Zambia/tree/master/Code): R code to perform the statistical analyses of the project and produce the figures. Data not uploaded for privacy reasons.
   * [`Report`](https://github.com/dario-domi/Diarrhoea-Treatment-in-Zambia/tree/master/Report): Report with the results of the (in-progress) analyses. Soon taking the form of a manuscript to be submitted for peer-review.
   * [`Pictures`](https://github.com/dario-domi/Diarrhoea-Treatment-in-Zambia/tree/master/Pictures): Figures produced by the R code and used within the manuscript.



#### Details of the 'Code' Folder
Each R script is heavily commented and its purpose is highlighted at the start of the script itself. A brief overview of each script is included below for convenience.  
* [`Agglomerated_Data_Analysis.R`](https://github.com/dario-domi/Diarrhoea-Treatment-in-Zambia/blob/master/Code/Agglomerated_Data_Analysis.R): Statistical analyses performed on the data, as aggregated over all health centres.   
* [`Single_Centre_Analysis.R`](https://github.com/dario-domi/Diarrhoea-Treatment-in-Zambia/blob/master/Code/Single_Centre_Analysis.R): Statistical tests and modelling accounting for the stratification of the data across different health centres.   
* [`Plots.R`](https://github.com/dario-domi/Diarrhoea-Treatment-in-Zambia/blob/master/Code/Plots.R) Code creating the figures to be used in the manuscript.
