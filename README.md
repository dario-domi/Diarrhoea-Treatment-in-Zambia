# Correct dispensation of diarrhoea treatment in Zambian children


## Project Background
The project is a collaboration between myself and the UK charity [ColaLife](https://www.colalife.org/), as part of the [Statisticians for Society](https://rss.org.uk/membership/volunteering-and-promoting/statisticians-for-society-initiative/) initiative of the Royal Statistical Society. I am the lead volunteer statistician for the project.

</br>
<p align="center">
<img src='Pictures/Logos/ColaLife_logo.jpg' width='160' height='60'> 
&emsp; &emsp; &emsp; &emsp;
<img src='Pictures/Logos/RSS_logo.png' width='130'>
</p>
</br>
The project focuses on dispensing behaviour of the recommended (and often life-saving) treatment of diarrhoea for children, in rural regions of Zambia. More details follow.

</br>
</br>
<p align="center">
<img src='Pictures/Logos/Co-Pack.jpg' width='350'>
&emsp; &emsp;
<img src='Pictures/Logos/Kit_Yamoyo.jpg', width='325'>
</p>
</br>


## Project Overview

Diarrhoea is a leading cause of child mortality across developing regions of the world. 
The World Health Organisation (WHO) and UNICEF recommend a simple treatment, recognised as effective in reducing child mortality: 
the parallel administration of Oral Rehidratations Salts (ORS) and zinc tablets. 

Within sub-saharan Africa, the recommended treatment is administered is less than 20% of cases among children under five years of age. Underlying reasons include unawareness of the recommended treatment among dispensing health personnel, who may often dispense just ORS or zinc, even if both elements are in stock.

In Zambia, the charity ColaLife has designed and introduced, in cooperation with the Zambian government, a _single_ pack containing both ORS and zinc: the ***ORS & zinc co-pack***.[^1]
The charity has also gathered data on the treatments dispensed by different health facilities to treat children diarrhoea, both *before* (Oct 2016) and *after* (Oct 2017) the co-pack introduction. In both cases, only facilities where ORS and zinc were also separately available for dispensing were considered. 

<ins>**Objective**</ins>:
The data is used to investigate whether a significantly higher proportion of children have been dispensed the recommended treatment (_i.e._, both ORS and zinc) following the introduction of the co-pack.

A scientific paper with the results of the analyses is being submitted for publication on a peer-review, international global health journal.

[^1]: Many more details on the decade-long story, including the induced change on WHO guidelines for diarrhoea treatment, can be read [here](https://www.colalife.org/2019/07/09/success-who-adds-co-packaged-ors-and-zinc-to-its-essential-medicines-for-children/). 

***

## Repository Structure and Scripts Summary
   Three folders constitute the repository. 
   
   * [`Data folder`](https://github.com/dario-domi/Diarrhoea-Treatment-in-Zambia/tree/master/Data): contains one spreadsheet, reporting diarrhoea-treatment dispensing behaviour in seven Zambian health facilities, before and after the co-pack introduction.
   * [`Code folder`](https://github.com/dario-domi/Diarrhoea-Treatment-in-Zambia/tree/master/Code): contains the R code to perform the statistical analyses of the project and produce associated visualisations.
   * [`Pictures folder`](https://github.com/dario-domi/Diarrhoea-Treatment-in-Zambia/tree/master/Pictures): Figures produced by the R code and used within the manuscript.

Each R script inside the `Code` folder is extensively commented. A brief overview of each script is included below for convenience.  
* [`Aggregate_Data_Analysis.R`](https://github.com/dario-domi/Diarrhoea-Treatment-in-Zambia/blob/master/Code/Aggregate_Data_Analysis.R): Analyses performed on the data, as aggregated over all health facilities.   
* [`Stratified_Data_Analysis.R`](https://github.com/dario-domi/Diarrhoea-Treatment-in-Zambia/blob/master/Code/Stratified_Data_Analysis.R): Analyses performed on the individual facilities and on the data as stratified across facilities.   
* [`Plots.R`](https://github.com/dario-domi/Diarrhoea-Treatment-in-Zambia/blob/master/Code/Plots.R) Code creating the figures used in the manuscript.

## Run the scripts in your own R/RStudio
