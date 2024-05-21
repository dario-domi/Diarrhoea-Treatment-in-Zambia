# Ensuring the success of diarrhoea treatment in Zambian children


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

Within sub-saharan Africa, the recommended treatment is administered is less than 1 in 5 diarrhoea cases among children under five years of age. Underlying reasons include unawareness of the recommended treatment among health personnel, who may often dispense just ORS or zinc, even if both elements are in stock.

In Zambia, the charity ColaLife has designed and introduced, in cooperation with the Zambian government, a _single_ pack containing both ORS and zinc: the ***ORS & zinc co-pack***.[^1]
The charity has also gathered data on the treatments dispensed by different health facilities to treat children diarrhoea, both *before* (Oct 2016) and *after* (Oct 2017) the co-pack introduction. In both cases, only facilities where ORS and zinc were also separately available for dispensing were considered. 

<ins>**Objective**</ins>:
The data is used to investigate whether a significantly higher proportion of children have been dispensed the recommended treatment (_i.e._, both ORS and zinc) following the introduction of the co-pack.

[^1]: Many more details on the decade-long story, including the induced change on WHO guidelines for diarrhoea treatment, can be read [here](https://www.colalife.org/2019/07/09/success-who-adds-co-packaged-ors-and-zinc-to-its-essential-medicines-for-children/). 

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
