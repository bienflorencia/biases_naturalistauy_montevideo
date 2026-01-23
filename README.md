# Socio-ecological drivers of urban biodiversity patterns in contributory science data

*Luis Orlando, Mauro Berazategui & Florencia Grattarola <a dir="ltr" href="http://orcid.org/0000-0001-8282-5732" target="_blank"><img class="is-rounded" src="https://upload.wikimedia.org/wikipedia/commons/0/06/ORCID_iD.svg" width="15"></a>*

This repository ([https://github.com/bienflorencia/biases_naturalistauy_montevideo](https://github.com/bienflorencia/biases_naturalistauy_montevideo)) contains the code and data to reproduce our study on the drivers shaping the distribution of contributory biodiversity observations in Montevideo (Uruguay) using data from iNaturalist.


## **Code**

  - `code/analyses.qmd`: a Quarto file with the code used to download the data, run the models, and produce figures.  
  - `code/analyses.html`: an HTML file with the code used to download the data, run the models, and produce figures.

## **Data**

  - `observations-536732`: an csv file containing the observations downloaded from iNaturalist.   
  - `montevideo_data.gpkg`: a geopackage file containing 3 layers for the Montevideo department; `grid`: the grid of 1,148 cells of 0.5 km² area, `env` the grid with environmental variables, and `social` the grid with social variables.  
  
Variables in the `montevideo_data.gpkg`: 
  
  - `id_hex`: identifier for the grid cell.     
  - `ndvi`: median normalized differences vegetation index.   
  - `ndmi`: median normalized differences moisture index.   
  - `pndbi`: built-up proportion (based on normalized differences built-up index).  
  - `protection`: proportion of protected area in the grid cell.    
  - `lcz`: local climate zones (urban landscape categories).  
  - `dist_coast`: distance from the centroid of the grid cell to the coastline (in meters).    
  - `alan`: median artificial light at night, measured as radiance (nW cm⁻² sr⁻¹).   
  - `pop`: number of people.  
  - `houses`: total number of houses in the grid cell.    
  - `education`: median adult (>21years) education in years in the grid cell.     
  - `N`: number of iNaturalist observations in the grid cell.     
  - `S`: species richness of iNaturalist observations per grid cell.       
  - `pres`: presence or absence of an iNaturalist observation in the grid cell (binary).    

## LICENCE

**Data** are available under the terms of the Creative Commons Attribution 4.0 International licence CC-BY 4.0 (https://creativecommons.org/licenses/by/4.0/legalcode.en).   

**Code** is available under the terms of the GPL-3.0 licence (https://www.gnu.org/licenses/gpl-3.0.html). 

## CITATION

> Orlando L., Berazategui M., Grattarola F. (2026) Socio-ecological drivers of urban biodiversity patterns in contributory science data. [Data/Code]
