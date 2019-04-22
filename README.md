# HIV-Gag-Immunogens
This repository includes source code scripts for all the analysis and figures reported in the following:

### Title of paper 
Sub-dominant principal components inform new vaccine targets for HIV Gag _(submitted)_ 
### Authors 
Syed Faraz Ahmed, Ahmed A. Quadeer, David Morales-Jimenez and Matthew R. McKay.

## Analysis

The MATLAB source code for the analysis reported in the paper is contained in the “Analysis” folder. MATLAB data files are contained within the "Data" subfolder. 

The data includes:
1. Raw MSA
2. Pre-processed Gag MSA matrix
3. List of Gag sites related to known biochemical domains
4. List of Gag sites related to HIV controllers and progressors

The Analysis folder also includes a code file "general_code.m" that can be used to infer sectors as described in the paper by provding any MSA and few parameters as inputs to the function.  

## Figures

The R scripts used for generating the accompanying figures are contained in the “Figures” folder.

## Usage

* Dependencies
1. MATLAB (preferrably v2017a or later) installed with the following additional toolboxes:
    1. Bioinformatics Toolbox
    2. Statistics and Machine Learning Toolbox
  
2. RStudio with R (preferrably R version 3.5.1 or later) installed with the following packages:
    1. `tidyverse`
    2. `ComplexHeatmap`
    3. `RColorbrewer`
    4. `multipanelfigure`
    5. `circlize`
    6. `scales`
    7. `ggpubr`
    8. `grid`
    9. `magick`
    

* To re-run the analysis presented in the paper
  * Open MATLAB and run the file `main.m`

* To re-generate the figures presented in the paper
  * Open RStudio and run the file `figures.rmd`
  
## Troubleshooing

For any queries related to code files, please contact faraz107@gmail.com
