# stigma_validation
Analysis of selected cytokines and metabolites in the publicly available INCOV cohort

## Terms of use

To reference or use the analysis results in your publication, please cite our GitHub repository and, if available, the accompanying publication . The raw data are available upon request to the senior author Prof. Katharina Hüfner.

## Usage

There are development packages required by the analysis pipeline. You may use `devtools` to install them:

```r

## tabular data transformation, statistical hypothesis testing, sourcing of scripts

devtools::install_github('PiotrTymoszuk/trafo')
devtools::install_github('PiotrTymoszuk/ExDA')
devtools::install_github('PiotrTymoszuk/soucer')

## management of figures and tables

devtools::install_github('PiotrTymoszuk/figur)

```
To launch the entire pipeline, you may source the `exec.R` script:

```r

source('exec.R')

```

## Reference

To be provided

## Contact

The maintainer odf the repository is [Piotr Tymoszuk](mailto:piotr.s.tymoszuk@gmail.com). Data requests should be addressed to the corresponding author of the study [Prof. Katharina Hüfner](mailto:Katharina.Huefner@i-med.ac.at).



