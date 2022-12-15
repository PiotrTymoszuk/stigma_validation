# Explorative data analysis
#
# characteristic of the INCOV study cohort, the characteristic of of the local
# STIGMA study collective is done elsewhere (SPSS, other members of the study
# team)
#
# Testing for the normality and EOV for the study variables (STIGMA and INCOV)
#
# Investigating correlation of the response variables in the STIGMA cohort
# with age and differences between the genders

# tools --------

  library(plyr)
  library(tidyverse)
  library(trafo)
  library(stringi)
  library(exda)
  library(soucer)

  select <- dplyr::select
  reduce <- purrr::reduce

  insert_head()

  source_all('./tools/tools.R',
             message = TRUE, crash = TRUE)

# analysis scripts -------

  insert_msg('Analysis scripts')

  c('./exploration scripts/cohort.R',
    './exploration scripts/distribution.R',
    './exploration scripts/age.R',
    './exploration scripts/gender.R') %>%
    source_all(message = TRUE, crash = TRUE)

# END -----

  insert_tail()
