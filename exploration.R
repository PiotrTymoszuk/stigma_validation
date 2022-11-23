# Explorative data analysis: characteristic of the study cohorts,
# distribution of numeric variables

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
    './exploration scripts/distribution.R') %>%
    source_all(message = TRUE, crash = TRUE)


# END -----

  insert_tail()
