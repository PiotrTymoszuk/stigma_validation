# Import and wrangling of the manuscript data
#
# The local STIGMA cohort dataset encompasses readouts of inflammation
# and metabolic markers of neurotransmitter precursors along with the
# explanatory variables (anxiety/depression signs and persistent symptoms)
# and confounders (age and sex)
#
# The INCOV validation dataset contains selected metabolites of the TRP decay
# pathway (so called TRYCATS pathway) and key inflammatory cytokines measured
# in blood of healthy individuals and COVID-19 participants at consecutive time
# points (acute, sub-acute and recovery) after clnical onset.


# tools --------

  library(plyr)
  library(tidyverse)
  library(soucer)
  library(trafo)
  library(stringi)
  library(readxl)
  library(foreign)

  insert_head()

  c('./tools/globals.R',
    './tools/tools.R') %>%
    source_all(message = TRUE, crash = TRUE)

# Sourcing the import scripts for single studies ------

  insert_msg('Sourcing the import scripts')

  source_all(c('./import scripts/local.R',
               './import scripts/incov.R'),
             message = TRUE, crash = TRUE)

# Updating the project globals ------

  insert_msg('Project globals')

  globals$incov_lexicon <-
    rbind(incov$annotation_proteome %>%
            filter(variable %in% globals$incov_proteins),
          incov$annotation_metabolome %>%
            filter(variable %in% globals$incov_metabolites))

# END -----

  insert_tail()
