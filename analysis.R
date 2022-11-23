# Data analysis

# tools --------

  library(plyr)
  library(tidyverse)
  library(trafo)
  library(stringi)
  library(exda)
  library(soucer)
  library(rlang)
  library(furrr)
  library(ggpubr)
  library(rstatix)

  insert_head()

  source_all('./tools/tools.R',
             message = TRUE, crash = TRUE)

# Analysis scripts -----

  insert_msg('Analysis scripts')

  c('./analysis scripts/correlation.R',
    './analysis scripts/time_course.R',
    './analysis scripts/long_cov_type.R') %>%
    source_all(message = TRUE, crash = TRUE)

# END -------

  insert_tail()
