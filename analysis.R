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
  library(lmqc)
  library(glmnet)
  library(caret)
  library(caretExtra)
  library(doParallel)

  insert_head()

  source_all('./tools/tools.R',
             message = TRUE, crash = TRUE)

# Analysis scripts for the the STIGMA cohort ------

  insert_msg('Anlyses of the STIGMA cohort')

  c('./stigma scripts/correlation.R',
    './stigma scripts/hads_cov.R',
    './stigma scripts/modeling.R') %>%
    source_all(message = TRUE, crash = TRUE)

# Analysis scripts for the INCOV cohort -----

  insert_msg('Analyses of the INCOV cohort')

  c('./incov scripts/correlation.R',
    './incov scripts/time_course.R') %>%
    source_all(message = TRUE, crash = TRUE)

# END -------

  insert_tail()
