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
  library(clustTools)
  library(glmnet)

  insert_head()

  source_all('./tools/tools.R',
             message = TRUE, crash = TRUE)

# Analysis scripts for the the STIGMA cohort ------

  insert_msg('Anlyses of the STIGMA cohort')

  ## the following analyses are done for the SIGMA (SIMMUN) cohort:
  ##
  ## 1) Correlation of inflammatory parameters with metabolic variables
  ##
  ## 2) Comparison of inflammatory and metabolic variables between people
  ## with and without depression/anxiety symptoms and uninfected vs COVID-19,
  ##
  ## 3) Correlation of stress scoring (PSS-4) and depression scoring (PHQ)
  ## with metabolic and inflammatory variables
  ##
  ## 4) Correlation of anti-RBD SARS-CoV-2 antibodies as a readout of humoral
  ## immunity with metabolic and inflammatory variables
  ##
  ## 5) Levels of inflammatory and metabolic parameters in healthy, CoV without
  ## persistent somatic symptoms and CoV with persistent somatic symptoms
  ##
  ## 6) Multi-parameter modeling of metabolic readouts as a function of
  ## inflammatory markers, signs of depression/anxiety, age, sex, CoV status,
  ## persistent somatic symptoms and anti-RBD levels

  c('./stigma scripts/correlation.R',
    './stigma scripts/hads_cov.R',
    './stigma scripts/pss_phq.R',
    './stigma scripts/anti_rbd.R',
    './stigma scripts/modeling.R') %>%
    source_all(message = TRUE, crash = TRUE)

# Analysis scripts for the INCOV cohort -----

  insert_msg('Analyses of the INCOV cohort')

  c('./incov scripts/correlation.R',
    './incov scripts/time_course.R') %>%
    source_all(message = TRUE, crash = TRUE)

# END -------

  insert_tail()
