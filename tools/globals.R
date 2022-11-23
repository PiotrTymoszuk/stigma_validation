# This script contains project globals

# libraries ----

  library(plyr)
  library(tidyverse)
  library(stringi)

# data container ------

  globals <- list()

# graphics -----

  globals$common_text <- element_text(size = 8,
                                      face = 'plain',
                                      color = 'black')

  globals$common_margin <- ggplot2::margin(t = 5,
                                           l = 4,
                                           r = 2,
                                           unit = 'mm')

  globals$common_theme <-
    theme_classic() +
    theme(axis.text = globals$common_text,
          axis.title = globals$common_text,
          plot.title = element_text(size = 8,
                                    face = 'bold'),
          plot.subtitle = globals$common_text,
          plot.tag = element_text(size = 8,
                                  face = 'plain',
                                  color = 'black',
                                  hjust = 0,
                                  vjust = 1),
          plot.tag.position = 'bottom',
          legend.text = globals$common_text,
          legend.title = globals$common_text,
          strip.text = globals$common_text,
          strip.background = element_rect(fill = 'gray95',
                                          color = 'gray80'),
          plot.margin = globals$common_margin,
          panel.grid.major = element_line(color = 'gray90'))

# Names and labels of variables of interest -----

  insert_msg('Variable names and labels')

  globals$incov_proteins <- c('IL6_INF',
                              'IL10_INF',
                              'TNF_INF',
                              'IFNG_INF')

  globals$incov_metabolites <- c('tryptophan',
                                 'kynurenine',
                                 'phenylalanine',
                                 'tyrosine',
                                 'dopamine 3-O-sulfate',
                                 'serotonin',
                                 'quinolinate')

  globals$incov_lexicon <-
    rbind(incov$annotation_proteome %>%
            filter(variable %in% globals$incov_proteins),
          incov$annotation_metabolome %>%
            filter(variable %in% globals$incov_metabolites))

# Labels for the time points ------

  insert_msg('Timepoint labels')

  globals$time_labs <- c('healthy' = 'healthy',
                         'T1' = 'acute',
                         'T2' = 'sub-acute',
                         'T3' = 'recovery')

# END -----

  insert_tail()
