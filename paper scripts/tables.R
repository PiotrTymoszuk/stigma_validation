# manuscript tables

  insert_head()

# container -----

  tabs <- list()
  suppl_tabs <- list()

# Tables 1 and 2: characteristic of the STIGMA and INCOV collectives -------

  insert_msg('Table 1 and 2: characteristic of the STIGMA and INCOV collective')

  tabs[c('stigma_cohort',
         'incov_cohort')] <-
    cohort$feat_tables %>%
    map(filter,!stri_detect(Variable, regex = '(post-COVID)|(Persistent)|(BMI)')) %>%
    map( ~ map_dfc(.x,
                   stri_replace,
                   regex = '^HADS.*\\nHADS\\+:\\s{1}',
                   replacement = '')) %>%
    list(x = .,
         label = c('table_1_stigma_cohort',
                   'table_2_incov_cohort'),
         ref_name = c('stigma_cohort',
                      'incov_cohort'),
         caption = c(paste('Characteristic of the local STIGMA cohort.',
                           'Numeric variables are presented as medians with',
                           'interquartile ranges. Categorical variables are',
                           'presented as percentages and counts within',
                           'complete observations.'),
                     paste('Characteristic of the external INCOV cohort.',
                           'Numeric variables are presented as medians with',
                           'interquartile ranges. Categorical variables are',
                           'presented as percentages and counts within',
                           'complete observations.'))) %>%
    pmap(mdtable)

# Table S1: numbers of samples available per time point -----

  insert_msg('Table S1: samples per timepoint')

  suppl_tabs$samples <- cohort$samples %>%
    mdtable(label = 'table_s1_sample_number',
            ref_name = 'samples',
            caption = paste('Number of available samples and sampling',
                            'timepoints in the INCOV cohort.'))

# END ------

  insert_tail()
