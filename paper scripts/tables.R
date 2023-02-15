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
    map(filter,!stri_detect(Variable, regex = '(post-COVID)|(BMI)')) %>%
    map(~map_dfc(.x,
                 stri_replace,
                 regex = '^HADS.*\\nHADS\\+:\\s{1}',
                 replacement = ''))

  ## the SIMMUN/STIGMA cohort included only participants with complete cases
  ## so I'll provide the n number only once

  tabs$stigma_cohort <- tabs$stigma_cohort %>%
    outer_rbind(tibble(Variable = 'Participants, n',
                       Healthy = count(cohort$analysis_tbl$stigma, cov)$n[1],
                       `SARS-CoV-2` = count(cohort$analysis_tbl$stigma, cov)$n[2]),
                .) %>%
    set_names(c('Variable', 'Uninfected',
                'SARS-CoV-2', 'Test', 'Significance'))

  ## skipping the somatic symptoms

  tabs$stigma_cohort <- tabs$stigma_cohort %>%
    filter(Variable %in% c('Participants, n',
                           'Sex',
                           'Age, years',
                           'Body mass class',
                           'Somatic comorbidity',
                           'Psychiatric comorbidity',
                           'HADS anxiety score',
                           'HADS depression score',
                           'Depression or anxiety signs, HADS ≥ 8',
                           'PSS-4 stress score',
                           'anti-RBD SARS-CoV-2, IgG, AU',
                           'COVID-19 severity'))

  ## the ready to use tables

  tabs[c('stigma_cohort',
         'incov_cohort')] <- tabs[c('stigma_cohort',
                                    'incov_cohort')] %>%
    list(x = .,
         label = c('table_1_stigma_cohort',
                   'table_2_incov_cohort'),
         ref_name = c('stigma_cohort',
                      'incov_cohort'),
         caption = c(paste('Characteristic of the local SIMMUN cohort.',
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
