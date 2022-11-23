# characteristic of the study cohorts

  insert_head()

# container ------

  cohort <- list()

# Globals -------

  insert_msg('Globals')

  ## variables

  cohort$var_lexicon <-
    c(sex = 'Sex',
      age = 'Age, years',
      bmi = 'BMI, kg/m\u00B2',
      bmi_class = 'Body mass class',
      ethnics = 'Ethnics',
      cov = 'Infection',
      severity = 'COVID-19 severity',
      long_cov = 'post-COVID-19 syndrome',
      psy_long_cov = 'Persistent depression, anxiety or sleep disorders',
      cogn_long_cov = 'Persistent memory or concentration problems',
      neuro_long_cov = 'Persistent neurological symptoms or smell/taste disorders',
      fatigue_long_cov = 'Persistent fatigue or psysical performance loss',
      resp_long_cov = 'Persistent cough, shortness of breath or other respiratory symptoms',
      gastro_long_cov = 'Persistent gastrointestinal symptoms') %>%
    compress(names_to = 'variable',
             values_to = 'label')

  ## analysis table

  cohort$analysis_tbl <- incov$clinic %>%
    filter(patient_id %in% incov$proteome$patient_id |
             patient_id %in% incov$metabolome$patient_id) %>%
    filter(!duplicated(patient_id)) %>%
    mutate(long_cov = ifelse(long_cov == 'healthy',
                             'no', as.character(long_cov)),
           psy_long_cov = ifelse(psy_long_cov %in% c('healthy',
                                                     'recovered'),
                                 'no', as.character(psy_long_cov)),
           neuro_long_cov = ifelse(neuro_long_cov %in% c('healthy',
                                                         'recovered'),
                                   'no', as.character(neuro_long_cov)),
           gastro_long_cov = ifelse(gastro_long_cov %in% c('healthy',
                                                           'recovered'),
                                    'no', as.character(gastro_long_cov)),
           cogn_long_cov = ifelse(cogn_long_cov %in% c('healthy',
                                                       'recovered'),
                                  'no', as.character(cogn_long_cov)),
           fatigue_long_cov = ifelse(fatigue_long_cov %in% c('healthy',
                                                             'recovered'),
                                     'no', as.character(fatigue_long_cov)),
           resp_long_cov = ifelse(resp_long_cov %in% c('healthy',
                                                       'recovered'),
                                  'no', as.character(resp_long_cov)))

  ## test type

  cohort$test_type <- c(sex = 'chisq',
                        age = 'mann_whitney_test',
                        bmi = 'mann_whitney_test',
                        bmi_class = 'chisq',
                        ethnics = 'chisq')

# Clinical stats -------

  insert_msg('Clinical characteristic')

  cohort$clin_stats <- cohort$analysis_tbl %>%
    explore(variables = cohort$var_lexicon$variable,
            split_factor = 'cov',
            what = 'table',
            pub_styled = TRUE) %>%
    reduce(left_join, by = 'variable') %>%
    set_names(c('variable', 'Healthy', 'SARS-CoV-2'))

# Testing for differences between the healthy and SARS-CoV-2 ----

  insert_msg('Testing for differences between healthy and infected')

  cohort$clin_test <- cohort$analysis_tbl %>%
    compare_variables(variables = names(cohort$test_type),
                      split_factor = 'cov',
                      what = 'test',
                      types = cohort$test_type,
                      ci = FALSE,
                      exact = FALSE,
                      pub_styled = TRUE)

# A common table with with clinical features and testing results ------

  insert_msg('A common table with clinical features')

  cohort$feat_table <-
    left_join(cohort$clin_stats,
              cohort$clin_test[c('variable', 'test', 'significance')],
              by = 'variable') %>%
    mutate(Healthy = ifelse(variable %in% names(cohort$test_type),
                            Healthy, NA),
           `SARS-CoV-2` = stri_replace(`SARS-CoV-2`,
                                       regex = '^no.*\\nyes:\\s{1}',
                                       replacement = ''),
           `SARS-CoV-2` = stri_replace(`SARS-CoV-2`,
                                       regex = '^healthy.*\\n',
                                       replacement = ''),
           test = stri_replace(test,
                               fixed = 'test',
                               replacement = ''),
           test = stri_replace(test,
                               fixed = 'Chi-squared',
                               replacement = '\u03C7\u00B2')) %>%
    map_dfc(stri_replace,
            regex = '^Mean.*\\nMedian\\s{1}=\\s{1}',
            replacement = '') %>%
    map_dfc(stri_replace,
            fixed = 'Range',
            replacement = 'range') %>%
    map_dfc(stri_replace,
            fixed = 'Complete',
            replacement = 'complete') %>%
    exchange('variable',
             dict = cohort$var_lexicon,
             key = 'variable',
             value = 'label') %>%
    set_names(c('Variable',
                'Healthy',
                'SARS-CoV-2',
                'Test',
                'Significance'))

# Sample numbers -------

  insert_msg('Numbers of samples')

  ## sampling time points

  cohort$sampling_time <-
    incov$clinic %>%
    group_by(timepoint) %>%
    filter(!duplicated(patient_id)) %>%
    summarize(median = median(time_po),
              lower_q = quantile(time_po, 0.25, na.rm = TRUE),
              upper_q = quantile(time_po, 0.75, na.rm = TRUE)) %>%
    mutate(days_pi = paste0(signif(median, 2),
                            ' [', signif(lower_q, 2),
                            ' - ', signif(upper_q, 2),
                            ']'),
           days_pi = ifelse(timepoint == 'healthy', NA, days_pi))

  ## sample numbers

  cohort$samples <- incov$clinic  %>%
    filter(patient_id %in% incov$proteome$patient_id |
             patient_id %in% incov$metabolome$patient_id) %>%
    count(timepoint)

  ## common table

  cohort$samples <- left_join(cohort$sampling_time[c('timepoint', 'days_pi')],
                              cohort$samples,
                              by = 'timepoint') %>%
    set_names(c('Time point',
                'Days post infection',
                'Sample number'))


# END -----

  insert_tail()
