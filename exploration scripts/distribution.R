# Normality of distribution and equality of variances
# Split by the timepoint, the CoV subset only

  insert_head()

# container -----

  distr <- list()

# globals: analysis tables -------

  insert_msg('Analysis tables')

  distr$analysis_tbl <- incov[c('proteome',
                                'metabolome')] %>%
    map(select,
        sample_id,
        patient_id,
        timepoint,
        any_of(globals$incov_lexicon$variable))

  ## reducing to a single data frame

  distr$analysis_tbl <- distr$analysis_tbl %>%
    reduce(left_join,
           by = c('sample_id', 'patient_id', 'timepoint'))

# Normality: Shapiro-Wilk tests ------

  insert_msg('Normality: Shapiro-Wilk test')

  distr$shapiro_test <- distr$analysis_tbl %>%
    explore(variables = globals$incov_lexicon$variable,
            split_factor = 'timepoint',
            what = 'normality',
            pub_styled = TRUE)

# EOV: Levene test --------

  insert_msg('Homogeneity of variances')

  ## done separately for all samples and the CoV individuals only

  distr$levene_test_all <- distr$analysis_tbl %>%
    compare_variables(variables = globals$incov_lexicon$variable,
                      split_factor = 'timepoint',
                      what = 'variance',
                      pub_styled = TRUE)

  distr$levene_test_cov <- distr$analysis_tbl %>%
    filter(timepoint != 'healthy') %>%
    compare_variables(variables = globals$incov_lexicon$variable,
                      split_factor = 'timepoint',
                      what = 'variance',
                      pub_styled = TRUE)

# END -------

  insert_tail()
