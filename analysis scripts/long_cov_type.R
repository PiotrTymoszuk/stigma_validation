# Comparison of metabolite and inflammatory protein levels between types
# of persistent symptoms in the INCOV cohort.
# Comparison of the recovery data and healthy controls, Kruskal-Wallis test

  insert_head()

# container -----

  lc_type <- list()

# globals -----

  insert_msg('Globals')

  ## variables and their labels

  lc_type$splits <- incov$clinic %>%
    select(ends_with('long_cov')) %>%
    names

  lc_type$split_labs <-
    c(long_cov = 'Any symptoms',
      psy_long_cov = 'Sleep disorder, depression or anxiety',
      neuro_long_cov = 'Neurological symptoms, smell/taste',
      gastro_long_cov = 'Gastrointestinal symptoms',
      cogn_long_cov = 'Concentration/memory problems',
      resp_long_cov = 'Respiratory symptoms',
      fatigue_long_cov = 'Fatigue, phys. perfomance loss')

  ## analysis table

  lc_type$analysis_tbl <- incov[c('proteome',
                                  'metabolome')] %>%
    map(select,
        sample_id,
        patient_id,
        timepoint,
        any_of(globals$incov_lexicon$variable),
        all_of(lc_type$splits)) %>%
    reduce(full_join,
           by = c('patient_id',
                  'sample_id',
                  'timepoint',
                  lc_type$splits)) %>%
    filter(timepoint %in% c('healthy', 'recovery')) %>%
    mutate(timepoint = droplevels(timepoint),
           long_cov = car::recode(long_cov,
                                  "'no' = 'recovered';
                                  'yes' = 'any symptoms'"),
           long_cov = factor(long_cov,
                             c('healthy', 'recovered', 'any symptoms')),
           psy_long_cov = car::recode(psy_long_cov,
                                      "'no' = 'other symptoms';
                                      'yes' = 'psychiatric'"),
           psy_long_cov = factor(psy_long_cov,
                                 c('healthy',
                                   'recovered',
                                   'other symptoms',
                                   'psychiatric')),
           neuro_long_cov = car::recode(neuro_long_cov,
                                        "'no' = 'other symptoms';
                                        'yes' = 'neuro/smell/taste'"),
           neuro_long_cov = factor(neuro_long_cov,
                                   c('healthy',
                                     'recovered',
                                     'other symptoms',
                                     'neuro/smell/taste')),
           gastro_long_cov = car::recode(gastro_long_cov,
                                         "'no' = 'other symptoms';
                                         'yes' = 'gastrointestinal'"),
           gastro_long_cov = factor(gastro_long_cov,
                                    c('healthy',
                                      'recovered',
                                      'other symptoms',
                                      'gastrointestinal')),
           cogn_long_cov = car::recode(cogn_long_cov,
                                       "'no' = 'other symptoms';
                                       'yes' = 'cognitive'"),
           cogn_long_cov = factor(cogn_long_cov,
                                  c('healthy',
                                    'recovered',
                                    'other symptoms',
                                    'cognitive')),
           resp_long_cov = car::recode(resp_long_cov,
                                       "'no' = 'other symptoms';
                                       'yes' = 'respiratory'"),
           resp_long_cov = factor(resp_long_cov,
                                  c('healthy',
                                    'recovered',
                                    'other symptoms',
                                    'respiratory')),
           fatigue_long_cov = car::recode(fatigue_long_cov,
                                          "'no' = 'other symptoms';
                                          'yes' = 'fatigue/performance'"),
           fatigue_long_cov = factor(fatigue_long_cov,
                                     c('healthy',
                                       'recovered',
                                       'other symptoms',
                                       'fatigue/performance')))

# descriptive stats -----

  insert_msg('Descriptive stats')

  lc_type$stats <- lc_type$splits %>%
    map(~explore(lc_type$analysis_tbl %>%
                   filter(!is.na(.data[[.x]])),
                 variables = globals$incov_lexicon$variable,
                 split_factor = .x,
                 what = 'table',
                 pub_styled = TRUE,
                 adj_method = 'BH')) %>%
    map(reduce, left_join, by = 'variable') %>%
    set_names(lc_type$splits)

  lc_type$stats$long_cov <-
    lc_type$stats$long_cov %>%
    set_names(c('variable',
                'healthy',
                'recovered',
                'symptoms'))

  lc_type$stats[names(lc_type$stats) != 'long_cov'] <-
    lc_type$stats[names(lc_type$stats) != 'long_cov'] %>%
    map(set_names,
        c('variable',
          'healthy',
          'recovered',
          'other symptoms',
          'symptoms'))

# Testing -------

  insert_msg('Testing')

  lc_type$test <- lc_type$splits %>%
    map(~compare_variables(lc_type$analysis_tbl %>%
                             filter(!is.na(.data[[.x]])),
                           variables = globals$incov_lexicon$variable,
                           split_factor = .x,
                           what = 'test',
                           types = 'kruskal_test',
                           exact = FALSE,
                           ci = FALSE,
                           pub_styled = TRUE)) %>%
    map(mutate,
        plot_cap = paste(eff_size, significance, sep = ', ')) %>%
    set_names(lc_type$splits)

# Post-hoc tests ------

  insert_msg('Post-hoc tests')

  ## the 'dopamine 3-O-sulfate' variable is incompatible with the
  ## formula used for post-hoc testing and will be substituted by 'DA'

  ## formulas

  lc_type$post_hoc_formulas <- lc_type$splits %>%
    map(function(split) c(globals$incov_lexicon$variable[globals$incov_lexicon$variable != 'dopamine 3-O-sulfate'],
                          'DA') %>%
          map(~paste0('`', .x, '` ~ ', split)) %>%
          map(as.formula) %>%
          set_names(globals$incov_lexicon$variable)) %>%
    set_names(lc_type$splits)

  ## testing

  lc_type$post_hoc_test <- lc_type$post_hoc_formulas %>%
    map(~map(.x,
             compare_means,
             data = lc_type$analysis_tbl %>%
               mutate(DA = `dopamine 3-O-sulfate`),
             method = 'wilcox.test',
             p.adjust.method = 'BH') %>%
          map(mutate,
              plot_cap = ifelse(p.adj > 0.1,
                                   'ns',
                                   ifelse(p.adj >= 0.05,
                                          paste0('ns (p = ', signif(p.adj, 2), ')'),
                                          paste('p =', signif(p.adj, 2))))))

  for(i in lc_type$post_hoc_test) {

    i$`dopamine 3-O-sulfate` <- i$`dopamine 3-O-sulfate` %>%
      mutate(.y = 'dopamine 3-O-sulfate')

  }



# Plotting ------

  insert_msg('Plotting')

  lc_type$plots <- set_names(lc_type$splits,
                             lc_type$splits) %>%
    map(function(split) list(variable = globals$incov_lexicon$variable,
                             plot_title = paste(globals$incov_lexicon$label,
                                                'recovery, INCOV',
                                                sep = ', '),
                             plot_subtitle = paste('Kruskal-Wallis test:',
                                                   lc_type$test[[split]]$significance)) %>%
          pmap(plot_variable,
               lc_type$analysis_tbl %>%
                 filter(!is.na(.data[[split]])),
               split_factor = split,
               type = 'box',
               y_lab = expression('Normalized log'[2] * ' concentration'),
               cust_theme = globals$common_theme) %>%
          set_names(globals$incov_lexicon$variable))

# Manual adjustments, adding multiple comparisons ------

  insert_msg('Manual scale adjustment and post-hoc results')

  ## any symptoms

  lc_type$plots$long_cov <- lc_type$plots$long_cov %>%
    map(~.x +
          scale_fill_manual(values = c(healthy = 'cornsilk2',
                                       recovered = 'steelblue3',
                                       `any symptoms` = 'indianred3'),
                            name = '')) %>%
    map2(lc_type$post_hoc_test$long_cov %>%
           map(~.x[c(1, 3), ]),
        add_p_chain)

  ## psychiatric symptoms

  lc_type$plots$psy_long_cov <- lc_type$plots$psy_long_cov %>%
    map(~.x +
          scale_fill_manual(values = c(healthy = 'cornsilk2',
                                       recovered = 'steelblue3',
                                       `other symptoms` = 'gray60',
                                       psychiatric = 'coral3'),
                            name = '')) %>%
    map2(lc_type$post_hoc_test$psy_long_cov %>%
           map(~.x[c(1, 4, 6), ]),
         add_p_chain)

  ## neurological symptoms

  lc_type$plots$neuro_long_cov <- lc_type$plots$neuro_long_cov %>%
    map(~.x +
          scale_fill_manual(values = c(healthy = 'cornsilk2',
                                       recovered = 'steelblue3',
                                       `other symptoms` = 'gray60',
                                       `neuro/smell/taste` = 'coral3'),
                            name = '')) %>%
    map2(lc_type$post_hoc_test$neuro_long_cov %>%
           map(~.x[c(1, 4, 6), ]),
         add_p_chain)

  ## gastrointestinal symptoms

  lc_type$plots$gastro_long_cov <- lc_type$plots$gastro_long_cov %>%
    map(~.x +
          scale_fill_manual(values = c(healthy = 'cornsilk2',
                                       recovered = 'steelblue3',
                                       `other symptoms` = 'gray60',
                                       gastrointestinal = 'coral3'),
                            name = '')) %>%
    map2(lc_type$post_hoc_test$gastro_long_cov %>%
           map(~.x[c(1, 4, 6), ]),
         add_p_chain)

  ## cognitive symptoms

  lc_type$plots$cogn_long_cov <- lc_type$plots$cogn_long_cov %>%
    map(~.x +
          scale_fill_manual(values = c(healthy = 'cornsilk2',
                                       recovered = 'steelblue3',
                                       `other symptoms` = 'gray60',
                                       cognitive = 'coral3'),
                            name = '')) %>%
    map2(lc_type$post_hoc_test$cogn_long_cov %>%
           map(~.x[c(1, 4, 6), ]),
         add_p_chain)

  ## respiratory symptoms

  lc_type$plots$resp_long_cov <- lc_type$plots$resp_long_cov %>%
    map(~.x +
          scale_fill_manual(values = c(healthy = 'cornsilk2',
                                       recovered = 'steelblue3',
                                       `other symptoms` = 'gray60',
                                       respiratory = 'coral3'),
                            name = '')) %>%
    map2(lc_type$post_hoc_test$resp_long_cov %>%
           map(~.x[c(1, 4, 6), ]),
         add_p_chain)

  ## respiratory symptoms

  lc_type$plots$fatigue_long_cov <- lc_type$plots$fatigue_long_cov %>%
    map(~.x +
          scale_fill_manual(values = c(healthy = 'cornsilk2',
                                       recovered = 'steelblue3',
                                       `other symptoms` = 'gray60',
                                       `fatigue/performance` = 'coral3'),
                            name = '')) %>%
    map2(lc_type$post_hoc_test$fatigue_long_cov %>%
           map(~.x[c(1, 4, 6), ]),
         add_p_chain)

# END ------

  insert_tail()
