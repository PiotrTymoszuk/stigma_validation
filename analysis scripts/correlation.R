# Correlation of inflammatory and metabolic INCOV variables (Spearman)

  insert_head()

# container -----

  corr <- list()

# globals ------

  insert_msg('Globals')

  ## analysis table

  corr$analysis_tbl <- incov[c('metabolome', 'proteome')] %>%
    map(select,
        sample_id,
        patient_id,
        timepoint,
        any_of(globals$incov_lexicon$variable)) %>%
    reduce(left_join, by = c('sample_id', 'patient_id', 'timepoint')) %>%
    filter(complete.cases(.)) %>%
    dlply('timepoint', as_tibble)

  ## variable pairs

  corr$pairs <- globals$incov_proteins %>%
    map(function(prot) globals$incov_metabolites %>%
          map(~c(prot, .x))) %>%
    unlist(recursive = FALSE)

# Correlation analysis ------

  insert_msg('Correlation')

  corr$test <- corr$analysis_tbl %>%
    map(function(dat) corr$pairs %>%
          map_dfr(~correlate_variables(dat,
                                       variables = .x,
                                       what = 'correlation',
                                       type = 'spearman',
                                       ci = TRUE)))

  ## appending with the plot elements

  corr$test <- corr$test %>%
    map(mutate,
        plot_cap = paste0('\u03C1 = ', signif(estimate, 2),
                          ' [', signif(lower_ci, 2),
                          ' - ', signif(upper_ci, 2), ']',
                          ', ', significance,
                          ', n = ', n),
        x_lab = exchange(variable1,
                         dict = globals$incov_lexicon,
                         key = 'variable',
                         value = 'axis_lab'),
        y_lab = exchange(variable2,
                         dict = globals$incov_lexicon,
                         key = 'variable',
                         value = 'axis_lab'))

# Bubble plots -------

  insert_msg('Bubble plots')

  corr$bubbles <-
    list(data = corr$test,
         plot_title = paste(names(corr$analysis_tbl),
                            'INCOV', sep = ', ')) %>%
    pmap(corr_buble) %>%
    map(~.x +
          scale_y_discrete(limits = c('phenylalanine',
                                      'tyrosine',
                                      'dopamine 3-O-sulfate',
                                      'tryptophan',
                                      'serotonin',
                                      'kynurenine',
                                      'quinolinate'),
                           labels = set_names(globals$incov_lexicon$label,
                                              globals$incov_lexicon$variable)) +
          scale_x_discrete(limits = c('IL6_INF',
                                      'IL10_INF',
                                      'TNF_INF',
                                      'IFNG_INF'),
                           labels = set_names(globals$incov_lexicon$label,
                                              globals$incov_lexicon$variable)))

# Classical correlation plots -------

  insert_msg('Classical correlation plots')

  corr$point_plots <- list(dat = corr$analysis_tbl,
                                 stat = corr$test,
                                 tit = paste('INCOV,', names(corr$analysis_tbl))) %>%
    pmap(function(dat, stat, tit) list(variables = corr$pairs,
                                       plot_subtitle = stat$plot_cap,
                                       x_lab = stat$x_lab,
                                       y_lab = stat$y_lab) %>%
           pmap(plot_correlation,
                data = dat,
                type = 'correlation',
                point_hjitter = 0,
                point_wjitter = 0,
                point_color = 'coral3',
                plot_title = tit,
                show_trend = FALSE,
                cust_theme = globals$common_theme) %>%
           map(~.x +
                 geom_smooth(method = 'lm') +
                 theme(plot.tag = element_blank())) %>%
           set_names(map(corr$pairs, paste, collapse = '_')))

# END ------

  insert_tail()
