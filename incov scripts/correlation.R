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
        significant = ifelse(p_adjusted < 0.05, 'yes', 'no'),
        font_face = ifelse(significant == 'yes', 'bold', 'plain'),
        correlation = ifelse(p_adjusted >= 0.05,
                             'ns',
                             ifelse(estimate > 0,
                                    'positive', 'negative')),
        correlation = factor(correlation,
                             c('positive',
                               'negative',
                               'ns')),
        bubble_lab = signif(estimate, 2),
        plot_lab = paste0(signif(estimate, 2),
                          ' [', signif(lower_ci, 2),
                          ' - ', signif(upper_ci, 2), ']'),
        plot_cap = paste0(plot_lab, ', ', significance, ', n = ', n),
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
    map2(corr$test,
         c('uninfected, INCOV',
           'acute CoV, INCOV',
           'sub-acute CoV, INCOV',
           'CoV recovery, INCOV'),
         ~ggplot(.x,
                 aes(x = variable1,
                     y = variable2,
                     fill = estimate,
                     size = abs(estimate))) +
           geom_point(shape = 21,
                      color = 'black') +
           geom_text(aes(label = bubble_lab,
                         color = significant,
                         fontface = font_face),
                     size = 2.5,
                     hjust = 0.5,
                     vjust = -1.5) +
           scale_x_discrete(labels = set_names(globals$stigma_lexicon$label,
                                               globals$stigma_lexicon$variable),
                            name = '') +
           scale_y_discrete(labels = set_names(globals$stigma_lexicon$label,
                                               globals$stigma_lexicon$variable),
                            name = '') +
           scale_fill_gradient2(low = 'steelblue',
                                mid = 'white',
                                high = 'firebrick',
                                midpoint = 0,
                                limits = c(-1, 1),
                                name = 'r') +
           scale_color_manual(values = c(no = 'gray70',
                                         yes = 'black')) +
           guides(color = 'none',
                  size = 'none') +
           globals$common_theme +
           theme(axis.title = element_blank()) +
           labs(title = .y,
                subtitle = "Spearman correlation"))

  ## extra adjustment of scales

  corr$bubbles <- corr$bubbles %>%
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
