# Main figures

  insert_head()

# container ------

  figs <- list()

# Figure 2 and 3: time course of inflammatory cytokines and TRP metabolites -------

  insert_msg('Figure 2 and 3: time course of cytokines and TRP metabolites')

  figs[c('incov_cyto',
         'incov_trp')] <- list(time_course$plots[c('IL6_INF',
                                                   'IL10_INF',
                                                   'TNF_INF',
                                                   'IFNG_INF')],
                               time_course$plots[c('tryptophan',
                                                   'serotonin',
                                                   'kynurenine',
                                                   'quinolinate')]) %>%
    map(~map(.x,
             ~.x +
               theme(legend.position = 'none',
                     axis.title.x = element_blank(),
                     plot.tag = element_blank()))) %>%
    map(~plot_grid(plotlist = .x,
                   ncol = 2,
                   align = 'hv')) %>%
    map2(.,
         time_course$plots[c('IL6_INF',
                             'tryptophan')],
         ~plot_grid(.x,
                    ggdraw() +
                      draw_text(.y$labels$tag %>%
                                  stri_replace_all(fixed = '\n',
                                                   replacement = ', '),
                                size = 8),
                    nrow = 2,
                    rel_heights = c(0.92, 0.08)))

  figs[c('incov_cyto',
         'incov_trp')] <- figs[c('incov_cyto',
                                 'incov_trp')] %>%
    list(x = .,
         label = c('figure_2_tc_cytokines_incov',
                   'figure_3_tc_trp_incov'),
         ref_name = c('incov_cyto',
                      'incov_trp'),
         caption = c(paste('Serum levels of cytokines in healthy controls',
                           'and COVID-19 individuals in the INCOV cohort'),
                     paste('Serum levels of TRP degradation products in',
                           'healthy controls and COVID-19 individuals',
                           'in the INCOV cohort'))) %>%
    pmap(as_figure,
         w = 180,
         h = 160)

# Figure 4: correlations ------

  insert_msg('Figure 4: correlation of TRP metabolites and cytokines')

  figs$corr_incov <-
    corr$bubbles %>%
    map(~.x +
          scale_y_discrete(limits = c('tryptophan',
                                      'serotonin',
                                      'kynurenine',
                                      'quinolinate'),
                           labels = set_names(globals$incov_lexicon$label,
                                              globals$incov_lexicon$variable)) +
          theme(axis.text.x = element_text(angle = 0,
                                           hjust = 0.5),
                legend.position = 'none')) %>%
    plot_grid(plotlist = .,
              align = 'hv') %>%
    plot_grid(get_legend(corr$bubbles[[1]]),
              ncol = 2,
              rel_widths = c(0.9, 0.1)) %>%
    as_figure(label = 'figure_4_incov_correlations',
              ref_name = 'corr_incov',
              caption = paste('Correlation of serum levels of cytokines,',
                              'TRP and TRP degradation products in acute',
                              'COVID-19 and convalescence in the',
                              'INCOV cohort.'),
              w = 180,
              h = 150)

# Figure 5 and 6: cytokines and TRP metabolites and psychiatric long CoV -------

  insert_msg('Figures 5 - 6: cytokines and TRP metabolites, long CoV')

  figs[c('incov_long_cyto',
          'incov_long_trp')] <-
    list(lc_type$plots$psy_long_cov[c('IL6_INF',
                                      'IL10_INF',
                                      'TNF_INF',
                                      'IFNG_INF')],
         lc_type$plots$psy_long_cov[c('tryptophan',
                                      'serotonin',
                                      'kynurenine',
                                      'quinolinate')]) %>%
    map(~map(.x,
             ~.x +
               scale_x_discrete(labels = function(x) stri_replace(x, fixed = ' ', replacement = '\n')) +
               theme(plot.tag = element_blank(),
                     legend.position = 'none'))) %>%
    map(~plot_grid(plotlist = .x,
                   ncol = 2,
                   align = 'hv'))

  figs[c('incov_long_cyto',
         'incov_long_trp')] <-
    map2(figs[c('incov_long_cyto',
                'incov_long_trp')],
         list(lc_type$plots$psy_long_cov$IL6_INF,
              lc_type$plots$psy_long_cov$tryptophan),
         ~plot_grid(.x,
                    ggdraw() +
                      draw_text(.y$labels$tag %>%
                                  stri_replace_all(fixed = '\n',
                                                   replacement = ', '),
                                size = 8),
                    nrow = 2,
                    rel_heights = c(1, 0.1)))

  figs[c('incov_long_cyto',
          'incov_long_trp')] <- figs[c('incov_long_cyto',
                                        'incov_long_trp')] %>%
    list(x = .,
         label = c('figure_5_lc_cyto_incov',
                   'figure_6_lc_trp_incov'),
         ref_name = c('incov_long_cyto',
                      'incov_long_trp'),
         caption = c(paste('Serum levels of cytokines in healthy controls,',
                           'complete COVID-19 recovery, non-psychiatric',
                           'and psychiatric',
                           'persistent symptoms in the INCOV cohort.'),
                     paste('Serum levels of TRP degradation products in',
                           'healthy controls, complete COVID-19 recovery,',
                           'non-psychiatric and psychiatric persistent symptoms',
                           'in the INCOV cohort.'))) %>%
    pmap(as_figure,
         w = 180,
         h = 165)

# Saving the figures on the disc ------

  insert_msg('Saving figures on the disc')

  figs %>%
    walk(pickle,
         format = 'pdf',
         path = './paper/figures',
         device = cairo_pdf)

# END -----

  insert_tail()
