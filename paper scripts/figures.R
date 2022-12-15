# Main figures

  insert_head()

# container ------

  figs <- list()

# Figure 1: Time course of neurotransmitter precursors, INCOV ------

  insert_msg('Figure 1: Time course of neurotransmitter precursors, INCOV')

  figs$incov_neuro_tc <-
    time_course$plots[c('tryptophan',
                        'kynurenine',
                        'quinolinate',
                        'phenylalanine',
                        'tyrosine')] %>%
    map(~.x +
          theme(legend.position = 'none',
                plot.tag = element_blank())) %>%
    map2(c(13, 11, 12, 9, 9),
         ~.x + expand_limits(y = .y)) %>%
    c(list(ggdraw() +
             draw_text(time_course$plots$tryptophan$labels$tag,
                       size = 8,
                       hjust = 0,
                       x = 0.05))) %>%
    plot_grid(plotlist = .,
              ncol = 2,
              align = 'hv',
              axis = 'tblr') %>%
    as_figure(label = 'figure_1_incov_precursor_time_course',
              ref_name = 'incov_neuro_tc',
              caption = paste('Serum levels of neurotransmitter precursor',
                              'and their decay products in course of COVID-19',
                              'and recovery.'),
              w = 180,
              h = 220)

# Figure 2: correlation of metabolic parameters with inflammation ------

  insert_msg('Figure2: Correlation of metabolites with inflammation markers')

  ## upper panel: the STIGMA cohort

  figs$met_infl$upper_panel <- met_infl$bubble_plots %>%
    map(~.x +
          theme(plot.subtitle = element_blank(),
                legend.position = 'none') +
          labs(fill = expression(rho)) +
          scale_y_discrete(limits = rev(c('trp', 'kyn', 'kyn_trp',
                                          'phe', 'tyr', 'phe_tyr')),
                           labels = set_names(globals$stigma_lexicon$label,
                                              globals$stigma_lexicon$variable)) +
          scale_x_discrete(limits = c('il6', 'crp', 'neo', 'nlr'),
                           labels = set_names(globals$stigma_lexicon$label,
                                              globals$stigma_lexicon$variable)))

  ## lower panel: INCOV cohort

  figs$met_infl$bottom_panel <- corr$bubbles %>%
    map(~.x +
          theme(plot.subtitle = element_blank(),
                legend.position = 'none') +
          labs(fill = expression(rho)) +
          scale_y_discrete(limits = rev(c('tryptophan',
                                          'kynurenine',
                                          'quinolinate',
                                          'phenylalanine',
                                          'tyrosine')),
                           labels = set_names(globals$incov_lexicon$label,
                                              globals$incov_lexicon$variable)))

  ## the entire figure

  figs$met_infl <- c(figs$met_infl$upper_panel,
                     figs$met_infl$bottom_panel) %>%
    plot_grid(plotlist = .,
              ncol = 2,
              align = 'hv',
              labels = c('A', '',
                         'B', ''),
              label_size = 10)  %>%
    plot_grid(get_legend(met_infl$bubble_plots[[1]]),
              ncol = 2,
              rel_widths = c(0.9, 0.1)) %>%
    as_figure(label = 'figure_2_metabolite_inflammation_correlation',
              ref_name = 'met_infl',
              caption = paste('Correlation of serum levels of aminoacid',
                              'precursors of neurotransmitters and their',
                              'decay products with markers of inflammation.'),
              w = 180,
              h = 220)

# Figure 3: regulation of precursors of by CoV and HADS ----

  insert_msg('Figure 3: neurotransmitter precursors by CoV and HADS')

  figs$hads_cov <- hads_cov$plots[c('cov', 'hads')] %>%
    map(~.x[c('trp', 'kyn', 'kyn_trp',
              'phe', 'tyr', 'phe_tyr')]) %>%
    unlist(recursive = FALSE) %>%
    map(~.x +
          theme(plot.tag = element_blank(),
                legend.position = 'none')) %>%
    plot_grid(plotlist = .,
              ncol = 3,
              align = 'hv',
              labels = c('A', '', '',
                         '', '', '',
                         'B', '', ''),
              label_size = 10) %>%
    as_figure(label = 'figure_3_hads_covid',
              ref_name = 'hads_cov',
              caption = paste('Levels of neurotransmitter precursor',
                              'aminoacids and their decay products in',
                              'STIGMA cohort participants stratified by',
                              'COVID-19 status and depression/anxiety signs.'),
              w = 180,
              h = 220)

# Figure 4: multi-variate modeling of metabolite levels, STIGMA -----

  insert_msg('Figure 4: multi-parameter modeling of the metabolite levels')

  figs$multi_model <- stigma_lm$forest_plots[c('trp', 'kyn', 'kyn_trp',
                                               'tyr', 'phe_tyr')] %>%
    map(~.x + theme(legend.position = 'none'))

  figs$multi_model[c('kyn_trp', 'tyr', 'phe_tyr')] <-
    map2(figs$multi_model[c('kyn_trp', 'tyr', 'phe_tyr')],
         c(0.7, 0.3, 0.3),
         ~.x + expand_limits(x = .y))

  figs$multi_model <- c(figs$multi_model[c('trp', 'kyn', 'kyn_trp')],
                        list(get_legend(stigma_lm$forest_plots[[1]])),
                        figs$multi_model[c('tyr', 'phe_tyr')])

  figs$multi_model <- figs$multi_model %>%
    plot_grid(plotlist = .,
              ncol = 2,
              align = 'hv',
              axis = 'tblr',
              labels = c('A', '', '', '', 'B'),
              label_size = 10) %>%
    as_figure(label = 'figure_4_multi_modeling',
              ref_name = 'multi_model',
              caption = paste('Results of multi-parameter modeling of',
                              'aminoacid neurotransmitter precursor and their',
                              'decay products.'),
              w = 180,
              h = 190)

# Figure 5: analysis summary --------

  insert_msg('Figure 5: analysis summary')

  figs$summary <- ggdraw() +
    draw_image('./aux files/summary.png')

  figs$summary <- figs$summary %>%
    as_figure(label = 'figure_5_summary',
              ref_name = 'summary',
              caption = paste('Schematic representation of effects of',
                              'inflammation, SARS-CoV-2 and',
                              'depression/anxiety symptoms on indolamine',
                              'and catecholamine neurotransmitter',
                              'precursor metabolism.'),
              w = 180,
              h = 1974/4014 * 180)

# Saving the figures on the disc ------

  insert_msg('Saving figures on the disc')

  figs %>%
    walk(pickle,
         format = 'pdf',
         path = './paper/figures',
         device = cairo_pdf)

# END -----

  insert_tail()
