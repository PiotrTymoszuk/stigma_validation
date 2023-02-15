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

# Figure 3: regulation of precursors of by CoV and humoral immunity ----

  insert_msg('Figure 3: neurotransmitter precursors by CoV status and IgG')

  ## top panel: effects of CoV

  figs$cov$upper_panel <-
    hads_cov$plots$cov[c('trp', 'kyn', 'kyn_trp',
                         'phe', 'tyr', 'phe_tyr')] %>%
    map(~.x +
          theme(plot.tag = element_blank(),
                legend.position = 'none')) %>%
    plot_grid(plotlist = .,
              ncol = 3,
              align = 'hv')

  ## bottom panel: effects of anti-RBD IgG

  figs$cov$bottom_panel <-
    rbd$plots$cov[c('trp', 'kyn', 'kyn_trp',
                    'phe', 'tyr', 'phe_tyr')] %>%
    map(~.x +
          labs(subtitle = .x$labels$subtitle %>%
                 stri_replace(fixed = '\u03C1 = ',
                              replacement = '') %>%
                 stri_replace(regex = ',\\s{1}n\\s{1}=.*$',
                              replacement = ''))) %>%
    plot_grid(plotlist = .,
              ncol = 3,
              align = 'hv')

  ## the entire figure

  figs$cov <- plot_grid(figs$cov$upper_panel,
                        figs$cov$bottom_panel,
                        nrow = 2,
                        labels = LETTERS,
                        label_size = 10) %>%
    as_figure(label = 'figure_3_covid',
              ref_name = 'cov',
              caption = paste('Association of neurotransmitter precursor',
                              'aminoacids and their decay products',
                              'with SARS-CoV-2 infection and anti-SARS-CoV-2',
                              'antibody response in the SIMMUN cohort.'),
              w = 180,
              h = 230)

# Figure 4: Mental health and metabolites ------

  insert_msg('Figure 4: Mental health scoring and metabolites')

  figs$pss <-
    plot_grid(mental$bubble_plot +
                scale_x_discrete(limits = c('hads_anx_score',
                                            'hads_dpr_score',
                                            'pss_stress_score'),
                                 labels = c('hads_anx_score',
                                            'hads_dpr_score',
                                            'pss_stress_score') %>%
                                   exchange(dict = globals$stigma_lexicon,
                                            key = 'variable',
                                            value = 'label')) +
                scale_y_discrete(limits = c('trp', 'kyn', 'kyn_trp',
                                            'phe', 'tyr', 'phe_tyr'),
                                 labels = c('trp', 'kyn', 'kyn_trp',
                                            'phe', 'tyr', 'phe_tyr') %>%
                                   exchange(dict = globals$stigma_lexicon,
                                            key = 'variable',
                                            value = 'label')),
              ncol = 2,
              rel_widths = c(0.85, 0.15)) %>%
    as_figure(label = 'figure_4_pss',
              ref_name = 'pss',
              caption = paste('Association of neurotransmitter precursor',
                              'aminoacids and their decay products',
                              'with anxiety, depression and stress scoring',
                              'in the SIMMUN cohort.'),
              w = 180,
              h = 100)

# Figure 5: multi-variate modeling of metabolite levels, STIGMA -----

  insert_msg('Figure 5: multi-parameter modeling of the metabolite levels')

  figs$multi_model <- stigma_lm$forest_plots[c('trp', 'kyn', 'kyn_trp',
                                               'tyr', 'phe_tyr')] %>%
    map(~.x +
          theme(legend.position = 'none') +
          scale_y_discrete(labels = function(x) stri_replace(x,
                                                             fixed = 'SARS-CoV-2',
                                                             replacement = 'CoV')))

  figs$multi_model[c('kyn_trp', 'tyr', 'phe_tyr')] <-
    map2(figs$multi_model[c('kyn_trp', 'tyr', 'phe_tyr')],
         c(0.8, 2.5, 1.5),
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
    as_figure(label = 'figure_5_multi_modeling',
              ref_name = 'multi_model',
              caption = paste('Results of multi-parameter modeling of',
                              'aminoacid neurotransmitter precursor and their',
                              'decay products.'),
              w = 180,
              h = 215)

# Figure 7: analysis summary --------

  insert_msg('Figure 7: analysis summary')

  figs$summary <- ggdraw() +
    draw_image('./aux files/summary.png')

  figs$summary <- figs$summary %>%
    as_figure(label = 'figure_7_summary',
              ref_name = 'summary',
              caption = paste('Schematic representation of effects of',
                              'inflammation, SARS-CoV-2 and',
                              'depression/anxiety symptoms on indolamine',
                              'and catecholamine neurotransmitter',
                              'precursor metabolism.'),
              w = 90,
              h = 1752/1854 * 90)

# Saving the figures on the disc ------

  insert_msg('Saving figures on the disc')

  figs %>%
    walk(pickle,
         format = 'pdf',
         path = './paper/figures',
         device = cairo_pdf)

# END -----

  insert_tail()
