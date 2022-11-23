# Supplementary Figures

  insert_head()

# container ------

  suppl <- list()

# Figures S1 and S2: cytokines and TRP metabolites and psychiatric long CoV -------

  insert_msg('Figures S1 - S2: cytokines and TRP metabolites, neurological long CoV')

  suppl[c('incov_neuro_cyto',
         'incov_neuro_trp')] <-
    list(lc_type$plots$neuro_long_cov[c('IL6_INF',
                                      'IL10_INF',
                                      'TNF_INF',
                                      'IFNG_INF')],
         lc_type$plots$neuro_long_cov[c('tryptophan',
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

  suppl[c('incov_neuro_cyto',
          'incov_neuro_trp')]<-
    map2(suppl[c('incov_neuro_cyto',
                 'incov_neuro_trp')],
         list(lc_type$plots$neuro_long_cov$IL6_INF,
              lc_type$plots$neuro_long_cov$tryptophan),
         ~plot_grid(.x,
                    ggdraw() +
                      draw_text(.y$labels$tag %>%
                                  stri_replace_all(fixed = '\n',
                                                   replacement = ', '),
                                size = 8),
                    nrow = 2,
                    rel_heights = c(1, 0.1)))

  suppl[c('incov_neuro_cyto',
          'incov_neuro_trp')] <- suppl[c('incov_neuro_cyto',
                                         'incov_neuro_trp')] %>%
    list(x = .,
         label = c('figure_s1_neuro_cyto_incov',
                   'figure_s2_neuro_trp_incov'),
         ref_name = c('incov_neuro_cyto',
                      'incov_neuro_trp'),
         caption = c(paste('Serum levels of cytokines in healthy controls,',
                           'complete COVID-19 recovery, non-neurological',
                           'and neurological',
                           'persistent symptoms in the INCOV cohort.'),
                     paste('Serum levels of TRP degradation products in',
                           'healthy controls, complete COVID-19 recovery,',
                           'non-neurological and neurological persistent symptoms',
                           'in the INCOV cohort.'))) %>%
    pmap(as_figure,
         w = 180,
         h = 165)

# Saving on the disc -----

  insert_msg('Saving the supplementary figures')

  suppl %>%
    walk(pickle,
         path = './paper/supplementary figures',
         format = 'pdf',
         device = cairo_pdf)

# END -----

  insert_tail()
