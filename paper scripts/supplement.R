# Supplementary Figures

  insert_head()

# container ------

  suppl <- list()

# Figure S1: time course of inflammatory cytokines, INCOV --------

  insert_msg('Figure S1: time course of inflammatory cytokines, INCOV')

  suppl$incov_infl_tc <-
    time_course$plots[c('IL6_INF',
                        'IL10_INF',
                        'TNF_INF',
                        'IFNG_INF')] %>%
    map(~.x +
          theme(legend.position = 'none',
                plot.tag = element_blank())) %>%
    map2(c(23, 19, 13, 50),
         ~.x + expand_limits(y = .y)) %>%
    plot_grid(plotlist = .,
              ncol = 2,
              align = 'hv',
              axis = 'tblr') %>%
    plot_grid(ggdraw() +
                draw_text(time_course$plots$IL6_INF$labels$tag %>%
                            stri_replace_all(fixed = '\n',
                                             replacement = ', '),
                          size = 8),
              nrow = 2,
              rel_heights = c(0.92, 0.08)) %>%
    as_figure(label = 'figure_s1_incov_precursor_time_course',
              ref_name = 'incov_neuro_tc',
              caption = paste('Serum levels of inflammatory cytokines',
                              'in course of COVID-19',
                              'and recovery.'),
              w = 180,
              h = 150)

# Figure S2: inflammatory parameters in CoV recovery -------

  insert_msg('Figure S2: inflammatory parameters in CoV recovery, STIGMA')

  suppl$hads_cov <-
    c(hads_cov$plots$cov[c('il6', 'crp', 'neo', 'nlr')],
      list(ggdraw()), list(ggdraw())) %>%
    map(~.x +
          theme(plot.tag = element_blank(),
                legend.position = 'none')) %>%
    plot_grid(plotlist = .,
              ncol = 3,
              align = 'hv') %>%
    as_figure(label = 'figure_s2_hads_covid',
              ref_name = 'hads_cov',
              caption = paste('Association of inflammatory markers',
                              'with SARS-CoV-2 infection convalescence',
                              'in the SIMMUN cohort'),
              w = 180,
              h = 120)

# Figure S3: correlations with age, STIGMA ------

  insert_msg('Figure S3: correlation with age, STIGMA')

  suppl$age <- age$point_plots[c('trp',
                                 'log_kyn',
                                 'log_kyn_trp',
                                 'log_phe',
                                 'log_tyr',
                                 'sqrt_phe_tyr')] %>%
    plot_grid(plotlist = .,
              ncol = 2,
              align = 'hv') %>%
    as_figure(label = 'figure_s3_age_correlations',
              ref_name = 'age',
              caption = paste('Correlation of aminoacid neurotransmitter',
                              'precursors and their decay products with age.'),
              w = 180,
              h = 220)

# Figure S4: levels of metabolites in genders, STIGMA ------

  insert_msg('Figure S4: levels of metabolites and gender, STIGMA')

  suppl$gender <- gender$plots[c('trp',
                                 'log_kyn',
                                 'log_kyn_trp',
                                 'log_phe',
                                 'log_tyr',
                                 'sqrt_phe_tyr')] %>%
    map(~.x + theme(legend.position = 'none')) %>%
    plot_grid(plotlist = .,
              ncol = 3,
              align = 'hv') %>%
    as_figure(label = 'figure_s4_gender',
              ref_name = 'gender',
              caption = paste('Levels of aminoacid neurotransmitter',
                              'precursors and their decay products',
                              'in females and males.'),
              w = 180,
              h = 120)

# Figure S5: error and R-squared, multiple linear regression ------

  insert_msg('Figure S5: Fits stats, multiple regression')

  suppl$fit_stats <- stigma_lm$fit_stat_plots %>%
    map2(c('pseudo_log', 'identity'),
         ~.x +
           scale_x_continuous(trans = .y) +
           theme(legend.position = 'none')) %>%
    plot_grid(plotlist = .,
              ncol = 2,
              align = 'hv',
              labels = LETTERS,
              label_size = 10) %>%
    plot_grid(get_legend(stigma_lm$fit_stat_plots[[1]] +
                           theme(legend.position = 'bottom')),
              nrow = 2,
              rel_heights = c(0.9, 0.1)) %>%
    as_figure(label = 'figure_s5_fit_stats',
              ref_name = 'fit_stats',
              caption = paste('Root mean square error and R-squared',
                              'statistics for multi-parameter linear models',
                              'of aminoacid neurotransmitter precursors',
                              'and their decay products in the STIGMA cohort'),
              w = 180,
              h = 110)

# Saving on the disc -----

  insert_msg('Saving the supplementary figures')

  suppl %>%
    walk(pickle,
         path = './paper/supplementary figures',
         format = 'pdf',
         device = cairo_pdf)

# END -----

  insert_tail()
