# Correlation of metabolic variables with PSS4 stress and PHQ9 depression
# scoring
#
# Spearman's correlation

  insert_head()

# container -----

  mental <- list()

# Analysis globals -----

  insert_msg('Analysis globals')

  ## variables

  mental$variables$met_infl <- globals$stigma_lexicon %>%
    filter(response == 'yes',
           variable %in% distr$stigma_normality_best$base_variable,
           var_group %in% c('metabolism', 'inflammation')) %>%
    .$variable

  mental$variables$mental <-
    c('pss_stress_score', 'phq_dpr_score',
      'hads_dpr_score', 'hads_anx_score')

  mental$pairs <- mental$variables$mental %>%
    map(function(x) mental$variables$met_infl %>%
          map(~c(x, .x))) %>%
    unlist(recursive = FALSE)

# Serial correlation -----

  insert_msg('Serial correlations')

  mental$test <- mental$pairs %>%
    map_dfr(~correlate_variables(stigma$data[.x] %>%
                                   filter(complete.cases(.)),
                                 variables = .x,
                                 what = 'correlation',
                                 type = 'spearman',
                                 ci = TRUE,
                                 pub_styled = FALSE))

  mental$test <- mental$test %>%
    mutate(significant = ifelse(p_adjusted < 0.05, 'yes', 'no'),
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
           plot_cap = paste0(plot_lab, ', ', significance, ', n = ', n))

# Bubble plot with the correlation coefficients ------

  insert_msg('Bubble plot with the correlation coefficients')

  mental$bubble_plot <- mental$test %>%
    ggplot(aes(x = variable1,
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
    labs(title = 'SIMMUN',
         subtitle = "Spearman correlation")

# Point plots ------

  insert_msg('Point plots')

  mental$point_plots <-
    list(var = mental$pairs,
         title = exchange(map_chr(mental$pairs, ~.x[[2]]),
                          dict = globals$stigma_lexicon,
                          key = 'variable',
                          value = 'base_label') %>%
           paste(., 'SIMMUN', sep = ', '),
         sub = mental$test$plot_cap,
         x_lab = exchange(map_chr(mental$pairs, ~.x[[1]]),
                          dict = globals$stigma_lexicon,
                          key = 'variable',
                          value = 'axis_label'),
         y_lab = exchange(map_chr(mental$pairs, ~.x[[2]]),
                          dict = globals$stigma_lexicon,
                          key = 'variable',
                          value = 'axis_label')) %>%
    pmap(function(var, title, sub, y_lab, x_lab) plot_correlation(data = stigma$data[var] %>%
                                                                    filter(complete.cases(.)),
                                                                  variables = var,
                                                                  type = 'correlation',
                                                                  point_hjitter = 0,
                                                                  point_wjitter = 0,
                                                                  cust_theme = globals$common_theme,
                                                                  plot_title = title,
                                                                  x_lab = x_lab,
                                                                  y_lab = y_lab,
                                                                  show_trend = FALSE,
                                                                  point_color = 'steelblue') +
           geom_smooth(method = 'gam') +
           labs(subtitle = sub) +
           theme(plot.tag = element_blank())) %>%
    set_names(mental$pairs %>%
                map_chr(paste, collapse = '_'))

# END -----

  insert_tail()

