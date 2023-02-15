# Correlation of the inflammatory and metabolic responses with age
# in the STIGMA cohort. No splitting for the study groups defined by PSS
# and anxiety/depression symptoms (HADS)

  insert_head()

# container ------

  age <- list()

# Analysis globals -----

  insert_msg('Analysis globals')

  ## only variables with the best normality are included in the analysis

  age$variables <- globals$stigma_lexicon %>%
    filter(response == 'yes',
           variable %in% distr$stigma_normality_best$variable) %>%
    .$variable

# Serial testing, analysis with Pearson's correlation ------

  insert_msg('Pearson correlation')

  age$test <- age$variables %>%
    map_dfr(~correlate_variables(stigma$data[c('age', .x)] %>%
                                   filter(complete.cases(.)),
                                 variables = c('age', .x),
                                 what = 'correlation',
                                 type = 'pearson',
                                 ci = TRUE,
                                 pub_styled = FALSE)) %>%
    mutate(correlation = ifelse(p_adjusted >= 0.05,
                                'ns',
                                ifelse(estimate > 0,
                                       'positive', 'negative')),
           correlation = factor(correlation,
                                c('positive',
                                  'negative',
                                  'ns')),
           plot_lab = paste0(signif(estimate, 2),
                             ' [', signif(lower_ci, 2),
                             ' - ', signif(upper_ci, 2), ']'),
           plot_cap = paste0(plot_lab, ', ',
                             significance,
                             ', n = ', n))

# Summary Forest plot with the correlation coefficients ------

  insert_msg('Forest plot of the correlation coefficients')

  age$forest_plot <- age$test %>%
    ggplot(aes(x = estimate,
               y = reorder(variable2, estimate),
               color = correlation)) +
    geom_vline(xintercept = 0,
               linetype = 'dashed') +
    geom_errorbarh(aes(xmin = lower_ci,
                       xmax = upper_ci),
                   height = 0) +
    geom_point(shape = 16,
               size = 2) +
    geom_text(aes(label = plot_lab),
              hjust = 0.5,
              vjust = -1.2,
              size = 2.75) +
    scale_y_discrete(labels = set_names(globals$stigma_lexicon$label,
                                        globals$stigma_lexicon$variable)) +
    scale_color_manual(values = c(positive = 'firebrick',
                                  negative = 'steelblue',
                                  ns = 'gray60'),
                       name = 'Correlation') +
    globals$common_theme +
    theme(axis.title.y = element_blank()) +
    labs(title = 'SIMMUN response variables and age',
         subtitle = "Pearson's correlation",
         x = 'r \u00B1 95% CI')

# Point plots for the correlations ------

  insert_msg('Point plots for the correlations')

  age$point_plots <-
    list(var = map(age$variables, ~c('age', .x)),
         title = exchange(age$variables,
                               dict = globals$stigma_lexicon,
                               key = 'variable',
                               value = 'base_label') %>%
           paste('SIMMUN', sep = ', '),
         sub = age$test$plot_cap,
         y_lab = exchange(age$variables,
                          dict = globals$stigma_lexicon,
                          key = 'variable',
                          value = 'axis_label')) %>%
    pmap(function(var, title, sub, y_lab) plot_correlation(data = stigma$data[var] %>%
                                                             filter(complete.cases(.)),
                                                           variables = var,
                                                           type = 'correlation',
                                                           point_hjitter = 0,
                                                           point_wjitter = 0,
                                                           cust_theme = globals$common_theme,
                                                           plot_title = title,
                                                           x_lab = 'age, years',
                                                           y_lab = y_lab,
                                                           show_trend = TRUE) +
           labs(subtitle = sub) +
           theme(plot.tag = element_blank())) %>%
    set_names(age$variables)

# END ------

  insert_tail()
