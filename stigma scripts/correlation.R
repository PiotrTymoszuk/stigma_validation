# Correlation between metabolic and inflammatory variables in the STIGMA cohort
#
# Spearman's correlation done separately for healthy and CoV convalescents

  insert_head()

# container -----

  met_infl <- list()

# Analysis globals -----

  insert_msg('Analysis globals')

  ## variables

  met_infl$variables <- globals$stigma_lexicon %>%
    filter(response == 'yes',
           variable %in% distr$stigma_normality_best$base_variable) %>%
    dlply('var_group', function(x) x$variable)

  met_infl$pairs <- met_infl$variables$inflammation %>%
    map(function(x) met_infl$variables$metabolism %>%
          map(~c(x, .x))) %>%
    unlist(recursive = FALSE)

  ## analysis tables: for healthy and CoV convalescents

  met_infl$analysis_tbl <- stigma$data %>%
    dlply('cov', as_tibble) %>%
    set_names(c('healthy', 'cov'))

# Serial correlation -----

  insert_msg('Serial correlations')

  met_infl$test <- met_infl$analysis_tbl %>%
    map(function(data) met_infl$pairs %>%
          map_dfr(~correlate_variables(data[.x] %>%
                                         filter(complete.cases(.)),
                                       variables = .x,
                                       what = 'correlation',
                                       type = 'spearman',
                                       ci = TRUE,
                                       pub_styled = FALSE)))


  met_infl$test <- met_infl$test %>%
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
        plot_cap = paste0(plot_lab, ', ', significance, ', n = ', n))

# Bubble plot with the correlation coefficients ------

  insert_msg('Bubble plot with the correlation coefficients')

  met_infl$bubble_plots <-
    map2(met_infl$test,
         c('uninfected, SIMMUN',
           'CoV recovery, SIMMUN'),
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

# Point plots ------

  insert_msg('Point plots')

  ## healthy participants

  met_infl$point_plots$healthy <-
    list(var = met_infl$pairs,
         title = exchange(map_chr(met_infl$pairs, ~.x[[2]]),
                          dict = globals$stigma_lexicon,
                          key = 'variable',
                          value = 'base_label') %>%
           paste(., 'healthy, SIMMUN', sep = ', '),
         sub = met_infl$test$healthy$plot_cap,
         x_lab = exchange(map_chr(met_infl$pairs, ~.x[[1]]),
                          dict = globals$stigma_lexicon,
                          key = 'variable',
                          value = 'axis_label'),
         y_lab = exchange(map_chr(met_infl$pairs, ~.x[[2]]),
                          dict = globals$stigma_lexicon,
                          key = 'variable',
                          value = 'axis_label')) %>%
    pmap(function(var, title, sub, y_lab, x_lab) plot_correlation(data = met_infl$analysis_tbl$healthy[var] %>%
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
    set_names(met_infl$pairs %>%
                map_chr(paste, collapse = '_'))

  ## CoV recovery participants

  met_infl$point_plots$cov <-
    list(var = met_infl$pairs,
         title = exchange(map_chr(met_infl$pairs, ~.x[[2]]),
                          dict = globals$stigma_lexicon,
                          key = 'variable',
                          value = 'base_label') %>%
           paste(., 'CoV recovery, SIMMUN', sep = ', '),
         sub = met_infl$test$cov$plot_cap,
         x_lab = exchange(map_chr(met_infl$pairs, ~.x[[1]]),
                          dict = globals$stigma_lexicon,
                          key = 'variable',
                          value = 'axis_label'),
         y_lab = exchange(map_chr(met_infl$pairs, ~.x[[2]]),
                          dict = globals$stigma_lexicon,
                          key = 'variable',
                          value = 'axis_label')) %>%
    pmap(function(var, title, sub, y_lab, x_lab) plot_correlation(data = met_infl$analysis_tbl$cov[var] %>%
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
                                                                  point_color = 'indianred3') +
           geom_smooth(method = 'gam') +
           labs(subtitle = sub) +
           theme(plot.tag = element_blank())) %>%
    set_names(met_infl$pairs %>%
                map_chr(paste, collapse = '_'))

# END -----

  insert_tail()
