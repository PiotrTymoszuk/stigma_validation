# Correlation of anti-RBD antibodies with the metabolic parameters.
#
#
# Done separately for healthy and CoV individuals

  insert_head()

# container -------

  rbd <- list()

# analysis globals ------

  insert_msg('Analysis globals')

  rbd$variables <- globals$stigma_lexicon %>%
    filter(response == 'yes',
           variable %in% distr$stigma_normality_best$base_variable,
           var_group == 'metabolism') %>%
    .$variable

  ## analysis tables

  rbd$analysis_tbl <- stigma$data[c('cov',
                                    'anti_rbd',
                                    'log_anti_rbd',
                                    'pss',
                                    rbd$variables)] %>%
    dlply('cov', as_tibble)

# serial correlation -------

  insert_msg('Serial correlation')

  rbd$test <- rbd$analysis_tbl %>%
    map(function(data) rbd$variables %>%
          map_dfr(~correlate_variables(data[c(.x, 'anti_rbd')] %>%
                                         filter(complete.cases(.)),
                                       variables = c('anti_rbd', .x),
                                       what = 'correlation',
                                       type = 'spearman',
                                       ci = TRUE,
                                       pub_styled = TRUE))) %>%
    map(mutate,
        eff_size = stri_replace(eff_size,
                                fixed = 'rho',
                                replacement = '\u03C1'),
        plot_cap = paste0(eff_size, ', ', significance, ', n = ', n))

# Point plots -------

  insert_msg('Correlation plots')

  ## healthy individuals

  rbd$plots$healthy <-
    list(variables = map(rbd$variables, ~c('anti_rbd', .x)),
         plot_title = exchange(rbd$variables,
                               dict = globals$stigma_lexicon,
                               key = 'variable',
                               value = 'label') %>%
           paste('SIMMUN, healthy', sep = ', '),
         plot_subtitle = rbd$test$healthy$plot_cap,
         y_lab = exchange(rbd$variables,
                          dict = globals$stigma_lexicon,
                          key = 'variable',
                          value = 'axis_label')) %>%
    pmap(plot_correlation,
         rbd$analysis_tbl$healthy,
         type = 'correlation',
         point_hjitter = 0,
         point_wjitter = 0,
         point_color = 'darkolivegreen4',
         x_lab = exchange('anti_rbd',
                          dict = globals$stigma_lexicon,
                          key = 'variable',
                          value = 'axis_label'),
         cust_theme = globals$common_theme,
         show_trend = FALSE) %>%
    map(~.x +
          geom_smooth(method = 'gam') +
          theme(plot.tag = element_blank())) %>%
    set_names(rbd$variables)

  ## CoV convalescence

  rbd$plots$cov <-
    list(variables = map(rbd$variables, ~c('anti_rbd', .x)),
         plot_title = exchange(rbd$variables,
                               dict = globals$stigma_lexicon,
                               key = 'variable',
                               value = 'label') %>%
           paste('SIMMUN, CoV recovery', sep = ', '),
         plot_subtitle = rbd$test$`SARS-CoV-2`$plot_cap,
         y_lab = exchange(rbd$variables,
                          dict = globals$stigma_lexicon,
                          key = 'variable',
                          value = 'axis_label')) %>%
    pmap(plot_correlation,
         rbd$analysis_tbl$`SARS-CoV-2`,
         type = 'correlation',
         point_hjitter = 0,
         point_wjitter = 0,
         point_color = 'coral3',
         x_lab = exchange('anti_rbd',
                          dict = globals$stigma_lexicon,
                          key = 'variable',
                          value = 'axis_label'),
         cust_theme = globals$common_theme,
         show_trend = FALSE) %>%
    map(~.x +
          theme(plot.tag = element_blank())) %>%
    map(~.x +
          geom_smooth(method = 'gam') +
          theme(plot.tag = element_blank())) %>%
    set_names(rbd$variables)

# END ------

  insert_tail()
