# Effects of each HADS strata, CoV status and persistent somatic symptoms
# on metabolic
# and inflammatory variables. Comparisons with Mann-Whitney U test.

  insert_head()

# container -----

  hads_cov <- list()

# analysis globals ------

  insert_msg('Analysis globals')

  ## variables

  hads_cov$variables <-
    set_names(as.character(distr$stigma_normality_best$variable),
              as.character(distr$stigma_normality_best$base_variable))

  hads_cov$analysis_tbl <- stigma$data %>%
    mutate(cov = car::recode(cov,
                             "'healthy' = 'uninfected';
                             'SARS-CoV-2' = 'CoV recovery'"),
           cov = factor(cov,
                        c('uninfected', 'CoV recovery')))

# descriptive stats ------

  insert_msg('Descriptive stats')

  hads_cov$stats <- c('hads' = 'hads_signs',
                      'cov' = 'cov') %>%
    map(~explore(hads_cov$analysis_tbl,
                 split_factor = .x,
                 variables = hads_cov$variables,
                 what = 'table',
                 pub_styled = TRUE))

# Testing ------

  insert_msg('Testing')

  hads_cov$test <- c('hads' = 'hads_signs',
                     'cov' = 'cov') %>%
    map(~compare_variables(hads_cov$analysis_tbl,
                           split_factor = .x,
                           variables = hads_cov$variables,
                           what = 'test',
                           types = 't_test',
                           ci = FALSE,
                           pub_styled = TRUE))

# Plotting -------

  insert_msg('Plotting')

  hads_cov$plots <- list(stats = hads_cov$test,
                         split = c('hads_signs', 'cov'))  %>%
    pmap(function(stats, split) list(variable = stats$variable,
                                     plot_title = exchange(stats$variable,
                                                           dict = globals$stigma_lexicon,
                                                           key = 'variable',
                                                           value = 'base_label') %>%
                                       paste('SIMMUN', sep = ', '),
                                     y_lab = exchange(stats$variable,
                                                      dict = globals$stigma_lexicon,
                                                      key = 'variable',
                                                      value = 'axis_label'),
                                     plot_subtitle = stats$significance) %>%
           pmap(plot_variable,
                hads_cov$analysis_tbl,
                split_factor = split,
                type = 'box',
                point_hjitter = 0,
                cust_theme = globals$common_theme) %>%
           set_names(names(stats$variable)))

  ## fill colors and X axis labels with the n numbers

  hads_cov$plots$hads <- hads_cov$plots$hads %>%
    map(~.x +
          scale_x_discrete(labels = .x$labels$tag %>%
                             stri_split(fixed = '\n') %>%
                             map(stri_replace,
                                 fixed = ': ',
                                 replacement = '\n')) +
          scale_fill_manual(values = c('steelblue', 'gray60')))

  hads_cov$plots$cov <- hads_cov$plots$cov %>%
    map(~.x +
          scale_x_discrete(labels = .x$labels$tag %>%
                             stri_split(fixed = '\n') %>%
                             map(stri_replace,
                                 fixed = ': ',
                                 replacement = '\n')) +
          scale_fill_manual(values = c('steelblue', 'indianred3')))

# END ------

  insert_tail()
