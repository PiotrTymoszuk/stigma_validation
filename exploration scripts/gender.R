# Effects of gender on the STIGMA response variables investigated
# by two-tailed T test

  insert_head()

# container -----

  gender <- list()

# analysis globals ------

  insert_msg('Analysis globals')

  ## dependent variables with the best globals normality after transformation
  ## are included in the analysis (they will be used later in ANCOVA as well)

  gender$variables <- globals$stigma_lexicon %>%
    filter(response == 'yes',
           variable %in% distr$stigma_normality_best$variable) %>%
    .$variable

# descriptive stats -------

  insert_msg('Descriptive stats')

  ## normality of the variables. Look acceptable also after
  ## splitting by gender

  gender$normality <- explore(stigma$data,
                              split_factor = 'sex',
                              variables = gender$variables,
                              what = 'normality',
                              pub_styled = TRUE)

  ## mean, median and other stats

  gender$stats <- explore(stigma$data,
                          split_factor = 'sex',
                          variables = gender$variables,
                          what = 'table',
                          pub_styled = TRUE)

# Testing -----

  insert_msg('T test')

  ## two-tailed T test with Cohen's d effect size statistic

  gender$test <- compare_variables(stigma$data,
                                   variables = gender$variables,
                                   split_factor = 'sex',
                                   what = 'eff_size',
                                   types = 'cohen_d',
                                   ci = FALSE,
                                   pub_styled = TRUE)

# Box plots ----

  insert_msg('Box plots')

  gender$plots <-
    list(variable = gender$variables,
         plot_title = exchange(gender$variables,
                               dict = globals$stigma_lexicon,
                               key = 'variable',
                               value = 'base_label') %>%
           paste('STIGMA', sep = ', '),
         plot_subtitle = gender$test$significance,
         y_lab = exchange(gender$variables,
                          dict = globals$stigma_lexicon,
                          key = 'variable',
                          value = 'axis_label')) %>%
    pmap(plot_variable,
         stigma$data,
         split_factor = 'sex',
         type = 'box',
         point_hjitter = 0,
         cust_theme = globals$common_theme) %>%
    map(~.x +
          scale_fill_manual(values = c(female = 'indianred3',
                                       male = 'cornflowerblue')) +
          labs(tag = .x$labels$tag %>%
                 stri_replace(fixed = '\n',
                              replacement = ', ') %>%
                 paste0('\n', .))) %>%
    set_names(gender$variables)

# END ------

  insert_tail()
