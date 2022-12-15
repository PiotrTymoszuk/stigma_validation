# Modeling of the levels of aminoacid precursors of neurotransmitters,
# they decay products and ratios with multi-parameter OLS regression
# with backwards elimination
#
# Explanatory variables are: neopterin as a representative inflammatory marker,
# signs of depression/anxiety as defined by HADS, CoV status, age
# (it correlates with multiple metabolite levels) and gender
# (correlation with some parameters)

  insert_head()

# container -----

  stigma_lm <- list()

# Analysis globals ------

  insert_msg('Analysis globals')

  ## response variables transformed to assure the best normality and EOV
  ## in respect to CoV and HADS strata

  stigma_lm$responses <- distr$stigma_normality_best %>%
    filter(base_variable %in% c('trp', 'kyn', 'kyn_trp',
                                'phe', 'tyr', 'phe_tyr')) %>%
    mutate(base_variable = factor(base_variable,
                                  c('trp', 'kyn', 'kyn_trp',
                                    'phe', 'tyr', 'phe_tyr'))) %>%
    arrange(base_variable)

  stigma_lm$responses <-
    set_names(as.character(stigma_lm$responses$variable),
              as.character(stigma_lm$responses$base_variable))

  ## explanatory variables and their labels to be presented in the plots

  stigma_lm$variables <-
    c('hads_signs', 'infection', 'log_neo', 'age', 'sex')

  stigma_lm$var_labs <-
    c('hads_signs' = 'DPR/ANX',
      'infection' = 'SARS-CoV-2',
      'log_neo' = 'log NEO, nmol/L',
      'age' = 'Age, per decade',
      'sex' = 'Sex')

  ## analysis tables with each of the responses, explanatory variables
  ## and complete cases only. NEO transformed with log to improve EOV
  ## and normality. Scaling age to decades.
  ## Re-coding the infection status as CoV recovery

  stigma_lm$analysis_tbl <- stigma$data %>%
    mutate(infection = car::recode(cov,
                                   "'healthy' = 'uninfected';
                             'SARS-CoV-2' = 'recovery'"),
           infection = factor(infection,
                              c('uninfected', 'recovery')))

  stigma_lm$analysis_tbl <- stigma_lm$responses %>%
    map(~stigma_lm$analysis_tbl[c('patient_id', .x, stigma_lm$variables)]) %>%
    map(~filter(.x, complete.cases(.x))) %>%
    map(mutate,
        age = age/10) %>%
    map(column_to_rownames, 'patient_id')

  ## full and null model formulas

  stigma_lm$full_formulas <- stigma_lm$responses %>%
    map(~paste(.x, paste(stigma_lm$variables, collapse = '+'), sep = ' ~ ')) %>%
    map(as.formula)

  stigma_lm$null_formulas <- stigma_lm$responses %>%
    map(~paste(.x, '~ 1')) %>%
    map(as.formula)

# Total numbers of cases ------

  insert_msg('Total observation numbers')

  stigma_lm$n_numbers <- stigma_lm$analysis_tbl %>%
    map_dbl(nrow)

# Construction of the null and full models -------

  insert_msg('construction of the null and full models')

  stigma_lm$null_models <- list(data = stigma_lm$analysis_tbl,
                                formula = stigma_lm$null_formulas) %>%
    pmap(make_lm,
         mod_fun = lm,
         family = NULL)

  stigma_lm$full_models <- list(data = stigma_lm$analysis_tbl,
                                formula = stigma_lm$full_formulas) %>%
    pmap(make_lm,
         mod_fun = lm,
         family = NULL)

# Backward elimination via step() -----

  insert_msg('Backwards elimination')

  stigma_lm$step_models <- stigma_lm$full_models %>%
    map(step, step_fun = MASS::stepAIC, direction = 'backward')

# Checking assumptions of the optimized models -----

  insert_msg('Assumptions of the optimized models')

  ## normality and EOV of the residuals

  stigma_lm$assumptions <- stigma_lm$step_models %>%
    map(summary,
        type = 'assumptions',
        type.predict = 'response')

  ## diagnostic plots of the residuals

  stigma_lm$resid_plots <- stigma_lm$step_models %>%
    map(plot,
        type = 'residuals',
        cust_theme = globals$common_theme,
        type.predict = 'response')

  ## response - explanatory variable relationship

  stigma_lm$relation_plots <- stigma_lm$step_models %>%
    map(plot,
        type = 'relationship',
        cust_theme = globals$common_theme,
        type.predict = 'response')

# LRT versus the null model ------

  insert_msg('LRT versus the null model')

  stigma_lm$lrt <- map2(stigma_lm$step_models,
                        stigma_lm$null_models,
                        ~anova(.x$model, .y$model))

  ## LRT summaries

  stigma_lm$lrt_summary <- stigma_lm$lrt %>%
    map(as.data.frame) %>%
    map(~.x[2, ]) %>%
    compress(names_to = 'response') %>%
    mutate(significance = ifelse(`Pr(>F)` < 0.05,
                                 paste('p =', signif(`Pr(>F)`, 2)),
                                 paste0('ns (p = ', signif(`Pr(>F)`, 2), ')')),
           plot_cap = paste0('F(', abs(Df), ', ',
                             Res.Df, ') = ', signif(`F`, 2)),
           plot_cap = paste(plot_cap, significance, sep = ', '),
           plot_cap = paste(plot_cap, Res.Df + 1, sep = ', n = ')) %>%
    as_tibble

# Fit goodness and errors ------

  insert_msg('Fit goodness and errors')

  stigma_lm$fit_stats <- stigma_lm$step_models %>%
    map_dfr(summary,
            type = 'fit')

# Cross-validation with caret ------

  insert_msg('Cross-validation')

  stigma_lm$step_formulas <- stigma_lm$step_models %>%
    map(formula)

  set.seed(123456)

  registerDoParallel(cores = 7)

  stigma_lm$caret_models <-
    map2(stigma_lm$step_formulas,
         stigma_lm$analysis_tbl,
         ~train(form = .x,
                data = .y,
                method = 'lm',
                metric = 'RMSE',
                trControl = trainControl(method = 'repeatedcv',
                                         number = 10,
                                         repeats = 50,
                                         savePredictions = 'final',
                                         returnData = TRUE,
                                         returnResamp = 'final')))

  stopImplicitCluster()

  stigma_lm$caret_models <- stigma_lm$caret_models %>%
    map(as_caretx)

# cross validation errors and R-squared ------

  insert_msg('Cross-validation fit stats')

  plan('multisession')

  stigma_lm$cv_stats <- stigma_lm$caret_models %>%
    future_map(summary.caretx,
               .options = furrr_options(seed = TRUE))

  plan('sequential')

  ## CV stats summary

  stigma_lm$cv_stats <- stigma_lm$cv_stats %>%
    map(~map(.x, ~.x[c(3, 4), c('statistic', 'estimate')]) %>%
          compress(names_to = 'data_type')) %>%
    compress(names_to = 'response') %>%
    mutate(statistic = factor(statistic, c('RMSE', 'rsq')),
           data_type = factor(data_type, c('train', 'cv')))

# Plots with the R-squares and RMSE for train and CV data ------

  insert_msg('Plots of the train and CV fit stats')

  stigma_lm$fit_stat_plots[c('RMSE', 'rsq')] <-
    list(data = dlply(stigma_lm$cv_stats, 'statistic'),
         title = c('Fit error, STIGMA', 'Explained variance, STIGMA'),
         x_lab = list('RMSE', expression('R'^2))) %>%
    pmap(function(data, title, x_lab) data %>%
           ggplot(aes(x = estimate,
                      y = response,
                      fill = data_type)) +
           geom_bar(stat = 'identity',
                    position = position_dodge(0.9),
                    color = 'black') +
          # geom_text(aes(label = signif(estimate, 2)),
           #          size = 2.5,
            #         color = 'white',
             #        hjust = 1.3,
              #       position = position_dodge(0.9)) +
           scale_y_discrete(limits = rev(names(stigma_lm$responses)),
                            labels = exchange(names(stigma_lm$responses),
                                              dict = globals$stigma_lexicon,
                                              key = 'variable',
                                              value = 'label')) +
           scale_fill_manual(values = c(train = 'darkolivegreen4',
                                        cv = 'steelblue3'),
                             labels = c(train = 'training',
                                        cv = 'CV'),
                             name = '') +
           globals$common_theme +
           theme(axis.title.y = element_blank()) +
           labs(title = title,
                x = x_lab,
                subtitle = paste('Multi-parameter linear modeling,',
                                 'backwards elimination')))

# Inference and Forest plots of the model betas -------

  insert_msg('Model inference and Forest plots of the betas')

  ## model estimates

  stigma_lm$inference <- stigma_lm$step_models %>%
    map(summary, type = 'inference') %>%
    map(mutate,
        var_lab = stigma_lm$var_labs[variable],
        level = ifelse(level == 'CoV recovery', '', level))

  ## model forest plots: no valid model present for log_phe (intercept only)
  ## hence working with safely()

  stigma_lm$forest_plots <-
    list(x = stigma_lm$inference %>%
           map(filter, p_value < 0.05),
         plot_title = exchange(stigma_lm$responses,
                               dict = globals$stigma_lexicon,
                               key = 'variable',
                               value = 'base_label') %>%
           paste('STIGMA', sep = ', '),
         x_lab = exchange(stigma_lm$responses,
                          dict = globals$stigma_lexicon,
                          key = 'variable',
                          value = 'axis_label'),
         plot_subtitle = stigma_lm$lrt_summary$plot_cap) %>%
    pmap(safely(plot_forest),
         variable = 'var_lab',
         hide_baseline = TRUE,
         estimate_size = 2.5,
         cust_theme = globals$common_theme) %>%
    map(~.x$result) %>%
    compact

# Appending the models with the baseline values ------

  insert_msg('Baseline values')

  stigma_lm$intercepts <- stigma_lm$inference[names(stigma_lm$forest_plots)] %>%
    map_dfr(filter,
            level == 'baseline') %>%
    mutate(baseline_lab = paste0('baseline: ', signif(estimate, 2))) %>%
    .$baseline_lab

  stigma_lm$forest_plots <-
    map2(stigma_lm$forest_plots,
         stigma_lm$intercepts,
         ~.x +
           labs(x = paste(.x$labels$x,
                          .y,
                          sep = ', ')))

# END ------

  insert_tail()
