# This script provides tools for project data analyses

# tools ----

  library(plyr)
  library(tidyverse)

# multiple testing -----

  re_adjust <- function(data, method = 'BH') {

    ## adjusts for multiple testing e.g. with the Benjamini-Hochberg method

    if(method != 'none') {

      data <- data %>%
        mutate(p_adjusted = p.adjust(p_value, method = method))

    }

    data %>%
      mutate(significance = ifelse(p_adjusted < 0.001,
                                   'p < 0.001',
                                   ifelse(p_adjusted >= 0.05,
                                          paste0('ns (p = ',
                                                 signif(p_adjusted, 2), ')'),
                                          paste('p =',
                                                signif(p_adjusted, 2)))))

  }

# Correlation bubble plot -------

  corr_buble <- function(data,
                         plot_title = NULL,
                         signif_only = TRUE) {

    ## Bubble plot with Spearman's correlation coefficients

    data <- data %>%
      mutate(significant = ifelse(p_adjusted < 0.05, 'signif', 'ns'))

    if(signif_only) {

      data <- data %>%
        mutate(estimate = ifelse(p_adjusted < 0.05, estimate, NA))

      if(all(is.na(data[['estimate']]))) {

        warning('No significant correlations to plot.', call. = FALSE)

        return(NULL)

      }

    }

    data %>%
      ggplot(aes(x = variable1,
                 y = variable2,
                 size = abs(estimate),
                 fill = estimate)) +
      geom_point(shape = 21) +
      geom_text(aes(label = signif(estimate, 2),
                    alpha = significant),
                size = 2.75,
                hjust = 0.5,
                vjust = -1.4) +
      scale_alpha_manual(values = c(ns = 0.15,
                                    signif = 1),
                         labels = c(ns = 'ns',
                                    signif = 'p < 0.05')) +
      scale_size(limits = c(-1, 1),
                 range = c(0, 5)) +
      scale_fill_gradient2(low = 'steelblue',
                           mid = 'white',
                           high = 'firebrick',
                           limits = c(-1, 1),
                           name = expression(rho)) +
      scale_x_discrete(labels = set_names(globals$incov_lexicon$label,
                                          globals$incov_lexicon$variable)) +
      scale_y_discrete(labels = set_names(globals$incov_lexicon$label,
                                          globals$incov_lexicon$variable)) +
      guides(size = 'none',
             alpha = 'none') +
      globals$common_theme +
      theme(axis.title = element_blank(),
            axis.text.x = element_text(hjust = 1, angle = 90)) +
      labs(title = plot_title,
           subtitle = paste('Spearman correlation,  n =', data$n[1]))

  }

# displaying post-hoc testing results: one-way plots ------

  add_p_one <- function(plot,
                        test,
                        txt_size = 2.5,
                        line_offset = 0.055,
                        txt_vjust = -0.4,
                        offset_factor = 2.2) {

    ## adds significant pairwise tests
    ## between healthy controls and times after CoV

    test <- test %>%
      mutate(plot_cap = ifelse(p.adj > 0.1,
                               'ns',
                               ifelse(p.adj >= 0.05,
                                      paste0('ns (p = ', signif(p.adj, 2), ')'),
                                      paste('p =', signif(p.adj, 2)))))

    y_diff <- diff(range(plot$data$variable))

    ## healthy - acute comparison

    y_tbl <- plot$data %>%
      filter(group %in% c('healthy', 'acute'))

    y_acute<- max(y_tbl$variable) +
      y_diff * line_offset

    if(test$plot_cap[1] != 'ns') {

      plot <- plot +
        annotate('segment',
                 x = 1,
                 xend = 2,
                 y = y_acute,
                 yend = y_acute,
                 size = 0.5) +
        annotate('text',
                 label = test$plot_cap[1],
                 x = 1.5,
                 y = y_acute,
                 vjust = txt_vjust,
                 size = txt_size)

    } else {

      y_acute <- -Inf

    }

    ## healthy - sub-acute comparison

    y_tbl <- plot$data %>%
      filter(group %in% c('healthy', 'sub-acute'))

    y_sub<- max(y_tbl$variable) +
      y_diff * line_offset

    if(test$plot_cap[2] != 'ns') {

      if(y_sub < y_acute + y_diff * line_offset * offset_factor) {

        y_sub <-  y_acute + y_diff* line_offset * offset_factor

      }

      plot <- plot +
        annotate('segment',
                 x = 1,
                 xend = 3,
                 y = y_sub,
                 yend = y_sub,
                 size = 0.5) +
        annotate('text',
                 label = test$plot_cap[2],
                 x = 2,
                 y = y_sub,
                 vjust = txt_vjust,
                 size = txt_size)

    } else {

      y_sub <- -Inf

    }

    ## healthy - recovery comparison

    y_tbl <- plot$data %>%
      filter(group %in% c('healthy', 'recovery'))

    y_reco <- max(y_tbl$variable) +
      y_diff * line_offset

    y_max <- max(y_acute, y_sub)

    if(test$plot_cap[3] != 'ns') {

      if(y_reco < y_max + y_diff * line_offset * offset_factor) {

        y_reco <-  y_max + y_diff * line_offset * offset_factor

      }

      plot <- plot +
        annotate('segment',
                 x = 1,
                 xend = 4,
                 y = y_reco,
                 yend = y_reco,
                 size = 0.5) +
        annotate('text',
                 label = test$plot_cap[3],
                 x = 2.5,
                 y = y_reco,
                 vjust = txt_vjust,
                 size = txt_size)

    }

    return(plot)

  }

  add_p_chain <- function(plot,
                          test,
                          txt_size = 2.5,
                          line_offset = 0.055,
                          txt_vjust = -0.4) {

    ## adds significant pairwise tests in a chain-wise manner
    ## group1 - group2, group2 - group3 etc.

    test <- test %>%
      mutate(plot_cap = ifelse(p.adj > 0.1,
                               'ns',
                               ifelse(p.adj >= 0.05,
                                      paste0('ns (p = ', signif(p.adj, 2), ')'),
                                      paste('p =', signif(p.adj, 2)))))

    plot_var <- test[[1]][1]

    for(i in 1:nrow(test)) {

      if(test$plot_cap[i] == 'ns') {

        next

      }

      y_tbl <- plot$data %>%
        filter(group %in% c(test$group1[[i]],
                            test$group2[[i]]))

      y_line <- max(y_tbl$variable) +
        diff(range(y_tbl$variable)) * line_offset

      plot <- plot +
        annotate('segment',
                 x = i + 0.05,
                 xend = i + 1 - 0.05,
                 y = y_line,
                 yend = y_line,
                 size = 0.5) +
        annotate('text',
                 label = test$plot_cap[[i]],
                 x = i + 0.5,
                 y = y_line,
                 vjust = txt_vjust,
                 size = txt_size)


    }

    return(plot)

  }

# END ------
