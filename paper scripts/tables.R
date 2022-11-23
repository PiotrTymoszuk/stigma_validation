# manuscript tables

  insert_head()

# container -----

  tabs <- list()
  suppl_tabs <- list()

# Table 2: characteristic of the INCOV collective -------

  insert_msg('Table 2: characteristic of the CALHN collective')

  tabs$cohort <- cohort$feat_table %>%
    mdtable(label = 'table_2_incov_cohort',
            ref_name = 'cohort',
            caption = 'Characteristic of the external INCOV cohort.')

# Table S1: numbers of samples available per time point -----

  insert_msg('Table S1: samples per timepoint')

  suppl_tabs$samples <- cohort$samples %>%
    mdtable(label = 'table_s1_sample_number',
            ref_name = 'samples',
            caption = paste('Number of available samples and sampling',
                            'timepoints in the INCOV cohort.'))

# END ------

  insert_tail()
