# Renders the manuscript and the supplementary material

insert_head()

# supplementary material ------

  insert_msg('Rendering the supplements')

  render('./paper/markdown/supplementary_material.Rmd',
         output_format = word_document2(number_sections = FALSE,
                                        reference_docx = 'ms_template.docx'),
         output_dir = './paper')

# paper -----

  insert_msg('Rendering the paper')

  render('./paper/markdown/methods_figures_tables.Rmd',
         output_format = word_document2(number_sections = FALSE,
                                        reference_docx = 'ms_template.docx'),
         output_dir = './paper')

# END -----

insert_tail()
