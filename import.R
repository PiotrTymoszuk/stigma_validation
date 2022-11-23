# Import of the whole-transcriptome data. The expression data
# will be restricted to the genes with EntrezID available. Genes will be named
# with their symbols

# tools --------

  library(plyr)
  library(tidyverse)
  library(soucer)
  library(trafo)
  library(stringi)
  library(readxl)

  insert_head()

  source_all('./tools/tools.R',
             message = TRUE, crash = TRUE)

# Sourcing the import scripts for single studies ------

  insert_msg('Sourcing the import scripts')

  source_all('./import scripts/incov.R',
             message = TRUE, crash = TRUE)

# fetching the project globals ------

  insert_msg('Project globals')

  source_all('./tools/globals.R',
             message = TRUE, crash = TRUE)

# END -----

  insert_tail()
