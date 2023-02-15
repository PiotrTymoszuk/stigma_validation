

  insert_head()

# container -----

  stigma <- list()

# Reading the dataset from SPSS -----

  insert_msg('Reading the raw SPSS dataset')

  ## raw data for the selected variables

  stigma$raw <- read.spss('./data/local/01.06.2022.sav',
                          to.data.frame = TRUE)

  stigma$stress <- read.spss('./data/local/27.10.2021.sav',
                             to.data.frame = TRUE)

# Wrangling, NA handling calculation of the ratios ------

  insert_msg('Wrangling')

  ## NA handling: participants with missing explanatory factors and confounders
  ## are excluded

  ## stress, depression and persistent somatic symptoms

  stigma$stress <- stigma$stress %>%
    as_tibble %>%
    transmute(patient_id = stri_replace(studien_id,
                                        regex = '\\s+$',
                                        replacement = ''),
              phq_dpr_score = phq_results,
              log_phq_dpr_score = log(phq_dpr_score + 1),
              sqrt_phq_dpr_score = sqrt(phq_dpr_score),
              pss_stress_score = pss_4_results,
              log_pss_stress_score = log(pss_stress_score + 1),
              sqrt_pss_stress_score = sqrt(pss_stress_score),
              ## extraction of the A1 SCI items of persistent somatic symptoms
              pss_sleep_problems = a1_sci_1_0_item_001,
              pss_gastrointestinal = a1_sci_1_0_item_002,
              pss_lump_throat = a1_sci_1_0_item_003,
              pss_headache = a1_sci_1_0_item_004,
              pss_sorrow = a1_sci_1_0_item_006,
              pss_motivation_loss = a1_sci_1_0_item_007,
              pss_libido_loss = a1_sci_1_0_item_009,
              pss_ticks = a1_sci_1_0_item_011,
              pss_concentration_problems = a1_sci_1_0_item_012)

  ## recoding the PSS:: 3 and more in a 4 scale indicates a symptom
  ## calculating the PSS summ

  stigma$pss <- c('pss_sleep_problems',
                  'pss_gastrointestinal',
                  'pss_lump_throat',
                  'pss_headache',
                  'pss_sorrow',
                  'pss_motivation_loss',
                  'pss_libido_loss',
                  'pss_ticks',
                  'pss_concentration_problems')

  for(i in stigma$pss) {

    stigma$stress <- stigma$stress %>%
      mutate(!!i := as.numeric(.data[[i]]),
             !!i := ifelse(.data[[i]] >= 3, 'yes', 'no'),
             !!i := factor(.data[[i]], c('no', 'yes')))

  }

  stigma$stress$pss_sum <- stigma$stress[stigma$pss] %>%
    map(~as.numeric(.x) - 1) %>%
    reduce(`+`)

  stigma$stress <- stigma$stress %>%
    mutate(pss = ifelse(pss_sum > 0, 'PSS+', 'PSS-'),
           pss = factor(pss, c('PSS-', 'PSS+')))

  ## inflammation , neurotransmitter precursors, demography, CoV
  ## antibodies

  stigma$data <- stigma$raw %>%
    as_tibble %>%
    transmute(patient_id = stri_replace(studien_id,
                                        regex = '\\s+$',
                                        replacement = ''),
              age = Alter_real,
              log_age = log(age),
              sqrt_age = sqrt(age),
              sex = car::recode(geschlecht,
                                "'Männlich' = 'male';
                                'Weiblich' = 'female'"),
              sex = factor(sex, c('female', 'male')),
              hads_anx_score = hads_anxiety_results,
              hads_dpr_score = hads_depression_results,
              hads_dpr_score = ifelse(is.na(hads_dpr_score),
                                      hads_anx_score,
                                      hads_dpr_score),
              hads_signs = ifelse(hads_anx_score >= 8 |
                                    hads_dpr_score >= 8,
                                  'HADS+', 'HADS-'),
              hads_signs = factor(hads_signs, c('HADS-', 'HADS+')),
              cov = car::recode(covid_real,
                                "'COVID pos' = 'SARS-CoV-2';
                                'COVID neg' = 'healthy'"),
              covid_normal_ward = ifelse(is.na(soziodemographische_daten_024_3),
                                         'no',
                                         as.character(soziodemographische_daten_024_3)),
              covid_normal_ward = car::recode(covid_normal_ward,
                                              "'ja' = 'yes'; 'nein' = 'no'"),
              covid_icu = ifelse(is.na(soziodemographische_daten_024_4),
                                 'no',
                                 as.character(soziodemographische_daten_024_4)),
              covid_icu = car::recode(covid_icu,
                                      "'ja' = 'yes'; 'nein' = 'no'"),
              severity = ifelse(cov == 'healthy',
                                'healthy',
                                ifelse(covid_icu == 'yes',
                                       'severe-critical',
                                       ifelse(covid_normal_ward == 'yes',
                                              'moderate', 'mild'))),
              severity = factor(severity,
                                c('healthy', 'mild',
                                  'moderate', 'severe-critical')),
              psych_comorb = car::recode(psy_real,
                                         "'ja' = 'yes';
                                         'nein' = 'no'"),
              somatic_comorb = car::recode(aktuell_aktive_k_rperliche_erkrankungen,
                                           "'Ja' = 'yes'; 'nein' = 'no'"),
              study_group = interaction(cov, hads_signs),
              bmi_class = cut(bmi,
                              c(-Inf, 25, 30, Inf),
                              c('normal', 'overweight', 'obesity')),
              smoking = car::recode(nikotin, "'Ja' = 'yes'; 'Nein' = 'no'"),
              alcohol = car::recode(alkohol, "'Ja' = 'yes'; 'Nein' = 'no'"),
              new_psych_illness = ifelse(seit_welchem_jahr_besteht_psychiatr._hauptdiagnose__icd_. %in% c('2020', '2021'),
                                         'yes', 'no'),
              new_psych_illness = factor(new_psych_illness, c('no', 'yes')),
              neo = Neopterin,
              log_neo = log(neo),
              sqrt_neo = sqrt(neo),
              il6 = IL_6,
              log_il6 = log(il6),
              sqrt_il6 = sqrt(il6),
              crp = CRP,
              log_crp = log(crp),
              sqrt_crp = sqrt(crp),
              nlr = NLR,
              log_nlr = log(nlr),
              sqrt_nlr = sqrt(nlr),
              sii = SII,
              log_sii = log(sii),
              sqrt_sii = sqrt(sii),
              trp = Tryptophan,
              log_trp = log(trp),
              sqrt_trp = sqrt(trp),
              kyn = Kynurenin,
              log_kyn = log(kyn),
              sqrt_kyn = sqrt(kyn),
              phe = Phenylalanin,
              log_phe = log(phe),
              sqrt_phe = sqrt(phe),
              tyr = Tyrosin,
              log_tyr = log(tyr),
              sqrt_tyr = sqrt(tyr),
              kyn_trp = kyn/trp,
              log_kyn_trp = log(kyn/trp),
              sqrt_kyn_trp = sqrt(kyn/trp),
              phe_tyr = phe/tyr,
              log_phe_tyr = log(phe/tyr),
              sqrt_phe_tyr = sqrt(phe/tyr),
              no = Nitrit,
              log_no = log(no),
              sqrt_no = sqrt(no),
              neutro = Segmentkernige_Neutrophile,
              log_neutro = log(neutro),
              sqrt_neutro = sqrt(neutro),
              anti_rbd = RBDpanIgIndex,
              log_anti_rbd = log(anti_rbd),
              sqrt_anti_rbd = sqrt(anti_rbd))

  stigma$data <- stigma$data %>%
    filter(!is.na(hads_signs),
           !is.na(cov),
           !is.na(age),
           !is.na(sex))

  stigma$data <- left_join(stigma$data,
                           stigma$stress,
                           by = 'patient_id')

  ## restricting the data to the complete modeling information

  for(i in globals$stigma_lexicon$variable) {

    stigma$data <- stigma$data %>%
      filter(!is.na(.data[[i]]))

  }

# Manually extracted free text information on persisting symptoms --------

  insert_msg('COVID-19-related Persisting symptom information, self-reported')

  stigma$pers_cov_symptoms <-
    read_tsv('./data/local/persistent_symptoms_text.tsv') %>%
    transmute(patient_id = stri_replace(studien_id,
                                     regex = '\\s+$',
                                     replacement = ''),
              symptom_text = welche_beschwerden_haben_sie_aktuell_noch,
              psy_long_cov = mental,
              neuro_long_cov = neuro_smell_taste,
              fatigue_long_cov = fatigue,
              resp_long_cov = respiratory,
              cogn_long_cov = neurocognitive,
              long_cov = ifelse(is.na(symptom_text), 'no', 'yes'))

# END ------

  stigma <- stigma[c('data', 'pss', 'pers_cov_symptoms')]

  rm(i)

  insert_tail()
