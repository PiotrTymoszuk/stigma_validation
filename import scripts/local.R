# Imports data for the local cohort

  insert_head()

# container -----

  stigma <- list()

# Reading the dataset from SPSS -----

  insert_msg('Reading the raw SPSS dataset')

  ## raw data for the selected variables

  stigma$raw <- read.spss('./data/local/01.06.2022.sav', to.data.frame = TRUE)

# Wrangling, NA handling calculation of the ratios ------

  insert_msg('Wrangling')

  ## NA handling: participants with missing explanatory factors and confounders
  ## are excluded

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
              pss = car::recode(persistierende_symptome_2,
                                "'keine oder covid neg' = 'PSS-';
                                'persistierende Symptome' = 'PSS+'"),
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
              sqrt_neutro = sqrt(neutro))

  stigma$data <- stigma$data %>%
    filter(!is.na(hads_signs),
           !is.na(cov),
           !is.na(age),
           !is.na(sex))

# Adding the manually extracted free text information on persisting symptoms --------

  insert_msg('Persisting symptom information')

  stigma$pers_symptoms <-
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

  stigma$data <- left_join(stigma$data,
                           stigma$pers_symptoms[c('patient_id',
                                                  'psy_long_cov',
                                                  'neuro_long_cov',
                                                  'fatigue_long_cov',
                                                  'resp_long_cov',
                                                  'cogn_long_cov',
                                                  'long_cov')],
                           by = 'patient_id')

# Restricting the persistent symptom indexes to the Covid-19 individuals only -----

  insert_msg('Persistent symptoms only for COVID-19')

  ## as defined by the 'covid_status' variable

  stigma$data <- stigma$data %>%
    mutate(long_cov = ifelse(cov == 'healthy',
                             'healthy',
                             ifelse(long_cov == 'yes',
                                    'yes', 'no')),
           long_cov = factor(long_cov, c('healthy', 'no', 'yes')),
           psy_long_cov = ifelse(cov == 'healthy',
                                 'healthy',
                                 ifelse(long_cov == 'no',
                                        'recovered',
                                        psy_long_cov)),
           psy_long_cov = factor(psy_long_cov,
                                 c('healthy', 'recovered', 'no', 'yes')),
           neuro_long_cov = ifelse(cov == 'healthy',
                                   'healthy',
                                   ifelse(long_cov == 'no',
                                          'recovered',
                                          neuro_long_cov)),
           neuro_long_cov = factor(neuro_long_cov,
                                   c('healthy', 'recovered', 'no', 'yes')),
           fatigue_long_cov = ifelse(cov == 'healthy',
                                     'healthy',
                                     ifelse(long_cov == 'no',
                                            'recovered',
                                            fatigue_long_cov)),
           fatigue_long_cov = factor(fatigue_long_cov,
                                     c('healthy', 'recovered', 'no', 'yes')),
           resp_long_cov = ifelse(cov == 'healthy',
                                  'healthy',
                                  ifelse(long_cov == 'no',
                                         'recovered',
                                         resp_long_cov)),
           resp_long_cov = factor(resp_long_cov,
                                  c('healthy', 'recovered', 'no', 'yes')),
           cogn_long_cov = ifelse(cov == 'healthy',
                                  'healthy',
                                  ifelse(long_cov == 'no',
                                         'recovered',
                                         cogn_long_cov)),
           cogn_long_cov = factor(cogn_long_cov,
                                  c('healthy', 'recovered', 'no', 'yes')))

# END ------

  insert_tail()
