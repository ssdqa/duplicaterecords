## code to prepare `DATASET` dataset goes here

dr_input_file_omop <- dplyr::tibble('definition_alias' = c('duplicate visits per day',
                                                           'conditions without PK'),
                                     'domain_tbl' = c('visit_occurrence',
                                                      'condition_occurrence'),
                                     'duplicate_columns' = c('visit_concept_id,visit_start_date,visit_end_date',
                                                             'condition_occurrence_id'),
                                     'include_exclude' = c('include',
                                                           'exclude'),
                                     'concept_field' = c(NA, 'condition_concept_id'),
                                     'date_field' = c('visit_start_date',
                                                      'condition_start_date'),
                                     'codeset_name' = c(NA, 'dx_hypertension'),
                                     'filter_logic' = c(NA, NA))

usethis::use_data(dr_input_file_omop, overwrite = TRUE)


dr_input_file_pcornet <- dplyr::tibble('definition_alias' = c('labs per visit + date',
                                                              'procedures without PK'),
                                       'domain_tbl' = c('lab_result_cm',
                                                        'procedures'),
                                       'duplicate_columns' = c('encounterid,result_date,lab_loinc',
                                                               'proceduresid'),
                                       'include_exclude' = c('include',
                                                             'exclude'),
                                       'concept_field' = c(NA, 'px'),
                                       'date_field' = c('result_date',
                                                        'px_date'),
                                       'codeset_name' = c(NA, 'px_mri'),
                                       'filter_logic' = c(NA, NA),
                                       'vocabulary_field' = c(NA, 'px_type'))

usethis::use_data(dr_input_file_pcornet, overwrite = TRUE)
