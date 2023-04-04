#' Creates a version of the data in which the specified `id_field` is replaced by a hashed version.
hash_arc_number = function(data, id_field) {
  data %>%
    mutate(across(c({{ id_field }}), ~as.numeric(.))) %>%
    rowwise() %>%
    mutate(across(c({{ id_field }}), ~ifelse(is.na(.), NA, digest::sha1(.)))) %>%
    ungroup()
}


#' Load all REDCap instruments into a single large data frame
#'
#' @import dplyr
#' @import tidyr
#' @import REDCapR
#' @export
#' @param API_key The REDCap API token.
#' @param arc_number_field_name The name of the ARC # field in your REDCap project.
#' @param redcap_server Either `'redcapsurvey'` or `'redcap'`. Default: `'redcapsurvey'`.
#' @param raw_or_label Either `'raw'`, to keep the raw response values in multiple choice questions, or `'label'` to keep the corresponding labels. Default: `'raw'`.
#' @param keep_individual_instruments If `TRUE`, will also load each individual REDCap instrument into its own data frame in your R session. Default: `FALSE`.
#' @return A wide-format data frame containing data from all REDCap instruments.
load_redcap_instruments = function(API_key,
                                   arc_number_field_name,
                                   redcap_server = c('redcapsurvey', 'redcap'),
                                   raw_or_label = c('raw', 'label'),
                                   keep_individual_instruments = F,
                                   hide_arc_number = T) {
  redcap_server_choice = match.arg(redcap_server)
  redcap_server_url = paste0('https://',
                             redcap_server_choice,
                             '.niddk.nih.gov/redcap_v12.0.19/api/')

  # load REDCap data
  data = REDCapR::redcap_read(redcap_uri = redcap_server_url,
                              token = API_key,
                              raw_or_label = match.arg(raw_or_label),
                              export_survey_fields = T)$data %>%
    as_tibble() %>%
    select(!ends_with('_identifier'))

  # get name of ID field, under assumption it will be first field
  id_name = data %>%
    .[1] %>%
    names()

  # load REDCap metadata and store it in a hidden object
  # may be needed for future work with REDCap data
  metadata = REDCapR::redcap_metadata_read(redcap_uri = redcap_server_url,
                                           token = API_key)$data
  .GlobalEnv$.redcap_metadata = metadata

  # generate ID mapping
  .GlobalEnv$.redcap_cdw_id_mapping = data %>%
    distinct(.data[[id_name]], .data[[arc_number_field_name]]) %>%
    drop_na() %>%
    mutate(raw_arc_number = .data[[arc_number_field_name]]) %>%
    hash_arc_number(as.symbol(arc_number_field_name))

  # hash arc num if required
  if (hide_arc_number) {
    data = data %>% hash_arc_number(as.symbol(arc_number_field_name))
  }

  # load individual responses if required
  if (keep_individual_instruments) {
    load_individual_redcap_instruments(API_key,
                                       arc_number_field_name,
                                       complete_redcap_data = data,
                                       redcap_server = redcap_server_choice)
  }

  data
}


#' Load individiual REDCap instruments into the R environment
#'
#' This may load a large number of data frames into your R session. To clear these individual data frames, call `clear_individual_redcap_instruments()`.
#' @export
#' @param API_key The REDCap API token.
#' @param arc_number_field_name The name of the ARC # field in your REDCap project.
#' @param complete_redcap_data If you have already loaded the complete REDCap data using `load_redcap_instruments()`, you can make this function faster by providing that data frame here. This is not required.
#' @param redcap_server Either `'redcapsurvey'` or `'redcap'`. Default: `'redcapsurvey'`.
load_individual_redcap_instruments = function(API_key,
                                              arc_number_field_name,
                                              complete_redcap_data = NULL,
                                              redcap_server = c('redcapsurvey', 'redcap'),
                                              envir = NULL) {
  if (is.null(envir)) {
    envir = globalenv()
  }

  redcap_server_choice = match.arg(redcap_server)
  redcap_server_url = paste0('https://',
                             redcap_server_choice,
                             '.niddk.nih.gov/redcap_v12.0.19/api/')

  # load REDCap metadata and store it in a hidden object
  # may be needed for future work with REDCap data
  metadata = REDCapR::redcap_metadata_read(redcap_uri = redcap_server_url,
                                           token = API_key)$data
  .GlobalEnv$.redcap_metadata = metadata

  id_name = metadata %>%
    filter(row_number() == 1) %>%
    .$field_name

  if (is.null(complete_redcap_data)) {
    complete_redcap_data = load_redcap_instruments(API_key,
                                                   arc_number_field_name,
                                                   redcap_server = redcap_server_choice)
  }

  forms = metadata %>% distinct(form_name)
  for (form in forms$form_name) {
    instrument = complete_redcap_data %>%
      select(id, starts_with('redcap'), matches(paste0(form, '_timestamp')),
             any_of(metadata %>%
                      filter(form_name == form,
                             field_name != id_name) %>%
                      .$field_name))
    assign(form, instrument, envir = envir)
  }
}


#' Clears individual REDCap instrument data frames, if they are present.
#' @export
clear_individual_redcap_instruments = function() {
  if (exists('.redcap_metadata', envir = .GlobalEnv)) {
    .GlobalEnv$.redcap_metadata %>%
      distinct(form_name) %>%
      with(rm(list=.$form_name,
              envir = .GlobalEnv))
  }
}


#' Read CDW csv files and apply standard preparations.
#' Probably not needed by users.
#' @param filename The name of the file to read.
#' @param base_filename The file location.
#' @param hide_arc_number If `TRUE`, hashes the ARC #. Default: `TRUE`.
load_and_prep_cdw_data = function(filename,
                                  base_filename,
                                  protocol_has_wildcard = F,
                                  hide_arc_number = T) {
  data = read_csv(Sys.glob(paste0(base_filename, '*', filename, '.csv'))) %>%
    rename(id = `Arc#`)

  if (hide_arc_number) {
    data = data %>% hash_arc_number(id)
  }

  data
}


#' Load all CDW data from nightly dump files and combine into a single data frame.
#'
#' @export
#' @param protocol_names The names of the protocol to load. This is the first portion of the file name. You can specify multiple names in a vector to load and combine multiple files.
#' @param path The file path where the nightly dump files are stored.
#' @param keep_individual_instruments If `TRUE`, will also load each individual CDW file into its own data frame in your R session. Default: `TRUE`.
#' @param hide_arc_number If `TRUE`, hashes the ARC #. Default: `TRUE`.
#' @return The combined CDW data in a long-format data frame.
load_cdw_data = function(protocol_names,
                         path,
                         keep_individual_instruments = T,
                         hide_arc_number = T) {
  base_filename = file.path(path, protocol_names)

  questionnaires = load_and_prep_cdw_data('TestsandQuestionnaires', base_filename, hide_arc_number = hide_arc_number)
  vitals = load_and_prep_cdw_data('VitalSigns', base_filename, hide_arc_number = hide_arc_number)
  drugs = load_and_prep_cdw_data('DrugIntake', base_filename, hide_= hide_arc_number)
  bio_samples = load_and_prep_cdw_data('BiologicalSamples', base_filename, hide_= hide_arc_number)

  if (keep_individual_instruments) {
    .GlobalEnv$cdw_questionnaires = questionnaires
    .GlobalEnv$cdw_vitals = vitals
    .GlobalEnv$cdw_drugs = drugs
    .GlobalEnv$cdw_bio_samples = bio_samples
  }

  question_short_form = questionnaires %>%
    distinct(Question, QuestionID)

  all_cdw_data = questionnaires %>%
    select(id, TDesc, Date, QuestionID, Answer) %>%
    bind_rows(vitals %>%
                mutate(TDesc = 'Vitals') %>%
                select(id, TDesc, `Vital Signs`, `Date Entered`, Value) %>%
                rename(QuestionID = `Vital Signs`,
                       Date = `Date Entered`,
                       Answer = Value)) %>%
    bind_rows(drugs %>%
                mutate(TDesc = 'DrugsAdministered') %>%
                select(id, TDesc, Drug, `Date Administered`, Dosage) %>%
                rename(QuestionID = Drug,
                       Date = `Date Administered`,
                       Answer = Dosage)) %>%
    bind_rows(bio_samples %>%
                select(id, Lab, Observation, `Collection Date`, Value) %>%
                rename(TDesc = Lab,
                       QuestionID = Observation,
                       Date = `Collection Date`,
                       Answer = Value)) %>%
    rename(test = TDesc,
           date = Date,
           question = QuestionID,
           response = Answer) %>%
    mutate(response = ifelse(response == '&nbsp' | response == '&nbsp;', NA, response),
           date = strptime(date, '%m/%d/%Y %H:%M:%S %p'))

  all_cdw_data
}


#' Combine the REDCap and CDW data into a single long format data frame.
#' @export
#' @param instrument_env The environment containing individual REDCap instruments. Default: .InstrumentEnv
#' @return The fully merged REDCap and CDW data in long format.
combine_redcap_and_cdw_data = function(redcap_data, cdw_data) {
  if (!exists('.redcap_metadata', envir = globalenv())) {
    stop('Missing REDCap metadata. Please reload REDCap data. Do not remove hidden items in your global environment.')
  }

  redcap_metadata = .GlobalEnv$.redcap_metadata

  id_name = redcap_metadata %>%
    filter(row_number() == 1) %>%
    .$field_name

  # get REDCap data in long form
  redcap_instruments_long = tibble()
  forms = redcap_metadata %>% distinct(form_name)
  for (form in forms$form_name) {
    instrument = redcap_data %>%
      select(id, starts_with('redcap'), matches(paste0(form, '_timestamp')),
             any_of(redcap_metadata %>%
                      filter(form_name == form,
                             field_name != id_name) %>%
                      .$field_name))
    redcap_instruments_long = instrument %>%
      mutate(instrument_name = form,
             across(-c(ends_with('timestamp')), ~as.character(.))) %>%
      pivot_longer(-c({{ id_name }}, starts_with('redcap_'), instrument_name, matches(paste0(form, '_timestamp')))) %>%
      rename(any_of(c(date = paste0(form, '_timestamp')))) %>%
      bind_rows(redcap_instruments_long)
  }

  cdw_data %>%
    mutate(redcap_event_name = 'CDW') %>%
    rename(instrument_name = test,
           value = response) %>%
    bind_rows(redcap_instruments_long %>%
                rename(question = name)) %>%
    select(id, redcap_event_name, date, instrument_name, question, value)
}


#' Reveal the mapping between REDCap ID, CDW ARC #, and hashed (hidden) ARC #.
#'
#' Note that the mapping shown will be based on the most recent REDCap data loaded with `load_redcap_instruments()` or `load_individual_redcap_instruments()`.
#' @export
#' @return The ID mapping
redcap_cdw_id_mapping = function() {
  if (!exists('.redcap_cdw_id_mapping', envir = globalenv())) {
    stop('Missing REDCap metadata. Please reload REDCap data. Do not remove hidden items in your global environment.')
  }
  .GlobalEnv$.redcap_cdw_id_mapping
}
