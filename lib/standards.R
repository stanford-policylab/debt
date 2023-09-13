# Contains the column schema for the project, including post-processing checks
# that ensure that all results were processed correctly. The schema includes
# both input column types (e.g., `race_sex_ethnicity`) as well as output column
# types (e.g., raw_row_number).

################################### GLOBALS ####################################

VALID_RACES <- c(
  "asian/pacific islander",
  "american indian/alaska native",
  "black",
  "white",
  "other"
)

VALID_SEXES <- c(
  "female",
  "male",
  "other"
)

VALID_ETH <- c(
  "hispanic",
  "non_hispanic",
  "other"
)

SCHEMA <- tribble(
  # NOTE: An `NULL` datatype represents a column that should _not_ appear in
  # clean dataframes.
  ~type,                ~subtype,           ~data_type,                               ~public,

  # Names
  "name",               "full_name",        NULL,                                     NA,
  "name",               "first_name",       is_character,                             FALSE,
  "name",               "middle_name",      is_character,                             FALSE,
  "name",               "last_name",        is_character,                             FALSE,
  "name",               "suffix",           is_character,                             FALSE,

  # Demographic information
  "race",               "full_race",        NULL,                                     NA,
  "race",               "race",             Curry(is_factor, .levels = VALID_RACES),  TRUE,
  "race",               "ethnicity",        Curry(is_factor, .levels = VALID_ETH),    TRUE,
  "sex",                NA,                 Curry(is_factor, .levels = VALID_SEXES),  TRUE,
  "race_sex_ethnicity", NA,                 NULL,                                     NA,
  "age",                NA,                 is_integer,                               TRUE,
  "date",               "dob",              is.Date,                                  FALSE,
  
  # Address information
  "address",            "full_address",     NULL,                                     NA,
  "address",            "street",           is_character,                             FALSE,
  "address",            "city",             is_character,                             FALSE,
  "address",            "state",            is_character,                             TRUE,
  "address",            "zip",              is_character,                             TRUE,

  # Booking and release
  "date",               "booking_date",     is.Date,                                  TRUE,
  "time",               "booking_time",     is_time,                                  TRUE,
  "date",               "release_date",     is.Date,                                  TRUE,
  "time",               "release_time",     is_time,                                  TRUE,
  "datetime",           "booking_datetime", NULL,                                     NA,
  "datetime",           "release_datetime", NULL,                                     NA,
  "datetime",           "dob",              is.Date,                                  FALSE,
  
  # Charge information
  "charge",             NA,                 is_character,                             TRUE,
  "statute",            NA,                 is_character,                             TRUE,
  "severity",           NA,                 is_character,                             TRUE,
  "release_type",       NA,                 is_character,                             TRUE,
  "court",              NA,                 is_character,                             TRUE,
  "warrant_type",       NA,                 is_character,                             TRUE,
  
  # Amount information
  "bond",               NA,                 is_double,                                TRUE,
  "fine",               NA,                 is_double,                                TRUE,
  
  # Notes
  "note",               NA,                 is_character,                             TRUE,

  # Calculated features
  "length_of_stay",     NA,                 is_double,                                TRUE,
  "ftp",                NA,                 is_logical,                               TRUE,
  "all_ftp",            NA,                 is_logical,                               TRUE,
  "any_ftp",            NA,                 is_logical,                               TRUE,

  # Misc.
  "booking_id",         NA,                 is_character,                             TRUE,
  "raw_row_number",     NA,                 is_integer,                               TRUE
)
