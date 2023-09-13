# Functions for automatically calculating certain features, such as age, length
# of stay, etc.

# Load in the required libraries
library(rprojroot)
library(fs)
ROOT <- path(find_root(has_file(".gitignore")))
source(path(ROOT, "lib", "common.R"))

################################### GLOBALS ####################################
SEARCH_COLS <- c("charge", "statute", "warrant_type", "note", "severity",
                 "release_type")
ZIPS_TO_FIPS <- read_csv(
    path(ROOT, "data", "zips_to_fips.csv"),
    col_types = cols(.default = "c")
  ) %>%
  # NOTE: About 25% of zip codes span two counties and about 5% span more than
  # two counties. This shouldn't effect demographic estimates too much, so we
  # simply take the first matching county.
  distinct(zip, .keep_all = TRUE)
CENSUS_DATA <- read_rds(path(ROOT, "data", "census_data.rds"))
FTP_REGEX <- c(
  "tx" = glue(
    "CPR?F|CAP(IAS)? ?PRO ?(FINE)?",
    "FTP|FAIL(URE)? ?(TO)? ?PAY|UNPAID",
    "(LAY|L(AI)?D|PAY|P(AI)?D) ?(OUT)? (FINE|FEE)",
    "(FINE|FEE)S? (PAY|P(AI)?D)",
    "DAYS? TO PAY|D2P",
    "PAY ?OR ?STAY",
    .sep = "|"
  ),
  "wi" = glue(
    "(?<!FDL )COMM(?!ENT)(MIT(MENT)?)?",
    "FTP|FAIL(URE)? ?(TO)? ?PAY",
    "(LAY|LAID|PAY|P(AI)?D) ?(OUT)? (FINE|FEE)",
    "(FINE|FEE)S? (PAY|P(AI)?D)",
    "DAYS? TO PAY|D2P",
    "PAY.{{0,8}}OR.{{0,5}}DAYS?",
    "MUNI(CIPAL)?|MJOC",
    "ORDINANCE",
    "PAY ?OR ?STAY",
    .sep = "|"
  ),
  "de" = "FTP|FAIL(URE)? ?(TO)? ?PAY",
  "ky" = glue(
    "FTP|FAIL(URE)? ?(TO)? PAY",
    "NON(-| )?PAYMENT ?(OF ?)?(FINE|FEE|COURT)",
    .sep = "|"
  ),
  "mo" = glue(
    "FTP|FAIL(URE)? ?(TO)? PAY",
    "560\\.?031",
    .sep = "|"
  ),
  "la" = glue(
    "FTP|FAIL(URE)? ?(TO)? PAY",
    .sep = "|"
  )
)

################################### IMPUTE #####################################
# These functions do the high-level work of imputing features. These functions
# take a single object `d` as an argument, which should be a named list with the
# following slots: `data`, a tibble; and `metadata`, a list to which parsing
# metadata can be appended. In addition, `impute` and `impute_ftp` take a
# `state` argument, which is used to dispatch the proper state-specific
# imputation function.

impute <- function(d, state, county) {
  # Top level function for imputing features.

  # Extract the old data and metadata.
  metadata <- d$metadata
  data <- d$data

  # Bundle the data and metadata.
  d <- list(data = data, metadata = list()) 

  # Validate the input.
  validate_input(d)

  # Pass through the pipeline
  d %<>%
    impute_length_of_stay() %>%
    impute_age() %>%
    impute_ethnicity(state, county) %>%
    impute_ftp(state)

  # Record metadata
  metadata[["impute"]] <- list(
    result = select(d$data, starts_with("impute")),
    subsidiary_functions = d$metadata
  )

  # Return
  list(
    data = d$data,
    metadata = metadata
  )
}


impute_length_of_stay <- function(d) {
  # Imputes a length of stay using the booking and release dates and times, if
  # available.

  # If booking and release dates are not available, return early.
  if (! all(c("booking_date", "release_date") %in% colnames(d$data))) {
    return(d)
  }

  # If times are available, include those in the calculation.
  if (all(c("booking_time", "release_time") %in% colnames(d$data))) {
    d$data %<>%
      mutate(
        length_of_stay = make_datetime(
          year = year(booking_date),
          month = month(booking_date),
          day(booking_date),
          hour = hour(booking_time),
          min = minute(booking_time),
          sec = second(booking_time)
        ) %--% make_datetime(
          year = year(release_date),
          month = month(release_date),
          day(release_date),
          hour = hour(release_time),
          min = minute(release_time),
          sec = second(release_time)
        ) / ddays(1)
      )
  } else {
    d$data %<>%
      mutate(length_of_stay = booking_date %--% release_date / ddays(1))
  }

  # Return
  d
}

impute_age <- function(d) {
  # Imputes age using date of birth and booking date.
  
  # If booking dates are not available, return early.
  if (! "booking_date" %in% colnames(d$data)) {
    return(d)
  }

  # If DOB is available, use that to impute age directly
  if ("dob" %in% colnames(d$data)) {
    d$data %<>%
      mutate(imputed_age = as.integer(floor(dob %--% booking_date / dyears(1))))
  # Otherwise, regress age against booking date to make sure the average isn't
  # growing by 1 year per year. If it is, then subtract the relevant number of
  # years from age.
  # NOTE: This occurs when DOB is not given and age is incorrectly imputed in
  # the raw data using only the earliest or last year of booking data given.
  } else if ("age" %in% colnames(d$data)) {
    age_model <- lm(
      formula = age ~ year,
      data = mutate(d$data, year = year(booking_date))
    )
    beta_year <- tidy(age_model) %>%
      filter(term == "year") %>%
      pull(estimate)

    if (abs(beta_year) > 0.5) {
      d$data %<>%
        mutate(
          y = year(booking_date),
          offset = if_else(rep_along(y, beta_year < 0), max(y) - y, y - min(y)),
          imputed_age = age - offset,
          imputed_age = as.integer(round(imputed_age))
        ) %>%
        select(-y, -offset)
    } else {
      d$data %<>%
        mutate(imputed_age = age)
    }

    # Record the model
    d$metadata[["impute_age"]][["model"]] <- tidy(age_model)
  # If neither age nor dob is available, there's nothing we can do.
  } else {
    return(d)
  }

  # If age already exists, compare age and imputed age.
  if (all(c("age", "imputed_age") %in% colnames(d$data))) {
    age_cols <- d$data %>%
      select(age, imputed_age) 
    d$metadata[["impute_age"]] <- list(
      cols = age_cols,
      stats = age_cols %>%
        summarize(
          mean_diff = mean(age - imputed_age, na.rm = TRUE),
          mean_sd = mean(age - imputed_age, na.rm = TRUE),
          pct_recovered = mean(are_na(age) & !are_na(imputed_age))
        )
    )

    # Replace `age` with imputed age.
    d$data %<>%
      select(-age) %>%
      rename(age = imputed_age)
  } else {
    d$data %<>%
      rename(age = imputed_age)
  }

  # Return
  d
}

impute_ethnicity <- function(d, geo_state, geo_county) {
  # Imputes ethnicity using census data on the percentage of individuals with a
  # given surname who are Hispanic.
  
  # If `last_name` is not present, return early.
  if (! "last_name" %in% colnames(d$data)) {
    return(d)
  }

  # If `geo_county == "statewide"` (and we don't have zip or state), then we
  # have to simply run the analysis using the 
  # not individual counties.
  has_county <- geo_county != "statewide"
  if (! has_county) {
    default_county_code <- "statewide"
  } else {
    # Use the state and county of the jail as defaults; otherwise, use zip code to
    # impute.
    default_county_code <- fips_codes %>%
      as_tibble() %>%
      mutate(
        state = str_to_lower(state),
        county = str_to_lower(str_replace_all(county, "[\\s.]+", "_"))
      ) %>%
      filter(state == geo_state, county == geo_county) %>%
      pull(county_code)
  }

  # Check for zip, age, and sex
  has_zip <- "zip" %in% colnames(d$data)
  has_age <- "age" %in% colnames(d$data)
  has_sex <- "sex" %in% colnames(d$data)

  # We can't rely on raw_row_number being unique at this stage of the process,
  # so we have to add in a new index.
  d$data %<>% mutate(temp_index = row_number())

  # Add dummy columns if any of age, sex, and zip is not present
  if (! has_zip) {
    d$data %<>% mutate(zip = na_chr)
  }
  if (! has_age) {
    d$data %<>% mutate(age = na_int)
  }
  if (! has_sex) {
    d$data %<>% mutate(sex = na_chr, sex = factor(sex, levels = VALID_SEXES))
  }

  # Create a tibble with the available (relevant) name and demographic
  # information.
  imputation <- d$data %>%
    select(surname = last_name, zip, age, sex, temp_index) %>%
    left_join(ZIPS_TO_FIPS, by = "zip") %>%
    mutate(
      county = if_else(! are_na(zip), county_code, default_county_code),
      state = if_else(! are_na(zip), state_abbrev, geo_state),
      sex = if_else(sex == "male", 1, 0, na_dbl)
    ) %>%
    # NOTE: The `wru` package can't handle missing states and counties, so we
    # have to filter them here.
    filter(!are_na(state), !are_na(county)) %>%
    # NOTE: If there are postal codes that are not one of the fifty states or
    # DC, this will break `wru`.
    filter(state %in% str_to_lower(state.abb)) %>%
    # NOTE: The `wru` package cannot robustly handle missingness in sex and age,
    # so we have to split the dataframe into pieces, and process each one
    # separately.
    mutate(age_present = !are_na(age), sex_present = !are_na(sex)) %>%
    group_by(age_present, sex_present) %>%
    group_modify(~ {
      if (has_county) {
        pred <- quietly(predict_race)(
            voter.file = .x,
            census.geo = "county",
            census.data = filter(
                CENSUS_DATA,
                has_age == .y$age_present,
                has_sex == .y$sex_present
              ) %>%
              pull(census_data) %>%
              first(),
            age = .y$age_present,
            sex = .y$sex_present
          )
      } else {
        # Only surname can be used if county-level information isn't available.
        pred <- quietly(predict_race)(
            voter.file = .x,
            surname.only = TRUE
          )
      }
      pred %>%
        extract2("result") %>%
        as_tibble()
    }) %>%
    ungroup() %>%
    # Impute "hispanic" if it is the most likely possibility.
    mutate(
      last_name = surname,
      imputed_ethnicity = if_else(
        condition = (
          pred.his == pmax(pred.whi, pred.bla, pred.his, pred.asi, pred.oth)
        ),
        true = "hispanic",
        false = "non_hispanic"
      ),
      imputed_ethnicity = factor(imputed_ethnicity, levels = VALID_ETH)
    )

  # Record the metadata
  d$metadata[["impute_ethnicity"]][["predictions"]] <- imputation

  # Remove the dummy columns
  if (! has_zip) {
    d$data %<>% select(-zip)
  }
  if (! has_age) {
    d$data %<>% select(-age)
  }
  if (! has_sex) {
    d$data %<>% select(-sex)
  }

  # Bind the imputations to data
  d$data %<>%
    left_join(
      select(imputation, imputed_ethnicity, temp_index),
      by = "temp_index"
    ) %>%
    arrange(temp_index) %>%
    select(-temp_index)


  # If ethnicity already exists, compare raw and imputed ethnicity.
  if (all(c("ethnicity", "imputed_ethnicity") %in% colnames(d$data))) {
    eth_cols <- d$data %>%
      select(ethnicity, imputed_ethnicity)
    d$metadata[["impute_ethnicity"]] <- list(
      cols = eth_cols,
      stats = eth_cols %>%
        summarize(
          mean_diff = mean(ethnicity != imputed_ethnicity, na.rm = TRUE),
          mean_diff_hispanic = mean(
            ethnicity == "hispanic" 
            & imputed_ethnicity != "hispanic",
            na.rm = TRUE
          ),
          pct_recovered = mean(are_na(ethnicity) & !are_na(imputed_ethnicity))
        ) 
    )

    # Replace missing values for ethnicity from `imputed_ethnicity`.
    # NOTE: The theory is that if someone's ethnicity was positively identified,
    # then that identification is likely (reasonably) accurate; if it was not,
    # then our best bet is to impute it.
    d$data %<>%
      mutate(
        ethnicity = if_else(
          condition = are_na(ethnicity),
          true = as.character(imputed_ethnicity),
          false = as.character(ethnicity),
        ),
        ethnicity = factor(ethnicity, levels = VALID_ETH)
      ) %>%
      select(-imputed_ethnicity)
  } else {
    d$data %<>%
      rename(ethnicity = imputed_ethnicity)
  }

  # Return
  d
}


impute_ftp <- function(d, state) {
  # Imputes whether an arrest involved failure to pay by looking for keywords in
  # the designated search columns.

  # If none of the search columns appear, return early.
  if (! any(SEARCH_COLS %in% colnames(d$data))) {
    return(d)
  }

  # Extract the text to be searched.
  search_text <- d$data %>%
    select(any_of(SEARCH_COLS)) %>%
    {exec(str_c_na, !!!., sep = " ")} %>%
    str_to_upper() 


  # Search for references to failure to pay, exlcuding references to child
  # support
  ftp <- str_detect(search_text, FTP_REGEX[[state]])
  csp <- str_detect(search_text, "CHILD SUPPORT|\\bCSP\\b")
  ftp_rows <- d$data %>%
    mutate(ftp = ftp & !csp) %>%
    select(booking_id, ftp)

  # Impute for each booking whether all or any associated charges are for
  # failure to pay.
  ftp_bookings <- ftp_rows %>%
    group_by(booking_id) %>%
    summarize(all_ftp = all(ftp), any_ftp = any(ftp))

  # NOTE: If, overall, all_ftp is greater than 80%, it's a sure sign that we
  # weren't given the full jail booking report. In that case, we must mark
  # `all_ftp` NA.
  if (with(ftp_bookings, mean(all_ftp, na.rm = TRUE) > 0.8)) {
    ftp_bookings %<>%
      mutate(all_ftp = na_lgl)
  }
  
  # Rejoin with the original data.
  d$data %<>%
    bind_cols(select(ftp_rows, ftp)) %>%
    left_join(ftp_bookings, by = "booking_id")

  # Return.
  d
}
