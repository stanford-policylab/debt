# Functions for ensuring that the output results make sense.

# Load in the required libraries
library(rprojroot)
library(fs)
ROOT <- path(find_root(has_file(".gitignore")))
source(path(ROOT, "lib", "common.R"))

################################### GLOBALS ####################################
VALID_ZIPS <- read_csv(
    path(ROOT, "data", "zips_to_fips.csv"),
    col_types = cols(.default = "c")
  ) %>%
  pull(zip) %>%
  unique()

################################ STANDARDIZING #################################

standardize <- function(d) {
  # Top level function for standardizing data.

  # Extract the old data and metadata.
  metadata <- d$metadata
  data <- d$data

  # Bundle the data and metadata.
  d <- list(data = data, metadata = list()) 

  # Get pre null rates
  pre_null_rates <- d$data %>%
    transpose_tibble() %>%
    mutate(pre_null_rate = map_dbl(col_content, ~ mean(are_na(.))))

  # Apply the standardization functions.
  d %<>%
    standardize_age() %>%
    standardize_booking_date() %>%
    standardize_release_date() %>%
    standardize_dob() %>%
    standardize_bond() %>%
    standardize_fine() %>%
    standardize_length_of_stay() %>%
    standardize_zip()

  # Get post null rates and record in the metadata
  metadata[["standardize"]] <- d$data %>%
    transpose_tibble() %>%
    mutate(post_null_rate = map_dbl(col_content, ~ mean(are_na(.)))) %>%
    left_join(pre_null_rates, by = c("col_name", "raw_col_number")) %>%
    select(col_name, raw_col_number, col_content = col_content.x,
           output = col_content.y, pre_null_rate, post_null_rate)

  # Apply the schema
  schema <- SCHEMA %>%
    transmute(
      col_name=pretty_colnames(type, subtype, both=FALSE),
      data_type=data_type
    ) %>%
    filter(
      ! map_lgl(data_type, is_null),
      col_name %in% colnames(d$data)
    )
  d$data %<>%
    select(!!!schema$col_name)

  # Throw a warning if any of the columns don't pass the schema test
  schema_errors <- ! map2_lgl(
    .x = schema$col_name,
    .y = schema$data_type,
    .f = ~ .y(d$data[[.x]])
  )
  if (any(schema_errors)) {
    warn(str_c(
      "The following columns did not pass their schema test: ",
      str_c(schema$col_name[schema_errors], collapse=", "),
      "."
    ))
  }
  
  # Return
  list(
    data = d$data,
    metadata = metadata
  )
}

standardize_age <- function(d) {
  if ("age" %in% colnames(d$data)) {
    d$data %<>%
      mutate(
        age = if_else(15 < age & 100 > age, age, NA_integer_)
      )
  }

  d
}

standardize_booking_date <- function(d) {
  if ("booking_date" %in% colnames(d$data)) {
    d$data %<>%
      mutate(
        booking_date = as_date(ifelse(
          ymd(20000101) < booking_date & ymd(20200101) > booking_date,
          booking_date,
          NA_real_
        )
      ))
  }

  d
}

standardize_release_date <- function(d) {
  if ("release_date" %in% colnames(d$data)) {
    d$data %<>%
      mutate(
        release_date = as_date(ifelse(
          ymd(20000101) < release_date & ymd(20200101) > release_date,
          release_date,
          NA_real_
        )
      ))
  }

  d
}

standardize_dob <- function(d) {
  if ("dob" %in% colnames(d$data)) {
    d$data %<>%
      mutate(
        dob = as_date(ifelse(
          ymd(19050101) < dob & ymd(20050101) > dob,
          dob,
          NA_real_
        )
      ))
  }

  d
}

standardize_bond <- function(d) {
  if ("bond" %in% colnames(d$data)) {
    d$data %<>%
      mutate(
        bond = if_else(
          # NOTE: Sometimes bond is $0 and sometimes it's `NA` when there's no
          # bond, so we eliminate all non-positive values to be safe.
          0 < bond & 1e6 > bond,
          bond,
          NA_real_
        )
      )
  }

  d
}

standardize_fine <- function(d) {
  if ("fine" %in% colnames(d$data)) {
    d$data %<>%
      mutate(
        fine = if_else(
          0 <= fine & 1e5 > fine,
          fine,
          NA_real_
        )
      )
  }

  d
}

standardize_length_of_stay <- function(d) {
  if ("length_of_stay" %in% colnames(d$data)) {
    d$data %<>%
      mutate(
        length_of_stay = if_else(
          0 <= length_of_stay & 3000 > length_of_stay,
          length_of_stay,
          NA_real_
        )
      )
  }

  d
}

standardize_zip <- function(d) {
  if ("zip" %in% colnames(d$data)) {
    d$data %<>%
      mutate(
        zip = if_else(zip %in% VALID_ZIPS, zip, na_chr)
      )
  }

  d
}
