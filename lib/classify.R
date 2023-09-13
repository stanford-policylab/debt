# Functions for classifying the columns of a dataframe.

# Load in the required libraries
library(rprojroot)
library(fs)
ROOT <- path(find_root(has_file(".gitignore")))
source(path(ROOT, "lib", "common.R"))

################################### GLOBALS ####################################
# Global variables required for subsequent execution. These are loaded once,
# instead of each time they're used, to avoid unnnecessary i/o and memory usage.

ALPHA_BOW <- dir_map("~/debt/data/col_class/alpha", read_rds) %>%
  set_names(path_ext_remove(path_file(dir_ls("~/debt/data/col_class/alpha"))))
NAME_BOW <- dir_map("~/debt/data/col_class/name", read_rds) %>%
  set_names(path_ext_remove(path_file(dir_ls("~/debt/data/col_class/name"))))
NUMERIC_DIST <- dir_map("~/debt/data/col_class/numeric", read_rds) %>%
  set_names(path_ext_remove(path_file(dir_ls("~/debt/data/col_class/numeric"))))

VALID_ZIP_CODES <- read_rds("~/debt/data/col_class/zip.rds")

CLASSIFICATION_COLS = c(
  "col_name",
  "raw_col_number",
  "col_content",
  "type_guess",
  "argmax",
  "subtype_guess",
  "keep"
)

############################### CLASSIFICATION #################################
# These functions do the actual work of classifying the raw datas columns. All
# functions take a single argument, `d`, which should be a list of length two
# containing both the data and the metadata.

classify <- function(d, n_max=1000) {
  # Top level function for classifying data.

  # Extract previous metadata and data.
  metadata <- d$metadata
  data <- d$data

  # Bundle the data and clean metadata for processing.
  d <- list(data = data, metadata = list())

  # Validate the input.
  validate_input(d)

  # Subset to `n_max` columns, ensuring that all the data is used if there are
  # less than 1,000 rows.
  d$data %<>%
    sample_n(min(nrow(d$data), n_max))

  # Transpose the tibble for processing.
  d$data %<>%
    transpose_tibble() %>%
    mutate(
      type_guess = NA_character_,
      argmax = NA_real_,
      subtype_guess = NA_character_,
      keep = TRUE
    )

  # Pass through the pipeline.
  d %<>%
    calc_alpha_cossim() %>%
    calc_numeric_cossim() %>%
    classify_column_types() %>%
    classify_race_columns() %>%
    classify_date_columns() %>%
    classify_name_columns() %>%
    classify_address_columns()

  # Apply content logic to extract the columns that should be kept. Anything
  # with an argmax of less than 10% is automatically dropped.
  # NOTE: `argmax` has to be zeroed out to avoid arithmetic errors.
  # NOTE: Respect keeps that were previously set to false.
  d$data %<>%
    mutate(
      argmax = if_else(is.finite(argmax), argmax, 0),
      old_keep = keep,
      above_threshold = argmax > 0.1,
      best_in_class = apply_content_logic(
        argmax = argmax,
        type = pretty_colnames(type_guess, subtype_guess)
      ),
      keep = old_keep & best_in_class & above_threshold
    )

  # Record results.
  metadata[["classify"]] <- list(
    result = d$data,
    subsidiary_functions = d$metadata
  )

  # Return the transpose of the original `data`, dropping columns that were not
  # classified and including the `type_guesses`.
  final_data <- data %>%
    transpose_tibble() %>%
    mutate(
      type_guess = d$data$type_guess,
      subtype_guess = d$data$subtype_guess
    ) %>%
    filter(d$data$keep)

  # Return
  list(
    data = final_data,
    metadata = metadata
  )
}

calc_alpha_cossim <- function(d) {
  # Classifies alphabetic columns, using the alphabetic bags of words.

  # Validate the input.
  validate_input(d, CLASSIFICATION_COLS)

  # Calculate alphabetic word frequencies, and then get similarity scores.
  d$data %<>%
    mutate(
      bow = map(
        .x = col_content,
        .f = to_bow
      ),
      alpha_cossim = map(
        .x = bow,
        .f = ~ map_dbl(ALPHA_BOW, cosine_similarity, y = .)
      ),
      # Fix the similarity vector names.
      alpha_cossim = map(
        .x = alpha_cossim,
        .f = rlang::set_names,
        nm = base::names(ALPHA_BOW)
      )
    )

  # Record the calculations in the metadata.
  d$metadata[["calc_alpha_cossim"]] <- d$data %>%
    select(col_name, raw_col_number, bow, alpha_cossim)

  # Remove unnecessary columns
  d$data %<>%
    select(-bow, -alpha_cossim)

  # Return
  d
}


calc_numeric_cossim <- function(d) {
  # Classifies numeric columns by attempting to parse in a variety of formats.

  # Validate the input.
  validate_input(d, CLASSIFICATION_COLS)

  # Attempt to parse as datetimes, plain dates, plain times, and ZIP codes.
  # Calculate the percentage of parsing failures, with a penalty applied for not
  # containing numeric words.
  # NOTE: string lengths have to be truncated (here to 30 char) or
  # `parse_date_time` will break.
  d$data %<>%
    mutate(
      temp_col_content = map(col_content, str_trunc, width = 30),
      parsed_datetime = map(
        .x = temp_col_content,
        .f = parse_date_time,
        orders = c("ymdT", "mdyT", "ymdR", "mdyR", "ymdIp", "mdyIp"),
        quiet = TRUE
      ),
      datetime_pct = 1 - map_dbl(
        .x = parsed_datetime,
        .f = ~ mean(are_na(.))
      ),
      parsed_date = map(
        .x = temp_col_content,
        .f = parse_date_time,
        orders = c("ymd", "mdy"),
        quiet = TRUE
      ),
      date_pct = 1 - map_dbl(
        .x = parsed_date,
        .f = ~ mean(are_na(.))
      ),
      parsed_time = map(
        .x = temp_col_content,
        .f = parse_date_time,
        orders = c("T", "IMp", "Ip"),
        quiet = TRUE
      ),
      time_pct = 1 - map_dbl(
        .x = parsed_time,
        .f = ~ mean(are_na(.))
      ),
      parsed_zip = map(
        .x = col_content,
        .f = str_extract,
        pattern = "\\d{5,}"
      ),
      zip_pct = map_dbl(
        .x = parsed_zip,
        .f = ~ mean(. %in% VALID_ZIP_CODES)
      ),
      # Calculate the simimlarity of the distributions of numerical values to
      # empirical distributions of age and amount.
      dist = map(
        .x = temp_col_content,
        .f = to_dist
      ),
      dist_numeric_cossim = map(
        .x = dist,
        .f = ~ map_dbl(NUMERIC_DIST, cosine_similarity, y = .)
      ),
      # Combine into a single vector of similarity scores.
      numeric_cossim = pmap(
        .l = list(
          datetime_pct,
          date_pct,
          time_pct,
          zip_pct,
          dist_numeric_cossim
        ),
        .f = `c`
      ),
      # Fix similarity vector names.
      numeric_cossim = map(
        .x = numeric_cossim,
        .f = rlang::set_names,
        # NOTE: Since "zip" is a subtype of "address", possible "zip" columns
        # get labeled as addresses here.
        nm = c("datetime", "date", "time", "address", base::names(NUMERIC_DIST))
      )
    )

  # Record calculations
  d$metadata[["calc_numeric_cossim"]] <- d$data %>%
    select(col_name, raw_col_number, parsed_datetime, datetime_pct, parsed_date,
           date_pct, parsed_time, time_pct, parsed_zip, zip_pct, dist,
           numeric_cossim)

  # Remove unnecessary columns
  d$data %<>%
    select(-starts_with("parsed"), -ends_with("pct"), -dist, -numeric_cossim,
           -dist_numeric_cossim, -temp_col_content)

  # Return
  d
}

classify_column_types <- function(d) {
  # Classifies the column type, separating between "alphabetic" columns---i.e.,
  # columns containing mostly alphabetic words---and "numeric" columns---i.e.,
  # columns containing mostly numeric symbols. Columns which are neither are
  # marked with `NA`.

  # Validate the input.
  validate_input(d, CLASSIFICATION_COLS)

  # Fetch the similarity scores
  d$data %<>%
    left_join(
      y = select(d$metadata[["calc_alpha_cossim"]], col_name, raw_col_number,
                 alpha_cossim),
      by = c("col_name", "raw_col_number")
    ) %>%
    left_join(
      y = select(d$metadata[["calc_numeric_cossim"]], col_name, raw_col_number,
                 numeric_cossim),
      by = c("col_name", "raw_col_number")
    )

  # Compute the percentage of alphabetic and numeric words, and apply them as
  # penalties to the sim scores.
  d$data %<>%
    mutate(
      total_tokens = map_int(
        .x = col_content,
        .f = ~sum(str_count(., "\\b\\w+\\b"), na.rm = TRUE)
      ),
      pct_alpha = map_int(
        .x = col_content,
        .f = ~sum(str_count(., "\\b[[:alpha:]]+\\b"), na.rm = TRUE)
      ) / total_tokens,
      pct_numeric = map_int(
        .x = col_content,
        .f = ~sum(str_count(., "\\b\\d+\\b"), na.rm = TRUE)
      ) / total_tokens,
      alpha_cossim = map2(
        .x = alpha_cossim,
        .y = pct_alpha,
        .f = `*`
      ),
      numeric_cossim = map2(
        .x = numeric_cossim,
        .y = pct_numeric,
        .f = `*`
      ),
      sim_score = map2(alpha_cossim, numeric_cossim, c),
      # For each column, find the argmax--i.e., the column type it is most
      # like--as well as the corresponding maximum similarity score.
      argmax = map_dbl(sim_score, ~ suppressWarnings(max(., na.rm = T))),
      type_guess = map2_chr(sim_score, argmax, ~ base::names(.x)[match(.y, .x)]),
      # Mark untypable columns for removal.
      type_guess = if_else(
        condition = are_na(type_guess),
        true = "unknown",
        false = type_guess
      ),
      keep = if_else(
        condition = type_guess != "unknown",
        true = TRUE,
        false = FALSE
      )
    )

  # Record calculations
  d$metadata[["classify_column_types"]] <- d$data %>%
    select(col_name, raw_col_number, total_tokens, pct_alpha, pct_numeric,
           alpha_cossim, numeric_cossim, sim_score, argmax, type_guess)

  # Remove unneeded columns
  d$data %<>%
    select(-total_tokens, -pct_alpha, -pct_numeric, -alpha_cossim,
           -numeric_cossim, -sim_score)

  # Return
  d
}

classify_race_columns <- function(d) {
  # Classifies columns containing races---that is, either races or ethnicities.
  # This is done using the following heuristic: if almost every row looks like
  # "hispanic" or "non-Latino", then it is classified as an ethnicity column; if
  # a large number of rows contain "black" or "white," then it is classified as
  # a race column. If both are true, it is classified as "full_race".
  
  # Validate the input
  validate_input(d, CLASSIFICATION_COLS)
  
  # Fetch just the race columns
  race_cols <- d$data %>%
    filter(type_guess == "race")
  
  # Attempt to guess whether the column contains race or ethnicity.
  race_cols %<>%
    mutate(
      temp_col_content = map(col_content, str_to_lower),
      not_na_pct = map_dbl(
        .x = temp_col_content,
        .f = ~ mean(!are_na(.))
      ),
      race_pct = not_na_pct * map_dbl(
        .x = temp_col_content,
        .f = ~ mean(str_detect(
          string = .,
          pattern = str_c("\\bw(hite)?\\b", "\\b\\ww\\b", "\\bb(lack)?\\b", 
                          "\\b\\wb\\b", "\\b\\waa\\b", 
                          "\\ba((frican)?.?a(merican)?)\\b", sep = "|")
        ), na.rm=TRUE)
      ),
      ethnicity_pct = not_na_pct * map_dbl(
        .x = temp_col_content,
        .f = ~ mean(str_detect(
          string = .,
          pattern =str_c("\\bh(ispanic)?\\b", "\\b\\wh\\b", "\\bl(atino)?\\b",
                          "\\b\\wl\\b",
                          "\\bn(on.?(h(ispanic)?)?(l(atino)?)?)?\\b",
                          "\\b\\wn\\b", sep = "|")
        ), na.rm=TRUE)
      ),
      both_pct = pmap(
        .l = list(
          race = race_pct,
          ethnicity = ethnicity_pct
        ),
        .f = `c`
      ),
      race_argmax = map_dbl(
        .x = both_pct,
        .f = max
      ),
      race_type = map2_chr(
        .x = both_pct,
        .y = race_argmax,
        .f = ~ base::names(.x)[match(.y, .x)]
      ),
      subtype_guess = if_else(
        # NOTE: Guess that it contains both race and ethnicity if the columns
        # are parseable in a roughly even high proportion.
        condition = race_pct > 0.5 & map_dbl(both_pct, sd) < 0.2,
        true = "full_race",
        false = race_type,
        missing = NA_character_
      ),
      # Only keep the best race columns, to avoid an unparseable result.
      # NOTE: `apply_content_logic` is used so that a warning can be raised if
      # there are virtually indistingushably good address columns.
      keep = apply_content_logic(race_argmax, subtype_guess)
    )
  
  # Record calculations
  d$metadata[["classify_race_columns"]] <- race_cols %>%
    select(col_name, raw_col_number, not_na_pct, race_pct, ethnicity_pct,
           race_argmax, race_type, subtype_guess)
  
  # Discard unneeded columns.
  race_cols %<>%
    select(-not_na_pct, -race_pct, -ethnicity_pct, -race_argmax, -race_type,
           -both_pct, -temp_col_content)
  
  # Rebind with the original data.
  d$data %<>%
    filter(type_guess != "race") %>%
    bind_rows(race_cols) %>%
    arrange(raw_col_number)
  
  # Return
  d
}

classify_date_columns <- function(d) {
  # Classifies columns containing dates---that is, either datetimes, or dates.
  # Processes the columns to find (1) booking dates, (2) release dates, and (3)
  # dates of birth.
  #
  # Booking dates are (1) dates after 2000 with a median separation from another
  # column (i.e., release dates) of less than 31 days. If there are none, then
  # booking dates are all those columns with median date after 2000.
  #
  # Release dates are paired with booking dates.
  #
  # Dates of birth are dates which are distributed like ages.
  #
  # Any datetime which cannot be classified as one of these three types is
  # discarded.

  # Validate the input
  validate_input(d, CLASSIFICATION_COLS)

  # Extract just the date or datetime columns. Then, fetch `parsed_datetime` and
  # `parsed_date`, and `pct_numeric`. Combine the the fist two into `gen_date`
  # depending on whether they come from datetime or a date.
  date_cols <- d$data %>%
    filter(type_guess == "datetime" | type_guess == "date") %>%
    left_join(
      y = select(d$metadata[["calc_numeric_cossim"]], col_name, raw_col_number,
                 parsed_datetime, parsed_date),
      by = c("col_name", "raw_col_number")
    ) %>%
    left_join(
      y = select(d$metadata[["classify_column_types"]], col_name,
                 raw_col_number, pct_numeric),
      by = c("col_name", "raw_col_number")
    ) %>%
    mutate(
      gen_date = if_else(
        condition = type_guess == "datetime",
        true = parsed_datetime,
        false = parsed_date,
        missing = NULL
      )
    ) %>%
    select(-starts_with("parsed"))

  # Attempt to find release dates, and calculate median dates and the 
  # distribution of dates. If the standard deviation is more than 50 years, or
  # if the median is implausible, assume that it is not really a date and mark
  # it for discard.
  date_cols %<>%
    mutate(
      median_date = as_datetime(map_dbl(gen_date, median, na.rm = TRUE)),
      release_date = find_booking_release_pairs(gen_date),
      sd_in_years = map_dbl(
        .x = gen_date,
        .f = ~ dseconds(sd(., na.rm = TRUE)) / dyears(1)
      ),
      keep = if_else(
        condition = sd_in_years > 50
                    | median_date > now()
                    | median_date < ymd_hms("1900/01/01 00:00:00"),
        FALSE,
        keep
      )
    )
  
  # If there were release dates, use them to identify booking dates. Otherwise,
  # anything with a median date after 2000/01/01 is a booking date.
  if (any(!are_na(date_cols$release_date))) {
    date_cols %<>%
      mutate(
        subtype_guess = if_else(
          condition = !are_na(release_date) | median_date %in% release_date,
          true = if_else(!are_na(release_date), "booking_date", "release_date"),
          false = "dob",
          missing = NA_character_
      )
    )
  }
  else {
    date_cols %<>%
      mutate(
        subtype_guess = if_else(
          condition = median_date > ymd_hms("2000/01/01 00:00:00"),
          true = "booking_date",
          false = "dob",
          missing = NA_character_
        )
      )
  }

  # Any remaining datetimes are tested as dates of birth.
  date_cols %<>%
    mutate(
      approx_age_dist = map(
        .x = gen_date,
        .f = ~ to_dist((. %--% lubridate::now()) %/% years(1))
      ),
      # Apply a penalty for containing non-numeric words.
      age_sim = map_dbl(
        .x = approx_age_dist,
        .f = cosine_similarity,
        y = NUMERIC_DIST[["age"]]
      ),
      argmax = if_else(
        condition = subtype_guess == "dob",
        true = age_sim,
        false = argmax
      )
    )

  # Record calculations
  d$metadata[["classify_datetime_columns"]] <- date_cols %>%
    select(col_name, raw_col_number, gen_date, median_date, sd_in_years, keep,
           release_date, approx_age_dist, age_sim, argmax, subtype_guess)

  # Remove unnecessary columns
  date_cols %<>%
    select(-gen_date, -median_date, -sd_in_years, -release_date, -pct_numeric,
           -approx_age_dist, -age_sim)
  
  # Rejoin with the rest of the data set.
  d$data %<>%
    filter(type_guess != "datetime" & type_guess != "date") %>%
    bind_rows(date_cols) %>%
    arrange(raw_col_number)

  # Return
  d
}


classify_name_columns <- function(d) {
  # Classifies the columns into first name, last name, and full name using
  # cosine similarity.
  #
  # Full names are identified by an average length of more than two words (i.e.,
  # "JOHANN D GAEBLER" is made up of three words).
  #
  # If the average length is less than two words, then the cosine similarity is
  # found with first and last names.

  # Validate the input
  validate_input(d, CLASSIFICATION_COLS)

  # Fetch `pct_alpha` and `bow` and filter to the colums of type "name".
  name_cols <- d$data %>%
    filter(type_guess == "name") %>%
    left_join(
      y = select(d$metadata[["classify_column_types"]], col_name,
                 raw_col_number, pct_alpha),
      by = c("col_name", "raw_col_number")
    ) %>%
    left_join(
      y = select(d$metadata[["calc_alpha_cossim"]], col_name, raw_col_number,
                 bow),
      by = c("col_name", "raw_col_number")
    )

  # Differentiate between full names, last names, and other names.
  name_cols %<>%
    mutate(
      avg_no_words = map_dbl(
        .x = col_content,
        .f = ~ mean(str_count(., pattern = "\\b\\w+\\b"), na.rm = TRUE)
      ),
      pct_not_na = map_dbl(
        .x = col_content,
        .f = ~ mean(!are_na(.))
      ),
      name_cossim = pmap(
        .l = list(pct_alpha, pct_not_na, bow),
        .f = ~ ..1 * ..2 * map_dbl(NAME_BOW, cosine_similarity, y = ..3)
      ),
      name_argmax = map_dbl(
        .x = name_cossim,
        .f = max
      ),
      name_type = map2_chr(
        .x = name_cossim,
        .y = name_argmax,
        .f = ~ base::names(.x)[match(.y, .x)]
      ),
      subtype_guess = if_else(
        condition = avg_no_words > 2,
        true = "full_name",
        false = name_type,
        missing = NA_character_
      ),
      # Only keep the best first and last name columns, to avoid an unparseable
      # result.
      # NOTE: `apply_content_logic` is used so that a warning can be raised if
      # there are virtually indistingushably good name columns.
      keep = apply_content_logic(name_argmax, subtype_guess)
    )

  # Record calculations
  d$metadata[["classify_name_columns"]] <- name_cols %>%
    select(col_name, avg_no_words, pct_not_na, name_cossim, name_argmax,
           name_type, type_guess, keep)

  # Discard unneeded columns.
  name_cols %<>%
    select(-avg_no_words, -pct_not_na, -name_cossim, -name_argmax, -name_type,
           -bow, -pct_alpha)

  # Rebind with the original data.
  d$data %<>%
    filter(type_guess != "name") %>%
    bind_rows(name_cols) %>%
    arrange(raw_col_number)

  # Return
  d
}


classify_address_columns <- function(d) {
  # Classifies address columns using `poster`.

  # Validate the input
  validate_input(d, CLASSIFICATION_COLS)

  # Filter to columns of type `address` and get `pct_zip` from
  # `classify_numeric`.
  addr_cols <- d$data %>%
    filter(type_guess == "address") %>%
    left_join(
      y = select(d$metadata[["calc_numeric_cossim"]], col_name, raw_col_number,
                 zip_pct),
      by = c("col_name", "raw_col_number")
    )

  # Attempt to guess whether each is a street, city, state or zip.
  # NOTE: Since `zip` is already readily identified as a numerial string and
  # `poster::postal_code` is over-eager, we use that percentage instead.
  addr_cols %<>%
    mutate(
      not_na_pct = map_dbl(
        .x = col_content,
        .f = ~ mean(!are_na(.))
      ),
      street_pct = not_na_pct * map_dbl(
        .x = col_content,
        .f = ~ mean(!are_na(road(.)))
      ),
      city_pct = not_na_pct * map_dbl(
        .x = col_content,
        .f = ~ mean(!are_na(city(.)))
      ),
      state_pct = not_na_pct * map_dbl(
        .x = col_content,
        .f = ~ mean(!are_na(state(.)))
      ),
      zip_pct = zip_pct * not_na_pct,
      addr_pct = pmap(
        .l = list(
          street = street_pct,
          city = city_pct,
          state = state_pct,
          zip = zip_pct
        ),
        .f = `c`
      ),
      addr_argmax = map_dbl(
        .x = addr_pct,
        .f = max
      ),
      addr_type = map2_chr(
        .x = addr_pct,
        .y = addr_argmax,
        .f = ~ base::names(.x)[match(.y, .x)]
      ),
      subtype_guess = if_else(
        # NOTE: Guess that it contains a full address if the columns are
        #       parseable in a roughly even high proportion.
        condition = street_pct > 0.5 & map_dbl(addr_pct, sd) < 0.2,
        true = "full_address",
        false = addr_type,
        missing = NA_character_
      ),
      # Only keep the best first and last address columns, to avoid an
      # unparseable result.
      # NOTE: `apply_content_logic` is used so that a warning can be raised if
      # there are virtually indistingushably good address columns.
      keep = apply_content_logic(addr_argmax, subtype_guess)
    )

  # Record calculations
  d$metadata[["classify_address_columns"]] <- addr_cols %>%
    select(col_name, raw_col_number, not_na_pct, street_pct, city_pct,
           state_pct, zip_pct, addr_argmax, addr_type, subtype_guess)

  # Discard unneeded columns.
  addr_cols %<>%
    select(-not_na_pct, -street_pct, -city_pct, -state_pct, -zip_pct,
           -addr_argmax, -addr_type)

  # Rebind with the original data.
  d$data %<>%
    filter(type_guess != "address") %>%
    bind_rows(addr_cols) %>%
    arrange(raw_col_number)

  # Return
  d
}
