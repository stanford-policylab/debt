# Utility functions used across multiple files.

################################### GLOBALS ####################################
STATE_TR = fips_codes %>%
  group_by(state, state_name) %>%
  summarize() %>%
  ungroup() %>%
  mutate(state_abbrev = str_to_lower(state))

#################################### UTILS #####################################

validate_input <- function(d, expected_cols=NULL) {
  # Validates the input to ensure:
  #   1. The argument `d` has the proper structure.
  #   2. The data are non-empty.
  #   3. The metadata have been properly passed.

  # Get the name of the calling function.
  calling_function <- as_name(sys.call(-1)[[1]])

  # Check that `d` has the proper structure.
  if (! is_list(d) | any(base::names(d) != c("data", "metadata"))) {
    abort(str_c(
      "Argument `d` in `",
      calling_function,
      "` is improper. Should have structure list(data = , metadata = )."
    ))
  }

  # Extract the data and metadata
  data <- d$data
  metadata <- d$metadata

  # Check that the arguments are the correct type of object.
  if (!is_tibble(data)) {
    abort(str_c(
      "Argument `d$data` in `",
      calling_function,
      "` must be a tibble."
    ))
  }

  if (!is_list(metadata)) {
    abort(str_c(
      "Argument `d$metadata` in `",
      calling_function,
      "` must be a list."
    ))
  }

  # Avoid overwriting metadata.
  if (calling_function %in% base::names(metadata)) {
    abort(str_c(
         "Metadata already contain a `",
         calling_function,
         "` entry. Bailing out to avoid overwrite."
    ))
  }

  # Check if metadata are empty.
  if (is_null(metadata)) {
    abort(str_c(
      "No metadata provided to `",
      calling_function,
      "`."
    ))
  }

  # Check to make sure `data` is not empty.
  if (! ncol(data) > 0) {
    abort(str_c(
      "Argument `d$data` in `",
      calling_function,
      "` has no columns."
    ))
  }

  # Check to ensure that the expected columns are present.
  if (!is_null(expected_cols) & any(colnames(data) != expected_cols)) {
    abort(str_c(
      "Argument `d$data` in `",
      calling_function,
      "` has improper column names. Expected: ",
      str_c(expected_cols, collapse = ", "),
      ". Found: ",
      str_c(colnames(data), collapse = ", "),
      "."
    ))
  }
}


transpose_tibble <- function(data) {
  # Converts a tibble of the form
  #
  #   COL1 | COL2 | ... | COLN
  #  ------+------+-----+------
  #    ... |  ... | ... |  ...
  #
  # into a nested tibble of the form
  #
  #   col_name | raw_col_number | col_content
  #  ----------+----------------+-------------
  #     "COL1" |              1 | <chr [n_1]>
  #     "COL2" |              2 | <chr [n_2]>
  #        ... |            ... |         ...
  #     "COLN" |              N | <chr [n_N]>
  #

  # Validate input.
  if (! is_tibble(data)) {
    abort("Invalid argument `data` to `transpose_tibble`.")
  }

  # Reshape the data.
  tibble(
    col_name = base::names(data),
    raw_col_number = seq_along(data),
    col_content = unclass(data)
  )
}


to_bow <- function(chr_vector) {
  # Converts a character vector into a bag of words, i.e., character vector of
  # length greater than or equal to the original where each element is a single
  # token. Words are automatically lower-cased, and punctuation and _numbers_
  # (although not necessarily numerical strings, e.g., `">=250g"`) are removed.
  # To avoid unnecessary recomputation, frequencies are then calculated. The
  # return is a (possibly empty) tibble of the following form:
  #
  #        value |    n
  #       -------+------
  #        "foo" | 1000
  #        "bar" |  200
  #          ... |  ...
  #

  # Validate the input
  if (!is_character(chr_vector)) {
    abort("Argument `char_vector` to `to_bow` must be a character vector.")
  }

  # Turn into a sanitized bow.
  bow <- chr_vector %>%
    str_split(pattern = "[[:punct:]\\s]+") %>%
    flatten_chr() %>%
    str_to_lower() %>%
    `[`(., !str_detect(., pattern = "^\\d*$")) %>%
    `[`(., !are_na(.))

  # Turn into a word frequency distribution
  bow %>%
    enframe(name=NULL) %>%
    count(value)
}


to_dist  <- function(vec) {
  # Converts a character vector containing numerical strings into a
  # distribution. Since the inner product of distributions above, if calculated
  # literally, is almost certain to be zero, since the distributions are highly
  # atomic. To avoid this, the distributions are binned logarithmically.
  # Anything longer than eight digits is extremely unlikely to represent a real
  # amount (as opposed to, say, some sort of S.O. number). `NA`s are removed
  # automatically. The return is a (possibly empty) tibble of the following
  # form:
  #
  #          value |    n
  #     -----------+------
  #        "[0,1]" | 1000
  #      "(1,1.2]" |  200
  #            ... |  ...
  #

  # Validate the input
  if (!is_character(vec) & !is_double(vec) & !is_integer(vec)) {
    abort(str_c(
      "Argument `vec` to `to_dist` must be a character, double, or integer ",
      "vector."
    ))
  }

  # Setup binning.
  breaks <- append(10^(0:80 / 10), 0, 0)

  # Parse characters first.
  if (is_character(vec)) {
    vec %<>%
      str_split(pattern = "[\\s]+") %>%
      flatten_chr() %>%
      # Extract all the numbers contained in the column as doubles.
      {suppressWarnings(parse_number(.))}
  }

  # Return distribution.
  vec %>%
    cut(breaks = breaks, right = FALSE) %>%
    as.character() %>%
    `[`(., !are_na(.)) %>%
    enframe(name=NULL) %>%
    count(value)
}


cosine_similarity <- function(x, y, avg = NULL) {
  # Calculates the cosine similarity of two vectors. Expects that `x` and `y`
  # will both be (possibly empty) dataframes with two columns: `value` and `n`.

  # Validate the input.
  if (!is_tibble(x)
      | ! is_tibble(y)
      | any(colnames(x) != c("value", "n") | colnames(y) != c("value", "n"))
  ) {
    abort(str_c(
      "Arguments to `cosine_similarity` must be tibbles that have two columns:
      `value`", " and `n`."
    ))
  }
  if (!(is_character(x$value) | is.factor(x$value))
      | !(is_character(y$value) | is.factor(y$value))
      | !(is_integer(x$n) | is_double(x$n))
      | !(is_integer(y$n) | is_double(y$n)))
  {
    abort(str_c(
      "Arguments to `cosine_similarity` must be dataframes with column types",
      " <chr> and <int> or <dbl>."
    ))
  }

  # Embed the words in a single vector space by taking the full join.
  full_join(x, y, by = "value") %>%
    select(-value, x = n.x, y = n.y) %>%
    mutate_all(as_double) %>%
    # Make `NA`s 0, since the full join introduces many.
    `[<-`(is.na(.), value = 0) %>%
    with(sum(x * y) / sqrt(sum(x * x) * sum(y * y)))
}


normalize <- function(x) {
  # Normalizes a bag of words or distribution so that it has norm 1.

  # Validate the input.
  if (any(colnames(x) != c("value", "n"))) {
      abort("Arguments to `normalize` must have two columns: `value` and `n`.")
  }
  if (!(is_character(x$value) | is.factor(x$value))
      | !(is_integer(x$n) | is_double(x$n)))
  {
    abort(str_c(
      "Arguments to `normalize` must be dataframes with column types",
      " <chr> or <fct> and <int> or <dbl>."
    ))
  }

  # Normalize `x`
  norm <- with(x, sqrt(sum(as_double(n) * as_double(n))))
  mutate(x, n = n / norm)
}


average_dists <- function(...) {
  # Averages a set of distributions or bags of words.

  # Get the arguments
  dists <- list2(...)

  # Validate the input. First, check column names.
  map(
    .x = dists,
    .f = ~ if (any(colnames(.) != c("value", "n"))) {
      abort(str_c(
        "Arguments to `average_dists` must have two columns: `value` and `n`.",
        " Found columns: ",
        str_c(colnames(.), collapse = ", "),
        "."
      ))
    }
  )
  # Next, check column types.
  map(
    .x = dists,
    .f = ~ if (!(is_character(.$value) | is.factor(.$value))
               | !(is_integer(.$n) | is_double(.$n)))
      {
        abort(str_c(
          "Arguments to `average_dists` must be dataframes with column types",
          " <chr> or <fct> and <int> or <dbl>. Found coltypes: ",
          str_c(map_chr(., typeof), collapse = ", "),
          "."
        ))
      }
  )

  # Set up the expression to evaluate, which is the row-wise average of the
  # columns that were `n` before the `full_join`.
  embedded_dists <- reduce(dists, full_join, by = "value") %>%
    `[<-`(is.na(.), value = 0)

  n_cols <- colnames(embedded_dists)[colnames(embedded_dists) != "value"]
  expr <- n_cols %>%
    syms() %>%
    reduce(call2, .fn = `+`) %>%
    call2(`/`, ., length(n_cols))

  # Evaluate the expression
  embedded_dists %>%
    transmute(
      value = value,
      n = !!expr
    ) %>%
    normalize
}


logit <- function(x) {
  # Computes logit(x), i.e., log(x / [1 - x]). Note that x = 1 and x = 0 are
  # correctly computed as Inf and -Inf, respectively.
  if (!is_double(x)) {
    abort("Argument `x` in `logit` must be a double vector.")
  }

  log(x / (1 - x))
}


apply_content_logic <- function(argmax, type) {
  # Takes two vectors of equal length, `argmax`, which is the highest similarity
  # score for a given column, and `type`, which denotes the type of content that
  # the argmax matched. Returns a logical vector of equal length, where `TRUE`
  # represents that the given column is a sufficiently good match to be included
  # in the final dataframe, and `FALSE` indicates otherwise.
  #
  # A sufficiently good match is within 1 of the logit of the best match.
  # I.e., if the highest similarity score with `names` is .789, then any score
  # s such that logit(s) > logit(.789) - 1 is also considered a "sufficiently
  # good match."

  # Validate the input
  if (!is_double(argmax)) {
    abort("Argument `argmax` in `apply_content_logic` must be a double vector.")
  }
  if (!is_character(type)) {
    abort("Argument `type` in `apply_content_logic` must be a character vector.")
  }
  if (length(argmax) != length(type)) {
    abort(str_c(
      "Arguments `type` and `argmax` in `apply_content_logic` must be the ",
      "same length."
    ))
  }
  if (any(0 > argmax | 1 < argmax)) {
    abort(str_c(
      "Argument `argmax` in `apply_content_logic` must contain values ",
      "between zero and one."
    ))
  }

  # Calculate the best match of each type
  data <- tibble(argmax = logit(argmax), type = type)
  best_in_class <- data %>%
    group_by(type) %>%
    summarize(best_score = max(argmax, na.rm = TRUE))

  # Calculate which matches are sufficiently good.
  full_join(data, best_in_class, by = "type") %>%
    with(argmax >= best_score - 1) %>%
    # Make sure to drop any missing values.
    if_else(TRUE, FALSE, FALSE)
}


find_booking_release_pairs <- function(dates) {
  # Takes a list of date vectors, and returns the following: a vector of the 
  # same length (as the list), with NA in every position that does _not_ contain
  # a booking date, and the corresponding release date in every position that 
  # _does_ contain a booking date.
  
  # Validate input
  if (any(map_lgl(dates, ~ any(class(.) != c("POSIXct", "POSIXt"))))) {
    abort(str_c("Argument `dates` in `find_booking_release_pairs` must be a ",
                "datetime."))
  }

  # Find the pairwise differences between the dates. Mark any that are are
  # between 0 and 31 days after with true, and then extract the corresponding
  # dates.
  pairwise_differences <- map_dfc(dates, ~ vector("double", length(dates)))
  for (i in seq_along(dates)) {
    for (j in seq_along(dates)) {
      pairwise_differences[i,j] = mean(
        as.duration(dates[[i]] %--% dates[[j]]) / ddays(1),
        na.rm=TRUE
      )
    }
  }
  pairwise_differences %<>%
    # Zero out differences that aren't in range
    `[<-`(. > 0, value = 0) %>%
    `[<-`(. < -31, value = 0) %>%
    mutate_all(~ (if_else(
     condition = . < 0,
     true = TRUE,
     false = FALSE,
     missing = FALSE
    )))

  # Test if there're too many matches for any columns.
  if (any(map_dbl(pairwise_differences, sum) > 1)) {
    warn("Non-unique booking_date / release_date pairs.")
    return(as_datetime(rep(NA_real_, length(dates))))
  }
  
  median_dates <- map_dbl(dates, median, na.rm=TRUE) %>%
    as_datetime()

  # Extract the booking dates
  pairwise_differences %>%
    mutate_all(~(if_else(., median_dates, ymd_hms(NA)))) %>%
    map_dbl(median, na.rm = TRUE) %>%
    as_datetime()
}


with2 <- function(df_x, df_y, expr) {
  # Implements a version of with in which `x$COL` refers to the `COL` column in
  # `df_x` and `y$COL` refers to the `COL` column in `df_y`.
  if (! is_tibble(df_x) | ! is_tibble(df_y)) {
    abort("Arguments `df_x`, `df_y` in `with2` must be tibbles.`")
  }

  expr <- enquo(expr)
  data_mask <- as_data_mask(list(x = df_x, y = df_y))
  eval_tidy(expr, data_mask)
}


extract_fct <- function(..., levels) {
  # Turn a collection of logical vectors into a factor. These must be of equal
  # length.

  # Get the arguments.
  lgl_df <- tibble(...)

  # Check that the input matches the levels.
  if (any(! base::names(lgl_df) %in% levels)) {
    abort("Arguments to `extract_fct` must be contained in `levels`.")
  }

  # Find the over-determined entries.
  overdetermined <- reduce(lgl_df, `+`) > 1

  # Warn about overdetermined entries.
  if (sum(overdetermined, na.rm = TRUE) > 0) {
    warn(str_c(
      "Found ",
      sum(overdetermined, na.rm = TRUE),
      " overdetermined entries in call to `extract_fct`."
    ))
  }

  # Create a single character vector containing "blah" in each index where
  # `lgl_df$blah` is true.
  reduce2(
    .x = lgl_df,
    .y = colnames(lgl_df),
    .f = fi_esle,
    missing = NA_character_,
    .init = vector("character", nrow(lgl_df))
  ) %>%
    # Replace over-determined entries with `NA`
    if_else(overdetermined, NA_character_, ., NA_character_) %>%
    factor(levels = levels)
}


fi_esle <- function(false, condition, true, missing = NULL) {
  # A nonce function for use with `extract_fct` which changes the order of the
  # arguments of `if_else`.
  if_else(condition, true, false, missing)
}


pretty_colnames <- function(type, subtype, both = TRUE) {
  # Takes a type and subtype and prints a nice name for the column. Accepts
  # `NA` in the `subtype` argument.
  # NOTE: Vectorized, but throws an error if `type` and `subtype` are not the
  # same length instead of recycling.

  # Validate input.
  if (!is_character(type) | !is_character(subtype)) {
    abort(str_c("Arguments `type` and `subtype` to `pretty_colnames` must be",
                "character vectors."))
  }
  if (length(type) != length(subtype)) {
    abort(str_c("Arguments `type` and `subtype` to `pretty_colnames` must be",
                "the same length."))
  }

  if (both) {
    ftype <- if_else(!are_na(type), type, "unknown")
    fsubtype <- if_else(!are_na(subtype), str_c(", ", subtype), "")
    col_names <- str_c(ftype, fsubtype)
  } else {
    col_names <- if_else(are_na(subtype), type, subtype, "unknown")
  }
  
  # Warn if there are unknown columns.
  if ("unknown" %in% col_names) {
    warn(str_c("There were uknown columns passed to `pretty_columns`. Make ",
               "sure the data are parsed correctly."))
  }

  # Return.
  col_names
}


collate <- function(vec) {
  # Collate a vector of length `n` into an object of length `1`. This checks for
  # (and removes) duplicated data.

  deduped_vec <- vec[!duplicated(vec)]

  list(deduped_vec)
}


is_factor <- function(x, .levels = NULL) {
  # Checks to see if `x` is a factor. If the optional parameter `levels` is
  # provided, the levels of `x` must equal them.
  
  if (is_null(.levels)) {
    return(is.factor(x))
  } else {
    if (is.factor(x)) {
      return(identical(.levels, levels(x)))
    } else {
      return(FALSE)
    }
  }
}


is_time <- function(x) {
  # Checks to see if `x` is encoding a valid time (e.g., a fixed number of
  # hours, minutes, and seconds. This is modelled after the output of `hms` in
  # the `lubridate` package.

  # Check to see if it is a period.
  if (!is.period(x)) {
    return(FALSE)
  }

  # Check to see that there are no more than 23 hours, 59 minutes, 59 seconds,
  # and no days, months, or years. Everything must be greater than or equal to
  # 0.
  # NOTE: This must be structured as an na. to
  all(
    (0 <= x@.Data & 60 > x@.Data)
      & (0 <= x@minute & 60 > x@minute)
      & (0 <= x@hour & 60 > x@hour)
      & (0 == x@day)
      & (0 == x@month)
      & (0 == x@year),
    na.rm = TRUE
  )
}


is_datetime <- function(x) {
  # Alias for `is.POSIXct`.
  is.POSIXct(x)
}


apply_merge_config <- function(tbl, merge_config) {
  # Takes a character vector of merge instructions describing how to merge
  # columns from a raw dataframe (see `merge_column_to_str`) and applies them by
  # merging the corresponding columns.

  if (!is_tibble(tbl)) {
    abort(str_c("Argument `tbl` to `apply_merge_instructions`",
          "must be a tibble, not type",
          typeof(merge_config),
          sep = " "
    ))
  }
  if (! all(map_lgl(merge_config, is_integer))) {
    abort("Invalid configuration file. Cannot merge.")
  }

  # Convenience function with easier to use arguments
  convenient_unite <- function(tbl_, colname, cols) {
    unite(tbl_, !!colname, !!!cols, sep=" ", na.rm = TRUE)
  }

  merge_cols <- map(merge_config, vars_select, .vars=colnames(tbl))
  merge_names <- map(merge_cols, str_c, collapse="_")
  reduce2(merge_names, merge_cols, convenient_unite, .init=tbl) %>%
    # NOTE: `unite` has an odd side-effect: `NA`s are coerced to the string "NA"
    # unless `na.rm=T` is passed. However, this results in empty strings.
    mutate_all(~ if_else(. == "", NA_character_, .))
}

apply_split_config <- function(tbl, config) {
  # Takes a character vector of split patterns (i.e., a character vector of
  # regexes defining where to split each column) along with the number of
  # columns the given column should be split into, and applies them by splitting
  # the corresponding columns at the indicated places.

  split_pattern <- config$split_pattern
  split_number <- config$split_number
  split_tbls <- list()

  for (i in seq_along(tbl)) {
    if (!is_na(split_pattern[[i]])) {
      col_ <- colnames(tbl)[[i]]
      into <- str_c("split", col_, seq(split_number[[i]]), sep = "_")
      split_tbls[[length(split_tbls) + 1]] <- tbl[[i]] %>%
        enframe(name = NULL) %>%
        separate(col = "value", into = into, sep = split_pattern[[i]], extra =
                 "merge", fill = "right")
    } else {
      split_tbls[[length(split_tbls) + 1]] <- tbl[i]
    }
  }

  bind_cols(!!!split_tbls)
}


simple_parse_addr <- function(
  full_addr = NULL,
  in_street = NULL,
  in_city = NULL,
  in_state = NULL,
  in_zip = NULL
) {
  # A function for parsing addresses according to the _schema_ columns, viz., 
  # street, city, state, zip.

  # If full address is provided, use; otherwise, combine; else, fail.
  if (! is_null(full_addr)) {
    char = full_addr
  } else if (
    ! (is_null(in_street) 
       & is_null(in_city) 
       & is_null(in_state) 
       & is_null(in_zip))
  ) {
    char = str_c_na(in_street, in_city, in_state, in_zip)
  } else {
    abort("Either a full address or address parts must be provided")
  }

  # Validate the input
  if (!is_character(char)) {
    abort(str_c("`simple_parse_addr` takes character vectors, not ",
                typeof(char),
                "."
    ))
  }

  # Parse using `poster`, and then simplify the result.
  parsed <- normalise_addr(char) %>%
    parse_addr() %>%
    transmute(
      out_street = str_c_na(house, category, near, house_number, road, unit, level,
                     staircase, entrance, po_box, sep = " "),
      out_city = str_c_na(suburb, city_district, city, sep = " "),
      state_name = str_c_na(state_district, state, sep = " "),
      out_zip = str_extract(postal_code, "^\\d{5}")
    ) %>%
    mutate_all(str_replace, pattern = "\\s+", replacement = " ") %>%
    mutate_all(str_to_title) %>%
    left_join(STATE_TR, by = "state_name") %>%
    select(out_street, out_city, out_state = state_abbrev, out_zip)

  # Add fallbacks if have zip. (Sometimes zip / state parse incorrectly.)
  if (! is_null(in_zip)) {
    parsed <- tibble(zip = in_zip) %>%
      mutate(zip = str_extract(zip, "^\\d{5}")) %>%
      left_join(ZIPS_TO_FIPS, by = "zip") %>%
      select(backup_state = state_abbrev, backup_zip = zip) %>%
      bind_cols(parsed) %>%
      mutate(
        out_state = coalesce(backup_state, out_state),
        out_zip = coalesce(backup_zip, out_zip)
      ) %>%
      select(out_street, out_city, out_state, out_zip)
  }

  # Rename columns
  parsed %<>%
    rename_all(~ str_sub(., 5, -1))

  # Return
  parsed
}


str_c_na <- function(..., sep = ", ") {
  # str_c_na(c("a", "b", "c"), c("1", "2", "3"), sep = "|")
  # "a|1" "b|2" "c|3"
  # NOTE: `unite` requires a `col` argument even though it is immediately thrown 
  #       away.
  joined <- unite(tibble(...), "X", sep = sep)[[1]]
  sep_literal <- str_c("\\Q", sep, "\\E")
  pattern <- str_c(
    str_c(sep_literal, "NA"),
    str_c("NA", sep_literal),
    "NA",
    sep = "|"
  )
  str_replace_all(joined, pattern, "") %>%
    str_replace_all("^$", NA_character_)
}


as_time <- function(dt) {
  # Coerces a datetime to a time.
  hours(hour(dt)) + minutes(minute(dt)) + seconds(second(dt))
}


local_select_formats <- function(trained, drop = FALSE) {
  # The default format selection procedure (`lubridate:::.select_formats`) for
  # `parse_date_time` will pick a format that actually parses few of the dates,
  # but parses them with a lot of elements. Instead, it's better to weight the
  # number successfully matched by the (incomprehensible) score that lubridate
  # produces

  nms <- names(trained)

  score <-
    nchar(gsub("[^%]", "", nms)) + # longer formats have priority
    grepl("%Y", nms, fixed = T)*1.5 +
    grepl("%y(?!%)", nms, perl = T)*1.6 + # y has priority over Y, but only when not followed by %
    grepl("%[Bb]", nms)*.31 + # B/b format has priority over %Om
    # C parser formats have higher priority
    grepl("%Om", nms)*.3 +
    grepl("%Op", nms)*.3 +
    grepl("%Ob", nms)*.32  # Ob has higher priority than B/b

  # Scale by the number actually parsed...
  score <- score * trained

  # ties are broken by `trained`
  n0 <- trained != 0
  if (drop) {
    score <- score[n0]
    trained <- trained[n0]
  } else {
    score[!n0] <- -100
  }

  # print(rbind(trained, score))
  names(trained)[order(score, trained, decreasing = T)]
}
