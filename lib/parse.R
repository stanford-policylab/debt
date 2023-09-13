# Functions for extracting results from a dataframe whose columns have been
# classified.

# Load in the required libraries
library(rprojroot)
library(fs)
ROOT <- path(find_root(has_file(".gitignore")))
source(path(ROOT, "lib", "common.R"))

################################### GLOBALS ####################################
# Global variables required for subsequent execution. These are loaded once,
# instead of each time they're used, to avoid unnnecessary i/o and memory usage.

PARSING_COLS = c(
  "col_name",
  "raw_col_number",
  "col_content",
  "type_guess",
  "subtype_guess",
  "output",
  "pre_null_rate",
  "post_null_rate"
)

################################### PARSING ####################################
# These functions do the high-level work of parsing. These functions take a
# single object `d` as an argument, which should be a named list with the
# following slots: `data`, a tibble; and `metadata`, a list to which parsing
# metadata can be appended.

parse <- function(d) {
  # Top level function for parsing data. Checks the data to ensure that the
  # classification is possible, i.e., there is no more than one column of any
  # given type or there are incompatible pairs of columns, e.g., full and
  # partial addresses.

  # Extract the old data and metadata.
  metadata <- d$metadata
  data <- d$data

  # Bundle the data and metadata.
  d <- list(data = data, metadata = list()) 

  # Validate the input.
  validate_input(d)
  
  # Abort if there are mutliple columns of any given type.
  d$data %>%
    count(type_guess, subtype_guess) %>%
    filter(n > 1) %>%
    with(map_chr(
      .x = pretty_colnames(type_guess, subtype_guess),
      .f =  ~ str_c("Found multiple columns of ", .x, ".")
    )) %>%
    str_c(collapse = " ") %>%
    {if (length(.) > 0) {abort(.)}}

  # Initialize the output column using `col_content` and calculate null rates.
  d$data %<>%
    mutate(
      output = col_content,
      pre_null_rate = map_dbl(col_content, ~ 1 - mean(!are_na(.))),
      post_null_rate = map_dbl(col_content, ~ 1 - mean(!are_na(.)))
    )

  # Pass through the pipeline.
  d %<>%
    parse_full_name() %>%
    parse_single_name() %>%
    parse_full_race() %>%
    parse_just_race() %>%
    parse_just_ethnicity() %>%
    parse_sex() %>%
    parse_race_sex_ethnicity() %>%
    parse_numeric() %>%
    parse_datetime() %>%
    parse_date() %>%
    parse_time() %>%
    parse_full_address() %>%
    parse_single_address()

  # Record metadata
  metadata[["parse"]] <- list(
    result = d$data,
    subsidiary_functions = d$metadata
  )
  
  # Issue a warning if any of the null rates increased by more than ten percent.
  d$data %>%
    filter(pre_null_rate - post_null_rate > 0.1) %>%
    with(map2_chr(
      .x = type_guess,
      .y = subtype_guess,
      .f = pretty_colnames,
      both = FALSE
    )) %>%
    str_c(collapse = " ") %>%
    {if (length(.) > 0) {
      warning(str_c("Null rate increased by more than 10% during parsing in:", 
                    ., sep = " "))
    }}

  # Bind the results into a single dataframe.
  final_cols <- d$data %>%
    with(pretty_colnames(type_guess, subtype_guess, both = FALSE)) %>%
    set_names(d$data$output, .)
  for (i in seq_along(final_cols)) {
    if (is_atomic(final_cols[[i]])) {
      final_cols[[i]] <- enframe(final_cols[[i]], name=NULL) %>%
        select(!!base::names(final_cols)[[i]] := value)
    }
  }
  
  final_data <- bind_cols(!!!final_cols) %>%
    mutate(raw_row_number = row_number()) %>%
    as_tibble()

  # Return
  list(
    data = final_data,
    metadata = metadata
  )
}


parse_full_name <- function(d) {
  # Parses full names into a standardized format using `humaniformat`.

  # Validate the input.
  validate_input(d, PARSING_COLS)

  # Extract "full_name" columns and noramlize.
  full_name_cols <- d$data %>%
    filter(type_guess == "name", subtype_guess == "full_name") %>%
    mutate(
      output = map(
        # NOTE: Passing empty strings breaks `humaniformat`.
        .x = map(col_content, ~ if_else(. != "", ., na_chr)),
        .f = ~ parse_names(format_period(format_reverse(str_to_title(.))))
      )
    ) %>%
    # Add post null rate
    mutate(
      post_null_rate = map(output, mutate_all, ~ are_na(.)),
      post_null_rate = map_dbl(post_null_rate, ~ mean(reduce(., `&`)))
    )

  # Record in the metadata.
  d$metadata[["parse_full_name"]] <- full_name_cols %>%
    select(col_name, raw_col_number, output)

  # Merge back into original dataset.
  d$data %<>%
    filter(type_guess != "name" | subtype_guess != "full_name") %>%
    bind_rows(full_name_cols) %>%
    arrange(raw_col_number)

  # Return
  d
}


parse_single_name <- function(d) {
  # Parses columns of type "first_name", "middle_name", "last_name", and
  # "suffix".

  # Validate the input
  validate_input(d, PARSING_COLS)

  # Filter out the "single_name" columns and normalize.
  single_name_cols <- d$data %>%
    filter(type_guess == "name", subtype_guess != "full_name") %>%
    # NOTE: `str_to_tile` has a very annoying bug where `NA` is turned to "Na".
    mutate(output = map(col_content, str_to_title)) %>%
    # Add post null rate
    mutate(post_null_rate = map_dbl(output, ~ 1 - mean(!are_na(.))))

  # Record calculations.
  d$metadata[["parse_single_names"]] <- single_name_cols %>%
    select(col_name, raw_col_number, output)

  # Reinsert the columns into the dataframe
  d$data %<>%
    filter(type_guess != "name" | subtype_guess == "full_name") %>%
    bind_rows(single_name_cols) %>%
    arrange(raw_col_number)

  # Return.
  d
}


parse_full_race <- function(d) {
  # Parses columns of type "full_race" into a standard format.
  
  # Validate the input
  validate_input(d, PARSING_COLS)
  
  # Filter out the race columns, and extract the race data.
  full_race_cols <- d$data %>%
    filter(type_guess == "race", subtype_guess == "full_race") %>%
    mutate(
      temp_col_content = map(col_content, str_to_lower),
      asian = map(
        .x = temp_col_content,
        .f = str_detect,
        pattern = str_c("\\ba(sian)?\\b", "\\b\\wa\\b", "\\bp(acific)?\\b",
                        "\\b\\wp\\b", "\\ba[hn]\\b", sep = "|")
      ),
      american_indian = map(
        .x = temp_col_content,
        .f = str_detect,
        pattern = "\\bi(ndian)?\\b|\\b\\wi\\b|\\bn(ative)?\\b"
      ),
      black = map(
        .x = temp_col_content,
        .f = str_detect,
        pattern = str_c("\\bb(lack)?\\b", "\\b\\wb\\b", "\\b\\waa\\b",
                        "\\ba((frican)?.?a(merican)?)\\b", "\\bb[hn]\\b",
                        sep = "|")
      ),
      hispanic = map(
        .x = temp_col_content,
        .f = str_detect,
        pattern = "\\bh(ispanic)?\\b|\\b\\wh\\b|\\bl(atino)?\\b|\\b\\wl\\b"
      ),
      non_hispanic = map(
        .x = temp_col_content,
        .f = str_detect,
        pattern = "\\bn(on.?(h(ispanic)?)?(l(atino)?)?)?\\b|\\b\\wn\\b"
      ),
      white = map(
        .x = temp_col_content,
        .f = str_detect,
        pattern = "\\bw(hite)?\\b|\\b\\ww\\b|\\bw[hn]\\b"
      ),
      race_other = pmap(
        .l = list(
          asian = asian,
          american_indian = american_indian,
          black = black,
          white = white,
          hispanic = hispanic,
          non_hispanic = non_hispanic
        ),
        # NOTE: We won't try to infer race for people listed as Hispanic or
        # Latino.
        .f = ~ ! (..1 | ..2 | ..3 | ..4)
               & if_else(..5 | ..6, na_lgl, TRUE, na_lgl)
      ),
      ethnicity_other = map2(
        .x = hispanic,
        .y = non_hispanic,
        # NOTE: Only impute ethnicity for individuals for whom it is given;
        # for others, it is calculated at a later stage.
        .f = ~ if_else(! (.x | .y), na_lgl, FALSE, na_lgl)
      ),
      race_output = pmap(
        .l = list(
          `asian/pacific islander` = asian,
          `american indian/alaska native` = american_indian,
          black = black,
          white = white,
          other = race_other
        ),
        .f = extract_fct,
        levels = VALID_RACES
      ),
      ethnicity_output = pmap(
        .l = list(
          hispanic = hispanic,
          non_hispanic = non_hispanic,
          other = ethnicity_other
        ),
        .f = extract_fct,
        levels = VALID_ETH
      ),
      output = map2(
        .x = race_output,
        .y = ethnicity_output,
        .f = ~ tibble(race = .x, ethnicity = .y)
      )
    ) %>%
    # Add post null rate
    mutate(
      post_null_rate = map(output, mutate_all, ~ are_na(.)),
      post_null_rate = map_dbl(post_null_rate, ~ mean(reduce(., `&`)))
    )

  
  # Record calculations in the metadata.
  d$metadata[["parse_full_race"]] <- full_race_cols %>%
    select(-col_content, -temp_col_content, -race_output, -ethnicity_output,
           -pre_null_rate)
  
  # Drop unnecessary columns.
  full_race_cols %<>%
    select(-asian, -american_indian, -black, -white, -race_other, -hispanic,
           -non_hispanic, -ethnicity_other, -race_output, -ethnicity_output,
           -temp_col_content)
  
  # Add the race columns back into the (transposed) dataframe.
  d$data %<>%
    filter(type_guess != "race" | subtype_guess != "full_race") %>%
    bind_rows(full_race_cols) %>%
    arrange(raw_col_number)
  
  # Return.
  d
}


parse_just_race <- function(d) {
  # Parses columns of type "race" into a standard format.

  # Validate the input
  validate_input(d, PARSING_COLS)

  # Filter out the race columns, and extract the race data.
  race_cols <- d$data %>%
    filter(type_guess == "race", subtype_guess == "race") %>%
    mutate(
      temp_col_content = map(col_content, str_to_lower),
      asian = map(
        .x = temp_col_content,
        .f = str_detect,
        pattern = "\\ba(sian)?\\b|\\b\\wa\\b|\\bp(acific)?\\b|\\b\\wp\\b"
      ),
      american_indian = map(
        .x = temp_col_content,
        .f = str_detect,
        pattern = "\\bi(ndian)?\\b|\\b\\wi\\b|\\bn(ative)?\\b|\\b\\wn\\b"
      ),
      black = map(
        .x = temp_col_content,
        .f = str_detect,
        pattern = str_c("\\bb(lack)?\\b", "\\b\\wb\\b", "\\b\\waa\\b",
                        "\\ba((frican)?.?a(merican)?)\\b", sep = "|")
      ),
      white = map(
        .x = temp_col_content,
        .f = str_detect,
        pattern = "\\bw(hite)?\\b|\\b\\ww\\b"
      ),
      other = pmap(
        .l = list(
          asian = asian,
          american_indian = american_indian,
          black = black,
          white = white
        ),
        .f = ~ !(..1 | ..2 | ..3 | ..4)
      ),
      output = pmap(
        .l = list(
          `asian/pacific islander` = asian,
          `american indian/alaska native` = american_indian,
          black = black,
          white = white,
          other = other
        ),
        .f = extract_fct,
        levels = VALID_RACES
      )
    ) %>%
    # Add post null rate
    mutate(post_null_rate = map_dbl(output, ~ 1 - mean(!are_na(.))))

  # Record calculations in the metadata.
  d$metadata[["parse_just_race"]] <- race_cols %>%
    select(-col_content, -temp_col_content, -pre_null_rate)

  # Drop unnecessary columns.
  race_cols %<>%
    select(-asian, -american_indian, -black, -white, -other, -temp_col_content)
  
  # Add the race columns back into the (transposed) dataframe.
  d$data %<>%
    filter(type_guess != "race" | subtype_guess != "race") %>%
    bind_rows(race_cols) %>%
    arrange(raw_col_number)

  # Return.
  d
}


parse_just_ethnicity <- function(d) {
  # Parses columns of type "ethnicity" into a standard format.
  
  # Validate the input
  validate_input(d, PARSING_COLS)
  
  # Filter out the ethnicity columns, and extract the ethnicity data.
  eth_cols <- d$data %>%
    filter(type_guess == "race", subtype_guess == "ethnicity") %>%
    mutate(
      temp_col_content = map(col_content, str_to_lower),
      hispanic = map(
        .x = temp_col_content,
        .f = str_detect,
        pattern = "\\bh(ispanic)?\\b|\\b\\wh\\b|\\bl(atino)?\\b|\\b\\wl\\b"
      ),
      non_hispanic = map(
        .x = temp_col_content,
        .f = str_detect,
        pattern = "\\bn(on.?(h(ispanic)?)?(l(atino)?)?)?\\b|\\b\\wn\\b"
      ),
      other = pmap(
        .l = list(
          hispanic = hispanic,
          non_hispanic = non_hispanic
        ),
        .f = ~ !(..1 | ..2)
      ),
      output = pmap(
        .l = list(
          hispanic = hispanic,
          other = other,
          non_hispanic = non_hispanic
        ),
        .f = extract_fct,
        levels = VALID_ETH
      )
    ) %>%
    # Add post null rate
    mutate(post_null_rate = map_dbl(output, ~ 1 - mean(!are_na(.))))
  
  # Record calculations in the metadata.
  d$metadata[["parse_just_ethnicity"]] <- eth_cols %>%
    select(-col_content, -temp_col_content, -pre_null_rate)
  
  # Drop unnecessary columns.
  eth_cols %<>%
    select(-hispanic, -non_hispanic, -other, -temp_col_content)
  
  # Add the ethnicity columns back into the (transposed) dataframe.
  d$data %<>%
    filter(type_guess != "race" | subtype_guess != "ethnicity") %>%
    bind_rows(eth_cols) %>%
    arrange(raw_col_number)
  
  # Return.
  d
}


parse_sex <- function(d) {
  # Parses columns of type "sex" into a standard format.

  # Validate the input
  validate_input(d, PARSING_COLS)

  # Filter out the sex columns, and extract the sex data.
  sex_cols <- d$data %>%
    filter(type_guess == "sex") %>%
    mutate(
      temp_col_content = map(col_content, str_to_lower),
      male = map(
        .x = temp_col_content,
        .f = str_detect,
        pattern = "\\bm|\\b\\wm\\b"
      ),
      female = map(
        .x = temp_col_content,
        .f = str_detect,
        pattern = "\\bw|\\b\\ww\\b|\\bf|\\b\\wf\\b"
      ),
      other = pmap(
        .l = list(
          male,
          female
        ),
        .f = ~ ! (..1 | ..2)
      ),
      output = pmap(
        .l = list(
          male = male,
          female = female,
          other = other
        ),
        .f = extract_fct,
        levels = VALID_SEXES
      )
    ) %>%
    # Add post null rate
    mutate(post_null_rate = map_dbl(output, ~ 1 - mean(!are_na(.))))

  # Record calculations in the metadata.
  d$metadata[["parse_sex"]] <- sex_cols %>%
    select(-col_content, -temp_col_content, -pre_null_rate)

  # Drop unnecessary columns.
  sex_cols %<>%
    select(-male, -female, -other, -temp_col_content)
  
  # Add the race columns back into the (transposed) dataframe.
  d$data %<>%
    filter(type_guess != "sex") %>%
    bind_rows(sex_cols) %>%
    arrange(raw_col_number)

  # Return.
  d
}


parse_race_sex_ethnicity <- function(d) {
  # Parses columns that combine race, sex, and ethnicity, e.g., "WM" for "white
  # male." (Note that this pattern is fairly specific to Odyssey jail booking
  # reports and may not generalize.)

  # Validate the input
  validate_input(d, PARSING_COLS)

  # Filter out the race / sex / ethnicity columns, and extract the data.
  rse_cols <- d$data %>%
    filter(type_guess == "race_sex_ethnicity") %>%
    mutate(
      # NOTE: The race / sex / ethnicity typically appear as the first two
      # characters of the column.
      rse = map(
        .x = col_content,
        .f = compose(~ str_sub(., start = 1, end = 2), str_to_lower)
      ),
      male = map(rse, str_detect, pattern = ".m"),
      female = map(rse, str_detect, pattern = ".[fw]"),
      sex_other = map(rse, str_detect, pattern = ".[^mfw]"),
      asian = map(rse, str_detect, pattern = "a."),
      american_indian = map(rse, str_detect, pattern = "i."),
      black = map(rse, str_detect, pattern = "b."),
      white = map(rse, str_detect, pattern = "w."),
      hispanic = map(rse, str_detect, pattern = "h."),
      non_hispanic = map(rse, str_detect, pattern = "n."),
      race_other = pmap(
        .l = list(
          asian = asian,
          american_indian = american_indian,
          black = black,
          white = white,
          hispanic = hispanic,
          non_hispanic = non_hispanic
        ),
        # NOTE: We won't try to infer race for people listed as Hispanic or
        # Latino.
        .f = ~ ! (..1 | ..2 | ..3 | ..4)
               & if_else(..5 | ..6, na_lgl, TRUE, na_lgl)
      ),
      ethnicity_other = map2(
        .x = hispanic,
        .y = non_hispanic,
        # NOTE: Only impute ethnicity for individuals for whom it is given;
        # for others, it is calculated at a later stage.
        .f = ~ if_else(! (.x | .y), na_lgl, FALSE, na_lgl)
      ),
      sex_output = pmap(
        .l = list(
          male = male,
          female = female,
          other = sex_other
        ),
        .f = extract_fct,
        levels = VALID_SEXES
      ),
      race_output = pmap(
        .l = list(
          `asian/pacific islander` = asian,
          `american indian/alaska native` = american_indian,
          black = black,
          white = white,
          other = race_other
        ),
        .f = extract_fct,
        levels = VALID_RACES
      ),
      ethnicity_output = pmap(
        .l = list(
          hispanic = hispanic,
          non_hispanic = non_hispanic,
          other = ethnicity_other
        ),
        .f = extract_fct,
        levels = VALID_ETH
      ),
      output = pmap(
        .l = list(
          sex_output,
          race_output,
          ethnicity_output
        ),
        .f = ~ tibble(sex = ..1, race = ..2, ethnicity = ..3)
      )
    ) %>%
    # Add post null rate
    mutate(
      post_null_rate = map(output, mutate_all, ~ are_na(.)),
      post_null_rate = map_dbl(post_null_rate, ~ mean(reduce(., `&`)))
    )

  
  # Record calculations in the metadata.
  d$metadata[["parse_race_sex_ethnicity"]] <- rse_cols %>%
    select(-col_content, -rse, -race_output, -ethnicity_output,
           -sex_output, -pre_null_rate)
  
  # Drop unnecessary columns.
  rse_cols %<>%
    select(-male, -female, -sex_other, -asian, -american_indian, -black, -white,
           -race_other, -hispanic, -non_hispanic, -ethnicity_other,
           -race_output, -ethnicity_output, -sex_output,  -rse)
  
  # Add the race columns back into the (transposed) dataframe.
  d$data %<>%
    filter(type_guess != "race_sex_ethnicity") %>%
    bind_rows(rse_cols) %>%
    arrange(raw_col_number)
  
  # Return.
  d
}


parse_numeric <- function(d) {
  # Parses columns of type "age" and "amount" into a standard format.

  # Validate the input
  validate_input(d, PARSING_COLS)

  # Filter out the numeric columns and extract the data.
  numeric_cols <- d$data %>%
    filter(type_guess %in% c("bond", "age", "fine")) %>%
    mutate(
      output = map(
        col_content,
        quietly(parse_number)
      ),
      output = map(output, "result"),
      output = map_if(
        .x=output,
        .p=type_guess == "age",
        .f=Compose(round, as.integer)
      )
    ) %>%
    # Add post null rate
    mutate(post_null_rate = map_dbl(output, ~ 1 - mean(!are_na(.))))

  # Record the calculations in the metadata.
  d$metadata[["parse_numeric"]] <- numeric_cols %>%
    select(-col_content, -pre_null_rate)

  # Add the numeric columns back into the (transposed) dataframe.
  d$data %<>%
    filter(! type_guess %in% c("age", "bond", "fine")) %>%
    bind_rows(numeric_cols) %>%
    arrange(raw_col_number)

  # Return
  d
}


parse_datetime <- function(d) {
  # Parses columns of type "booking_date", "release_date", or "dob" into a
  # standard format.

  # Validate the input
  validate_input(d, PARSING_COLS)

  # Filter out the datetime columns and extract the data.
  datetime_cols <- d$data %>%
    filter(type_guess == "datetime") %>%
    mutate(
      output = map(
        col_content,
        quietly(parse_date_time),
        orders = c("ymdT", "mdYT", "ymdR", "mdYR", "ymdIp", "mdYIp", "Tymd",
                   "Tmdy"),
        quiet = TRUE,
        # Allow one component to be missing in case datetimes are mixed with
        # dates.
        truncated = 1,
        select_formats = local_select_formats
      ),
      output = map(output, "result"),
      output = map_if(
        .x=output,
        .p=subtype_guess != "dob",
        .f=~ tibble(
            date = make_date(year(.x), month(.x), day(.x)),
            time = hours(hour(.x)) + minutes(minute(.x)) + seconds(second(.x))
          )
      ),
      output = map_if(
        .x=transpose(list(o=output, s=subtype_guess)),
        .p=subtype_guess != "dob",
        .f=~ set_names(.$o, str_c(
          str_split(.$s, "_")[[1]][[1]],
          c("_date", "_time")
        ))
      ),
      output = map_if(
        .x=output,
        .p=subtype_guess == "dob",
        .f=as_date
      )
    ) %>%
    # Add post null rate
    mutate(
      post_null_rate = map(output, mutate_all, ~ are_na(.)),
      post_null_rate = map_dbl(post_null_rate, ~ mean(reduce(., `&`)))
    )

  # Record the calculations in the metadata.
  d$metadata[["parse_datetime"]] <- datetime_cols %>%
    select(-col_content, -pre_null_rate)

  # Add the numeric columns back into the (transposed) dataframe.
  d$data %<>%
    filter(type_guess != "datetime") %>%
    bind_rows(datetime_cols) %>%
    arrange(raw_col_number)

  # Return
  d
}


parse_date <- function(d) {
  # Parses columns of type "date" into a standard format.

  # Validate the input
  validate_input(d, PARSING_COLS)

  # Filter out the datetime columns and extract the data.
  date_cols <- d$data %>%
    filter(type_guess == "date") %>%
    mutate(
      output = map(
        col_content,
        quietly(parse_date_time),
        orders = c("ymd", "mdy", "Ym", "mY"),
        quiet = TRUE,
        select_formats = local_select_formats
      ),
      output = map(output, "result"),
      output = map(
        output,
        as_date
      )
    ) %>%
    # Add post null rate
    mutate(post_null_rate = map_dbl(output, ~ 1 - mean(!are_na(.))))

  # Record the calculations in the metadata.
  d$metadata[["parse_date"]] <- date_cols %>%
    select(-col_content, -pre_null_rate)

  # Add the numeric columns back into the (transposed) dataframe.
  d$data %<>%
    filter(type_guess != "date") %>%
    bind_rows(date_cols) %>%
    arrange(raw_col_number)

  # Return
  d
}


parse_time <- function(d) {
  # Parses columns of type "time" into a standard format

  # Validate the input
  validate_input(d, PARSING_COLS)

  # Filter out the datetime columns and extract the data.
  time_cols <- d$data %>%
    filter(type_guess == "time") %>%
    mutate(
      output = map(
        col_content,
        quietly(parse_date_time),
        orders = c("T", "R", "r"),
        quiet = TRUE,
        select_formats = local_select_formats
      ),
      output = map(output, "result"),
      output = map(output, as_time)
    ) %>%
    # Add post null rate
    mutate(post_null_rate = map_dbl(output, ~ 1 - mean(!are_na(.))))

  # Record the calculations in the metadata.
  d$metadata[["parse_time"]] <- time_cols %>%
    select(-col_content, -pre_null_rate)

  # Add the numeric columns back into the (transposed) dataframe.
  d$data %<>%
    filter(type_guess != "time") %>%
    bind_rows(time_cols) %>%
    arrange(raw_col_number)

  # Return
  d
}


parse_full_address <- function(d) {
  # Parses columns of subtype "full_address" into a standard format.

  # Validate the input.
  validate_input(d, PARSING_COLS)

  # Extract full_address columns and normalize using poster.
  full_addr_cols <- d$data %>%
    filter(type_guess == "address", subtype_guess == "full_address") %>%
    mutate(output = map(col_content, ~ simple_parse_addr(full_addr = .))) %>%
    # Add post null rate
    mutate(
      post_null_rate = map(output, mutate_all, ~ are_na(.)),
      post_null_rate = map_dbl(post_null_rate, ~ mean(reduce(., `&`)))
    )

  # Record in the metadata.
  d$metadata[["parse_full_address"]] <- full_addr_cols %>%
    select(col_name, raw_col_number, output)

  # Merge back into original dataset.
  d$data %<>%
    filter(type_guess != "address" | subtype_guess != "full_address") %>%
    bind_rows(full_addr_cols) %>%
    arrange(raw_col_number)

  # Return.
  d
}


parse_single_address <- function(d) {
  # Parses columns of subtypes "street", "city", "state", and "zip" into a
  # standard format.

  # Validate the input
  validate_input(d, PARSING_COLS)

  # Extract single_address columns and normalize using poster.
  single_addr_cols <- d$data %>%
    filter(type_guess == "address", subtype_guess != "full_address") %>%
    mutate(temp_content = col_content)
  
  # If there are states, attempt to expand the abbreviations.
  if ("state" %in% single_addr_cols$subtype_guess) {
    expand_state_abbrev <- function(addr_state) {
      exp_addr_state <- addr_state %>%
        str_to_upper() %>%
        str_trim() %>%
        magrittr::extract(set_names(state.name, state.abb), .)
      
      coalesce(exp_addr_state, addr_state)
    }
    
    single_addr_cols %<>%
      mutate(
        temp_content = map_if(
          .x = temp_content,
          .p = subtype_guess == "state",
          .f = expand_state_abbrev
        )
      )
  }

  # Skip processing if no single address columns.
  if(nrow(single_addr_cols) > 0) {
    addr_output <- single_addr_cols %>%
      with(exec(
        simple_parse_addr,
        !!!set_names(temp_content, str_c("in_", subtype_guess))
      ))
    single_addr_cols <- d$data %>%
      filter(type_guess == "address", subtype_guess != "full_address") %>%
      mutate(output = map(subtype_guess, pull, .data = addr_output)) %>%
      # Add post_null_rate
      mutate(post_null_rate = map_dbl(output, ~ 1 - mean(!are_na(.))))
  }
  
  # Record in the metadata.
  d$metadata[["parse_single_address"]] <- single_addr_cols %>%
    select(col_name, raw_col_number, output)
  
  # Remove temporary columns.
  single_addr_cols %<>%
    select(-any_of("temp_content"))
  
  # Merge back into original dataset.
  d$data %<>%
    filter(type_guess != "address" | subtype_guess == "full_address") %>%
    bind_rows(single_addr_cols) %>%
    arrange(raw_col_number)
  
  # Return.
  d
}
