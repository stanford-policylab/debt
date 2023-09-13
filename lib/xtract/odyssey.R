# Functions for extracting data from Odyssey Booking Report .csv and .txt files

################################## LIBRARIES ###################################

library(magrittr)
library(tidyverse)
library(rlang)
library(tidyselect)
library(fs)

source("~/debt/lib/pdf.R")

################################### GLOBALS ####################################

FILLER_REGEXES <- str_c("(?i)^\\s*((Inmate Bookings)|(Date Range)|",
                        "(Nodes:)|(Main Jail)|(SO #)|(Printed on)|",
                        "(ICE Holding)|(Total.*:)|(Inmate's))")

CHARGE_COL <- 5L

BOND_COL <- 7L

################################## EXTRACTION ##################################

extract_odyssey_dir <- function(dirpath) {
  
  filenames <- dir_ls(dirpath)
  
  new_filenames <- str_replace(filenames, "\\.[:alpha:]{3}$", "") %>% 
    str_c("_extracted.csv")
  
  walk2(.x = filenames, 
        .y = new_filenames, 
        .f = ~ write_csv(x = extract_odyssey(.x), path = .y))
}

extract_odyssey <- function(path, n_max = Inf) {
  
  is_txt <- str_detect(path, ".txt$")
  
  is_csv <- str_detect(path, ".csv$")
  
  if (!is_txt && !is_csv) {
    abort(str_c("Argument `path` to `extract_odssey` must be a file path to a ", 
                "text file or CSV."))
  }
  
  if (is_txt) {
    raw_odyssey_data <- read_lines(path, n_max = n_max) %>% 
      extract_text(FILLER_REGEXES)
    
    # TODO(Josh): make sure metadata passed on later
    metadata <- raw_odyssey_data$metadata
    
    raw_odyssey_data <- raw_odyssey_data$data
  }
  
  if (is_csv) {
    raw_odyssey_data <- read_csv(path, n_max = n_max) %>% 
      remove_empty_columns() %>% 
      remove_filler_rows_from_csv(FILLER_REGEXES)
  }
  
  # TODO(Josh): give this variable a better name
  cleaned_odyssey_data <- raw_odyssey_data %>% 
    remove_empty_columns()
  
  ncols <- ncol(cleaned_odyssey_data)
  
  if (! (ncols %in% c(5, 7, 8, 9))) {
    abort("Unrecognized number of columns.")
  }
  
  # treat counties with missing bond (e.g. Webb with 5 columns) as 8 columns
  if (ncols == 5) {
    cleaned_odyssey_data %<>% 
      mutate(`na_6` = NA,
             `na_7` = NA,
             `na_8` = NA)
  }
  
  # treat counties with address and missing bond (e.g. Kendall with 7 columns) as 9 columns
  if (ncols == 7) {
    cleaned_odyssey_data %<>% 
      mutate(`na_8` = NA,
             `na_9` = NA)
  }
  
  address_included <- ncols == 7
  
  is_wide <- (ncols == 9 || address_included)
  
  cleaned_odyssey_data %>% 
    split_into_individuals() %>%
    map_if(check_if_at_least_two_rows,
           make_charge_and_bond_rows,
           CHARGE_COL + is_wide + address_included, 
           BOND_COL + is_wide + address_included) %>% 
    bind_rows() %>% 
    remove_empty_columns() %>% 
    map_dfc(~ str_replace_all(.x, "@", " ")) %>% 
    split_race_sex()
}

split_race_sex <- function(df, race_regex = "^[BWH][MF]", 
                           n_sampled_rows = 1000, race_sex_threshold = 0.1) {
  
  # Validate the input.
  if (! is.data.frame(df)) {
    abort("Argument `df` to `remove_empty_columns` must be a data frame.")
  }
  
  if (! is_character(race_regex)) {
    abort("Argument `race_regex` to `split_race_sex` must be a string.")
  }
  
  n_rows <- nrow(df)
  
  if (n_rows < n_sampled_rows) {
    n_sampled_rows <- n_rows
  }
  
  race_sex_col_likelihood <- sample_n(df, n_rows) %>% 
    map(str_detect, race_regex) %>%
    map_dbl(mean, na.rm = T)
  
  if (max(race_sex_col_likelihood, na.rm = T) < race_sex_threshold) {
    return(df)
  }
  
  race_sex_col_name <- names(which.max(race_sex_col_likelihood))
  
  race_sex_col <- pull(df, race_sex_col_name)

  mutate(df,
         race = substr(race_sex_col, 1, 1),
         sex = substr(race_sex_col, 2, 2)) %>% 
    select(-race_sex_col_name)
}

remove_empty_columns <- function(df, empty_threshold = 0.1) {

  # Validate the input.
  if (! is.data.frame(df)) {
    abort("Argument `df` to `remove_empty_columns` must be a data frame")
  }
  if (! (is_double(empty_threshold) && between(empty_threshold, 0, 1))) {
    abort(str_c("Argument `threshold` to `remove_empty_columns` must be a ",
                "double between 0 and 1."))
  }

  select_if(df, ~ mean(! is.na(.)) > empty_threshold)
}
    
remove_filler_rows_from_csv <- function(df, regexes) {
  
  # Validate the input.
  if (! is_character(regexes)) {
    abort(str_c("Argument `regexes` to `remove_filler_rows_from_csv` must be ",
                "a character string."))
  }

  first_col_contents <- pull(df, 1)

  filter(df, is.na(first_col_contents) |
           ! str_detect(first_col_contents, regexes))
}

split_into_individuals <- function (df) {
  # Given a data frame generated from an Odyssey Booking Report, split the 
  # df into a list of dfs containing all charge information for each SONumber
  
  # Validate the input.
  if (! is.data.frame(df)) {
    abort("Argument `df` to `split_into_individuals` must be a data frame")
  }
  
  # assumption: first column always contains so_number, name, or NA
  # assumption: so_number contains at least three digits, and name never does
  # problem: Iturrubiate-Guerrero has an SONumber at end of name...
  contains_so_number <- str_detect(pull(df, 1), "[:digit:]{3,}") %>% 
    replace_na(replace = FALSE)
  
  contains_name <- str_detect(pull(df, 1), ", ") %>% 
    replace_na(replace = FALSE)
  
  contains_only_so_number <- which(contains_so_number & (! contains_name))
  
  map2(
    .x = contains_only_so_number,
    .y = c(contains_only_so_number[-1] - 1, nrow(df)),
    .f = ~ df[.x:.y, ]
  )
}

# Add a function to copy Gregg county shared names to each date

check_if_at_least_two_rows <- function(df) {
  at_least_two_rows <- nrow(df) >= 2
  
  if (! at_least_two_rows) {
    warning(str_c("Less than two rows detected. Identifier: ", 
                  df[[1, 1]],
                  " ",
                  df[[1, 2]],
                  " ",
                  df[[1, 3]]))
    return(FALSE)
  }
  
  TRUE
}

make_charge_and_bond_rows <- function(df, charge_col, bond_col) {
  # Given a data frame consisting of all rows for a given SONumber, 
  # generate a data frame with one row for each charge/bond pair, with each
  # row containing the same non-charge/bond info
  #
  # NOTE: Passing in col indices is hacky. Should use classify.R fxns to find 
  # charge/bond column indices
  
  # Validate the input.
  if (! is.data.frame(df)) {
    abort(str_c("Argument `df` to `make_rows_for_each_charge_and_bond` must ",
                "be a data frame"))
  }
  
  if (nrow(df) < 2) {
    print(df)
    abort(str_c("Argument `df` to `make_rows_for_each_charge_and_bond` must ",
                "contain at least 2 rows."))
  }
  
  if(!is_integer(charge_col) || !is_integer(bond_col) || 
     charge_col < 1 || bond_col < 1) {
    abort(str_c("Arguments `charge_col` and `bond_col` to ",
                "`make_rows_for_each_charge_and_bond` must be integers ",
                "greater than 0."))
  }
  
  # TODO(Josh): Incredibly hacky, fix this
  if (ncol(df) < bond_col) {
  warning("Bond column missing, interpreting as NA.")
  charges_and_bond <- select(df, charge = charge_col) %>% 
    mutate(bond = NA)
  }
  else {
    charges_and_bond <- select(df, charge = charge_col, bond = bond_col)
  }
  
  charges_and_bond %<>% filter(! (is.na(charge) & is.na(bond)))
  
  if (nrow(charges_and_bond) == 0) {
    warning(str_c("No charges or bond detected. Identifier: ", df[[1, 1]]))
    charges_and_bond <- tibble(charge = NA, bond = NA)
  }
  
  # A row can be inserted between SONumber (always 1st row) and name, so
  # make sure to pull name info from the right row (see Johnson raw_csv)
  row_containing_name <- pull(df, 1) %>% 
    str_detect(",") %>% 
    which() %>% 
    min()
  
  if (is.na(row_containing_name) || is.infinite(row_containing_name)) {
    row_containing_name <- 2
  }
  
  # extract all columns before charge/bond and concatenate the rows
  # NOTE: all valid information contained in first two rows
  shared_info <- select(df, 1:(charge_col - 1)) %>% 
    slice(c(1, row_containing_name)) %>% 
    pmap(c) %>% 
    unlist()
  
  names(shared_info) <- str_c("col", 1:length(shared_info))
  
  # bind the shared non-charge/bond info to each charge/bond pair
  # note: bind_rows applied to a named vector generates a one-row tibble
  bind_rows(shared_info) %>% 
    cbind(charges_and_bond) %>% 
    as_tibble()
}

################################# DEPRECATED ###################################

# rows_as_list <- pmap(df, c, use.names = F)
# 
# rows_for_each_so_number <- map2(
#   contains_so_number,
#   c(contains_so_number[-1] - 1, nrow(df)),
#   ~ rows_as_list[.x:.y])