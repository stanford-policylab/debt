# Implements the API-specific functions.

# Load in the required libraries
library(rprojroot)
library(fs)
ROOT <- path(find_root(has_file(".gitignore")))
source(path(ROOT, "lib", "common.R"))
source(path(ROOT, "lib", "classify.R"))
source(path(ROOT, "lib", "parse.R"))
source(path(ROOT, "lib", "dedupe.R"))
source(path(ROOT, "lib", "standardize.R"))
source(path(ROOT, "lib", "impute.R"))
source(path(ROOT, "lib", "sanitize.R"))

################################# FILE SYSTEM ##################################

debt_path <- function(state, subgeo, type = "", filename = "") {
  # Finds default paths for the debtors' prison project directory.

  # Validate the input.
  if (!is_character(state) | !is_character(subgeo) | !is_character(type) |
      !is_character(filename)
  ) {
    abort("Arguments to `debt_path` must be character vectors.")
  }
  if (is_null(state) | is_null(subgeo)) {
    abort(str_c(
      "Please pass at least a `state` and `subgeo` argument to `debt_path`."
    ))
  }

  # Sanitize the path tokens.
  path_tokens <- c("~", "debt", "data", "states", state, subgeo, type,
                   filename) %>%
    str_to_lower() %>%
    str_replace_all("\\s+", "_") %>%
    path_sanitize() %>%
    path_expand()

  exec(fs::path, !!!path_tokens)
}


debt_processable <- function() {
  # Fetches the states and subgeographies of all of the paths that can be
  # processed.

  dir_ls("~/debt/data/states") %>%
    dir_ls() %>%
    map(path, "config", c("col_config.csv", "merge_config", "split_config.csv")) %>%
    map_lgl(~ all(map_lgl(., file_exists))) %>%
    enframe() %>%
    mutate(
      state = map_chr(name, ~ path_split(.)[[1]][[7]]),
      subgeo = map_chr(name, ~ path_split(.)[[1]][[8]])
    ) %>%
    filter(value) %>%
    select(state, subgeo)
}


debt_cleaned <- function() {
  # Fetches the states and subgeographies of all the paths that have been
  # cleaned.

  dir_ls("~/debt/data/states") %>%
    dir_ls() %>%
    map(path, "clean", c("clean.csv", "clean.rds")) %>%
    map_lgl(~ all(map_lgl(., file_exists))) %>%
    enframe() %>%
    mutate(
      state = map_chr(name, ~ path_split(.)[[1]][[7]]),
      subgeo = map_chr(name, ~ path_split(.)[[1]][[8]])
    ) %>%
    filter(value) %>%
    select(state, subgeo) %>%
    filter(! state %in% c("ok", "de"))
}

################################## LOADING #####################################

debt_load_raw_file <- function(state, subgeo, filename, ..., random = FALSE) {
  # Loads a single file for processing. Optional parameters are passed on to
  # `read_csv`.
  #
  # NOTE: For testing purposes, if `random` is `TRUE`, a random file from the
  # given state and subgeography will be loaded. For instance,
  #     > load_file("tx", "el paso county")
  # will load one of the CSVs contained in `/el_paso_county/raw_csv` at random.

  # Combine to form the path
  filepath <- debt_path(state, subgeo, "raw_csv")

  # If `random` is `TRUE`, get a random file.
  if (random) {
    random_file <- sample(path_file(dir_ls(filepath)), 1)
    filepath <- path(filepath, random_file)
  } else {
    filepath %<>% path(filename)
  }

  # Read the resulting CSV
  data <- quietly(read_csv)(
      filepath,
      col_types = cols(.default = "c"),
      na = c("", "NA"),
      ...
    ) %>%
    extract2("result")

  # Record specs and any problems.
  metadata <- list()
  metadata[["load"]] <- list(
    spec = attr(data, "spec"),
    problems = problems(data)
  )

  # Return
  list(
    data = data,
    metadata = metadata
  )
}


debt_load_raw_dir <- function(state, subgeo, ...) {
  # A wrapper around `debt_load_raw_file` which loads an entire directory and
  # returns the resulting list. Loads the result in parallel. Optional
  # parameters are passed on to `read_csv`.

  # Combine to form the path to the directory.
  dirpath <- debt_path(state, subgeo, "raw_csv")

  # Get the filenames.
  filenames <- path_file(dir_ls(dirpath))

  # Load the files and bind the resulting rows.
  future_map(
      .x = filenames,
      .f = debt_load_raw_file,
      state = state,
      subgeo = subgeo,
      ...
    ) %>%
    transpose() %$%
    list(
      data = bind_rows(data),
      metadata = transpose(set_names(metadata, filenames))
    )
}


debt_load_col_config <- function(state, subgeo) {
  # Loads a configuration file from the appropriate directory and returns the 
  # result as a tibble.
  
  config <- read_csv(
    file = debt_path(state, subgeo, "config", "col_config.csv"),
    col_types = "iccl"
  )
  
  # Verify that the configuration file is valid
  if (any(colnames(config) != c("raw_col_number", "type_guess", "subtype_guess",
                                "keep"))
  ) {
    abort(str_c("Invalid column configuration. Column configuration files must",
                "have four columns: `raw_col_number`, `type_guess`,",
                "`subtype_guess`, and `keep`.",
                sep = " "
    ))
  }
  if (any(map2_lgl(
    .x = config$type_guess,
    .y = config$subtype_guess,
    .f = ~ ! .y %in% filter(SCHEMA, type == .x)$subtype
  ))) {
    warn(str_c("Invalid column config. Some subtype guesses do not match the",
                "corresponding type guess.",
                sep = " "
    ))
  }

  # Return
  config
}


debt_load_merge_config <- function(state, subgeo) {
  # Load the configuration file for merging columns from the appropriate
  # subdirectory, returning the result as a list of integer vectors.

  read_lines(
    file = debt_path(state, subgeo, "config", "merge_config")
  ) %>%
    map(~ parse_integer(flatten_chr(str_split(., ",\\s+"))))
}


debt_load_split_config <- function(state, subgeo) {
  # Load the configuration file for splitting columns from the appropraite
  # subdirectory, returning the result as a tibble.

  config <- read_csv(
    file = debt_path(state, subgeo, "config", "split_config.csv"),
    col_types = "?i"
  ) 

  # Verify that the file is valid
  if (any(colnames(config) != c("split_pattern", "split_number"))) {
    abort(str_c("Invalid splitting configuration. Splitting configuration",
                "files must have two columns: `split_pattern` and",
                "`split_number`.",
                sep = " "
    ))
  }
  
  config
}


debt_load_classified <- function(state, subgeo) {
  # Uses the classification configuration file to load the data from a given
  # location as a tibble for further processing.

  # Load the config file
  col_config <- debt_load_col_config(state, subgeo)
  split_config <- debt_load_split_config(state, subgeo)
  merge_config <- debt_load_merge_config(state, subgeo)

  # Load the raw data from the directory
  d <- debt_load_raw_dir(state, subgeo)

  # Update metadata with loading schema
  d$metadata[["classify"]] <- list(
    col_config = col_config,
    merge_config = merge_config,
    split_config = split_config
  )

  # Clean up data
  d$data %<>%
    apply_merge_config(merge_config) %>%
    apply_split_config(split_config) %>%
    transpose_tibble() %>%
    left_join(
      y = col_config,
      by = "raw_col_number"
    ) %>%
    filter(keep) %>%
    select(-keep)

  # Return
  d
}

debt_load_charge_sep <- function(state, subgeo) {
  # Loads the charge separator, if it exists; otherwise, returns NULL.

  path_ <- debt_path(state, subgeo, "config", "charge_sep")

  if (file_exists(path_)) {
    return(read_file(path_))
  } else {
    return(NULL)
  }
}

debt_load_clean <- function(state, subgeo, csv = FALSE) {
  if (csv) {
    read_csv(debt_path(state, subgeo, "clean", "clean.csv"))
  } else {
    read_rds(debt_path(state, subgeo, "clean", "clean.rds"))
  }
}

debt_load_clean_all <- function(csv = FALSE) {
  # Loads all clean dataframes

  states <- debt_cleaned()$state
  subgeos <- debt_cleaned()$subgeo

  res <- future_map2(states, subgeos, debt_load_clean, csv = csv)

  if (csv) {
    res <- pmap(
      .l = list(
        states = states,
        subgeos = subgeos,
        dfs = res
      ),
      .f = ~mutate(..3, state = ..1, county = ..2)
    ) %>%
      bind_rows()
  } else {
    res <- transpose(res)
    res$data <- pmap(
      .l = list(
        states = states,
        subgeos = subgeos,
        dfs = res$data
      ),
      .f = ~mutate(..3, geo_state = ..1, geo_county = ..2)
    ) %>%
    map(~ mutate(., booking_id = as.character(booking_id))) %>%
    bind_rows()
  }

  # Order the columns correctly.
  schema <- SCHEMA %>%
    transmute(
      col_name = pretty_colnames(type, subtype, both = FALSE),
      data_type = data_type
    ) %>%
    filter(
      ! map_lgl(data_type, is_null),
      col_name %in% colnames(res$data)
    )
  res$data %<>%
    select(geo_state, geo_county, !!!schema$col_name)

  # Return
  res
}

################################ PROCESSING ####################################

debt_process <- function(state, subgeo) {
  # Cleans a given state and subgeography

  # Load the charge separator, if there is one
  separator <- debt_load_charge_sep(state, subgeo)

  # Sanitize the state and county input
  state <- str_to_lower(state)
  subgeo <- str_to_lower(str_replace_all(subgeo, "\\s+", "_"))

  debt_load_classified(state, subgeo) %>%
    parse() %>%
    dedupe(separator = separator) %>%
    impute(state = state, county = subgeo) %>%
    standardize() %>%
    sanitize() %>%
    debt_write_clean(state = state, subgeo = subgeo)
}


debt_process_collection <- function(states, subgeos) {
  # Cleans the given states and subgeographies.

  if (length(states) != length(subgeos)) {
    abort("Arguments `states` and `subgeos` must have the same length.")
  }

  future_map2(
    .x = states,
    .y = subgeos,
    .f = safely(debt_process)
  )
}


debt_process_all <- function() {
  # Cleans all possible states and subgeographies.

  states <- debt_processable()$state
  subgeos <- debt_processable()$subgeo

  debt_process_collection(states, subgeos)
}


debt_classify <- function(state, subgeo, ..., overwrite = FALSE) {
  # Classifies the data for a given state and subgeography.

  # Load the raw data, and classify it. Save the result as a configuration file.
  col_config <- debt_load_raw_dir(state, subgeo, ...) %>%
    classify() %$%
    select(
      .data = metadata$classify$result,
      raw_col_number,
      type_guess,
      subtype_guess, 
      keep
    )

  # Get the paths where the configuration file should be saved.
  config_paths <- list(
    col="col_config.csv",
    merge="merge_config",
    split="split_config.csv"
  ) %>%
    map_chr(debt_path, state=state, subgeo=subgeo, type="config")

  # Don't overwrite an old configuration file unless overwrite is true.
  if (any(map_lgl(config_paths, file_exists)) & !overwrite) {
    abort(str_c("Configuration files at",
          config_paths[1],
          config_paths[2],
          "or",
          config_paths[3],
          "already exist. Bailing out.",
          sep = " "
    ))
  }

  # Write the result for col config, with defaults for merge and split
  debt_write_col_config(state, subgeo, col_config)
  debt_write_merge_config(state, subgeo, as_list(seq(nrow(col_config))))
  debt_write_split_config(
    state=state,
    subgeo=subgeo,
    config=tibble(
      split_pattern=rep(NA_character_, nrow(col_config)),
      split_number=rep(NA_character_, nrow(col_config))
    )
  )
}

################################## WRITING #####################################

debt_write_col_config <- function(state, subgeo, config) {
  # Writes a column configuration file to the appropriate directory as a csv.
  
  # Verify that the configuration file is valid
  if (any(colnames(config) != c("raw_col_number", "type_guess", "subtype_guess",
                                "keep"))
  ) {
    abort(str_c("Invalid column configuration. Column configuration files must",
                "have four columns: `raw_col_number`, `type_guess`,",
                "`subtype_guess`, and `keep`",
                sep = " "
    ))
  }

  # Write the configuration file
  write_csv(
    config,
    path = debt_path(state, subgeo, "config", "col_config.csv")
  )
}

debt_write_split_config <- function(state, subgeo, config) {
  # Writes a split configuration file to the appropriate directory as a CSV.
  
  # Verify that the file is valid
  if (any(colnames(config) != c("split_pattern", "split_number"))) {
    abort(str_c("Invalid splitting configuration. Splitting configuration",
                "files must have two columns: `split_pattern` and",
                "`split_number`.",
                sep=" "
    ))
  }

  write_csv(
    config,
    path=debt_path(state, subgeo, "config", "split_config.csv")
  )
}


debt_write_merge_config <- function(state, subgeo, config) {
  # Writes a merge configuration file to the appropriate directory as a
  # plaintext file. Each line represents a "merge," and so consists of a series
  # of comma-separated integers.

  # Verify that the merge config is valid
  if (any(! map_lgl(config, is_integer))) {
    abort(str_c("Invalid merging configuration. Merging configurations must",
                "be a list of integer vectors.",
                sep=" "
    ))
  }
  config <- map(config, ~ .[!are_na(.)])

  write_lines(
    x=map_chr(config, ~ if(length(.) > 0) {str_c(., collapse=", ")} else {""}),
    path=debt_path(state, subgeo, "config", "merge_config")
  )
}


debt_write_charge_sep <- function(state, subgeo, separator) {
  # Writes the charge separator to a designated file.

  path_ <- debt_path(state, subgeo, "config", "charge_sep")

  if (is_character(separator) & length(separator) == 1) {
    # Only write the separator if it's not ""
    if (separator != "") {
      write_file(separator, path_)
    }
  } else {
    abort("Invalid charge separator. Config not written.")
  }
}

debt_write_clean <- function(state, subgeo, d) {
  # Writes a clean tibble and its metadata to a designated file.

  # Write internal CSV and RDS
  path_rds <- debt_path(state, subgeo, "clean", "clean.rds")
  path_csv <- debt_path(state, subgeo, "clean", "clean.csv")

  write_rds(d, path_rds, compress = "none")
  write_csv(d$data, path_csv)

  # Write public CSV and RDS
  public_path_rds <- debt_path(state, subgeo, "clean", "public_clean.rds")
  public_path_csv <- debt_path(state, subgeo, "clean", "public_clean.csv")

  public_columns <- SCHEMA %>%
    mutate(col_name = pretty_colnames(type, subtype, both = FALSE)) %>%
    filter(public) %>%
    pull(col_name)

  d_public <- d$data %>%
    select(any_of(public_columns))

  write_rds(d_public, public_path_rds, compress = "gz")
  write_csv(d_public, public_path_csv)
}
