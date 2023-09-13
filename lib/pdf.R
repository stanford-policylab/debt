# Functions for normalizing data from pdfs.

# Load in the required libraries
library(rprojroot)
library(fs)
ROOT <- path(find_root(has_file(".gitignore")))
source(path(ROOT, "lib", "common.R"))

################################## EXTRACTION ##################################

extract_text <- function(lines, ..., num_fields = NULL) {
  # Extracts text from the given collection of lines. (The intended collection
  # of lines is the output of `pdftotext` run in table mode.)
  #
  # Optional arguments should be character vectors that define regexes that will
  # detect rows that should be dropped, e.g., "(?i)^page".
  #
  # If defined, the `num_fields` argument is an integer that overrides automatic
  # detection of the number of fields on each page.

  # Capture the regex inputs.
  regexes <- list(...)

  # Validate the input
  if (! is_character(lines)) {
    warn("Argument `lines` to `extract_text` must be a character vector.")
  }
  if (! all(map_lgl(regexes, is_character))) {
    warn("Optional arguments to `extract_text` must be character vectors.")
  }
  if (! is_null(num_fields) 
      & ! is_integer(num_fields)
      & length(num_fields) != 1
      & any(num_fields < 1)
  ) {
    warn("Argument `num_fields` must be a positive length one integer vector ")
  }
  
  # Accounts for pages of a PDF with only one inmate. Prevents name from being
  # read in as multiple columns. @ symbols replaced later on.
  lines <- str_replace(lines, "^(\\s*\\S+,)\\s(\\S+)\\s?(\\S+)?", "\\1@\\2@\\3")

  # Extract pages
  pages <- split_into_pages(lines) %>%
    map(remove_filler_rows, ...) %>%
    map(pad_page)

  # Find where fields might begin.
  possible_field_starts <- pages %>%
    map(pct_not_whitespace) %>%
    map(find_field_starts)

  # Guess the number of fields if not provided.
  if (is_null(num_fields)) {
    num_fields <- guess_num_fields(possible_field_starts)
  }

  # Guess the start indices for the fields.
  field_starts <- map(
    .x = possible_field_starts,
    .f = safely(guess_field_range),
    num_fields = num_fields
  ) %>%
    transpose()

  # Parse the pages.
  parsing_results <- map2(pages, field_starts$result, safely(parse_page)) %>%
    transpose()
  
  # Record problems.
  metadata <- list(field_starts = field_starts$error, parsing =
                   parsing_results$error)

  # Return.
  list(
       data = bind_rows(parsing_results$result),
       metadata = metadata
  )
}


pct_not_whitespace <- function(page) {
  # This function takes a representation of a page (i.e., a character vector
  # where each element is a row extracted from the pdf) and returns a double
  # containing the number of non-whitespace characters contained in that column.

  # Validate input.
  if (! is_character(page)) {
    abort("Argument `page` to `pct_not_whitespace` must be a character vector.")
  }

  # Extract the positions of non-whitespace characters.
  non_whitespace_chars <- str_locate_all(page, "[^\\s]") %>%
    map(~ .[, "start"])

  # Get the max length.
  max_length <- max(str_length(page))

  # For each index, calculate what percentage of rows have a non-whitespace
  # character at that index.
  map_dbl(
    .x = seq(1, max_length),
    .f = ~ mean(map_lgl(non_whitespace_chars, `%in%`, x = .x), na.rm = TRUE)
  )
}

find_field_starts <- function(pct_vct, threshold = 0.1) {
  # This function takes a vector of doubles and returns the indices of the
  # beginnings of the beginnings of colums (i.e., fields). The beginning of a
  # column is defined to be a non-whitespace index which follows two whitespace
  # indices.
  #
  # NOTE: This function determines which columns contain whitespace through the
  # use of a `threshold` argument.

  # Validate the input.
  if (! is_double(pct_vct)) {
    abort("Argument `pct_vct` to `guess_column_start` must be a double vector.")
  }
  if (! is_double(threshold) | length(threshold) != 1) {
    abort(str_c("Argument `threshold` to `guess_column_start` must be a ",
                "double vector of length 1."))
  }
  if (any(1 < pct_vct) | any(0 > pct_vct)) {
    abort(str_c("Argument `pct_vct` to `guess_column_start` must contain ",
                "only values between 0 and 1, inclusive."))
  }
  if (1 < threshold | 0 > threshold) {
    abort(str_c("Argument `threhold` to `guess_column_start` must be between ",
                "0 and 1."))
  }
  
  # Create a logical vector which is `TRUE` if an index is a non-whitespace
  # column following two whitespace columns.
  is_column_start <- (pct_vct > threshold
                      & lag(pct_vct, n = 1, default = 0) <= threshold
                      & lag(pct_vct, n = 2, default = 0) <= threshold)

  # Set everything that is not a possible column start to `-Inf`.
  if_else(is_column_start, pct_vct, -Inf)
}

guess_num_fields <- function(column_start_indices) {
  # Uses a list of column_starts (i.e., a list of integer vectors) to guess the
  # number of fields in a pdf. The guess, based on the assumption that _many_
  # pages will parse correctly, is the mode. In the event of a tie, it assumes
  # the larger, since rows may be underpopulated.

  # Get the number of fields from each page.
  num_parsed_fields <- map_int(column_start_indices, ~ sum(is.finite(.)))

  # Find the mode
  num_field_dist <- tabulate(num_parsed_fields)
  max(which(num_field_dist == max(num_field_dist)))
}

guess_field_range <- function(column_start_pct, num_fields) {
  # Calculate the threshold for column starts that are "good enough" (i.e., have
  # a high number of non-whitespace characters).

  # Validate input.
  if (!is_double(column_start_pct)) {
    abort(str_c("Argument `column_start_pct` to `guess_field_range` must be ",
                "a vector of doubles."))
  }
  if (!is_integer(num_fields) | length(num_fields) != 1L) {
    abort(str_c("Argument `num_fields` to `guess_field_range` must be a ",
                "single integer"))
  }

  # Get the threshold for inclusion.
  threshold <- sort(column_start_pct, decreasing = TRUE)[[num_fields]]

  # Extract indicies of column starts at or above the threshold.
  start_indices <- seq_along(column_start_pct)[column_start_pct >= threshold]

  # If the number of parsed fields is not correct, throw an error.
  if (length(start_indices) != num_fields) {
    abort("Unable to parse page; found incorrect number of columns.")
  }

  start_indices
}

remove_filler_rows <- function(page, ...) {
  # Given a page (i.e., a character vector), removes the filler rows. (Filler
  # rows are headers, footers, etc.) Empty rows are removed automatically.
  #
  # Optional arguments are regexes that detect filler rows. For instance "^page"
  # is a common filler row.

  # Capture optional args.
  regexes <- list(..., "^$")

  # Validate the input.
  if (! is_character(page)) {
    abort("Argument `page` to `remove_filler_rows` must be a character vector.")
  }
  if (! all(map_lgl(regexes, is_character))) {
    abort(str_c("Optional arguments passed to `remove_filler_rows` must be ",
                "character vectors."))
  }

  # Find the rows that are valid (i.e., do not match any of the regexes).
  valid_rows <- ! reduce(map(regexes, str_detect, string = page), `|`)

  # Return.
  page[valid_rows]
}

parse_page <- function(page, start_indices) {
  # Given a page and a set of indices indicating where fields should start,
  # attempts to parse the page (i.e., character vector) into fields. Checks to
  # ensure that fields are "reasonable," and returns `NA` for fields that are
  # incorrectly parsed.

  # Early return if `start_indices` is NULL.
  if (is_null(start_indices)) {
    return(NULL)
  }

  # Validate the input.
  if (! is_character(page)) {
    abort("Argument `page` to `parse_page` must be a character vector.")
  }
  if (! is_integer(start_indices)) {
    abort("Argument `start_indices` to `parse_page` must be an integer vector.")
  }

  # Calculate where the fields should end.
  end_indices <- append(start_indices[-1] - 1, max(str_length(page)))

  # Split the vectors at those indices and return the result as a dataframe.
  parsed_df <- pmap_dfc(
    .l = list(start_indices, end_indices),
    .f = str_sub,
    string = page
  )

  # Find the columns where the text spans a boundary.
  txt_across_boundary <- mutate_all(
    .tbl = parsed_df,
    .funs = funs(str_detect(., "[^\\s]$"))
  ) %>%
    # Last column ends at last non-whitespace by default, so need to eliminate
    # these.
    mutate_at(
      .vars = vars(last_col()),
      .funs = function(x) {FALSE}
    ) %>%
    list(
      .,
      # Shift to the right and add a column of `FALSE` at the beginning.
      mutate_at(lag(.), 1L, function(x) {FALSE})
    ) %>%
    # Or the columns to ensure that both sides of a misparsing become `NA`.
    pmap_dfc(`|`)
 
  # Issue a warning message containing the number of misparsings.
  num_unparseable <- sum(map_int(txt_across_boundary, sum, na.rm = TRUE))
  if (num_unparseable > 0) {
    warn(str_c("Found ",
               num_unparseable,
               " unparseable entries on this page."
    ))
  }

  # Return the dataframe with unparseable entries removed and whitespace
  # trimmed.
  pmap_dfc(
    .l = list(txt_across_boundary, parsed_df),
    .f = if_else,
    true = NA_character_,
    missing = NA_character_
  ) %>%
    mutate_all(str_squish) %>%
    mutate_all(funs(if_else(. != "", ., NA_character_, NA_character_)))
}

split_into_pages <- function(lines) {
  # Splits lines (a character vector) into pages (a list of character vectors)
  # using the newpage character, "\f".

  # Validate input.
  if (! is_character(lines)) {
    abort("Argument `lines` to `split_into_pages` must be a character vector.")
  }

  # `pdftotext` adds a "\f" to the very end, so we drop it.
  lines <- lines[-length(lines)]

  # Find the lines corresponding to the beginnings and ends of pages.
  is_page_start <- str_detect(lines, fixed("\f"))
  is_page_start[[1]] <- TRUE
  is_page_end <- lead(is_page_start, default = TRUE)
  page_start_lines <- seq(1, length(lines))[is_page_start]
  page_end_lines <- seq(1, length(lines))[is_page_end]

  # Add the subsets to the list.
  map2(
    .x = page_start_lines,
    .y = page_end_lines,
    .f = ~ lines[seq(.x, .y)]
  )
}

pad_page <- function(page) {
  # Function to render all of the lines on a page to be the same length. Expects
  # a `page` (i.e., character vector) argument.

  # Validate input
  if (! is_character(page)) {
    warn("Argument `page` to `pad_page` must be a character vector.")
  }

  # Find the length of the longest line.
  max_len <- max(str_length(page), na.rm = TRUE)

  # Pad the remaining lines to that length.
  str_pad(page, max_len, side = "right")
}


################################# DEPRECATED ###################################

# guess_number_of_fields <- function(document) {
#   # Takes a document (i.e., a list of _pages_, each of which is itself a
#   # character vector where each element is a row from the pdf) and attempts to
#   # guess the number of fields present in the data.
