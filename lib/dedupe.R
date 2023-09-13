# Functions for deduplicating rows which replicate bookings.

# Load in the required libraries
library(rprojroot)
library(fs)
ROOT <- path(find_root(has_file(".gitignore")))
source(path(ROOT, "lib", "common.R"))

################################### GLOBALS ####################################

FIXED_COLS <- c(
  "first_name",
  "middle_name",
  "last_name",
  "suffix",
  "age",
  "dob",
  "sex",
  "race",
  "street",
  "city",
  "state",
  "zip",
  "booking_date",
  "release_date",
  "booking_time",
  "release_time",
  "booking_id"
)

CHARGE_COLS <- c(
  "charge",
  "statute",
  "severity"
)

############################### DEDEDUPLICATION ################################
# These functions try to intelligently deduplicate the jail booking logs, to
# ensure that each row corresponds to a single booking.

dedupe <- function(d, separator=NULL) {
  # Top level function for deduplicating data. Groups by data that should be
  # fixed for a given booking (name, age, sex, dob, etc.), and then collates the
  # other fields.

  # Extract the old metadata and data
  metadata <- d$metadata
  data <- d$data
  
  # Bundle the data and clean metadata for this stage.
  d <- list(data = data, metadata = list())

  # Validate the input.
  validate_input(d)

  # Record the group rows to see size of change.
  pre_group_rows <- nrow(d$data)

  # NOTE: `group_by` errors if given column names that don't appear in the
  # dataframe.
  fixed_cols <- FIXED_COLS[FIXED_COLS %in% colnames(d$data)]
  charge_cols <- CHARGE_COLS[CHARGE_COLS %in% colnames(d$data)]

  # Group by the `fixed_colnames`, `collate` everything else, and then return
  # the resulting dataset.
  d$data %<>% mutate(booking_id=md5(str_c_na(!!!syms(fixed_cols))))

  # Record split rows to see size of change.
  pre_split_rows <- nrow(d$data)

  # Split the charge rows if multiple charges are stored in each row.
  if (! is_null(separator)) {
    # Deduplicate the result.
    d$data %<>%
      mutate_at(charge_cols, str_split, pattern = separator) %>%
      mutate(
        lens = transpose(map(
          .x=list(!!!syms(charge_cols)),
          .f=~ map_int(., length)
        )),
        lens = map(lens, flatten_int),
        diff_len = ! map_lgl(lens, ~ all(. == as.integer(mean(.))))
      ) %>%
      mutate_at(
        .vars=charge_cols,
        .funs=~ map_if(.x=., .p=diff_len, .f= ~ vec_cast(NA, .))
      ) %>%
      select(-lens, -diff_len) %>%
      unnest_legacy() %>%
      mutate_at(charge_cols, str_trim) %>%
      filter_at(
        .vars=charge_cols,
        .vars_predicate=~ . != ""
      ) %>%
      # Only keep unique combos of the charge columns.
      mutate(charge_id = md5(str_c_na(!!!syms(charge_cols)))) %>%
      group_by(booking_id, charge_id) %>%
      summarize_all(first) %>%
      ungroup() %>%
      select(-charge_id)
  }

  # Calculate the number of rows collapsed and split
  rows_collapsed <- pre_group_rows - length(unique(d$data$booking_id))
  rows_split <- nrow(d$data) - pre_split_rows

  # Record the calculations in the metadata.
  metadata[["dedupe"]] <- list(
    result = d$data,
    stats = list(
      pre_num_rows = pre_group_rows,
      post_group_rows = nrow(d$data),
      collapsed_pct = rows_collapsed / pre_group_rows,
      split_pct = rows_split / rows_collapsed
    )
  )

  # Return the result.
  list(
    data = d$data,
    metadata = metadata
  )
}
