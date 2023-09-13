# Functions for removing private information from data.

# Load in the required libraries
library(rprojroot)
library(fs)
ROOT <- path(find_root(has_file(".gitignore")))
source(path(ROOT, "lib", "common.R"))

################################## SANITIZING ##################################

sanitize <- function(d) {
  # Function for sanitizing data.

  # Extract the old data and metadata.
  metadata <- d$metadata
  data <- d$data

  # Replace possible SSN in free text fields
  if ("note" %in% colnames(data)) {
    note <- data$note
    sanitized_note <- note %>%
      str_replace_all(
        "\\d{3}[ ,/\\.-\\\\]?\\d{2}[ ,/\\.-\\\\]\\d{4}",
        "---------"
      )

    data$note <- sanitized_note

    # record differences in the metadata
    metadata[["sanitize"]] <- tibble(
        note,
        sanitized_note,
        data$raw_row_number
      ) %>%
      filter(note != sanitized_note)
  }

  list(
    data = data,
    metadata = metadata
  )
}
