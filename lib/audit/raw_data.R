# Responsible for the raw data output in the auditing app.
# 
# The selectors and other inputs for the raw data need to be isolated in their
# own namespace to prevent collisions.

##################################### UI #######################################

raw_dataUI <- function(id) {
  ns <- NS(id)
  
  uiOutput(outputId = ns("raw_data"))
}

################################### SERVER #####################################

raw_data <- function(
  input,
  output,
  session,
  state,
  subgeo,
  data,
  config
) {
  # NB: if the namespace gets updated, this has to change.
  ns <- NS(str_c(state, subgeo))
  
  output$raw_data <- renderUI(render_raw_data(data, config, input, session, ns))
  
  # Return the current selections as a configuration file
  tibble(
    type_guess = extract_reactive_values(input, "_type_guess"),
    subtype_guess = extract_reactive_values(input, "_subtype_guess"),
    keep = extract_reactive_values(input, "_keep")
  ) %>% 
    mutate(raw_col_number = row_number()) %>%
    select(raw_col_number, everything())
}

#################################### UTIL ######################################

render_raw_data <- function(data, config, input, session, ns) {
  # Have to ensure that the config makes sense with the data input
  if (nrow(config) > ncol(data)) {
    config <- slice(config, seq(ncol(data)))
  } else if (nrow(config) < ncol(data)) {
    diff = ncol(data) - nrow(config)
    config <- add_row(
      .data=config,
      raw_col_number=seq(nrow(config) + 1, ncol(data)),
      type_guess=rep(NA, diff),
      subtype_guess=rep(NA, diff),
      keep=rep(FALSE, diff)
    )
  }
  
  # Render one row for each column of data
  pmap(
    # Create a value for `col_name`, `col_content`, `raw_col_numer`, 
    #`type_guess` and `keep` for each call.
    .l = left_join(transpose_tibble(data), config, by = "raw_col_number"),
    .f = render_column,
    input = input,
    session = session,
    ns = ns
  ) %>%
    tagList()
}

render_column <- function(
  col_name,
  raw_col_number,
  col_content,
  type_guess,
  subtype_guess,
  keep,
  input,
  session,
  ns
) {
  # Add an observer to update the subtype slider
  observeEvent(
    eventExpr = input[[str_c(raw_col_number, "_type_guess")]],
    handlerExpr = updateSelectInput(
      session = session,
      inputId = str_c(raw_col_number, "_subtype_guess"),
      label = "Column subtype",
      choices = with(
        data = filter(
          .data = SCHEMA,
          type == input[[str_c(raw_col_number, "_type_guess")]]
        ),
        expr = subtype
      ),
      selected = {
        choices <- with(
          data = filter(
            .data = SCHEMA,
            type == input[[str_c(raw_col_number, "_type_guess")]]
          ),
          expr = subtype
        )
        if (any(subtype_guess %in% choices) & ! is_na(choices)) {
          subtype_guess
        } else {
          NULL
        }
      }
    )
  )
  
  # Return the html for the row
  fluidRow(
    # `type_guess` and `keep` inputs.
    column(
      width = 6,
      # Column name heading
      h3(str_c("Column name: ", "\"", col_name, "\"")),
      # `type_guess` selector
      selectInput(
        inputId = ns(str_c(raw_col_number, "_type_guess")),
        label = "Column type",
        choices = unique(SCHEMA$type),
        selected = type_guess
      ),
      selectInput(
        inputId = ns(str_c(raw_col_number, "_subtype_guess")),
        label = "Column subtype",
        choices = filter(SCHEMA, type == type_guess)$subtype,
        selected = subtype_guess
      ),
      # `keep` button
      checkboxInput(
        inputId = ns(str_c(raw_col_number, "_", "keep")),
        label = "Keep this column in the clean data?",
        value = keep
      )
    ),
    column(
      width = 6,
      DT::renderDataTable(as_tibble(col_content))
    )
  )
}

extract_reactive_values <- function(input, type) {
  # Extracts the `keep` or `type_guess` values from the `input` (reactive)
  # object. Returns the result as a logical vector _in order_ of the raw column
  # number. Expects values of type `type` to be stored in indices named 
  # according to the following format:
  #    "[RAW_COL_NUMBER]_[TYPE]"
  
  # Convert from reactive values to list
  input_list <- reactiveValuesToList(input)
  
  # Get the names of the indices storing the variables of the given type
  type_names <- input_list %>%
    `[`(ends_with(type, ignore.case = FALSE, vars = names(.))) %>%
    names() %>%
    `[`(order(as.integer(str_extract(., "^\\d+"))))
  
  # Return
  input_list[type_names] %>%
    flatten_chr()
}
