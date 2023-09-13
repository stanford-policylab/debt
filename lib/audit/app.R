# Creates the shiny app for auditing data

# Load in the required libraries
library(rprojroot)
library(fs)
ROOT <- path(find_root(has_file(".gitignore")))
source(path(ROOT, "lib", "common.R"))
source(path(ROOT, "lib", "debt.R"))

# Source the modules
source("raw_data.R")

################################## GLOBALS #####################################
COUNTY_LISTS <- dir_ls("/share/data/debt/states/") %>%
  map(dir_ls) %>%
  map(path_file) %>%
  set_names(path_file)
STATE_LIST <- names(COUNTY_LISTS)

##################################### UI #######################################

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      titlePanel("Auditing"),
      h3(textOutput(outputId = "location")),
      selectInput(inputId = "state", label = "state", choices = STATE_LIST),
      selectInput(inputId = "subgeo", label = "subgeo", choices = character(0)),
      actionButton(inputId = "classify", label = "classify"),
      actionButton(inputId = "load", label = "load"),
      textInput(
        inputId="charge_separator",
        label="Charge Separator",
        placeholder="Input the charge separator here.",
      ),
      textAreaInput(
        inputId="merge_config",
        label="Merge Instructions",
        value="",
        placeholder="Input the merge configuration here."
      ),
      htmlOutput(outputId="merge_config"),
      textAreaInput(
        inputId="split_config",
        label="Split Instructions",
        value="",
        placehold="Input the split configuration here."
      ),
      tableOutput(outputId="split_config"),
      actionButton(inputId = "submit", label = "submit")
    ),
    mainPanel(
      uiOutput(outputId = "table")
    )
  ),
  theme = "float.css"
)

################################### SERVER #####################################

server <- function(input, output, session) {
  # Render the county selection box
  observeEvent(
    eventExpr=input$state,
    handlerExpr=updateSelectInput(
      session=session,
      inputId="subgeo",
      label="subgeo",
      choices=COUNTY_LISTS[[input$state]]
    )
  )

  # Load in the three configuration files
  col_config <- reactive({
    input$load
    input$submit
    tryCatch(
      debt_load_col_config(input$state, input$subgeo),
      error=function(cond){}
    )
  })
  merge_config <- reactive({
    input$load
    input$submit
    valueExpr=tryCatch(
      debt_load_merge_config(input$state, input$subgeo),
      error=function(cond){}
    )
  })
  split_config <- reactive({
    input$load
    input$submit
    tryCatch(
      debt_load_split_config(input$state, input$subgeo),
      error=function(cond){}
    )
  })
  
  # Load in the correct county's data
  # NOTE: since reactives are lazily evaluated
  data <- reactive({
    input$submit
    tryCatch(
      # For faster loading, set `n_max = 100`
      debt_load_raw_dir(input$state, input$subgeo, n_max = 100)$data %>%
        apply_merge_config(merge_config()) %>%
        apply_split_config(split_config())
    )
  })
  
  # Update the textboxes and charge separator with the correct configs
  observeEvent(
    eventExpr=list(input$subgeo, input$load, input$submit),
    handlerExpr={
      tryCatch({
        updateTextAreaInput(
          session=session,
          inputId="merge_config",
          value=str_c(
            map_chr(
              merge_config(),
              str_c, collapse=", "
            ),
            collapse="\n"
          )
        )
        updateTextAreaInput(
          session=session,
          inputId="split_config",
          value = format_csv(split_config(), col_names = FALSE)
        )
        updateTextInput(
          session=session,
          inputId="charge_separator",
          # NOTE: Loading a NULL value borks the input.
          value = {
            charge_sep <- debt_load_charge_sep(input$state, input$subgeo)
            if (is_null(charge_sep)) {
              ""
            } else {
              charge_sep
            }
          }
        )
      },
      error=function(cond){}
      )
    }
  )

  # Let the audtior know what they are looking at
  output$location <- renderText({
    str_c(input$state, " --- ", input$subgeo)
  })

  # Create a space for the raw data table to appear
  output$table <- renderUI({
    raw_dataUI(str_c(input$state, input$subgeo))
  })
  
  # Create spaces for the merge and split configs to appear
  output$merge_config <- renderUI(HTML(str_c(
    map_chr(
      new_merge_config(),
      str_c, collapse=", "
    ),
    collapse="<br/>"
  )))
  output$split_config <- renderTable(new_split_config())

  # Export the current selections as a `new_config`
  new_col_config <- reactive({
    callModule(
      module = raw_data,
      id = str_c(input$state, input$subgeo),
      state = input$state,
      subgeo = input$subgeo,
      data = data(),
      config = col_config()
    )
  })
  new_merge_config <- reactive(map(
    .x = flatten_chr(str_split(input$merge_config, "\n")),
    .f = ~ parse_integer(flatten_chr(str_split(., ",")))
  ))
  new_split_config <- reactive(read_csv(
    file=str_c("split_pattern,split_number\n", input$split_config),
    col_types="ci"
  ))
  
  # Force reload of column configuration when auditor clicks load
  observeEvent(
    eventExpr=input$load,
    handlerExpr=new_col_config()
  )
  
  # Classify the current state / county on click
  observeEvent(
    eventExpr=input$classify,
    handlerExpr={
      debt_classify(input$state, input$subgeo, overwrite=T)
      showNotification(p(str_c("Classified ", input$state, ": ", input$subgeo)))
    }
  )
  
  # Write the information when the auditor clicks "submit"
  observeEvent(
    eventExpr = input$submit,
    handlerExpr = {
      # Write the config files
      debt_write_col_config(input$state, input$subgeo, new_col_config())
      debt_write_merge_config(input$state, input$subgeo, new_merge_config())
      debt_write_split_config(input$state, input$subgeo, new_split_config())
      if (input$charge_separator != "") {
        debt_write_charge_sep(input$state, input$subgeo, input$charge_separator)
      }

      # Flash a message stating that the save was successful
      showNotification(p(str_c("Configuration saved for ", input$state, ": ",
                               input$subgeo, ".")))
    }
  )
}

#################################### MAIN ######################################

shinyApp(ui = ui, server = server)
