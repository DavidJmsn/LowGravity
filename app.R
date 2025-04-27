# Load required packages
library(shiny)
library(shinyMobile)

# Define UI for the workout tracker app
ui <- f7Page(
  title = "Workout Tracker",
  options = list(theme = 'ios'),
  f7SingleLayout(
    navbar = f7Navbar(
      title = "Workout Tracker",
      rightPanel = TRUE
    ),
    f7Panel(
      title = "Quit",
      side = "right",
      effect = "floating",
      f7Block(
        # Quit button on top-right
        f7Button(
          inputId = "quit",
          icon = f7Icon("xmark_circle"),
          color = "red",
          fill = FALSE
        )
      )
    ),
    # Compact input block: grid with 2 columns
    f7Block(
      strong = TRUE,
      inset = TRUE,
      f7Grid(
        cols = 2, gap = TRUE,
        f7Text(
          inputId = "date",
          label = "Date",  # COMMENT: consider f7DatePicker for built-in validation
          value = as.character(Sys.Date()),
          placeholder = "YYYY-MM-DD"
        ),
        f7Text(
          inputId = "exercise",
          label = "Exercise",
          placeholder = "e.g., pullup, run"
        ),
        f7Text(
          inputId = "set",
          label = "Set",
          placeholder = "e.g., 1, 2.5"
        ),
        f7Text(
          inputId = "reps",
          label = "Reps / Time",
          placeholder = "e.g., 12, 7:43"
        ),
        f7Text(
          inputId = "resistance",
          label = "Resistance",
          placeholder = "e.g., 50"
        ),
        f7Select(
          inputId = "unit",
          label = "Units",
          choices = c("lbs", "Kg", "Metres", "Km", "Sec", "Min", "Other"),
          selected = "lbs"
        ),
        conditionalPanel(
          condition = "input.unit == 'Other'",
          f7Text(
            inputId = "unit_other",
            placeholder = "Specify unit"
          )
        )  # COMMENT: validate unit_other when 'Other' is selected to avoid empty unit
      ),
      f7Slider(
        inputId = "effort",
        label = "Effort",
        min = 1,
        max = 10,
        value = 5,
        step = 1,
        scale = TRUE,
        scaleSteps = 9,
        scaleSubSteps = 0
      ),
      f7Button(
        inputId = "add_set",
        label = "Add Set",
        icon = f7Icon("plus"),
        fill = TRUE
      )
    ),
    # Display current workout log
    f7Block(
      strong = TRUE,
      inset = TRUE,
      tableOutput("workout_table")  # COMMENT: consider DT::renderDataTable for interactivity
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # COMMENT: rbind on reactiveValues can be inefficient for many rows; consider reactiveVal(list()) or data.table
  workout_log <- reactiveValues(
    df = data.frame(
      date       = as.Date(character()),
      workout    = character(),
      set        = numeric(),
      reps       = character(),
      resistance = character(),
      unit       = character(),
      effort     = numeric(),
      stringsAsFactors = FALSE
    )
  )
  
  observeEvent(input$add_set, {
    req(
      input$date,
      input$exercise,
      nzchar(input$set),
      input$reps,
      input$resistance,
      input$unit
    )
    unit_value <- if (input$unit == "Other") input$unit_other else input$unit
    
    # Create and append new entry
    new_row <- data.frame(
      date       = as.Date(input$date),  # COMMENT: wrap in tryCatch to catch invalid dates
      workout    = input$exercise,
      set        = as.numeric(input$set),
      reps       = input$reps,
      resistance = input$resistance,
      unit       = unit_value,
      effort     = input$effort,
      stringsAsFactors = FALSE
    )
    workout_log$df <- rbind(workout_log$df, new_row)
    
    # Reset inputs
    updateF7Text(session, "exercise", value = "")
    updateF7Text(session, "set", value = "")
    updateF7Text(session, "reps", value = "")
    updateF7Text(session, "resistance", value = "")
    updateF7Select(session, "unit", selected = "lbs")
    updateF7Text(session, "unit_other", value = "")
    updateF7Slider(session, "effort", value = 5)
  })
  
  observeEvent(input$quit, stopApp())
  
  output$workout_table <- renderTable({
    workout_log$df[, c("workout", "set", "reps", "resistance")]
  })
}

# Run the Shiny app
shinyApp(ui, server)
