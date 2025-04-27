# Load required packages
library(shiny)
library(shinyMobile)

# Define UI for the workout tracker app
ui <- f7Page(
  title = "Workout Tracker",
  theme = "ios",
  f7SingleLayout(
    navbar = f7Navbar(
      title = "Workout Tracker",
      # Quit button on top-right
      f7Button(
        inputId = "quit",
        icon = f7Icon("xmark_circle"),
        color = "red",
        fill = FALSE
      )
    ),
    # Input block
    f7Block(
      strong = TRUE,
      inset = TRUE,
      # Date input (text, default to today)
      f7Text(
        inputId = "date",
        label = "Date",
        value = as.character(Sys.Date()),
        placeholder = "YYYY-MM-DD"
      ),
      # Exercise name
      f7Text(
        inputId = "exercise",
        label = "Exercise",
        placeholder = "e.g., pullup, run"
      ),
      # Set number (text input for consistent UI)
      f7Text(
        inputId = "set",
        label = "Set",
        placeholder = "e.g., 1, 2.5"
      ),
      # Reps or time
      f7Text(
        inputId = "reps",
        label = "Reps / Time",
        placeholder = "e.g., 12, 7:43"
      ),
      # Resistance and Units in one row
      fluidRow(
        column(
          width = 6,
          f7Text(
            inputId = "resistance",
            label = "Resistance",
            placeholder = "e.g., 50"
          )
        ),
        column(
          width = 6,
          f7Select(
            inputId = "unit",
            label = "Units",
            choices = c("lbs", "Kg", "Metres", "Km", "Sec", "Min", "Other"),
            selected = "lbs"
          ),
          # Show custom unit text when "Other" is selected
          conditionalPanel(
            condition = "input.unit == 'Other'",
            textInput(
              inputId = "unit_other",
              label = NULL,
              placeholder = "Specify unit"
            )
          )
        )
      ),
      # Perceived effort with integer ticks
      f7Slider(
        inputId = "effort",
        label = "Perceived Effort",
        min = 1,
        max = 10,
        value = 5,
        step = 1,
        scale = TRUE,
        scaleSteps = 9,
        scaleSubSteps = 0
      ),
      # Add Set button
      f7Button(
        inputId = "add_set",
        label = "Add Set",
        icon = f7Icon("plus"),
        fill = TRUE
      )
    ),
    # Display current workout log with date column
    f7Block(
      strong = TRUE,
      inset = TRUE,
      tableOutput("workout_table")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Initialize reactiveValues to store the workout log
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
  
  # Observe Add Set button click and append a new row
  observeEvent(input$add_set, {
    req(input$date, input$exercise, nzchar(input$set), input$reps, input$resistance, input$unit)
    # Determine final unit (custom if Other)
    unit_value <- if (input$unit == "Other") input$unit_other else input$unit
    
    # Create new row, coercing inputs as needed
    new_row <- data.frame(
      date       = as.Date(input$date),
      workout    = input$exercise,
      set        = as.numeric(input$set),
      reps       = input$reps,
      resistance = input$resistance,
      unit       = unit_value,
      effort     = input$effort,
      stringsAsFactors = FALSE
    )
    
    # Append to log
    workout_log$df <- rbind(workout_log$df, new_row)
    
    # Clear inputs (date remains)
    updateF7Text(session, "exercise", value = "")
    updateF7Text(session, "set", value = "")
    updateF7Text(session, "reps", value = "")
    updateF7Text(session, "resistance", value = "")
    updateSelectInput(session, "unit", selected = "lbs")
    updateTextInput(session, "unit_other", value = "")
    updateF7Slider(session, "effort", value = 5)
  })
  
  # Quit button closes the app
  observeEvent(input$quit, stopApp())
  
  # Render the workout log as a table
  output$workout_table <- renderTable({
    workout_log$df
  })
}

# Run the Shiny app
shinyApp(ui, server)
