# Load required packages
library(shiny)
library(shinyMobile)

# Define UI for the workout tracker app
ui <- f7Page(
  title = "Workout Tracker",
  options = list(theme = 'ios'),
  f7SingleLayout(
    navbar = f7Navbar(title = "Workout Tracker", rightPanel = TRUE),
    f7Panel(
      title = "Quit",
      side = "right",
      effect = "floating",
      f7Block(
        f7Button(inputId = "quit", icon = f7Icon("xmark_circle"), color = "red", fill = FALSE)
      )
    ),
    # Main input block with reduced spacing
    f7Block(
      strong = TRUE,
      inset = FALSE,  # remove extra padding
      f7Grid(
        cols = 2,
        gap = FALSE,  # remove grid gaps for compact layout
        f7Text(inputId = "date",       label = "Date",       value = as.character(Sys.Date()),      placeholder = "YYYY-MM-DD"),
        f7Text(inputId = "exercise",   label = "Exercise",   placeholder = "e.g., pullup, run"),
        f7Text(inputId = "set",        label = "Set",        placeholder = "e.g., 1, 2.5"),
        f7Text(inputId = "reps",       label = "Reps / Time", placeholder = "e.g., 12, 7:43"),
        f7Text(inputId = "resistance", label = "Resistance", placeholder = "e.g., 50"),
        f7Select(
          inputId = "unit",
          label = "Units",
          choices = c("lbs", "Kg", "Metres", "Km", "Sec", "Min", "Other"),
          selected = "lbs"
        ),
        conditionalPanel(
          condition = "input.unit == 'Other'",
          f7Text(inputId = "unit_other", placeholder = "Specify unit")
        )
      ),
      # Keep buttons and sliders compact
      f7Slider(inputId = "effort", label = "Effort", min = 1, max = 10, value = 5, step = 1, scale = TRUE, scaleSteps = 9, scaleSubSteps = 0),
      f7Button(inputId = "add_set", label = "Add Set", icon = f7Icon("plus"), fill = TRUE)
    ),
    f7Block(strong = TRUE, inset = TRUE, tableOutput("workout_table"))
  )
)

# Define server logic (unchanged)
server <- function(input, output, session) {
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
    workout_log$df <- rbind(workout_log$df, new_row)
    
    # Reset inputs after adding
    updateF7Text(session = session, inputId = "exercise",    value = "")
    updateF7Text(session = session, inputId = "set",         value = "")
    updateF7Text(session = session, inputId = "reps",        value = "")
    updateF7Text(session = session, inputId = "resistance",  value = "")
    updateF7Select(session = session, inputId = "unit",      selected = "lbs")
    updateF7Text(session = session, inputId = "unit_other", value = "")
    updateF7Slider(session = session, inputId = "effort",    value = 5)
  })
  
  observeEvent(input$quit, stopApp())
  
  output$workout_table <- renderTable({
    workout_log$df[, c("workout", "set", "reps", "resistance")]
  })
}

# Run the Shiny app
shinyApp(ui, server)
