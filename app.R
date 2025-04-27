# Load required packages
library(shiny)
library(shinyMobile)

# Define UI for the workout tracker app with a compact mobile layout
ui <- f7Page(
  title = "Workout Tracker",
  options = list(theme = 'ios'),
  f7TabLayout(
    panels = tagList(
      f7Panel(
        title = "Quit",
        side = "right",
        effect = "cover",
        f7Block(
          f7Button(inputId = "quit", icon = f7Icon("xmark_circle_fill"), color = "red", fill = TRUE)
        )
      )
    ),
    navbar = f7Navbar(title = "Workout Tracker", rightPanel = TRUE),
    f7Tabs(
      id = "tabset",
      animated = TRUE,
      style = "toolbar",
      # new_set entry tab with compact grid-based inputs
      f7Tab(
        tabName = "new_set",
        title = "New Set",
        icon = f7Icon("text_badge_plus"),
        active = TRUE,
        f7List(
          inset = FALSE,
          # Grid of inputs
          f7Grid(
            cols = 2,
            gap = TRUE,
            f7DatePicker(inputId = "date", label = NULL, placeholder = as.character(Sys.Date())),
            f7Toggle(inputId = "auto_inc", label = ""),
            f7Select(
              inputId = "exercise",
              label = NULL,
              choices = c("pullup", "dip", "pushup", "run", "lsit", "chinup", "Other"),
              selected = "pullup"
            ),
            f7Text(inputId = "set", label = NULL, placeholder = "Set"),
            f7Text(inputId = "reps", label = NULL, placeholder = "Reps/Time"),
            f7Text(inputId = "resistance", label = NULL, placeholder = "Resistance"),
            f7Select(
              inputId = "unit",
              label = NULL,
              choices = c("lbs", "Kg", "Metres", "Km", "Sec", "Min", "Other"),
              selected = "lbs"
            )
          ),
          # Custom unit input when needed
          conditionalPanel(
            condition = "input.unit == 'Other'",
            f7Text(inputId = "unit_other", label = NULL, placeholder = "Unit")
          ),
          # Effort slider and add button
          f7Slider(
            inputId = "effort",
            label = NULL,
            min = 1, max = 10,
            value = 5, step = 1,
            scale = TRUE, scaleSteps = 9, scaleSubSteps = 0
          ),
          f7Button(inputId = "add_set", label = "Add Set", icon = f7Icon("plus"), fill = TRUE)
        )
      ),
      # workout tab
      f7Tab(
        tabName = "workout",
        title = "Workout",
        icon = f7Icon("play"), # "play_pause"
        f7Block(
          inset = TRUE,
          strong = TRUE,
          tableOutput("workout_table")
        ),
        f7Card(
          f7Button(inputId = "end_workout", label = "End Workout", icon = f7Icon("flag_circle_fill"))
        )
      ),
      f7Tab(
        tabName = "history", 
        title = "History",
        icon = f7Icon("timer"),
        f7Card(
          
        )
      )
    )
  )
)

# Define server logic
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
    req(input$date, input$exercise, nzchar(input$set), input$reps, input$resistance, input$unit)
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
    
    if (isTRUE(input$auto_inc)) {
      next_set <- as.numeric(input$set) + 1
      # Correct update function signatures
      updateF7Text("set", value = as.character(next_set), session = session)
    } else {
      updateF7Select("exercise", selected = "pullup", session = session)
      updateF7Text("set", value = "", session = session)
      updateF7Text("reps", value = "", session = session)
      updateF7Text("resistance", value = "", session = session)
      updateF7Select("unit", selected = "lbs", session = session)
      updateF7Text("unit_other", value = "", session = session)
      updateF7Slider("effort", value = 5, session = session)
    }
  })
  
  observeEvent(input$quit, stopApp())
  
  output$workout_table <- renderTable({
    workout_log$df[, c("workout", "set", "reps", "resistance")]
  })
  
  observeEvent(input$end_workout, {
    f7Dialog(
      id = "end_confirm",
      title = "End workout?",
      type = "confirm",
      text = "Click confirm to end and save workout",
    )
  })
  
  observeEvent(input$end_confirm, {
    print(input$end_confirm)
    if(input$end_confirm == TRUE){
      f7Notif(
        text = "Your workout has been saved",
        icon = f7Icon("bolt_fill"),
        title = "Notification",
        titleRightText = "now"
      )
    } else {
      f7Toast(text = paste("Canceled ending workout"))
    }

  })
}

# Run the Shiny app
shinyApp(ui, server)
