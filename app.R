library(shiny)
library(shinyMobile)
library(bigrquery)
library(data.table)
library(gargle)

# Launch in browser by default
theme_ios <- list(theme = 'ios')
options(shiny.launch.browser = TRUE)

# Authentication setup
project_id <- Sys.getenv("project_id")
ds <- Sys.getenv("dataset")
# dataset <- paste0(project_id, ".", ds)
dataset <- bq_dataset(project_id, ds)
bigrquery::bq_deauth()
service_json <- Sys.getenv("service_account")
token <- gargle::credentials_service_account(
  path = service_json,
  scopes = c(
    "https://www.googleapis.com/auth/userinfo.email",
    "https://www.googleapis.com/auth/bigquery"
  )
)
bigrquery::bq_auth(token = token)

# UI definition
ui <- f7Page(
  title   = "Low Gravity",
  options = theme_ios,
  f7TabLayout(
    panels = f7Panel(
      title  = "Quit",
      side   = "right",
      effect = "cover",
      f7Block(
        f7Button(
          inputId = "quit",
          icon    = f7Icon("xmark_circle_fill"),
          color   = "red",
          fill    = TRUE
        )
      )
    ),
    navbar = f7Navbar(title = "Low Gravity", rightPanel = TRUE),
    f7Tabs(
      id        = "tabset",
      animated  = F,
      style     = "toolbar",
      # swipeable = TRUE,
      # New Set Tab
      f7Tab(
        tabName = "new_set", title = "New Set", icon = f7Icon("text_badge_plus"), active = TRUE,
        f7List(
          f7Grid(
            cols = 2, gap = TRUE,
            f7DatePicker("date", NULL, placeholder = as.character(Sys.Date())),
            f7Toggle("auto_inc", "Keep inputs"),
            f7Text("workout_name", NULL, placeholder = "Workout Name"),
            f7Select(
              "exercise", NULL,
              choices  = c("pullup","dip","pushup","run","lsit","chinup","Other"),
              selected = "pullup"
            ),
            f7Text("set", NULL, placeholder = "Set"),
            f7Text("reps", NULL, placeholder = "Reps/Time"),
            f7Text("resistance", NULL, placeholder = "Resistance"),
            f7Select(
              "unit", NULL,
              choices  = c("lbs","Kg","Metres","Km","Sec","Min","Other"),
              selected = "lbs"
            )
          ),
          conditionalPanel("input.unit == 'Other'",
                           f7Text("unit_other", NULL, placeholder = "Unit")
          ),
          f7Slider("effort", "Effort", 1, 10, value = 5, step = 1,
                   scale = TRUE, scaleSteps = 9, scaleSubSteps = 0
          ),
          f7Button("add_set", "Add Set", icon = f7Icon("plus"), fill = TRUE)
        )
      ),
      # Workout Tab
      f7Tab(
        tabName = "workout", title = "Workout", icon = f7Icon("play"),
        f7Block(inset = TRUE, strong = TRUE, 
                uiOutput("workout_table_ui")
        ),
        f7Card(
          f7Button("end_workout", "End Workout", icon = f7Icon("flag_circle_fill")),
          f7Button("edit_workout", "Edit", icon = f7Icon("pencil"), color = "blue", fill = FALSE)
        )
      ),
      # History Tab
      f7Tab(
        tabName = "history", title = "History", icon = f7Icon("timer"),
        f7Card(uiOutput("old_workouts_ui"))
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Use data.table for efficient row binding
  workout_log <- reactiveVal(setDT(list(
    date         = as.Date(character()),
    workout_name = character(),
    workout      = character(),
    set          = numeric(),
    reps         = character(),
    resistance   = character(),
    unit         = character(),
    effort       = numeric()
  )))
  
  # Edit mode toggle
  edit_mode <- reactiveVal(FALSE)
  
  # Store the workouts history data
  workouts_history <- reactiveVal(NULL)
  
  # Store the selected workout data for the popup
  selected_workout_data <- reactiveVal(NULL)
  
  observeEvent(input$add_set, {
    # Validate required inputs
    req(
      input$date, input$exercise,
      nzchar(input$set), nzchar(input$reps),
      nzchar(input$resistance), nzchar(input$unit)
    )
    
    # Determine unit value
    unit_val <- if (input$unit == "Other") input$unit_other else input$unit
    
    # Build and append new row
    new_row <- data.table(
      date         = as.Date(input$date),
      workout_name = input$workout_name,
      workout      = input$exercise,
      set          = as.numeric(input$set),
      reps         = input$reps,
      resistance   = input$resistance,
      unit         = unit_val,
      effort       = input$effort
    )
    workout_log(rbind(workout_log(), new_row))
    
    # Reset or auto-increment inputs
    if (isTRUE(input$auto_inc)) {
      updateF7Text(
        "set", value = as.character(as.numeric(input$set) + 1),
        session = session
      )
    } else {
      # Reset all inputs in one loop
      to_reset <- list(
        list(fn = updateF7Select, args = list("exercise", selected = "pullup")),
        list(fn = updateF7Text,   args = list("set",        value = "")),
        list(fn = updateF7Text,   args = list("reps",       value = "")),
        list(fn = updateF7Text,   args = list("resistance", value = "")),
        list(fn = updateF7Select, args = list("unit",       selected = "lbs")),
        list(fn = updateF7Text,   args = list("unit_other", value = "")),
        list(fn = updateF7Slider, args = list("effort",     value = 5))
      )
      lapply(to_reset, function(x) do.call(x$fn, c(x$args, list(session = session))))
    }
  })
  
  observeEvent(input$quit, stopApp())
  
  # Toggle edit mode
  observeEvent(input$edit_workout, {
    edit_mode(!edit_mode())
    if (edit_mode()) {
      f7Toast(text = "Edit mode enabled")
    } else {
      f7Toast(text = "Edit mode disabled")
    }
  })
  
  # Create observers for each row's buttons dynamically
  observe({
    log_data <- workout_log()
    if (nrow(log_data) > 0 && edit_mode()) {
      lapply(1:nrow(log_data), function(i) {
        # Delete button for this row
        observeEvent(input[[paste0("delete_row_", i)]], {
          log_data <- workout_log()
          if (i <= nrow(log_data)) {
            log_data <- log_data[-i]
            workout_log(log_data)
            f7Toast(text = "Row deleted")
          }
        }, ignoreInit = TRUE, ignoreNULL = TRUE)
        
        # Move up button for this row
        observeEvent(input[[paste0("move_up_", i)]], {
          log_data <- workout_log()
          if (nrow(log_data) > 1 && i > 1 && i <= nrow(log_data)) {
            # Swap rows
            temp_row <- log_data[i]
            log_data[i] <- log_data[i - 1]
            log_data[i - 1] <- temp_row
            workout_log(log_data)
            f7Toast(text = "Row moved up")
          }
        }, ignoreInit = TRUE, ignoreNULL = TRUE)
        
        # Move down button for this row
        observeEvent(input[[paste0("move_down_", i)]], {
          log_data <- workout_log()
          if (nrow(log_data) > 1 && i < nrow(log_data)) {
            # Swap rows
            temp_row <- log_data[i]
            log_data[i] <- log_data[i + 1]
            log_data[i + 1] <- temp_row
            workout_log(log_data)
            f7Toast(text = "Row moved down")
          }
        }, ignoreInit = TRUE, ignoreNULL = TRUE)
      })
    }
  })
  
  # Dynamic table UI with edit controls
  output$workout_table_ui <- renderUI({
    log_data <- workout_log()
    if (nrow(log_data) == 0) {
      return(p("No workout data yet"))
    }
    
    if (edit_mode()) {
      # Edit mode table with action buttons
      tagList(
        tags$table(
          class = "table",
          tags$thead(
            tags$tr(
              tags$th("Workout"),
              tags$th("Set"),
              tags$th("Reps"),
              tags$th("Resistance"),
              tags$th("Actions")
            )
          ),
          tags$tbody(
            lapply(1:nrow(log_data), function(i) {
              tags$tr(
                tags$td(log_data[i, workout]),
                tags$td(log_data[i, set]),
                tags$td(log_data[i, reps]),
                tags$td(log_data[i, resistance]),
                tags$td(
                  div(
                    class = "row",
                    f7Button(
                      inputId = paste0("delete_row_", i),
                      label = "",
                      icon = f7Icon("trash"),
                      color = "red"
                    ),
                    f7Button(
                      inputId = paste0("move_up_", i),
                      label = "",
                      icon = f7Icon("arrow_up"),
                      color = "blue"
                    ),
                    f7Button(
                      inputId = paste0("move_down_", i),
                      label = "",
                      icon = f7Icon("arrow_down"),
                      color = "blue"
                    )
                  )
                )
              )
            })
          )
        ),
        f7Button("edit_done", "Done", color = "green", fill = TRUE)
      )
    } else {
      # Regular table
      tableOutput("workout_table")
    }
  })
  
  # Regular table output
  output$workout_table <- renderTable({
    workout_log()[, .(workout, set, reps, resistance)]
  })
  
  # Exit edit mode
  observeEvent(input$edit_done, {
    edit_mode(FALSE)
    f7Toast(text = "Edit mode disabled")
  })
  
  # Confirm end of workout
  observeEvent(input$end_workout, {
    f7Dialog(
      id    = "end_confirm",
      title = "End workout?",
      type  = "confirm",
      text  = "Click confirm to end and save workout"
    )
  })
  
  # Handle confirmation
  observeEvent(input$end_confirm, {
    if (isTRUE(input$end_confirm)) {
      f7Notif(
        text           = "Your workout has been saved",
        icon           = f7Icon("bolt_fill"),
        title          = "Notification",
        titleRightText = "now"
      )
      # Upload to BigQuery
      bq_table_upload(
        paste0(project_id, ".", ds, ".", input$date, "_", input$workout_name),
        workout_log()
      )
    } else {
      # Show discard confirmation when cancel is clicked
      f7Dialog(
        id    = "discard_confirm",
        title = "Discard workout?",
        type  = "confirm",
        text  = "Do you want to discard this workout?"
      )
    }
  })
  
  # Handle discard confirmation
  observeEvent(input$discard_confirm, {
    if (isTRUE(input$discard_confirm)) {
      # Reset workout log
      workout_log(setDT(list(
        date         = as.Date(character()),
        workout_name = character(),
        workout      = character(),
        set          = numeric(),
        reps         = character(),
        resistance   = character(),
        unit         = character(),
        effort       = numeric()
      )))
      f7Toast(text = "Workout discarded")
    } else {
      f7Toast(text = "Continued workout")
    }
  })
  
  # Get the historic workouts data
  get_workouts_history <- function() {
    # Get tables from the dataset
    tables <- bq_dataset_tables(paste0(project_id, ".", ds))
    
    # Initialize the result data frame
    result <- data.frame(
      Workout = character(),
      Date = character(),
      Sets = integer(),
      TableName = character(),  # Store the full table name for fetching data later
      stringsAsFactors = FALSE
    )
    
    # Process each table
    for (table_obj in tables) {
      # Extract just the table name
      table_name <- table_obj$table
      
      # Use regex to extract date and workout name
      date_pattern <- "^(\\d{4}-\\d{2}-\\d{2}|\\d{8})_(.+)$"
      date_match <- regmatches(table_name, regexec(date_pattern, table_name))
      
      # Try to get row count directly from table metadata
      tryCatch({
        # Create a table reference
        table_ref <- bq_table(table_obj$project, table_obj$dataset, table_obj$table)
        # Get table metadata which includes row count
        table_info <- bq_table_meta(table_ref)
        row_count <- as.integer(table_info$numRows)
        
        if (length(date_match[[1]]) > 0) {
          date_str <- date_match[[1]][2]
          workout_name <- date_match[[1]][3]
          
          # Format date for display if it's in YYYYMMDD format
          if (nchar(date_str) == 8) {
            date_str <- paste0(
              substr(date_str, 1, 4), "-",
              substr(date_str, 5, 6), "-",
              substr(date_str, 7, 8)
            )
          }
          
          # Add to result with the full table name
          result <- rbind(result, data.frame(
            Workout = workout_name,
            Date = date_str,
            Sets = row_count,
            TableName = table_name,
            stringsAsFactors = FALSE
          ))
        } else {
          # If pattern doesn't match, just use the table name
          result <- rbind(result, data.frame(
            Workout = table_name,
            Date = "",
            Sets = row_count,
            TableName = table_name,
            stringsAsFactors = FALSE
          ))
        }
      }, error = function(e) {
        # If we can't get row count, just show "N/A"
        if (length(date_match[[1]]) > 0) {
          date_str <- date_match[[1]][2]
          workout_name <- date_match[[1]][3]
          
          # Format date for display if it's in YYYYMMDD format
          if (nchar(date_str) == 8) {
            date_str <- paste0(
              substr(date_str, 1, 4), "-",
              substr(date_str, 5, 6), "-",
              substr(date_str, 7, 8)
            )
          }
          
          # Add to result with the full table name
          result <<- rbind(result, data.frame(
            Workout = workout_name,
            Date = date_str,
            Sets = "N/A",
            TableName = table_name,
            stringsAsFactors = FALSE
          ))
        } else {
          # If pattern doesn't match, just use the table name
          result <<- rbind(result, data.frame(
            Workout = table_name,
            Date = "",
            Sets = "N/A",
            TableName = table_name,
            stringsAsFactors = FALSE
          ))
        }
      })
    }
    
    # Sort by date (most recent first)
    if (nrow(result) > 0 && all(result$Date != "")) {
      result <- result[order(result$Date, decreasing = TRUE), ]
    }
    
    return(result)
  }
  
  # Get workouts history when the history tab is selected
  observeEvent(input$tabset, {
    if (input$tabset == "history") {
      workouts_history(get_workouts_history())
    }
  }, ignoreInit = TRUE)
  
  # Old workouts UI with View buttons
  output$old_workouts_ui <- renderUI({
    history_data <- workouts_history()
    
    if (is.null(history_data) || nrow(history_data) == 0) {
      return(p("No workout history available"))
    }
    
    # Create a table with View buttons
    tags$table(
      class = "table",
      tags$thead(
        tags$tr(
          tags$th("Workout"),
          tags$th("Date"),
          tags$th("Sets"),
          tags$th("Action")
        )
      ),
      tags$tbody(
        lapply(1:nrow(history_data), function(i) {
          tags$tr(
            tags$td(history_data$Workout[i]),
            tags$td(history_data$Date[i]),
            tags$td(history_data$Sets[i]),
            tags$td(
              f7Button(
                inputId = paste0("view_workout_", i),
                label = "View",
                color = "blue"
              )
            )
          )
        })
      )
    )
  })
  
  # Create observers for each View button
  # Create observers for each View button
  observe({
    history_data <- workouts_history()
    if (!is.null(history_data) && nrow(history_data) > 0) {
      lapply(1:nrow(history_data), function(i) {
        observeEvent(input[[paste0("view_workout_", i)]], {
          # Get the table name
          table_name <- history_data$TableName[i]
          workout_title <- paste0(history_data$Date[i], " - ", history_data$Workout[i])
          
          # Query the table data from BigQuery with proper backticks for escaping identifiers
          query <- sprintf("SELECT * FROM `%s.%s.%s`", project_id, ds, table_name)
          
          tryCatch({
            print(paste("Executing query:", query))
            workout_data <- bq_project_query(project_id, query) %>% bq_table_download()
            selected_workout_data(workout_data)
            
            # Show the popup with the workout data
            f7Popup(
              id = "workout_detail_popup",
              title = workout_title,
              f7Block(
                h4(paste("Workout:", history_data$Workout[i])),
                h4(paste("Date:", history_data$Date[i])),
                tableOutput("popup_workout_table")
              )
            )
          }, error = function(e) {
            # More detailed error logging
            error_msg <- paste("Error loading workout data:", e$message)
            print(error_msg)
            print(paste("Failed query:", query))
            f7Toast(text = error_msg)
          })
        }, ignoreInit = TRUE)
      })
    }
  })
  # observe({
  #   history_data <- workouts_history()
  #   if (!is.null(history_data) && nrow(history_data) > 0) {
  #     lapply(1:nrow(history_data), function(i) {
  #       observeEvent(input[[paste0("view_workout_", i)]], {
  #         # Get the table name
  #         table_name <- history_data$TableName[i]
  #         workout_title <- paste0(history_data$Date[i], " - ", history_data$Workout[i])
  #         
  #         # Query the table data from BigQuery
  #         query <- paste0("SELECT * FROM ", paste0(project_id, ".", ds), ".", table_name, "")
  #         
  #         tryCatch({
  #           workout_data <- bq_project_query(project_id, query) %>% bq_table_download()
  #           selected_workout_data(workout_data)
  #           
  #           # Show the popup with the workout data
  #           f7Popup(
  #             id = "workout_detail_popup",
  #             title = workout_title,
  #             f7Block(
  #               h4(paste("Workout:", history_data$Workout[i])),
  #               h4(paste("Date:", history_data$Date[i])),
  #               tableOutput("popup_workout_table")
  #             )
  #           )
  #         }, error = function(e) {
  #           f7Toast(text = paste("Error loading workout data:", e$message))
  #           cat("\n", paste0( "Error loading workout data: ", e$message, "\n"))
  #         })
  #       }, ignoreInit = TRUE)
  #     })
  #   }
  # })
  
  # Render the workout details in the popup
  output$popup_workout_table <- renderTable({
    workout_data <- selected_workout_data()
    print(workout_data)
    if (!is.null(workout_data)) {
      # Select and format columns for display
      if ("workout" %in% names(workout_data) && "set" %in% names(workout_data) && 
          "reps" %in% names(workout_data) && "resistance" %in% names(workout_data)) {
        workout_data[, c("workout", "set", "reps", "resistance", "effort")]
      } else {
        # If the expected columns aren't present, show all data
        workout_data
      }
    }
  })
}

# Run app
shinyApp(ui, server)