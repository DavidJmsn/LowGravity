library(shiny)
library(shinyMobile)
library(bigrquery)
library(data.table)
library(gargle)

# Launch in browser by default
theme_ios <- list(theme = 'ios')
options(shiny.launch.browser = TRUE)

dataset <- Sys.getenv("dataset")
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
      animated  = FALSE,
      style     = "toolbar",
      
      # new_set and workout tabs unchanged...
      f7Tab(
        tabName = "new_set", title = "New Set", icon = f7Icon("text_badge_plus"), active = TRUE,
        f7List(
          f7Grid(
            cols = 2, gap = TRUE,
            f7DatePicker("date", NULL, placeholder = as.character(Sys.Date())),
            f7Toggle("auto_inc", "Keep inputs"),
            f7Text("workout_name", NULL, placeholder = "Workout Name"),
            f7Select("exercise", NULL, choices = c("pullup","dip","pushup","run","lsit","chinup","Other"), selected = "pullup"),
            f7Text("set", NULL, placeholder = "Set"),
            f7Text("reps", NULL, placeholder = "Reps/Time"),
            f7Text("resistance", NULL, placeholder = "Resistance"),
            f7Select("unit", NULL, choices = c("lbs","Kg","Metres","Km","Sec","Min","Other"), selected = "lbs")
          ),
          conditionalPanel("input.unit == 'Other'", f7Text("unit_other", NULL, placeholder = "Unit")),
          f7Slider("effort", "Effort", 1, 10, value = 5, step = 1, scale = TRUE, scaleSteps = 9, scaleSubSteps = 0),
          f7Button("add_set", "Add Set", icon = f7Icon("plus"), fill = TRUE)
        )
      ),
      f7Tab(
        tabName = "workout", title = "Workout", icon = f7Icon("play"),
        f7Block(inset = TRUE, strong = TRUE, uiOutput("workout_table_ui")),
        f7Card(
          f7Button("end_workout", "End Workout", icon = f7Icon("flag_circle_fill")),
          f7Button("edit_workout", "Edit", icon = f7Icon("pencil"), color = "blue", fill = FALSE)
        )
      ),
      
      # History tab with View buttons only
      f7Tab(
        tabName = "history", title = "History", icon = f7Icon("timer"),
        uiOutput("old_workouts_ui")
      )
    )
  )
)

server <- function(input, output, session) {
  # reactive values and existing observers unchanged...
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
  edit_mode <- reactiveVal(FALSE)
  
  observeEvent(input$add_set, {
    req(input$date, input$exercise, nzchar(input$set), nzchar(input$reps), nzchar(input$resistance), nzchar(input$unit))
    unit_val <- if (input$unit == "Other") input$unit_other else input$unit
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
    if (isTRUE(input$auto_inc)) {
      updateF7Text("set", value = as.character(as.numeric(input$set) + 1), session = session)
    } else {
      reset_list <- list(
        list(fn = updateF7Select, args = list("exercise", selected = "pullup")),
        list(fn = updateF7Text,   args = list("set",        value = "")),
        list(fn = updateF7Text,   args = list("reps",       value = "")),
        list(fn = updateF7Text,   args = list("resistance", value = "")),
        list(fn = updateF7Select, args = list("unit",       selected = "lbs")),
        list(fn = updateF7Text,   args = list("unit_other", value = "")),
        list(fn = updateF7Slider, args = list("effort",     value = 5))
      )
      lapply(reset_list, function(x) do.call(x$fn, c(x$args, list(session = session))))
    }
  })
  observeEvent(input$quit, stopApp())
  observeEvent(input$edit_workout, { edit_mode(!edit_mode()); f7Toast(text = if(edit_mode()) "Edit mode enabled" else "Edit mode disabled") })
  
  output$workout_table_ui <- renderUI({
    log_data <- workout_log()
    if (nrow(log_data) == 0) return(p("No workout data yet"))
    if (edit_mode()) {
      tags$div(
        tags$table(class = "table",
                   tags$thead(tags$tr(tags$th("Workout"), tags$th("Set"), tags$th("Reps"), tags$th("Resistance"), tags$th("Actions"))),
                   tags$tbody(lapply(1:nrow(log_data), function(i) {
                     tags$tr(
                       tags$td(log_data[i, workout]),
                       tags$td(log_data[i, set]),
                       tags$td(log_data[i, reps]),
                       tags$td(log_data[i, resistance]),
                       tags$td(div(
                         class = "row",
                         f7Button(inputId = paste0("delete_row_", i), icon = f7Icon("trash"), color = "red"),
                         f7Button(inputId = paste0("move_up_", i), icon = f7Icon("arrow_up"), color = "blue"),
                         f7Button(inputId = paste0("move_down_", i), icon = f7Icon("arrow_down"), color = "blue")
                       ))
                     )
                   }))
        ),
        f7Button("edit_done", "Done", color = "green", fill = TRUE)
      )
    } else {
      tableOutput("workout_table")
    }
  })
  output$workout_table <- renderTable({ workout_log()[, .(workout, set, reps, resistance)] })
  observeEvent(input$edit_done, { edit_mode(FALSE); f7Toast(text = "Edit mode disabled") })
  observeEvent(input$end_workout, { f7Dialog(id = "end_confirm", title = "End workout?", type = "confirm", text = "Click confirm to end and save workout") })
  observeEvent(input$end_confirm, { if (isTRUE(input$end_confirm)) { f7Notif(text = "Your workout has been saved", icon = f7Icon("bolt_fill"), title = "Notification", titleRightText = "now"); bq_table_upload(paste0(dataset, ".", input$date, "_", input$workout_name), workout_log()) } else { f7Dialog(id = "discard_confirm", title = "Discard workout?", type = "confirm", text = "Do you want to discard this workout?") } })
  observeEvent(input$discard_confirm, { if (isTRUE(input$discard_confirm)) { workout_log(setDT(list(date = as.Date(character()), workout_name = character(), workout = character(), set = numeric(), reps = character(), resistance = character(), unit = character(), effort = numeric()))); f7Toast(text = "Workout discarded") } else { f7Toast(text = "Continued workout") } })
  
  # HISTORY: build table of past workouts with View buttons
  output$old_workouts_ui <- renderUI({
    tables <- bq_dataset_tables(dataset)
    if (length(tables) == 0) return(f7Card(p("No past workouts found")))
    tagList(
      tags$table(class = "table",
                 tags$thead(tags$tr(tags$th("Workout"), tags$th("Date"), tags$th("Sets"), tags$th("Action"))),
                 tags$tbody(
                   lapply(seq_along(tables), function(i) {
                     tbl <- tables[[i]]
                     name <- tbl$table
                     m <- regexec("^(\\d{4}-\\d{2}-\\d{2}|\\d{8})_(.+)$", name)
                     parts <- regmatches(name, m)[[1]]
                     date_str <- if (nchar(parts[2]) == 8) paste0(substr(parts[2],1,4),"-",substr(parts[2],5,6),"-",substr(parts[2],7,8)) else parts[2]
                     workout_name <- parts[3]
                     meta <- try(bq_table_meta(bq_table(tbl$project, tbl$dataset, tbl$table)), silent = TRUE)
                     count <- if (!inherits(meta, "try-error")) as.integer(meta$numRows) else "N/A"
                     tags$tr(
                       tags$td(workout_name), tags$td(date_str), tags$td(count),
                       tags$td(f7Button(inputId = paste0("view_", i), label = "View", color = "blue"))
                     )
                   })
                 )
      )
    )
  })
  
  # handle View button clicks with standard shiny modal dialogs
  observe({
    tables <- bq_dataset_tables(dataset)
    lapply(seq_along(tables), function(i) {
      local({ ii <- i; tbl <- tables[[ii]]
      observeEvent(input[[paste0("view_", ii)]], {
        data <- bq_table_download(bq_table(tbl$project, tbl$dataset, tbl$table))
        showModal(modalDialog(
          title = tbl$table,
          renderTable(data),
          size = "l",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      }, ignoreInit = TRUE)
      })
    })
  })
}

shinyApp(ui, server)
