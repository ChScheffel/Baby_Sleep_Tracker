# Load required libraries
library(shiny)
library(bslib)
library(readxl)
library(ggplot2)
library(ggExtra)
library(ggprism)
library(tidyverse)
library(data.table)
library(DT)
library(rsconnect)
library(googlesheets4)
library(googledrive)
library(gargle)
library(lubridate)
library(ggiraph)

# set up google sheets options
#options(
#  # whenever there is one account token found, use the cached token
#  gargle_oauth_email = TRUE,
#  # specify auth tokens should be stored in a hidden directory ".secrets"
#  gargle_oauth_cache = ".secrets"
#)

# Authenticate using the cached token (non-interactive on the server)
#drive_auth(
#  cache  = ".secrets",
#  email  = "christoph_scheffel@tu-dresden.de"
#)

gs4_deauth()

SHEET_ID <- "1zjIo8BgFHBdTkhP3hqpxtFm62aIQowg5udcFKWrVeEw"

# ---- UI Definition ----
ui <- page_sidebar(
  theme = bs_theme(version = 5, preset = "bootstrap"),  # Bootstrap 5.3 = dark mode support
  title = "Richard's Sleep Data Visualization",
  
  # Dark mode toggle (appears top-right)
  input_dark_mode(id = "dark_mode", mode = "light"),
  
  sidebar = sidebar(
    dateRangeInput("date_range", "Date Range",
                   start = "2026-08-19", end = Sys.Date(),
                   format = "yyyy-mm-dd"),
    selectInput("status_filter", "Status", choices = c("All"), selected = "All"),
    selectInput("select_month", "Lebensmonat", choices = c("All"), selected = "All"),
    conditionalPanel(
      condition = "input.select_month != null && input.tabs == 'compare'",
      dateInput("compare_date", "Select date for comparison", value = Sys.Date() - 1)
    )
  ),
  navset_tab(
    id = "tabs",
    nav_panel("Sleep Raster Plot", value = "sleep", plotOutput("sleepPlot", height = "600px")),
    nav_panel("Activity Time Plot", value = "activity", girafeOutput("activityPlot", width = "100%", height = "600px")),
    nav_panel("Compare States", value = "compare", plotOutput("comparePlot", height = "600px")),
    nav_panel("Data Table", value = "table", DTOutput("sleepTable"))
  )
)

# Functions

# define birth date of the baby
birth_date <- as.Date("2026-08-09")

# Which life month does a date fall into?
# Month 1 = birth_date until (birth_date + 1 month) - 1 day, etc.
life_month_of <- function(d) {
  d <- as.Date(d)
  n <- interval(birth_date, d) %/% months(1) + 1L
  n
}

# Start and end dates of life month n
life_month_start <- function(n) birth_date %m+% months(n - 1)
life_month_end   <- function(n) birth_date %m+% months(n) - 1   # day before next anniversary
# ---- Server Logic ----
server <- function(input, output, session) {
  
  is_dark <- reactive({
    input$dark_mode == "dark"
  })
  
  # ---- Helper: dark-aware ggplot theme ----
  theme_app <- function(dark = FALSE, base_size = 12) {
    if (dark) {
      theme_minimal(base_size = base_size) +
        theme(
          panel.background = element_rect(fill = "#222222", color = NA),
          plot.background  = element_rect(fill = "#222222", color = NA),
          text             = element_text(color = "#EEEEEE"),
          axis.text        = element_text(color = "#EEEEEE"),
          axis.title       = element_text(color = "#EEEEEE"),
          plot.title       = element_text(color = "#FFFFFF"),
          legend.text      = element_text(color = "#EEEEEE"),
          legend.title     = element_text(color = "#EEEEEE"),
          panel.grid       = element_line(color = "#444444")
        )
    } else {
      theme_minimal()
    }
  }
  
  tryCatch({# --- 1. Load and Clean Data from Google Sheets ---
    raw_data <- reactive({
      # Read all sheets and combine them into one data frame
      sheet1 <- read_sheet(SHEET_ID, sheet = 1, col_types = "Dctttt")
      sheet2 <- read_sheet(SHEET_ID, sheet = 2, col_types = "Dctttt")
      sheet3 <- read_sheet(SHEET_ID, sheet = 3, col_types = "Dctttt")
      # Standardize columns and types
      clean_sheet <- function(df) {
        df |>
          select(Datum, Status, Beginn, Ende) |>
          mutate(
            Datum = as.Date(Datum),
            Beginn = as.POSIXct(Beginn, tz = "UTC"),
            Ende = as.POSIXct(Ende, tz = "UTC")
          ) |>
          fill(Datum, .direction = "down")
      }
      bind_rows(clean_sheet(sheet1), clean_sheet(sheet2)) |>
        filter(!is.na(Status))
    })
  }, error = function(e) {
    showNotification(
      paste("Could not load data from Google Sheets:", conditionMessage(e)),
      type = "error", duration = NULL
    )
    return(NULL)
  })


    # --- 2. Build Minute-Level Sleep Data Grid ---
  sleep_data_grid <- reactive({
    req(!is.null(raw_data()))
    df <- raw_data()

    grid <- data.table(
      datetime = seq(
        from = as.POSIXct(min(df$Datum, na.rm = T), tz = "UTC"),
        to   = as.POSIXct(max(df$Datum, na.rm = T), tz = "UTC") + 24*60*60 - 60, # up to 23:59
        by   = "1 min"
      )
    )
    grid[, date := as.Date(datetime)]
    grid[, time := format(datetime, "%H:%M")]
    grid[, minute_of_day := as.integer(format(datetime, "%H")) * 60L + as.integer(format(datetime, "%M"))]
    grid
  })
  
  # --- 3. Assign Status to Each Minute (Non-Equi Join) ---
  minute_status <- reactive({
    req(!is.null(raw_data()))
    df <- as.data.table(raw_data())
    grid <- copy(sleep_data_grid())
    
    # Compute minutes since midnight for intervals
    df[, Beginn_min := as.integer(format(Beginn, "%H")) * 60L + as.integer(format(Beginn, "%M"))]
    df[, Ende_min := as.integer(format(Ende, "%H")) * 60L + as.integer(format(Ende, "%M"))]
    
    # Give the grid a start AND end column (foverlaps needs two distinct columns)
    grid[, minute_of_day := as.integer(format(datetime, "%H")) * 60L + as.integer(format(datetime, "%M"))]

    # Non-equi join: assign Status to each minute
    result <- df[grid, 
                 on = .(Datum = date, Beginn_min <= minute_of_day, Ende_min >= minute_of_day),
                 .(datetime, date = i.date, time = i.time, Status = x.Status),
                 nomatch = NA
    ]
    
    # re-derive minute_of_day for the result so downstream plots can use it
    result[, minute_of_day := as.integer(format(datetime, "%H")) * 60L + as.integer(format(datetime, "%M"))]
      
    # Merge back to grid to fill in missing (awake/unknown)
#    result <- result[, .(date, time, Status)]
#    grid[result, Status := i.Status]
#    grid[, Status := replace_na(Status, "Awake")]
    result
  })
  
  # --- 4. UI Dynamic Choices for Status Filter ---
  observe({
    statuses <- unique(minute_status()$Status)
    updateSelectInput(session, "status_filter", choices = c("All", statuses))
  })
  
  # --- 4. UI Dynamic Choices for Month Filter ---
 
  observe({
    # 1. Compute "month number" for the start and end of the selected date range
    start_month <- life_month_of(input$date_range[1])
    end_month   <- life_month_of(input$date_range[2])
    
    # Guard against reversed ranges
    if (start_month > end_month) {
      showNotification("Start date is after end date.", type = "error")
      return()
    }
    
    # 2. Build a sequence of month numbers between start and end
    months <- seq(start_month, end_month)
    
    # 3. Create labels: "All", "Month 1", "Month 2", ...
    choices <- setNames(
      as.list(c("All", months)),                       # values
      c("All", paste0("Month ", months, " (", format(life_month_start(months), "%d %b"),
                      " \u2013 ", format(life_month_end(months), "%d %b %Y"), ")")     # labels
    )
    )
    
    # Preserve the user's current selection if it is still valid
    current <- input$select_month
    selected <- if (!is.null(current) && current %in% choices) current else "All"
    
    # 4. Update the selectInput dropdown with these choices
    updateSelectInput(session, "select_month", choices = choices, selected = selected)
  })
  
  # --- 5. Filtered Data for Plot/Table ---
  filtered_data <- reactive({
    df <- minute_status()
    # Filter by date range
    df <- df[date >= input$date_range[1] & date <= input$date_range[2]]
    # Filter by status if not "All"
    if (input$status_filter != "All") {
      df <- df[Status == input$status_filter]
    }
    # Filter by month if not "All"
    if (input$select_month != "All") {
     
      selected_month <- as.integer(input$select_month)
      df <- df[life_month_of(date) == selected_month]
    }
    df
  })
  
  # --- 6. Raster/Tile Plot (Date × Time-of-Day) ---
  output$sleepPlot <- renderPlot({
    df <- filtered_data()
    df |>
      mutate(
        time = as.POSIXct(time, format = "%H:%M", tz = "UTC")  # Convert time to POSIXct
      ) |>
      ggplot(aes(x = date, y = time, fill = Status)) +
      geom_tile() +
      #ggprism::theme_prism(base_size = 12, base_line_size = 0.5, base_fontface = "plain", base_family = "sans") +
      scale_fill_manual(
        values = c("Schlaf" = "skyblue4", "Essen" = "tan3", "Wach" = "tan", "NA" = "grey80"),
        name = "State"
      ) +
      scale_y_datetime(date_breaks = "2 hour", date_labels = "%H:%M") +
      scale_x_date(date_minor_breaks = "1 day", date_breaks = "5 days", date_labels = "%d %b") +
      labs(x = "Date", y = "Time") +
      ggtitle("Visualization of Richard's sleeping patterns") +
      theme_app(is_dark())+          # <-- reactive theme
      theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels)
})
  
  # --- Count States ---
  
  output$activityPlot <- renderGirafe({
    df <- filtered_data()
    
    plot_df <- df |>
      group_by(date) |>
      count(Status) |>
      pivot_wider(names_from = Status, values_from = n) |>
      mutate(across(c(Wach, Essen, Schlaf), ~ . / 60)) |>
      pivot_longer(cols = c(Essen, Wach, Schlaf),
                   names_to = "State", values_to = "Hours") |>
      mutate(
        tooltip = paste0(
          "<b>", format(date, "%d %b %Y"), "</b><br>",
          "State: ", State, "<br>",
          "Hours: ", sprintf("%.1f", Hours)
        )
      )
    
    p <- ggplot(plot_df, aes(x = date, y = Hours, group = State, color = State)) +
      geom_line_interactive(lwd = 1, linetype = 1) +
      geom_point_interactive(
        aes(tooltip = tooltip, data_id = date,
            onclick = sprintf("Shiny.setInputValue('pt_click', '%s')", date)),
       # ← tooltip + hover identity
        size = 2
      ) +
      geom_boxplot(aes(x = max(df$date) + 7, y = Hours, fill = State),
                   width = 2, alpha = 0.4, outlier.shape = 21) +
      scale_color_manual(values = c("Essen" = "coral3", "Wach" = "darksalmon", "Schlaf" = "darkseagreen")) +
      scale_fill_manual(values = c("Essen" = "coral3", "Wach" = "darksalmon", "Schlaf" = "darkseagreen")) +
      scale_y_continuous(breaks = seq(0, 24, by = 2)) +
      scale_x_date(
        date_minor_breaks = "1 day",
        breaks = seq(from = min(df$date), to = max(df$date), by = "5 days"),
        date_labels = "%d %b"
      ) +
      labs(x = "Datum", y = "Stunden") +
      ggtitle("Richards Aktivitäten pro Tag") +
      theme_app(is_dark()) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    girafe(
      ggobj = p,
      width_svg  = 10,
      height_svg = 5,
      options = list(
        opts_tooltip(css = "padding:5px;background:rgba(0,0,0,0.8);color:white;"),
        opts_hover(css = "stroke-width:3")   # highlight on hover
      )
    )
  })
  
  observeEvent(input$pt_click, {
    clicked_date <- as.Date(input$pt_click)
    details <- filtered_data() |> filter(date == clicked_date)
    showModal(modalDialog(
      title = paste("Details for", format(clicked_date, "%d %b %Y")),
      renderTable(details),
      easyClose = TRUE
    ))
  })
  
  # --- 8. Interactive Data Table ---
  
  format_hours <- function(x) {
    # Handle NA and missing columns gracefully
    out <- rep(NA_character_, length(x))
    valid <- !is.na(x)
    
    total_minutes <- round(x[valid] * 60)        # round to nearest minute
    h <- total_minutes %/% 60
    m <- total_minutes %% 60
    
    out[valid] <- paste0(h, "h", sprintf("%02d", m), "min")
    out
  }
  
  output$sleepTable <- renderDT({
    df <- filtered_data()
    df %>%
      group_by(date) |>
      count(Status) |>
      pivot_wider(names_from = Status, values_from = n) |>
      mutate(across(c(Wach, Essen, Schlaf), ~ . / 60)) |>
      # Replace the old round() with the formatted strings
      mutate(across(any_of(c("Wach", "Essen", "Schlaf")), format_hours))
  })
  
  # --- Compare States: Staircase Plot ---
  output$comparePlot <- renderPlot({
    today_utc <- lubridate::today(tzone = "UTC")
    validate(need(!is.null(input$compare_date), "Choose day to compare!"))
    validate(need(input$compare_date != today_utc, "Choose different day than today!"))
    
    plot_df <- minute_status() %>%
      filter(date == today_utc | date == input$compare_date) %>%
      filter(!is.na(Status)) %>%
      mutate(Label = ifelse(date == today_utc, 'Today', as.character(input$compare_date)))
    
    # Compute cumulative hours per status per label
    cum_df <- plot_df %>%
      arrange(Label, Status, minute_of_day) %>%
      group_by(Label, Status) %>%
      mutate(cum_hours = row_number() / 60) %>%
      ungroup()
    
    endpoints <- cum_df %>%
      group_by(Label, Status) %>%
      slice_max(minute_of_day, n = 1) %>%     # take each line's last point
      ungroup() %>%
      mutate(minute_of_day = 1440L)           # move it to midnight, keep cum_hours
    
    cum_df <- bind_rows(cum_df, endpoints) %>%
      arrange(Label, Status, minute_of_day)
    
    comparison_label <- as.character(input$compare_date)
    linetype_vals <- setNames(c('solid', 'dashed'), c('Today', comparison_label))
    
    ggplot(cum_df, aes(x = minute_of_day, y = cum_hours, color = Status, linetype = Label)) +
      
      geom_step(linewidth = 1.2) +
      scale_linetype_manual(values = linetype_vals) +
      scale_x_continuous(
        breaks = seq(0, 1440, by = 120),
        labels = function(x) sprintf('%02d:%02d', x %/% 60, x %% 60),
        expand = c(0, 0)
      ) +
     
      coord_cartesian(xlim = c(0, 1440)) +
      scale_color_manual(values = c("Essen" = "coral3", "Wach" = "darksalmon", "Schlaf" = "darkseagreen"))+
      scale_y_continuous(breaks = seq(0, 24, by = 2))+
      labs(
        x = 'Time of Day',
        y = 'Cumulative Hours',
        color = 'Status',
        linetype = 'Day',
        title = 'Vergleich der Zustände (Staircase Plot)'
      ) +
      theme_app(is_dark()) +          # <-- reactive theme
      theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels
  })
}

# ---- Run the App ----
shinyApp(ui = ui, server = server)
