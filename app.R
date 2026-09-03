# =============================================================================
# UCSB Economics Department - Pre-Major GPA Calculator (Shiny)
# -----------------------------------------------------------------------------
# A multi-page wizard that:
#   1. asks whether the student is an Economics or Economics & Accounting major
#   2. collects the status of each pre-major course (UCSB / other UC / non-UC /
#      in progress / not yet taken)
#   3. collects grades for completed UC courses (+ optional ECON 5 supplement)
#   4. reports the current pre-major GPA and the average grade still needed in
#      remaining courses to reach the 2.85 requirement.
#
# Run locally:   shiny::runApp()   (from this folder)  or  Rscript -e "shiny::runApp('.', launch.browser=TRUE)"
# Deploy:        rsconnect::deployApp()   (to shinyapps.io)
# =============================================================================

library(shiny)

source(file.path("R", "gpa_logic.R"), local = TRUE)

STATUS_CHOICES <- c(
  "Completed at UCSB"                       = "ucsb",
  "Completed at another UC campus"          = "uc_other",
  "Completed at a non-UC school (transfer/AP)" = "non_uc",
  "Currently in progress"                   = "in_progress",
  "Not yet taken"                           = "planned"
)

LETTER_CHOICES <- names(GRADE_POINTS)

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML("
    :root { --navy:#003660; --gold:#FEBC11; --bg:#f4f6f8; }
    body { background:var(--bg); font-family:'Segoe UI',Roboto,Helvetica,Arial,sans-serif; color:#1d2733; }
    .wrap { max-width:760px; margin:24px auto; background:#fff; border-radius:14px;
            box-shadow:0 6px 24px rgba(0,0,0,.08); overflow:hidden; }
    .topbar { background:var(--navy); color:#fff; padding:22px 28px; }
    .topbar h2 { margin:0; font-size:22px; font-weight:700; }
    .topbar .sub { opacity:.85; font-size:14px; margin-top:4px; }
    .gold-rule { height:5px; background:var(--gold); }
    .content { padding:26px 28px 30px; }
    .steps { display:flex; gap:6px; margin-bottom:22px; }
    .step { flex:1; height:6px; border-radius:3px; background:#dfe4ea; }
    .step.active { background:var(--gold); }
    .step.done { background:var(--navy); }
    .qlabel { font-weight:600; font-size:16px; margin:18px 0 8px; }
    .course-row { display:flex; align-items:center; justify-content:space-between;
                  gap:12px; padding:10px 0; border-bottom:1px solid #eef1f4; }
    .course-name { font-weight:600; min-width:90px; }
    .hint { color:#5a6b7b; font-size:13px; }
    .btn-nav { background:var(--navy); color:#fff; border:none; padding:10px 22px;
               border-radius:8px; font-weight:600; }
    .btn-nav:hover { background:#00254a; color:#fff; }
    .btn-back { background:#e6eaee; color:#1d2733; border:none; padding:10px 22px;
                border-radius:8px; font-weight:600; }
    .navrow { display:flex; justify-content:space-between; margin-top:26px; }
    .result-card { border-radius:12px; padding:20px 22px; margin-bottom:16px; }
    .ok   { background:#e7f6ec; border:1px solid #bfe6cd; }
    .warn { background:#fff4e0; border:1px solid #ffe1a8; }
    .bad  { background:#fde8e8; border:1px solid #f6c6c6; }
    .info { background:#eef4fb; border:1px solid #cfe0f3; }
    .bignum { font-size:40px; font-weight:800; color:var(--navy); line-height:1; }
    .pill { display:inline-block; background:var(--navy); color:#fff; border-radius:999px;
            padding:4px 14px; font-weight:700; font-size:18px; }
    .muted { color:#5a6b7b; font-size:13px; }
    .selectize-input, select.form-control { min-width:230px; }
  "))),
  div(class = "wrap",
    div(class = "topbar",
      h2("UCSB Economics Pre-Major GPA Calculator"),
      div(class = "sub", "Check your pre-major GPA and the grades you need to reach the 2.85 requirement")
    ),
    div(class = "gold-rule"),
    div(class = "content",
      uiOutput("steps"),
      uiOutput("page")
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {
  rv <- reactiveValues(page = 1, major = NULL, statuses = list(),
                       grades = list(), result = NULL)

  output$steps <- renderUI({
    labels <- c("Major", "Courses", "Grades", "Results")
    div(class = "steps",
      lapply(seq_along(labels), function(i) {
        cls <- "step"
        if (i < rv$page) cls <- paste(cls, "done")
        if (i == rv$page) cls <- paste(cls, "active")
        div(class = cls)
      })
    )
  })

  # ----- PAGE ROUTER -----
  output$page <- renderUI({
    switch(as.character(rv$page),
      "1" = page_major(),
      "2" = page_status(),
      "3" = page_grades(),
      "4" = page_results()
    )
  })

  # ----- PAGE 1: major -----
  page_major <- function() {
    tagList(
      div(class = "qlabel", "Which major are you pursuing?"),
      radioButtons("major", NULL,
        choices = c("Economics (B.A.)" = "econ",
                    "Economics & Accounting (B.A.)" = "econ_acct"),
        selected = if (!is.null(rv$major)) rv$major else "econ"),
      div(class = "hint",
        "Economics counts ECON 1, 2, and 10A toward the 2.85 GPA. ",
        "Economics & Accounting also counts ECON 3A and 3B."),
      div(class = "navrow",
        span(),
        actionButton("to2", "Next →", class = "btn-nav")
      )
    )
  }
  observeEvent(input$to2, { rv$major <- input$major; rv$page <- 2 })

  # ----- PAGE 2: course status -----
  page_status <- function() {
    courses <- premajor_courses(rv$major)
    tagList(
      div(class = "qlabel", "What is the status of each pre-major course?"),
      div(class = "hint", style = "margin-bottom:10px",
        "Only courses taken at a UC campus count toward the 2.85 GPA. ",
        "Non-UC courses give credit but are excluded from the GPA."),
      lapply(seq_len(nrow(courses)), function(i) {
        code <- courses$code[i]
        prev <- rv$statuses[[code]]
        div(class = "course-row",
          div(
            div(class = "course-name", code),
            if (courses$ucsb_only[i]) div(class = "hint", "Must be taken at UCSB")
          ),
          selectInput(paste0("status_", i), NULL, choices = STATUS_CHOICES,
            selected = if (!is.null(prev)) prev else "ucsb", width = "300px")
        )
      }),
      div(class = "navrow",
        actionButton("back1", "← Back", class = "btn-back"),
        actionButton("to3", "Next →", class = "btn-nav")
      )
    )
  }
  observeEvent(input$back1, { rv$page <- 1 })
  observeEvent(input$to3, {
    courses <- premajor_courses(rv$major)
    st <- list()
    for (i in seq_len(nrow(courses))) st[[courses$code[i]]] <- input[[paste0("status_", i)]]
    rv$statuses <- st
    rv$page <- 3
  })

  # ----- PAGE 3: grades -----
  page_grades <- function() {
    courses <- premajor_courses(rv$major)
    completed <- courses$code[ vapply(courses$code,
      function(c) isTRUE(rv$statuses[[c]] %in% COUNTS_IN_GPA), logical(1)) ]

    grade_inputs <-
      if (length(completed) == 0) {
        div(class = "hint", "No completed UC courses to grade — ",
            "enter grades here once you have them.")
      } else {
        lapply(seq_along(completed), function(j) {
          code <- completed[j]
          prev <- rv$grades[[code]]
          div(class = "course-row",
            div(class = "course-name", code),
            selectInput(paste0("grade_", j), NULL, choices = LETTER_CHOICES,
              selected = if (!is.null(prev)) prev else "B", width = "140px")
          )
        })
      }

    tagList(
      div(class = "qlabel", "Enter your grades for completed UC courses"),
      grade_inputs,
      tags$hr(),
      div(class = "qlabel", "Optional: ECON 5 supplement"),
      div(class = "hint", style = "margin-bottom:8px",
        "If your designated pre-major GPA falls below 2.85, a first-attempt ",
        "ECON 5 grade taken at a UC may be used to help you reach it — ",
        "but only if it raises your GPA."),
      checkboxInput("has_econ5", "I have a UC ECON 5 grade to include",
        value = isTRUE(rv$grades[["__has_econ5"]])),
      conditionalPanel("input.has_econ5 == true",
        div(class = "course-row",
          div(class = "course-name", "ECON 5"),
          selectInput("econ5_grade", NULL, choices = LETTER_CHOICES,
            selected = if (!is.null(rv$grades[["__econ5"]])) rv$grades[["__econ5"]] else "A",
            width = "140px")
        ),
        checkboxInput("econ5_first", "This is my first attempt at ECON 5",
          value = if (!is.null(rv$grades[["__econ5_first"]])) rv$grades[["__econ5_first"]] else TRUE)
      ),
      div(class = "navrow",
        actionButton("back2", "← Back", class = "btn-back"),
        actionButton("to4", "See my results →", class = "btn-nav")
      )
    )
  }
  observeEvent(input$back2, { rv$page <- 2 })
  observeEvent(input$to4, {
    courses <- premajor_courses(rv$major)
    completed <- courses$code[ vapply(courses$code,
      function(c) isTRUE(rv$statuses[[c]] %in% COUNTS_IN_GPA), logical(1)) ]
    g <- list()
    for (j in seq_along(completed)) g[[completed[j]]] <- input[[paste0("grade_", j)]]
    g[["__has_econ5"]]   <- isTRUE(input$has_econ5)
    g[["__econ5"]]       <- input$econ5_grade
    g[["__econ5_first"]] <- isTRUE(input$econ5_first)
    rv$grades <- g

    entries <- list()
    for (code in courses$code) {
      entries[[code]] <- list(status = rv$statuses[[code]], grade = g[[code]])
    }
    econ5 <- NULL
    if (isTRUE(input$has_econ5)) {
      econ5 <- list(grade = input$econ5_grade, first_attempt = isTRUE(input$econ5_first))
    }
    rv$result <- evaluate_premajor(rv$major, entries, econ5)
    rv$page <- 4
  })

  # ----- PAGE 4: results -----
  page_results <- function() {
    r <- rv$result
    fmt <- function(x) if (is.na(x)) "–" else sprintf("%.3f", x)

    cards <- list()

    # Current GPA card
    cards[[length(cards)+1]] <- div(class = "result-card info",
      div(class = "muted", "Current pre-major GPA (UC courses with grades)"),
      div(class = "bignum", fmt(r$current_gpa)),
      if (r$met_only_with_econ5)
        div(style="margin-top:8px",
          "With your ECON 5 grade applied: ",
          span(class="pill", fmt(r$effective_gpa)))
    )

    # Outcome card
    if (r$met) {
      cards[[length(cards)+1]] <- div(class = "result-card ok",
        strong("✅ You currently meet the 2.85 pre-major GPA requirement."),
        if (r$has_remaining) div(class="muted", style="margin-top:6px",
          "You still have remaining courses below — keep your grades up to stay above 2.85."))
    } else if (r$has_remaining) {
      if (r$impossible) {
        cards[[length(cards)+1]] <- div(class = "result-card bad",
          strong("Reaching 2.85 is not possible with your remaining courses alone."),
          div(style="margin-top:8px",
            "Even straight A's in ", paste(r$remaining_courses, collapse=", "),
            " would not bring you to 2.85. Speak with the Economics Undergraduate ",
            "Advising Office about the ECON 10A Retake Exam and other options."))
      } else {
        e5 <- !is.na(r$required_letter_econ5)
        cards[[length(cards)+1]] <- div(class = "result-card warn",
          div("To reach a 2.85 pre-major GPA you need to average at least"),
          div(style="margin:10px 0", span(class="pill", r$required_letter),
              span(class="muted", sprintf("  (a %.2f grade-point average)", r$required_avg))),
          div("across your remaining course(s): ",
              strong(paste(r$remaining_courses, collapse=", ")), "."),
          if (!is.na(r$projected_gpa)) div(class="muted", style="margin-top:6px",
            sprintf("That would put your pre-major GPA at about %.2f.", r$projected_gpa)),
          if (e5) div(style="margin-top:10px",
            "With your ECON 5 grade applied, the requirement eases to an average of ",
            span(class="pill", r$required_letter_econ5),
            span(class="muted", sprintf("  (%.2f GPA).", r$required_avg_econ5)))
        )
      }
    } else {
      cards[[length(cards)+1]] <- div(class = "result-card bad",
        strong("You do not currently meet the 2.85 pre-major GPA requirement,"),
        " and there are no remaining pre-major courses recorded. ",
        "Speak with the Economics Undergraduate Advising Office about the ",
        "ECON 10A Retake Exam as another avenue for admission.")
    }

    # Warnings
    if (length(r$warnings) > 0) {
      cards[[length(cards)+1]] <- div(class = "result-card bad",
        strong("Please note:"),
        tags$ul(lapply(r$warnings, function(w) tags$li(w))))
    }
    # Notes
    if (length(r$notes) > 0) {
      cards[[length(cards)+1]] <- div(class = "result-card info",
        tags$ul(lapply(r$notes, function(n) tags$li(n))))
    }

    # Retake exam standing note
    cards[[length(cards)+1]] <- div(class = "muted", style="margin-top:4px",
      "If you fall short of 2.85, the Department offers an ECON 10A Retake Exam as ",
      "another path to admission. Contact the Economics Undergraduate Advising ",
      "Office (2121 North Hall) for details and to confirm any ECON 5 supplement.")

    tagList(
      div(class = "qlabel", "Your pre-major GPA results"),
      cards,
      div(class = "navrow",
        actionButton("back3", "← Edit grades", class = "btn-back"),
        actionButton("restart", "Start over", class = "btn-nav")
      )
    )
  }
  observeEvent(input$back3, { rv$page <- 3 })
  observeEvent(input$restart, {
    rv$page <- 1; rv$major <- NULL; rv$statuses <- list()
    rv$grades <- list(); rv$result <- NULL
  })
}

shinyApp(ui, server)
