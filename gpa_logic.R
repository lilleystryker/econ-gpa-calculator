# =============================================================================
# UCSB Economics Department - Pre-Major GPA Logic
# -----------------------------------------------------------------------------
# Pure calculation functions, no UI dependencies. Sourced by app.R (Shiny) and
# mirrored in the standalone HTML calculator.
#
# Policy reference (2025-2026 major sheets + Econ Undergraduate Advising):
#   * Pre-major GPA requirement: 2.85 minimum.
#   * GPA-counted courses:
#       - Economics .............. ECON 1, ECON 2, ECON 10A
#       - Economics & Accounting . ECON 1, ECON 2, ECON 3A, ECON 3B, ECON 10A
#   * MATH / WRIT / PSTAT are required pre-major courses but DO NOT factor
#     into the 2.85 GPA.
#   * Only courses taken at a UC campus count toward the 2.85 GPA. Non-UC
#     (transfer/AP) courses give credit but are excluded from the GPA.
#   * ECON 10A must be taken at UCSB.
#   * No grade below C is accepted in a pre-major course.
#   * ECON 5 may SUPPLEMENT the pre-major GPA, but only when including it
#     helps the student reach 2.85 (it can never lower / impede the GPA).
#     First attempt of ECON 5 only.
# =============================================================================

PREMAJOR_REQUIRED_GPA <- 2.85
MIN_PASSING_GRADE_PTS <- 2.0   # a "C"

# UC standard grade -> grade points -----------------------------------------
GRADE_POINTS <- c(
  "A+" = 4.0, "A" = 4.0, "A-" = 3.7,
  "B+" = 3.3, "B" = 3.0, "B-" = 2.7,
  "C+" = 2.3, "C" = 2.0, "C-" = 1.7,
  "D+" = 1.3, "D" = 1.0, "D-" = 0.7,
  "F"  = 0.0
)

# Letter grades in ascending GPA order (used to find the minimum letter grade
# that meets a numeric threshold). A+ and A both map to 4.0; we surface "A".
GRADE_ORDER <- c("F", "D-", "D", "D+", "C-", "C", "C+",
                 "B-", "B", "B+", "A-", "A")

# Status codes a course can have:
#   "ucsb"        - completed at UCSB                    (counts in GPA)
#   "uc_other"    - completed at another UC campus       (counts in GPA)
#   "non_uc"      - completed at a non-UC school / AP    (credit only, excluded)
#   "in_progress" - currently enrolled (no grade yet)    (target course)
#   "planned"     - not yet taken                        (target course)
COUNTS_IN_GPA <- c("ucsb", "uc_other")
REMAINING     <- c("in_progress", "planned")

# ---------------------------------------------------------------------------
# Course tables per major. Units come straight from the 2025-2026 major sheets
# (ECON 1+2 = 10 -> 5 each; ECON 3A-B = 10 -> 5 each; ECON 10A = 5).
# ---------------------------------------------------------------------------
premajor_courses <- function(major) {
  if (identical(major, "econ")) {
    data.frame(
      code      = c("ECON 1", "ECON 2", "ECON 10A"),
      units     = c(5, 5, 5),
      ucsb_only = c(FALSE, FALSE, TRUE),
      stringsAsFactors = FALSE
    )
  } else if (identical(major, "econ_acct")) {
    data.frame(
      code      = c("ECON 1", "ECON 2", "ECON 3A", "ECON 3B", "ECON 10A"),
      units     = c(5, 5, 5, 5, 5),
      ucsb_only = c(FALSE, FALSE, FALSE, FALSE, TRUE),
      stringsAsFactors = FALSE
    )
  } else {
    stop("Unknown major: ", major)
  }
}

# Map a numeric grade-point threshold to the minimum letter grade that meets
# or exceeds it. Returns NA if the threshold exceeds 4.0 (impossible).
min_letter_for_points <- function(threshold) {
  if (is.na(threshold)) return(NA_character_)
  if (threshold <= 0)   return(GRADE_ORDER[1])   # any passing grade works
  if (threshold > 4.0)  return(NA_character_)     # impossible
  for (g in GRADE_ORDER) {
    if (GRADE_POINTS[[g]] + 1e-9 >= threshold) return(g)
  }
  NA_character_
}

# Weighted GPA from a data.frame of completed courses (columns: units, points).
weighted_gpa <- function(units, points) {
  if (length(units) == 0 || sum(units) == 0) return(NA_real_)
  sum(units * points) / sum(units)
}

# ---------------------------------------------------------------------------
# Core evaluator.
#
# entries: a list keyed by course code. Each element is a list with:
#     status  - one of the status codes above
#     grade   - letter grade string (only used when status counts in GPA)
# econ5: optional list(grade = <letter>, first_attempt = TRUE/FALSE) describing
#     a UC-taken first-attempt ECON 5 grade available to supplement.
#
# Returns a list summarising the student's pre-major GPA standing.
# ---------------------------------------------------------------------------
evaluate_premajor <- function(major, entries, econ5 = NULL) {
  courses <- premajor_courses(major)
  warnings <- character(0)
  notes <- character(0)

  comp_units <- numeric(0); comp_points <- numeric(0); comp_codes <- character(0)
  rem_units  <- numeric(0); rem_codes  <- character(0)
  nonuc_codes <- character(0)
  below_c_codes <- character(0)

  for (i in seq_len(nrow(courses))) {
    code  <- courses$code[i]
    units <- courses$units[i]
    e <- entries[[code]]
    status <- if (is.null(e)) "planned" else e$status

    # ECON 10A residency rule.
    if (courses$ucsb_only[i] && status %in% c("uc_other", "non_uc")) {
      warnings <- c(warnings, paste0(
        code, " must be completed at UCSB. A grade from another school cannot ",
        "satisfy this requirement."))
    }

    if (status %in% COUNTS_IN_GPA) {
      g <- e$grade
      if (is.null(g) || is.na(g) || !(g %in% names(GRADE_POINTS))) {
        # Treated as not-yet-graded if no valid grade supplied.
        rem_units <- c(rem_units, units); rem_codes <- c(rem_codes, code)
      } else {
        pts <- GRADE_POINTS[[g]]
        comp_units <- c(comp_units, units)
        comp_points <- c(comp_points, pts)
        comp_codes <- c(comp_codes, code)
        if (pts < MIN_PASSING_GRADE_PTS) below_c_codes <- c(below_c_codes, code)
      }
    } else if (status %in% REMAINING) {
      rem_units <- c(rem_units, units); rem_codes <- c(rem_codes, code)
    } else if (status == "non_uc") {
      nonuc_codes <- c(nonuc_codes, code)
    }
  }

  current_gpa <- weighted_gpa(comp_units, comp_points)

  # ---- Econ 5 supplement (only when it helps) ----
  econ5_used <- FALSE
  econ5_units <- 5
  econ5_pts <- NA_real_
  gpa_with_e5 <- current_gpa
  if (!is.null(econ5) && !is.null(econ5$grade) && econ5$grade %in% names(GRADE_POINTS)) {
    if (isFALSE(econ5$first_attempt)) {
      notes <- c(notes, paste0(
        "Only the FIRST attempt of ECON 5 may supplement the pre-major GPA; ",
        "a repeat ECON 5 grade cannot be used."))
    } else {
      econ5_pts <- GRADE_POINTS[[econ5$grade]]
      cand <- weighted_gpa(c(comp_units, econ5_units), c(comp_points, econ5_pts))
      # Apply ECON 5 whenever it raises the GPA (it may only help, never hurt),
      # whether or not it is the deciding factor in reaching 2.85.
      if (!is.na(cand) && !is.na(current_gpa) && cand > current_gpa + 1e-9) {
        econ5_used <- TRUE
        gpa_with_e5 <- cand
      } else if (!is.na(cand) && is.na(current_gpa)) {
        # No completed designated courses yet; surface as informational only.
        gpa_with_e5 <- cand
      }
    }
  }

  effective_gpa <- if (econ5_used) gpa_with_e5 else current_gpa

  # ---- Required average across remaining (in-progress / planned) courses ----
  rem_total_units <- sum(rem_units)
  required_avg <- NA_real_
  required_letter <- NA_character_
  projected_gpa <- NA_real_
  required_avg_e5 <- NA_real_
  required_letter_e5 <- NA_character_

  comp_total_units <- sum(comp_units)
  comp_total_points <- sum(comp_units * comp_points)

  if (rem_total_units > 0) {
    target_units <- comp_total_units + rem_total_units
    needed_points <- PREMAJOR_REQUIRED_GPA * target_units - comp_total_points
    required_avg <- needed_points / rem_total_units
    required_letter <- min_letter_for_points(required_avg)
    if (!is.na(required_letter)) {
      ach <- GRADE_POINTS[[required_letter]]
      projected_gpa <- (comp_total_points + ach * rem_total_units) / target_units
    }

    # With a usable first-attempt Econ 5 grade, the requirement can ease.
    if (!is.null(econ5) && !is.null(econ5$grade) &&
        econ5$grade %in% names(GRADE_POINTS) && !isFALSE(econ5$first_attempt)) {
      e5p <- GRADE_POINTS[[econ5$grade]]
      target_units_e5 <- comp_total_units + econ5_units + rem_total_units
      needed_e5 <- PREMAJOR_REQUIRED_GPA * target_units_e5 -
        comp_total_points - e5p * econ5_units
      cand_avg <- needed_e5 / rem_total_units
      # Econ 5 may only help, never hurt the required grade.
      if (!is.na(cand_avg) && cand_avg < required_avg - 1e-9) {
        required_avg_e5 <- cand_avg
        required_letter_e5 <- min_letter_for_points(cand_avg)
      }
    }
  }

  # ---- Status determination ----
  met <- !is.na(effective_gpa) && effective_gpa >= PREMAJOR_REQUIRED_GPA
  outstanding <- rem_total_units > 0

  if (length(below_c_codes) > 0) {
    warnings <- c(warnings, paste0(
      "Grade below C in: ", paste(below_c_codes, collapse = ", "),
      ". A pre-major course must be passed with a C or higher; this course ",
      "would need to be retaken."))
  }
  if (length(nonuc_codes) > 0) {
    notes <- c(notes, paste0(
      "Non-UC credit (excluded from the GPA) for: ",
      paste(nonuc_codes, collapse = ", "),
      ". You receive course credit, but the 2.85 must be met with your ",
      "UC-taken pre-major courses."))
  }
  if (econ5_used) {
    notes <- c(notes, paste0(
      "Your ECON 5 grade was applied because it raises your pre-major GPA. ",
      "Confirm eligibility with the Economics Undergraduate Advising Office."))
  }

  list(
    major               = major,
    current_gpa         = current_gpa,
    effective_gpa       = effective_gpa,
    met                 = met,
    met_only_with_econ5 = econ5_used && met,
    has_remaining       = outstanding,
    remaining_courses   = rem_codes,
    completed_courses   = comp_codes,
    required_avg        = required_avg,
    required_letter     = required_letter,
    required_avg_econ5  = required_avg_e5,
    required_letter_econ5 = required_letter_e5,
    projected_gpa       = projected_gpa,
    impossible          = outstanding && is.na(required_letter) &&
                            !is.na(required_avg) && required_avg > 4.0,
    below_c_courses     = below_c_codes,
    nonuc_courses       = nonuc_codes,
    warnings            = warnings,
    notes               = notes
  )
}
