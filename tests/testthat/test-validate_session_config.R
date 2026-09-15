# Contract tests for .validate_session_config (LSA-05 extraction).
# Covers the Phase-4 validation normalisations (LSA-08 uniform policy, LSA-09
# shared zoom ceiling, LSA-15 no trailing slashes) and the applied structural
# decisions: DuckDB roi_db (LSA-116/117), no roi_tables_path (LSA-117), no
# session_notes (LSA-03).
#
# Run from the project root:
#   Rscript -e "testthat::test_dir('R/refactored/tests/testthat')"

tmp_project <- function() {
  file.path(tempdir(), paste0("seg_proj_", as.integer(runif(1, 1, 1e7))))
}

# ── Defaults / structural decisions ────────────────────────────────────────────
test_that("valid defaults assemble session_data and create dirs", {
  p <- tmp_project()
  on.exit(unlink(p, recursive = TRUE))
  sd <- suppressWarnings(.validate_session_config(project_path = p, roi_user = "ana"))
  expect_equal(sd$roi_user, "ana")
  expect_true(dir.exists(file.path(p, "templates")))
  expect_true(dir.exists(file.path(p, "app_presets")))
  expect_true(dir.exists(sd$temp_path))
  # LSA-15: stored directory paths carry no trailing slash.
  expect_false(grepl("/$", sd$templates_path))
  expect_false(grepl("/$", sd$preset_path))
  expect_false(grepl("/$", sd$temp_path))
})

test_that("roi_user is required (CRAN item 7 F8/D3-a)", {
  p <- tmp_project()
  on.exit(unlink(p, recursive = TRUE))
  # Missing, NULL, blank and NA all stop with an actionable message; the old
  # "warn + NA + wait in-app" flow is replaced by the required gate.
  expect_error(.validate_session_config(project_path = p), "'roi_user' is required")
  expect_error(.validate_session_config(project_path = p, roi_user = NULL),
               "'roi_user' is required")
  expect_error(.validate_session_config(project_path = p, roi_user = "   "),
               "'roi_user' is required")
  expect_error(.validate_session_config(project_path = p, roi_user = NA_character_),
               "'roi_user' is required")
  expect_error(.validate_session_config(project_path = p, roi_user = c("a", "b")),
               "'roi_user' is required")
  # A valid name passes the gate.
  sd <- suppressWarnings(.validate_session_config(project_path = p, roi_user = "Silva J."))
  expect_equal(sd$roi_user, "Silva J.")
})

test_that("roi_db is a DuckDB path (LSA-116/117)", {
  p <- tmp_project()
  on.exit(unlink(p, recursive = TRUE))
  sd <- suppressWarnings(.validate_session_config(project_path = p, roi_user = "x"))
  expect_equal(sd$roi_db, file.path(p, "rois.duckdb"))

  sd2 <- suppressWarnings(.validate_session_config(roi_user = "x"))
  expect_equal(sd2$roi_db, "rois.duckdb")
  expect_warning(.validate_session_config(roi_user = "x"), "rois.duckdb")
})

test_that("dropped parameters are absent (LSA-117 CSV, LSA-03 session_notes)", {
  p <- tmp_project()
  on.exit(unlink(p, recursive = TRUE))
  sd <- suppressWarnings(.validate_session_config(project_path = p, roi_user = "x"))
  expect_null(sd$roi_tables_path)
  expect_null(sd$session_notes)
  expect_false("session_notes" %in% names(formals(.validate_session_config)))
  expect_false("roi_tables_path" %in% names(formals(.validate_session_config)))
})

# ── Validation policy (LSA-08 / LSA-09) ─────────────────────────────────────────
test_that("guide intervals: 0 = off, negative warns + default 0, bad type errors (LSA-08)", {
  p <- tmp_project()
  on.exit(unlink(p, recursive = TRUE))
  # 0 is the valid "guides off" value (no warning, no coercion)
  sd0 <- suppressWarnings(.validate_session_config(
    project_path = p, roi_user = "x",
    time_guide_interval = 0, freq_guide_interval = 0))
  expect_equal(sd0$time_guide_interval, 0)
  expect_equal(sd0$freq_guide_interval, 0)
  # defaults are also 0 (guides off)
  sd_def <- suppressWarnings(.validate_session_config(project_path = p, roi_user = "x"))
  expect_equal(sd_def$time_guide_interval, 0)
  expect_equal(sd_def$freq_guide_interval, 0)
  # negative -> default 0, with a warning
  expect_warning(
    sd_neg <- .validate_session_config(
      project_path = p, roi_user = "x",
      time_guide_interval = -5, freq_guide_interval = -2),
    "non-negative"
  )
  expect_equal(sd_neg$time_guide_interval, 0)
  expect_equal(sd_neg$freq_guide_interval, 0)
  # type/shape violation -> hard error
  expect_error(
    .validate_session_config(project_path = p, roi_user = "x",
                             time_guide_interval = "fast"),
    "time_guide_interval"
  )
  expect_error(
    .validate_session_config(project_path = p, roi_user = "x",
                             freq_guide_interval = c(1, 2)),
    "freq_guide_interval"
  )
})

test_that("zoom_freq shares the .MAX_ZOOM_FREQ_KHZ ceiling (LSA-09)", {
  p <- tmp_project()
  on.exit(unlink(p, recursive = TRUE))
  sd <- suppressWarnings(.validate_session_config(
    project_path = p, roi_user = "x", zoom_freq = c(0, 190)))
  expect_equal(sd$zoom_freq, c(0, 190))
  # configured up to the ceiling is accepted; above it errors
  sd2 <- suppressWarnings(.validate_session_config(
    project_path = p, roi_user = "x", zoom_freq = c(0, .MAX_ZOOM_FREQ_KHZ)))
  expect_equal(sd2$zoom_freq[2], .MAX_ZOOM_FREQ_KHZ)
  expect_error(.validate_session_config(project_path = p, roi_user = "x",
                                        zoom_freq = c(0, .MAX_ZOOM_FREQ_KHZ + 1)),
               "between 0 and")
})

test_that("zoom_time: initial time window validated, clamped at runtime", {
  p <- tmp_project()
  dir.create(p, recursive = TRUE)
  on.exit(unlink(p, recursive = TRUE))
  vc <- function(...) .validate_session_config(project_path = p, roi_user = "x", ...)
  # NULL default -> no zoom_time stored (runtime uses the 60 s auto window)
  expect_null(suppressWarnings(vc())$zoom_time)
  # continuous seconds: no rounding
  sd <- suppressWarnings(vc(zoom_time = c(12.345, 45.678)))
  expect_equal(sd$zoom_time, c(12.345, 45.678))
  # inverted -> sorted, with a warning
  expect_warning(
    sd2 <- vc(zoom_time = c(45, 12)),
    "Sorted 'zoom_time'"
  )
  expect_equal(sd2$zoom_time, c(12, 45))
  # shape/type/negative/zero-width violations hard-error
  expect_error(vc(zoom_time = 30), "zoom_time")
  expect_error(vc(zoom_time = c("a", "b")), "zoom_time")
  expect_error(vc(zoom_time = c(-5, 30)), "non-negative")
  expect_error(vc(zoom_time = c(10, 10)), "zero-width")
})

# ── Hard validation errors ─────────────────────────────────────────────────────
test_that("invalid scalar parameters error", {
  p <- tmp_project()
  dir.create(p, recursive = TRUE)
  on.exit(unlink(p, recursive = TRUE))
  vc <- function(...) .validate_session_config(project_path = p, roi_user = "x", ...)
  expect_error(vc(label_angle = 33), "label_angle")
  expect_error(vc(show_label = "yes"), "show_label")
  expect_error(vc(dyn_range = 0), "dyn_range")
  expect_error(vc(wl = 1000), "wl")
  expect_error(vc(ovlp = 15), "ovlp")
  expect_error(vc(color_scale = "rainbow"), "color_scale")
  expect_error(vc(pitch_shift = 3), "pitch_shift")
  expect_error(vc(nav_autosave = "y"), "nav_autosave")
})

# ── AUD-20: zoom_freq rounds to 0.1 kHz regardless of argument order ────────────
test_that("AUD-20: zoom_freq is rounded in BOTH branches", {
  p <- tmp_project(); dir.create(p, recursive = TRUE)
  on.exit(unlink(p, recursive = TRUE))
  vc <- function(...) .validate_session_config(project_path = p, roi_user = "x", ...)
  # In-order input rounds to 0.1.
  sd_in <- suppressWarnings(vc(zoom_freq = c(5.017, 10.123)))
  expect_equal(sd_in$zoom_freq, c(5.0, 10.1))
  # Out-of-order input (used to store sort() unrounded) rounds identically.
  sd_out <- suppressWarnings(vc(zoom_freq = c(10.123, 5.017)))
  expect_equal(sd_out$zoom_freq, c(5.0, 10.1))
})

# ── AUD-21: scalar/shape boundary checks (completes LSA-08) ─────────────────────
test_that("AUD-21: NA / vector / zero-width parameters hard-error", {
  p <- tmp_project(); dir.create(p, recursive = TRUE)
  on.exit(unlink(p, recursive = TRUE))
  vc <- function(...) .validate_session_config(project_path = p, roi_user = "x", ...)
  # NA logical no longer passes is.logical().
  expect_error(vc(show_label = NA), "show_label")
  expect_error(vc(visible_bp = NA), "visible_bp")
  # Vector logical / angle no longer collapse silently through all().
  expect_error(vc(play_norm = c(TRUE, FALSE)), "play_norm")
  expect_error(vc(label_angle = c(10, 20)), "label_angle")
  # Vector numeric no longer dies with a raw "length > 1" error.
  expect_error(vc(wl = c(1024, 2048)), "wl")
  # Zero-width ranges (equal endpoints) are rejected.
  expect_error(vc(dyn_range = c(0, 0)), "zero-width")
  expect_error(vc(zoom_freq = c(5, 5)), "zero-width")
})
