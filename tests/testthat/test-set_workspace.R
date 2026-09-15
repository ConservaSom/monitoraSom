# Tests for set_workspace (workspace bootstrap, flow #0; SW cycle).
# CRAN-lean subset: only the example_data = FALSE blocks are ported. The 7
# example_data = TRUE blocks (upstream test-set_workspace.R: SW-102 x4, SW-104,
# SW-03/04, AUD-17 rewrite) call .ws_load_dataset("ls_soundscapes"/"ls_recordings"),
# both 25 MB / 2.9 MB and evicted to Zenodo (CRAN-30/31), so they stay
# monorepo-only. The lean blocks assert the directory skeleton, the plain-text
# marker, the app_presets xlsx seeds, and the path helpers — none
# of which touch the example datasets.
#
# No data-seam override is needed here (upstream reassigned .ws_load_dataset /
# .load_roi_label_lists to read repo fixtures because the functions were sourced
# into globalenv). In the installed package the namespace resolves them directly:
# roi_label_lists ships in data/ and .load_roi_label_lists' requireNamespace +
# utils::data branch loads it; .ws_load_dataset is only reached on the excluded
# example_data = TRUE path.

suppressPackageStartupMessages({
  library(DBI); library(duckdb)
})

skip_if_deps_missing <- function() {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("duckdb")
  testthat::skip_if_not_installed("openxlsx")
  testthat::skip_if_not_installed("tuneR")
}

read_golden <- function(f) {
  utils::read.csv(test_path("golden/set_workspace", f), stringsAsFactors = FALSE)
}

# CRAN item 3 (§5): the v2 skeleton default is the essential tier only
# (DEC-3 removed match_scores from the default: created on demand).
EXPECTED_DIRS <- sort(c("app_presets", "templates"))

# Fresh empty project dir under tempdir(); never mutates committed fixtures.
new_project <- function() {
  p <- tempfile("sw_proj_"); dir.create(p); p
}

# =============================================================================
# SW-101 / SW-01 / SW-13 / SW-07 — directory skeleton anchored to project_path
# =============================================================================
test_that("SW-101: creates the essential skeleton (3 dirs) and no .duckdb (CRAN item 3)", {
  skip_if_deps_missing()
  p <- new_project(); on.exit(unlink(p, recursive = TRUE))
  res <- suppressMessages(set_workspace(project_path = p, example_data = FALSE))
  expect_equal(sort(list.dirs(p, recursive = FALSE, full.names = FALSE)),
               EXPECTED_DIRS)
  # SW-101: the skeleton must not pre-create any DuckDB file.
  expect_length(list.files(p, pattern = "\\.duckdb$", recursive = TRUE), 0L)
  # SW-12: documented return is a named logical (created flags), invisible.
  expect_type(res, "logical")
  expect_named(res)
  expect_true(all(res))
})

test_that("SW-07: v2 skeleton is the essential subset of the golden (CRAN item 3 divergence)", {
  skip_if_deps_missing()
  p <- new_project(); on.exit(unlink(p, recursive = TRUE))
  suppressMessages(set_workspace(project_path = p, example_data = FALSE))
  g <- read_golden("g1_dirs_only_tree.csv")
  golden_dirs <- sort(g$path[g$type == "dir" & g$path != "R"])  # drop usethis R/
  got_dirs <- sort(list.dirs(p, recursive = FALSE, full.names = FALSE))
  # Intentional divergence (CRAN item 3, §5 tier model): the original golden
  # recorded the 15-dir skeleton; the v2 default keeps only the essential trio.
  expect_true(all(got_dirs %in% golden_dirs))
  expect_equal(got_dirs, EXPECTED_DIRS)
})

test_that("SW-01: dirs are anchored to project_path, not getwd()", {
  skip_if_deps_missing()
  proj <- new_project(); work <- new_project()
  on.exit(unlink(c(proj, work), recursive = TRUE))
  old <- getwd(); setwd(work); on.exit(setwd(old), add = TRUE)
  suppressMessages(set_workspace(project_path = proj, example_data = FALSE))
  expect_true(dir.exists(file.path(proj, "templates")))           # fix of SW-01
  # Golden g5 recorded the bug (dirs leaked to workdir); the fix leaves it empty.
  expect_length(list.dirs(work, recursive = FALSE), 0L)
})

test_that("SW-01: rerun reports existing dirs (all FALSE) and is non-destructive", {
  skip_if_deps_missing()
  p <- new_project(); on.exit(unlink(p, recursive = TRUE))
  suppressMessages(set_workspace(project_path = p))
  res2 <- suppressMessages(set_workspace(project_path = p))
  expect_true(all(!res2))
})

# =============================================================================
# SW-103 — pure base R: required project_path + plain-text marker (no usethis)
# =============================================================================
test_that("SW-103: project_path is required and validated", {
  expect_error(set_workspace(project_path = NULL), "required")
  expect_error(set_workspace(project_path = tempfile("missing_")),
               "does not exist")
})

test_that("SW-103: writes monitoraSom.proj marker and NO usethis artifacts", {
  skip_if_deps_missing()
  p <- new_project(); on.exit(unlink(p, recursive = TRUE))
  suppressMessages(set_workspace(project_path = p, example_data = FALSE))
  marker <- file.path(p, "monitoraSom.proj")
  expect_true(file.exists(marker))
  lines <- readLines(marker)
  expect_match(lines[1], "monitoraSom project marker", fixed = TRUE)
  expect_true(any(grepl("^marker_version:", lines)))
  expect_true(any(grepl("^package: monitoraSom", lines)))
  # Divergence from golden g1 (had .Rproj / .gitignore / R/): none here.
  expect_false(file.exists(file.path(p, ".gitignore")))
  expect_length(list.files(p, pattern = "\\.Rproj$"), 0L)
})

test_that("SW-103: existing marker is not overwritten", {
  skip_if_deps_missing()
  p <- new_project(); on.exit(unlink(p, recursive = TRUE))
  suppressMessages(set_workspace(project_path = p))
  before <- readLines(file.path(p, "monitoraSom.proj"))
  expect_message(set_workspace(project_path = p), "marker ALREADY EXISTS")
  after <- readLines(file.path(p, "monitoraSom.proj"))
  expect_identical(before, after)
})

# =============================================================================
# SW-08 — writability probe
# =============================================================================
test_that("SW-08: a non-writable project_path errors up front", {
  skip_if_deps_missing()
  p <- new_project()
  Sys.chmod(p, "0500")
  # Skip where chmod cannot remove write access (e.g. running as root).
  if (file.access(p, mode = 2L) == 0L) {
    Sys.chmod(p, "0700"); unlink(p, recursive = TRUE)
    skip("project_path still writable after chmod (likely root); SW-08 not testable")
  }
  on.exit({ Sys.chmod(p, "0700"); unlink(p, recursive = TRUE) })
  expect_error(set_workspace(project_path = p), "not writable")
})

# =============================================================================
# SW-14 — app_presets seeded with BOTH editable defaults
# =============================================================================
test_that("SW-14: seeds roi_label_lists.xlsx AND roi_types.xlsx", {
  skip_if_deps_missing()
  p <- new_project(); on.exit(unlink(p, recursive = TRUE))
  suppressMessages(set_workspace(project_path = p, example_data = FALSE))
  presets <- file.path(p, "app_presets")
  expect_true(file.exists(file.path(presets, "roi_label_lists.xlsx")))
  # Golden g1 seeded only the label list; roi_types.xlsx is the SW-14 addition.
  expect_true(file.exists(file.path(presets, "roi_types.xlsx")))
  types <- as.character(openxlsx::read.xlsx(file.path(presets, "roi_types.xlsx"))[[1]])
  expect_gt(length(types[nzchar(types)]), 0L)
})

# =============================================================================
# NA-skip branch (behaviour preserved) — golden g4
# =============================================================================
test_that("NA path skips its directory; an explicit path creates it (golden g4)", {
  skip_if_deps_missing()
  p <- new_project(); on.exit(unlink(p, recursive = TRUE))
  suppressMessages(
    set_workspace(project_path = p, example_data = FALSE, soundscapes_path = NA))
  expect_false(dir.exists(file.path(p, "soundscapes")))
  # CRAN item 3: recordings/ is tier 4 — skipped by default, created on demand.
  expect_false(dir.exists(file.path(p, "recordings")))
  suppressMessages(
    set_workspace(project_path = p, recordings_path = "recordings"))
  expect_true(dir.exists(file.path(p, "recordings")))
})

# =============================================================================
# AUD-15 — path helpers: absoluteness detection + "~" expansion
# =============================================================================
test_that("AUD-15: .ws_is_absolute recognises POSIX / drive / UNC forms", {
  expect_true(.ws_is_absolute("/abs/path"))
  expect_true(.ws_is_absolute("C:/win/path"))
  expect_true(.ws_is_absolute("C:\\win\\path"))
  expect_true(.ws_is_absolute("\\\\server\\share"))
  expect_false(.ws_is_absolute("bare_name"))
  expect_false(.ws_is_absolute("nested/bare"))
})

test_that("AUD-15: .ws_anchor expands a leading ~ instead of anchoring a literal one", {
  anchored <- .ws_anchor("/some/project", "~/sound")
  # The "~/sound" argument lands in the home directory, not under project_path.
  expect_identical(anchored, path.expand("~/sound"))
  expect_false(grepl("/some/project", anchored, fixed = TRUE))
  # A bare name is still anchored under project_path.
  expect_identical(.ws_anchor("/some/project", "soundscapes"),
                   file.path("/some/project", "soundscapes"))
  # An absolute path is honoured as-is.
  expect_identical(.ws_anchor("/some/project", "/elsewhere/x"), "/elsewhere/x")
})

# =============================================================================
# AUD-16 — invalid dir arguments error up front, before any side effect
# =============================================================================
test_that("AUD-16: a vector directory argument errors and names the argument", {
  skip_if_deps_missing()
  p <- new_project(); on.exit(unlink(p, recursive = TRUE))
  expect_error(
    suppressMessages(set_workspace(project_path = p,
                                   soundscapes_path = c("a", "b"))),
    "soundscapes_path")
})

test_that("AUD-16: a NULL directory argument errors BEFORE any side effect", {
  skip_if_deps_missing()
  p <- new_project(); on.exit(unlink(p, recursive = TRUE))
  expect_error(
    suppressMessages(set_workspace(project_path = p, soundscapes_path = NULL)),
    "soundscapes_path")
  # No marker, no directory tree left behind (partial state).
  expect_length(list.files(p, all.files = TRUE, no.. = TRUE), 0L)
})

# =============================================================================
# AUD-17 — coverage: absolute-path branch, NA-skip return-vector semantics
#          (the example_data = TRUE rewrite block stays monorepo-only)
# =============================================================================
test_that("AUD-17: an absolute soundscapes_path is created outside project_path", {
  skip_if_deps_missing()
  p <- new_project(); ext <- file.path(tempfile("sw_ext_"), "external_sounds")
  on.exit(unlink(c(p, dirname(ext)), recursive = TRUE))
  res <- suppressMessages(
    set_workspace(project_path = p, soundscapes_path = ext))
  expect_true(dir.exists(ext))                       # created at the absolute path
  expect_false(dir.exists(file.path(p, "soundscapes")))
  expect_true(res[["soundscapes"]])                  # flag still named in return
})

test_that("AUD-17: an NA-skipped directory is ABSENT from the return vector", {
  skip_if_deps_missing()
  p <- new_project(); on.exit(unlink(p, recursive = TRUE))
  res <- suppressMessages(
    set_workspace(project_path = p, soundscapes_path = NA))
  expect_false("soundscapes" %in% names(res))        # absent, not FALSE
  expect_true("templates" %in% names(res))           # essential trio present
})
