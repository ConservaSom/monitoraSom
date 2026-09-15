# Contract tests for _segmentation_setup.R (LSA-05 extraction).
# Covers LSA-19 (durable path values) and LSA-24 (HTML-escaped labels).
# .load_roi_label_lists is not unit-tested here: its branches need an XLSX
# fixture (openxlsx reads and writes) or the installed monitoraSom dataset.
# AUD-18: readxl/fs were dropped from the package (AUD-09/10); xlsx read-back in
# this file uses openxlsx (the writer's library), so neither is required.
#
# Run from the project root:
#   Rscript -e "testthat::test_dir('R/refactored/tests/testthat')"

library(testthat)

sdf <- function() {
  data.frame(
    soundscape_path = c("/a/REC.wav", "/b/REC.wav", "/c/X.wav"),
    soundscape_file = c("REC.wav", "REC.wav", "X.wav"),
    status = c("segmented", "no_soi", "unsegmented"),
    stringsAsFactors = FALSE
  )
}

test_that("choice values are durable paths, not basenames (LSA-19)", {
  ch <- .build_soundscape_choices(sdf())
  expect_equal(unname(ch), c("/a/REC.wav", "/b/REC.wav", "/c/X.wav"))
  # The two same-basename recordings map to distinct values.
  expect_equal(length(unique(unname(ch))), 3L)
})

test_that("labels carry status colours over the readable filename", {
  ch <- .build_soundscape_choices(sdf())
  labs <- names(ch)
  expect_match(labs[1], "#27ae60")          # segmented -> green span
  expect_match(labs[1], "REC.wav")
  expect_match(labs[2], "#e74c3c")          # no_soi -> red span
  expect_equal(labs[3], "X.wav")            # unsegmented -> plain label
})

test_that("missing status colour falls back to a plain label", {
  df <- sdf()
  df$status <- c("weird", "segmented", "unsegmented")
  labs <- names(.build_soundscape_choices(df))
  expect_equal(labs[1], "REC.wav")          # unknown status -> no span
})

test_that("filenames are HTML-escaped in labels (LSA-24)", {
  skip_if_not_installed("htmltools")
  df <- data.frame(
    soundscape_path = c("/a/<b>&.wav", "/c/<i>.wav"),
    soundscape_file = c("<b>&.wav", "<i>.wav"),
    status = c("segmented", "unsegmented"),   # one coloured, one plain branch
    stringsAsFactors = FALSE
  )
  labs <- names(.build_soundscape_choices(df))
  # markup metacharacters (< > &) are escaped in both the span and plain branch,
  # so raw tags cannot reach the selectize HTML renderer
  expect_match(labs[1], "&lt;b&gt;&amp;\\.wav")
  expect_false(grepl("<b>", labs[1], fixed = TRUE))
  expect_match(labs[2], "&lt;i&gt;\\.wav")
  expect_false(grepl("<i>", labs[2], fixed = TRUE))
  # values (selectize keys) keep the raw, unescaped path
  expect_equal(unname(.build_soundscape_choices(df)), c("/a/<b>&.wav", "/c/<i>.wav"))
})

# ── .load_roi_type_list (LSA-101 custom ROI type vocabulary) ───────────────────
test_that("ROI type list falls back to the built-in vocabulary", {
  # NULL project -> built-in
  expect_identical(.load_roi_type_list(NULL), .ROI_TYPE_CHOICES)
  # project dir without a preset file -> built-in
  tmp <- tempfile("lsa101_")
  dir.create(file.path(tmp, "app_presets"), recursive = TRUE)
  expect_identical(.load_roi_type_list(tmp), .ROI_TYPE_CHOICES)
})

test_that("ROI type list reads the project preset's first column", {
  skip_if_not_installed("openxlsx")
  tmp <- tempfile("lsa101_")
  dir.create(file.path(tmp, "app_presets"), recursive = TRUE)
  preset <- file.path(tmp, "app_presets", "roi_types.xlsx")
  # first column = type names (incl. a blank and a duplicate to be cleaned)
  openxlsx::write.xlsx(
    data.frame(
      roi_type = c("whistle", "trill", "", "whistle", "buzz"),
      stringsAsFactors = FALSE
    ),
    preset
  )
  out <- .load_roi_type_list(tmp)
  expect_equal(out, c("whistle", "trill", "buzz"))  # blank dropped, de-duped
})

# ── .blank_plot_message (A2 bad-WAV graceful render) ───────────────────────────
test_that("blank plot message returns a ggplot carrying the message", {
  skip_if_not_installed("ggplot2")
  msg <- "Could not read this recording:\nbad magic"
  p <- .blank_plot_message(msg)
  expect_s3_class(p, "ggplot")
  labels <- vapply(
    p$layers,
    function(ly) tryCatch(
      as.character(ly$aes_params$label), error = function(e) NA_character_
    ),
    character(1)
  )
  expect_true(msg %in% labels)
})

test_that("blank plot message rejects non-scalar / non-character input", {
  expect_error(.blank_plot_message(c("a", "b")))
  expect_error(.blank_plot_message(42))
})

# ── .seed_app_presets (A6 auto-seed editable default presets) ──────────────────
test_that("seed app presets is a no-op when preset_dir is NULL", {
  res <- .seed_app_presets(NULL)
  expect_false(any(res))
})

test_that("seed app presets writes both files and they read back", {
  skip_if_not_installed("openxlsx")
  # the labels seed resolves the bundled dataset via .load_roi_label_lists(),
  # which finds `roi_label_lists` in the loaded package under test_local.
  tmp <- tempfile("seed_")
  res <- .seed_app_presets(tmp)
  expect_true(res[["roi_types"]])
  expect_true(res[["roi_label_lists"]])
  expect_true(file.exists(file.path(tmp, "roi_types.xlsx")))
  expect_true(file.exists(file.path(tmp, "roi_label_lists.xlsx")))
  # the seeded types file round-trips through the loader to the built-in vocab
  proj <- tempfile("seedproj_")
  dir.create(file.path(proj, "app_presets"), recursive = TRUE)
  file.copy(
    file.path(tmp, "roi_types.xlsx"),
    file.path(proj, "app_presets", "roi_types.xlsx")
  )
  expect_equal(.load_roi_type_list(proj), unique(.ROI_TYPE_CHOICES))
})

test_that("seed app presets never overwrites an existing preset", {
  skip_if_not_installed("openxlsx")
  tmp <- tempfile("seed_")
  dir.create(tmp, recursive = TRUE)
  types_file <- file.path(tmp, "roi_types.xlsx")
  openxlsx::write.xlsx(data.frame(roi_type = "custom_only"), types_file)
  res <- .seed_app_presets(tmp)
  expect_false(res[["roi_types"]])          # existing file left alone
  expect_true(res[["roi_label_lists"]])     # missing file still seeded
  expect_equal(as.character(openxlsx::read.xlsx(types_file)[[1]]), "custom_only")
})

# ── .validate_soundscape_table (LSA-203 table-guided mode) ─────────────────────

# Helper: create N real (empty) files so file.exists() passes; returns paths.
.make_files <- function(n, ext = ".wav") {
  vapply(seq_len(n), function(i) {
    p <- tempfile(sprintf("lsa203_%d_", i), fileext = ext)
    file.create(p)
    normalizePath(p, winslash = "/")
  }, character(1))
}

test_that("row order is preserved verbatim (a shuffled table stays shuffled)", {
  paths <- .make_files(5)
  shuffled <- paths[c(4, 1, 5, 2, 3)]
  df <- data.frame(
    soundscape_path = shuffled,
    soundscape_file = basename(shuffled),
    stringsAsFactors = FALSE
  )
  out <- .validate_soundscape_table(df)
  expect_equal(out$soundscape_path, shuffled)   # no sorting applied
})

test_that("minimum contract: soundscape_file derived from basename when absent", {
  paths <- .make_files(2)
  df <- data.frame(soundscape_path = paths, stringsAsFactors = FALSE)
  out <- .validate_soundscape_table(df)
  expect_equal(names(out), c("soundscape_path", "soundscape_file"))
  expect_equal(out$soundscape_file, basename(paths))
})

test_that("duplicate soundscape_path rows are dropped (first kept) with a warning", {
  paths <- .make_files(2)
  df <- data.frame(
    soundscape_path = c(paths[1], paths[2], paths[1]),
    stringsAsFactors = FALSE
  )
  expect_warning(out <- .validate_soundscape_table(df), "duplicate")
  expect_equal(out$soundscape_path, c(paths[1], paths[2]))  # first occurrence
})

test_that("missing files are a hard error naming the offenders (Q1)", {
  paths <- .make_files(1)
  ghost <- "/no/such/dir/missing_REC.wav"
  df <- data.frame(
    soundscape_path = c(paths[1], ghost), stringsAsFactors = FALSE
  )
  expect_error(.validate_soundscape_table(df), "missing_REC.wav", fixed = TRUE)
})

test_that("non-data.frame, missing column and zero-row inputs error", {
  expect_error(.validate_soundscape_table(list(a = 1)), "data.frame")
  expect_error(
    .validate_soundscape_table(data.frame(x = 1)), "soundscape_path"
  )
  expect_error(
    .validate_soundscape_table(
      data.frame(soundscape_path = character(0), stringsAsFactors = FALSE)
    ),
    "no rows"
  )
})

# ── .read_soundscape_table (LSA-203 store reader, Q6) ──────────────────────────

test_that("CSV store round-trips preserving physical row order", {
  paths <- .make_files(3)
  ordered <- paths[c(3, 1, 2)]
  csv <- tempfile("lsa203_store_", fileext = ".csv")
  utils::write.csv(
    data.frame(soundscape_path = ordered, soundscape_file = basename(ordered),
               stringsAsFactors = FALSE),
    csv, row.names = FALSE
  )
  df <- .read_soundscape_table(csv)
  expect_equal(df$soundscape_path, ordered)     # CSV keeps the custom order
  # composes with the validator
  expect_equal(.validate_soundscape_table(df)$soundscape_path, ordered)
})

test_that("a nonexistent store path errors", {
  expect_error(.read_soundscape_table("/no/such/store.csv"), "not found")
})

# ── .channel_choices (item 6 channel dropdown) ─────────────────────────────────

test_that("channel choices are L/R for mono/stereo, 1..N for multichannel", {
  skip_if_not_installed("tuneR")
  mono <- tuneR::Wave(left = 1:10, samp.rate = 1000, bit = 16)
  expect_equal(.channel_choices(mono), c(L = "left"))
  stereo <- tuneR::Wave(left = 1:10, right = 1:10, samp.rate = 1000, bit = 16)
  expect_equal(.channel_choices(stereo), c(L = "left", R = "right"))
  # multichannel (WaveMC): tokens are 1..N (forward-compat, inert until 4-ch)
  mc <- methods::new("WaveMC",
    .Data = matrix(0, nrow = 10, ncol = 4), samp.rate = 1000, bit = 16
  )
  expect_equal(.channel_choices(mc),
               c(`1` = "1", `2` = "2", `3` = "3", `4` = "4"))
})

test_that("DuckDB store is read back as a df_soundscapes table", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  paths <- .make_files(2)
  store <- tempfile("lsa203_", fileext = ".duckdb")
  con <- .duckdb_connect(store)
  .duckdb_upsert_metadata(con, .records_to_schema(list(
    list(soundscape_path = paths[1], soundscape_file = basename(paths[1])),
    list(soundscape_path = paths[2], soundscape_file = basename(paths[2]))
  )))
  DBI::dbDisconnect(con, shutdown = TRUE)
  df <- .read_soundscape_table(store)
  expect_true(all(paths %in% df$soundscape_path))
  expect_equal(.validate_soundscape_table(df)$soundscape_file, basename(paths))
})
