# Tests for the Raven / Audacity annotation importers (FEAT-05).
#
# Run from the project root:
#   Rscript -e "testthat::test_file('R/refactored/tests/testthat/test-import_annotations.R')"

fx <- system.file("extdata", "import_annotations", package = "monitoraSom")
raven_fixture <- file.path(fx, "raven_selection.txt")
audacity_fixture <- file.path(fx, "audacity_labels.txt")

# --- Raven reader --------------------------------------------------------

test_that("read_raven_selection maps to the canonical ROI schema", {
  rois <- read_raven_selection(raven_fixture, soundscape_path = "recs/REC.wav")
  expect_true(all(names(.roi_schema_spec()) %in% names(rois)))
  # The Waveform/Spectrogram duplicate of selection 1 collapses to one row.
  expect_equal(nrow(rois), 2L)
  # Hz -> kHz on the spectrogram row.
  expect_equal(rois$roi_min_freq, c(2.0, 1.5))
  expect_equal(rois$roi_max_freq, c(6.5, 4.2))
  expect_equal(rois$roi_start, c(0.5, 3.1))
  # Channel 1 -> left, 2 -> right.
  expect_equal(rois$roi_channel, c("left", "right"))
  expect_equal(rois$roi_label, c("Myiothlypis flaveola", "Basileuterus culicivorus"))
  expect_true(all(rois$roi_source == "import_raven"))  # F3: rich origin stamp
  # Begin File is honoured over the argument for the file name.
  expect_true(all(rois$soundscape_file == "REC.wav"))
})

# --- Audacity reader -----------------------------------------------------

test_that("read_audacity_labels handles standard + frequency-continuation lines", {
  rois <- read_audacity_labels(audacity_fixture, soundscape_path = "recs/REC.wav")
  expect_true(all(rois$roi_source == "import_audacity"))  # F3: rich origin stamp
  expect_true(all(names(.roi_schema_spec()) %in% names(rois)))
  expect_equal(nrow(rois), 2L)
  expect_equal(rois$roi_label, c("whistle", "buzz"))
  expect_equal(rois$roi_start, c(0.5, 3.1))
  # The first label carries a frequency band (Hz -> kHz); the second does not.
  expect_equal(rois$roi_min_freq, c(2.0, NA))
  expect_equal(rois$roi_max_freq, c(6.5, NA))
  expect_true(all(rois$soundscape_file == "REC.wav"))
})

# --- round-trip ----------------------------------------------------------

test_that("Raven read -> write -> read preserves the key fields", {
  rois <- read_raven_selection(raven_fixture, soundscape_path = "recs/REC.wav")
  tmp <- tempfile(fileext = ".txt"); on.exit(unlink(tmp))
  write_raven_selection(rois, tmp)
  back <- read_raven_selection(tmp)
  expect_equal(back$roi_start, rois$roi_start)
  expect_equal(back$roi_end, rois$roi_end)
  expect_equal(back$roi_min_freq, rois$roi_min_freq)
  expect_equal(back$roi_max_freq, rois$roi_max_freq)
  expect_equal(back$roi_channel, rois$roi_channel)
  expect_equal(back$roi_label, rois$roi_label)
})

test_that("Audacity read -> write -> read preserves the key fields", {
  rois <- read_audacity_labels(audacity_fixture)
  tmp <- tempfile(fileext = ".txt"); on.exit(unlink(tmp))
  write_audacity_labels(rois, tmp)
  back <- read_audacity_labels(tmp)
  expect_equal(back$roi_start, rois$roi_start)
  expect_equal(back$roi_end, rois$roi_end)
  expect_equal(back$roi_label, rois$roi_label)
  expect_equal(back$roi_min_freq, rois$roi_min_freq)
  expect_equal(back$roi_max_freq, rois$roi_max_freq)
})

# --- guards --------------------------------------------------------------

test_that("readers reject a missing / non-scalar path", {
  expect_error(read_raven_selection(tempfile()), "existing file path")
  expect_error(read_audacity_labels(c("a", "b")), "existing file path")
})

test_that("read_raven_selection errors on a table missing the time columns", {
  bad <- tempfile(fileext = ".txt"); on.exit(unlink(bad))
  writeLines(paste("Selection", "Annotation", sep = "\t"), bad)
  expect_error(read_raven_selection(bad), "missing required column")
})
