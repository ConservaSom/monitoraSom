# ---------------------------------------------------------------------------
# Example-project download helper (CRAN-30 / CRAN-95).
#
# The runnable example project, the "basileuterus-culicivorus" walkthrough
# (14 WAVs, a ready `rois.duckdb`, the project marker and the walkthrough in
# three synchronized formats, ~39 MB), is too large to ship inside the
# package tarball. It is distributed instead as a single compressed bundle
# hosted on a PUBLIC release of the dedicated data repository. This file is
# the download helper that fetches that bundle into a folder on the user's
# machine and returns the example directory ready for `setwd()` /
# `project_path`, plus the check that reports whether the data is already
# there.
#
# CRAN Internet-resource policy (CRAN-95): a package that reaches the network
# must "fail gracefully with an informative message if the resource is not
# available or has changed (and not give a check warning nor error)". Every
# network path below is therefore wrapped so a failure surfaces as a
# `message()` and an invisible `NULL`, never a condition that `R CMD check`
# would flag. The helper only runs when asked: nothing in the package's
# examples, tests or vignette calls it unless the data is already there.
# ---------------------------------------------------------------------------

# The public release asset backing `fetch_example_data()`, live since
# 2026-08-29: published by the maintainer from the dedicated data repository
# (`ConservaSom/monitoraSom-example-data`, tag `example-v1`); the URL was
# verified end to end against the live asset. The `file://` override via
# `options(monitoraSom.data_url = ...)` remains the offline/mirror path.
.EXAMPLE_DATA_URL <- paste0(
  "https://github.com/ConservaSom/monitoraSom-example-data/releases/download/",
  "example-v1/monitoraSom-example-basileuterus-culicivorus.tar.gz"
)

# The example project directory: its name inside the bundle and, once
# unpacked, inside the cache. The bundle has NO wrapping directory (a hard
# `ep_tar()` contract in the data repository), so it untars straight into the
# cache and the example lands at `<cache>/<name>` — returned ready to use.
.EXAMPLE_DATA_NAME <- "basileuterus-culicivorus"

# The example-project inventory — every file the bundle must deliver for the
# example to be a runnable monitoraSom project: the 14 WAVs, `rois.duckdb`,
# the project marker and the walkthrough in its three synchronized formats.
# Paths are relative to the example directory. Kept as a single source of
# truth for both the post-download verification (what to expect after
# unpacking) and `is_example_data_cached()` (what to look for).
.EXAMPLE_DATA_FILES <- c(
  file.path("soundscapes", c(
    "W54393S25597_20201104_170000.wav",
    "W54431S25613_20191102_055000.wav",
    "W54431S25613_20191102_064000.wav",
    "W54431S25613_20191104_154000.wav",
    "W54431S25613_20191105_060000.wav",
    "W54443S25620_20191105_054000.wav",
    "W54448S25622_20191101_073000.wav",
    "W54448S25622_20191101_125000.wav",
    "W54448S25622_20191102_070000.wav",
    "W54448S25622_20191103_055000.wav",
    "W54448S25622_20191104_171000.wav",
    "W54448S25623_20191102_064000.wav"
  )),
  file.path("recordings", c("Bcu_1.wav", "Bcu_2.wav")),
  "rois.duckdb",
  "monitoraSom.proj",
  paste0("basileuterus-culicivorus", c(".qmd", ".Rmd", ".R"))
)

#' Resolve the example-data URL through a single indirection (CRAN-95).
#'
#' Reads `getOption("monitoraSom.data_url")` and falls back to the built-in
#' [.EXAMPLE_DATA_URL] constant. The indirection lets a user point at a mirror
#' or a local copy without editing the package, and makes a later URL/DOI
#' pin a one-line change.
#' @return A single character URL.
#' @noRd
.example_data_url <- function() {
  getOption("monitoraSom.data_url", default = .EXAMPLE_DATA_URL)
}

#' Per-user cache directory for the example data.
#'
#' Uses [tools::R_user_dir()] with the `"cache"` scope, the CRAN-sanctioned
#' location for downloaded package data. Not created here — callers create it
#' only when they are about to write.
#' @return A single character path (may not exist yet).
#' @noRd
.example_data_cache_dir <- function() {
  tools::R_user_dir("monitoraSom", which = "cache")
}

#' Path of the cached example project.
#'
#' Pure path arithmetic: joins the cache directory with the example name. The
#' directory itself may not exist yet on a machine that never fetched it.
#' @return A single character path (may not exist yet).
#' @noRd
.example_data_dir <- function() {
  file.path(.example_data_cache_dir(), .EXAMPLE_DATA_NAME)
}

#' Is the downloaded example data already cached?
#'
#' @description
#' Reports whether the example project (the
#' `basileuterus-culicivorus` walkthrough corpus: 14 WAVs, `rois.duckdb`, the
#' project marker and the walkthrough in three formats, kept outside the
#' package to stay within CRAN size limits) has already been downloaded to
#' the local cache by [fetch_example_data()]. It is a pure,
#' side-effect-free check: it touches no network and creates nothing.
#'
#' @details
#' Use it to guard optional example/vignette code so it runs only when the
#' data is present; this pattern keeps the package's checks network-free:
#' `if (is_example_data_cached()) { ... }`. The example lives under
#' [tools::R_user_dir()]`("monitoraSom", "cache")`, in the
#' `basileuterus-culicivorus` subdirectory that [fetch_example_data()]
#' returns.
#'
#' @return A logical scalar: `TRUE` if every expected file of the example
#'   project is present in the cache, `FALSE` otherwise.
#'
#' @seealso [fetch_example_data()] to populate the cache.
#' @export
#' @examples
#' # Non-destructive check; FALSE on a machine that has never fetched the data.
#' library(monitoraSom)
#' is_example_data_cached()
is_example_data_cached <- function() {
  example_dir <- .example_data_dir()
  files <- file.path(example_dir, .EXAMPLE_DATA_FILES)
  all(file.exists(files))
}

#' Download the monitoraSom example project into the local cache
#'
#' @description
#' Fetches the runnable example project (the `basileuterus-culicivorus`
#' walkthrough: 14 WAVs, a ready `rois.duckdb`, the project marker and the
#' walkthrough in three formats) and unpacks it into a per-user cache. It is
#' **opt-in**: you call it once, by hand; nothing in the package triggers a
#' download on its own.
#'
#' @details
#' The project is hosted as a single compressed bundle on a public release and
#' unpacked into [tools::R_user_dir()]`("monitoraSom", "cache")`, landing at
#' `<cache>/basileuterus-culicivorus`. A second call does nothing unless
#' `overwrite = TRUE`: the function returns early when
#' [is_example_data_cached()] is already `TRUE`.
#'
#' Following CRAN's Internet-resource policy, the function **fails gracefully**:
#' if the host is unreachable, the download times out, or the bundle is
#' malformed, it emits an informative [message()] and returns `NULL`
#' invisibly; it never errors or warns. Wrap dependent example code in
#' `if (is_example_data_cached())` so a machine with no network still checks
#' cleanly.
#'
#' The download URL is resolved through `getOption("monitoraSom.data_url", ...)`,
#' so you can point the function at a mirror or a local copy without editing the
#' package: `options(monitoraSom.data_url = "file:///path/to/bundle.tar.gz")`.
#'
#' @param overwrite Logical scalar. When `FALSE` (default) an existing cache is
#'   kept and the download is skipped; when `TRUE` the bundle is re-downloaded
#'   and the cached files are replaced.
#' @param timeout Positive numeric scalar: the download timeout in seconds
#'   (default `300`). Applied for the duration of the call and restored
#'   afterwards; the session-wide `getOption("timeout")` is never lowered.
#' @param quiet Logical scalar passed to [utils::download.file()]: when `FALSE`
#'   (default) a progress bar is shown, when `TRUE` the download is silent.
#'
#' @return The path of the example project directory (a single character
#'   string) on success, ready for `setwd()` or the `project_path` argument
#'   of the pipeline functions; `NULL` on any failure. Called for its side
#'   effect of populating the cache.
#'
#' @seealso [is_example_data_cached()] to test the cache without downloading.
#' @export
#' @examples
#' \dontrun{
#' # Fetch once; later calls are a no-op unless overwrite = TRUE.
#' example_project <- fetch_example_data()
#' setwd(example_project)  # or: launch_segmentation_app(project_path = ...)
#'
#' # Point at a local bundle instead of the public release (offline testing):
#' options(monitoraSom.data_url = "file:///tmp/monitoraSom-example-bcu.tar.gz")
#' fetch_example_data(overwrite = TRUE)
#' }
fetch_example_data <- function(overwrite = FALSE, timeout = 300, quiet = FALSE) {
  cache_dir <- .example_data_cache_dir()
  example_dir <- .example_data_dir()

  if (!isTRUE(overwrite) && is_example_data_cached()) {
    message("- Example project already cached at '", example_dir,
            "'. Pass overwrite = TRUE to re-download.")
    return(example_dir)
  }

  url <- .example_data_url()
  # A single tryCatch spans the whole network + unpack path: any failure below
  # becomes an informative message and an invisible NULL (CRAN-95), never a
  # condition R CMD check would flag.
  ok <- tryCatch({
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    }

    # Raise the download timeout only for this call, then restore it.
    old_timeout <- getOption("timeout")
    on.exit(options(timeout = old_timeout), add = TRUE)
    options(timeout = max(as.numeric(timeout), old_timeout))

    tmp <- tempfile(fileext = ".tar.gz")
    on.exit(unlink(tmp), add = TRUE)

    status <- utils::download.file(url, destfile = tmp, mode = "wb", quiet = quiet)
    if (!identical(status, 0L)) {
      stop("download returned status ", status, call. = FALSE)
    }

    # The bundle has no wrapping directory, so the example project unpacks
    # straight into <cache>/<example name> (see .EXAMPLE_DATA_NAME).
    utils::untar(tmp, exdir = cache_dir)

    missing <- .EXAMPLE_DATA_FILES[
      !file.exists(file.path(example_dir, .EXAMPLE_DATA_FILES))
    ]
    if (length(missing) > 0L) {
      stop("bundle is missing: ", paste(missing, collapse = ", "), call. = FALSE)
    }
    TRUE
  }, error = function(e) {
    message("- Could not fetch the example project from '", url, "': ",
            conditionMessage(e),
            "\n  The example data is optional; the package works without it.")
    FALSE
  })

  if (!isTRUE(ok)) return(invisible(NULL))

  message("- Example project downloaded to '", example_dir, "'.")
  example_dir
}
