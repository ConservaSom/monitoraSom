#' Cross-platform parallel map backend (FSM-212)
#'
#' @description Reusable parallel `lapply` replacement that behaves uniformly on
#'   Windows, Linux and macOS. Extracted from `_detect_recorder.R` so it can be
#'   shared by `fetch_soundscape_metadata` now and `run_matching` later.
#'
#'   Backend selection when `ncores > 1` (in priority order):
#'   1. **`mirai`** (when installed) -> `mirai::daemons(ncores)` +
#'      `mirai::mirai_map()`. Identical on every OS and leak-free (daemons are
#'      torn down on exit). This is the path that closes FSM-212: the old
#'      `makePSOCKcluster` route leaked workers on Windows and was silently
#'      serial on POSIX.
#'   2. **POSIX `mclapply`** (no `mirai`, Unix) -> the previous behaviour kept as
#'      a fallback.
#'   3. **serial `lapply`** (no `mirai`, Windows, or `ncores <= 1`).
#'
#'   For an R programmer: think of `.par_map` as `lapply()` whose engine is
#'   chosen at runtime. `mirai` is the analogue of a `future`/`parallel` cluster,
#'   but the daemons are persistent background R processes addressed by message
#'   passing rather than re-forked per call.
#'
#' @keywords internal
#' @noRd

# TRUE when the optional `mirai` package can be loaded. `mirai` is an optional
# dependency (a future `Suggests`); the runtime gate is the optional-dep
# contract until a DESCRIPTION exists.
.mirai_available <- function() {
  requireNamespace("mirai", quietly = TRUE)
}

# Prime fresh `mirai` daemons so internal (unexported) functions referenced by
# name inside the mapped closure resolve on each worker. Returns TRUE when the
# workers were primed, FALSE when neither dev sources nor an installed package
# could be resolved (the caller then falls back to mclapply/serial rather than
# running a half-primed mirai path).
#
# Precedence note (deviation from the literal plan ordering, justified by the
# stale installed package, see project memory): the dev source dir is preferred
# over `library(monitoraSom)`. During development the installed monitoraSom is
# the OLD pre-refactor build, so loading it into workers would break parity.
# The dev/test harness sets `options(monitoraSom.src_dir = "R/refactored")`; at
# packaging time that option is unset and the installed package is used instead.
.worker_bootstrap <- function(files = NULL) {
  src_dir <- getOption("monitoraSom.src_dir", default = NULL)
  use_source <- !is.null(src_dir) && length(files) > 0L
  use_library <- !use_source && requireNamespace("monitoraSom", quietly = TRUE)
  if (!use_source && !use_library) return(FALSE)

  if (use_source) {
    mirai::everywhere(
      {
        for (.f in .files) {
          sys.source(file.path(.dir, .f), envir = globalenv())
        }
      },
      .args = list(.dir = src_dir, .files = as.character(files))
    )
  } else {
    mirai::everywhere(suppressMessages(library(monitoraSom)))
  }
  TRUE
}

# Convert a `mirai` worker-death error value into the same structured
# list(ok, record, error) shape `.safe_read_metadata` produces, so the caller's
# Filter(ok)/errs logic holds even when a daemon dies outside tryCatch.
.normalize_worker_result <- function(r, path) {
  is_err <- tryCatch(mirai::is_error_value(r),
                     error = function(e) inherits(r, c("errorValue", "miraiError")))
  if (!isTRUE(is_err)) return(r)
  msg <- tryCatch(conditionMessage(r), error = function(e) as.character(r))
  list(ok = FALSE, record = NULL, error = list(
    soundscape_path = path,
    error_class     = "worker_error",
    error_message   = msg,
    timestamp       = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ))
}

# Cross-platform parallel map (FSM-212). `worker_setup` is an optional character
# vector of source-file basenames used to prime mirai daemons (resolved against
# getOption("monitoraSom.src_dir")). Order-preserving, like lapply. `pb = TRUE`
# shows one tick per element of `x`: the serial path draws a base-R
# `utils::txtProgressBar`, the mirai path uses mirai's native collector bar
# `[mirai::.progress]`. The mclapply fallback stays bar-less, as the original
# pbapply bar did on PSOCK clusters. No new dependency: the serial bar is base R
# and the mirai bar ships with the already-present mirai engine.
.par_map <- function(x, f, ncores, worker_setup = NULL, pb = FALSE) {
  if (ncores <= 1) {
    if (!pb || length(x) == 0L) return(lapply(x, f))
    bar <- utils::txtProgressBar(min = 0, max = length(x), style = 3)
    on.exit(close(bar), add = TRUE)
    res <- vector("list", length(x))
    for (i in seq_along(x)) {
      res[[i]] <- f(x[[i]])
      utils::setTxtProgressBar(bar, i)
    }
    return(res)
  }

  if (.mirai_available()) {
    mirai::daemons(ncores)
    on.exit(mirai::daemons(0L), add = TRUE)
    primed <- if (is.null(worker_setup)) TRUE else
      isTRUE(.worker_bootstrap(worker_setup))
    if (primed) {
      # mirai's native collector bar `[mirai::.progress]` preserves the plain
      # `[]` collection semantics (worker-death errorValues flow through to
      # .normalize_worker_result; `.stop` early-stopping is deliberately NOT
      # used) while drawing a progress bar as tasks resolve. Verified available
      # and exported in mirai 2.7.1.
      res <- if (pb && length(x) > 0L) mirai::mirai_map(x, f)[mirai::.progress]
             else mirai::mirai_map(x, f)[]
      return(Map(.normalize_worker_result, res, x))
    }
    # Bootstrap could not resolve our sources: abandon the mirai path cleanly
    # (on.exit already tears the daemons down) and fall back below.
  }

  if (.Platform$OS.type == "unix") {
    if (ncores > parallel::detectCores()) {
      stop("ncores cannot exceed the number of available cores")
    }
    return(parallel::mclapply(x, f, mc.cores = ncores))
  }
  lapply(x, f)
}
